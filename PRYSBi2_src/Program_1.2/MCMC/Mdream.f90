module Mdream

	!------------------------------------------------------------------------------------------------
	! In this module, one of MCMC methods (DREAM:Vrugt et al. 2009) is condcted.
	!
	! This module is needed for MCMC calculation, but simple simulation also uses
	! this subroutine in this program.
	!
	! When using this program as simple simulation, each MCMC step corespond to each ensemble.
	! In the simple simulation, the parameter values do not change throughout ensembles (MCMC steps).
	!
	! The subroutine is an important component in this program, in which subroutine "calc_llk" is
	! repeatedly called.
	!
	! Vrught et al. (2009) International Journal of Nonlinear Sciences and Numerical Simulation
	!-------------------------------------------------------------------------------------------------

	use Mdream_param   !!! Should be changed !!!
	use Mrunif
	use Mrnorm
	use Mrmulti
	use Mread_data     !!! Should be changed !!!
	use Mcalc_llk      !!! Should be changed !!!
	use Mgelman_rubin
	use Moutput_param
	use Moutput_est
	use Mmodel_setting !!! Should be changed !!!

	implicit none

	private
	public :: dream
	contains

	subroutine dream

		!----- Read Data -----
		call read_data

		!----- initial setting -----
		dream_pm(:) = 1d0/dble(dream_ncr)
		dream_dm(:) = 1d0

		call rmulti( temp_weight(1:dream_ncr), temp_i(1:dream_nchain) )
		dream_cr(:) = dble( temp_i(1:dream_nchain) ) / dble(dream_ncr)

		!----- Prepare the file for output -----
		if( dream_simulate_version /= 3 ) then
			call output_param(1)
		endif
		call output_est(1)

		!----- Model setting -----
		call model_setting

		!----- Start MCMC -----
		do 1111 i_step = 1,  dream_nstep  !!! For each MCMC step !!!


			if( i_step == 1 ) then

				do i_chain = 1, dream_nchain
					do i_dim = 1, dream_ndim

						temp_r(1) = dream_x_lim(i_dim, 1)
						temp_r(2) = dream_x_lim(i_dim, 2)

						if( dream_x_typeprior(i_dim) == 0 ) then
							call runif( temp_r(1), temp_r(2), temp_r(3) )
							dream_x( i_dim, i_chain ) = temp_r(3)
						end if

						if( dream_x_typeprior(i_dim) == 1 ) then
							do
								temp_r(4) = dream_x_avprior(i_dim)
								temp_r(5) = dream_x_sdprior(i_dim)
								call rnorm( temp_r(4), temp_r(5), temp_r(3) )

								if( temp_r(1) < temp_r(3) .and. temp_r(3) < temp_r(2) ) then
									exit
								end if
							end do
							dream_x( i_dim, i_chain ) = temp_r(3)
						endif

					end do
				end do

			end if


			if( i_step == 1 ) then

				do i_chain = 1, dream_nchain
					temp_x = dream_x( :, i_chain )
					call calc_llk( temp_x, &
					 			 & dream_x_typeprior, dream_x_avprior, dream_x_sdprior, &
					 			 & dream_x_llk(i_chain), i_chain, dream_est_x( :, :, i_chain) )
				end do

			end if


			do 2222 i_chain = 1, dream_nchain !!! For each chain !!!

				dream_diff(:) = 0d0
				do i_delta = 1, dream_delta
					do
						call rmulti( temp_weight(1:dream_nchain), temp_i(1:2) )

						if( temp_i(1) /= temp_i(2) ) then
							if( temp_i(1) /= i_chain ) then
								if( temp_i(2) /= i_chain ) exit
							end if
						end if
					end do

					temp_r( 1:dream_ndim ) = &
					& dream_x( :, temp_i(1) ) - dream_x( :, temp_i(2) )

					do i_dim = 1, dream_ndim
						dream_diff(i_dim) = dream_diff(i_dim) + temp_r(i_dim)
					end do

				end do


				temp_r(1) = dream_cr(i_chain)
				do i_dim = 1, dream_ndim
					call runif( 0d0, 1d0, temp_r(2) )
					if( temp_r(1) < temp_r(2) ) then
						dream_change(i_dim) = 1
					else
						dream_change(i_dim) = 0
					end if
				end do
				dream_ddash = dble( sum( dream_change ) )


				do 3333 i_dim = 1, dream_ndim

					if( dream_change(i_dim) == 1 ) then

						call runif( -dream_b, dream_b, temp_r(1) )
						call rnorm( 0d0, dream_bdash, temp_r(2) )

						dream_z( i_dim, i_chain ) = dream_x( i_dim, i_chain ) + &
						& (1d0 + temp_r(1)) * 2.38d0/sqrt(2d0*dream_delta*dream_ddash) * &
						& dream_diff(i_dim) + temp_r(2)

					end if

					if( dream_change(i_dim) == 0) then
						dream_z( i_dim, i_chain ) = dream_x( i_dim, i_chain )
					end if

				3333 continue ! from i_dim = 1 to ndim


				dream_inlimit(:) = 1
				do i_dim = 1, dream_ndim
					temp_r(1) = dream_x_lim(i_dim, 1)
					temp_r(2) = dream_x_lim(i_dim, 2)
					temp_r(3) = dream_z( i_dim, i_chain )
					if( temp_r(1) < temp_r(3) .and. temp_r(3) < temp_r(2) ) then
						dream_inlimit(i_dim) = 0
					end if
				end do


				if( sum( dream_inlimit(:) ) == 0 .or. dream_simulate_version == 3) then

					temp_x = dream_z( :, i_chain )
					call calc_llk( temp_x, &
								 & dream_x_typeprior, dream_x_avprior, dream_x_sdprior, &
								 & dream_z_llk(i_chain), i_chain, dream_est_z( :, :, i_chain) )

					dream_alpha( i_chain ) = &
					& min( exp( dream_z_llk(i_chain) - dream_x_llk(i_chain) ), 1d0 )

				end if

				if( sum( dream_inlimit(:) ) /= 0 .and. dream_simulate_version /= 3) then

					dream_z_llk(i_chain) = -99999999d0
					dream_est_z( :, :, i_chain) = -9999d0

					dream_alpha( i_chain ) = 0d0

				endif

				call runif( 0d0, 1d0, temp_r(1) )
				if( dream_alpha( i_chain ) > temp_r(1) ) then
					dream_x( :, i_chain ) = dream_z( :, i_chain )
					dream_x_llk(i_chain) = dream_z_llk(i_chain)
					dream_est_x( :, :, i_chain ) = dream_est_z( :, :, i_chain )
				end if

				! Save
				dream_x_all(:,:,i_step) = dream_x
				dream_x_llk_all( :, i_step ) = dream_x_llk(:)
				dream_est_x_out( :, :, i_chain ) = dream_est_x( :, :, i_chain )

			2222 continue ! from i_chain = 1 to nchain



			if( i_step <= dream_nburn ) then
				temp_i(1) = i_step / 2 + 1
				temp_i(2) = i_step - temp_i(1) + 1
				do i_chain = 1, dream_nchain
					dream_ohm( i_chain ) = &
					& sum( dream_x_llk_all( i_chain, temp_i(1):i_step ) )  / dble( temp_i(2) )
				end do

				temp_r(1) = sum( dream_ohm(:) ) / dble( dream_nchain ) ! The mean of ohm
				temp_r(2) = sum( ( dream_ohm(:) - temp_r(1) )**2 ) / dble( dream_nchain )
				temp_r(3) = sqrt( temp_r(2) ) ! S.D. of ohm
				dream_iqr = 1.349d0 * temp_r(3) ! IQR (assuming normal distribution)
				dream_q1  = - 0.674d0 * temp_r(3) + temp_r(1) ! Q1 of ohm
				dream_minllk_loc = minloc(dream_ohm, dim=1)
			end if


			if( i_step <= dream_nburn ) then
				do i_chain = 1, dream_nchain
					if( dream_ohm( i_chain ) < dream_q1 - 2d0*dream_iqr ) then
						dream_x( :, i_chain ) = dream_x( :, dream_minllk_loc )
						dream_x_llk(i_chain) = dream_x_llk(dream_minllk_loc)
						dream_est_x( :, :, i_chain ) = dream_est_x( :, :, dream_minllk_loc )
					end if
				end do
			end if

			if( i_step /= 1 ) then
				do i_chain = 1, dream_nchain
					call rmulti( dream_pm(:), temp_si )
					dream_cr( i_chain ) = dble(temp_si(2)) / dble(dream_ncr)

					do i_dim = 1, dream_ndim
						temp_rr(1) = sum( dream_x_all(i_dim,:,i_step) / dble(dream_nchain) )
						temp_r(i_dim) = sum( ( dream_x_all(i_dim,:,i_step) - temp_rr(1) )**2 ) / &
						& dble(dream_nchain)
						temp_r(i_dim) = sqrt(temp_r(i_dim))
					end do

					dream_dm( temp_si(2) ) = dream_dm( temp_si(2) ) + &
					& sum( ( dream_x_all(:,i_chain,i_step) - &
					& dream_x_all(:,i_chain,i_step-1) )**2 / temp_r(1:dream_ndim)**2 )

					dream_pm( temp_si(2) ) = dream_dm( temp_si(2) ) / sum( dream_dm(:) )

				end do
			end if


			temp_i(1) = i_step - dream_nburn
			if( temp_i(1) >= dream_minstep ) then
				if( mod(i_step, dream_gelman_interval) == 0 ) then
					temp_i(2) = i_step - dream_nburn ! The length of current MCMC
					temp_i(3) = temp_i(2) / 2 + dream_nburn + 1 ! The start step of Gelman-Rubin
					do i_dim = 1, dream_ndim
						temp_gelman(:,1:(i_step-temp_i(3)+1)) = dream_x_all( i_dim, :, temp_i(3):i_step )
						call gelman_rubin( temp_gelman(:,1:(i_step-temp_i(3)+1)) , dream_rhat(i_dim) )
					enddo
				end if
			else
				dream_rhat(:) = -9999d0
			end if
			dream_rhat_all( : ) = dream_rhat(:)


			dream_stop(:) = 1
			if( temp_i(1) > dream_minstep ) then

				do i_dim = 1, dream_ndim

					if( dream_rhat(i_dim) /= -9999d0 .and. dream_rhat(i_dim) < 1.1d0 ) then
						dream_stop(i_dim) = 0
					end if

				end do
				if( sum( dream_stop(:) ) == 0 ) then
					write(*,"('Gelman-Rubin diagnostics reached below 1.1')")
					write(*,"('Step:',i8)") i_step
					write(*,*) '...End <<<'
					goto 123
				end if

			end if


			!--- Output -----------------------------------
			if( mod( i_step, dream_param_interval ) == 0 .and. dream_simulate_version == 2 ) then
				call output_param(2)
			end if

			if( mod( i_step, dream_est_interval ) == 0 ) then
				call output_est(2)
			end if

			if( dream_simulate_version /= 2 ) then
				call output_est(2)
			end if
			!--------------------------------------------------

		1111 continue ! from i_step = 1 to nstep

		123	if(myrank==1 .and. dream_simulate_version/=3) write(*,*) 'End one grid'

	end subroutine

end module Mdream
