module path_kdata

	use var_read_all
	use var_divide

	implicit none

	contains

	subroutine s_path_kdata

		implicit none

		integer :: istatus(temp_mpi_status_size)
		integer :: icore

		! Allocation (for each core)
		if( myrank /= 0 ) then
			eachgrd = mpi_num_grid(myrank)
			allocate( eachdata_topo      (1:nvar_topo, 1:eachgrd) )
			allocate( eachdata_gcode     (1:eachgrd) )
			allocate( eachdata_weather   (1:ntime, 1:nvar_weather, 1:eachgrd) )
			allocate( eachdata_yield     (iyrstt:iyrend, 8, 1:eachgrd) )
			allocate( eachdata_soil      (1:nsol, 1:nvar_soil, 1:eachgrd) )
			allocate( eachdata_pheno     (1:nvar_pheno, 1:eachgrd) )
			allocate( eachdata_co2       (iyrstt:iyrend, 1:eachgrd) )
      allocate( eachdata_param     (1:nensemble, 1:nvar_param, 1:eachgrd) )
      allocate( eachdata_fix_param (1:nvar_fix_param, 1:eachgrd) )
      allocate( eachdata_gdp (iyrstt:iyrend, 1:eachgrd) )
		endif

		! Send topology data to each core
		if( myrank==0 ) then
			do icore = 1, nprocs-1
				call mpi_send( kdata_topo( :, mpi_knum(1, icore) ), mpi_num_grid(icore)*nvar_topo, temp_mpi_real, &
					& icore, 1, temp_mpi_comm_world, ierr )
			enddo
		endif
		if( myrank/=0 ) then
			call mpi_recv( eachdata_topo(:, :), eachgrd*nvar_topo, temp_mpi_real, &
				& 0, 1, temp_mpi_comm_world, istatus, ierr )
		endif
		call mpi_barrier(temp_mpi_comm_world, ierr)

		! Send grid code number to each core
		if( myrank==0 ) then
			do icore = 1, nprocs-1
				call mpi_send( kdata_gcode( mpi_knum(1, icore) ), mpi_num_grid(icore)*7, temp_mpi_character, &
					& icore, 1, temp_mpi_comm_world, ierr )
			enddo
		endif
		if( myrank/=0 ) then
			call mpi_recv( eachdata_gcode(1), eachgrd*7, temp_mpi_character, &
				& 0, 1, temp_mpi_comm_world, istatus, ierr )
		endif
		call mpi_barrier(temp_mpi_comm_world, ierr)

		! Send yield data to each core
		if( myrank==0 ) then
			do icore = 1, nprocs-1
				call mpi_send( kdata_yield( :, :, mpi_knum(1, icore) ), mpi_num_grid(icore)*8*nyear, temp_mpi_real, &
					& icore, 1, temp_mpi_comm_world, ierr )
			enddo
		endif
		if( myrank/=0 ) then
			call mpi_recv( eachdata_yield(:, :, :), eachgrd*8*nyear, temp_mpi_real, &
				& 0, 1, temp_mpi_comm_world, istatus, ierr )
		endif
		call mpi_barrier(temp_mpi_comm_world, ierr)

		! Send soil data to each core
		if( myrank==0 ) then
			do icore = 1, nprocs-1
				call mpi_send( kdata_soil( :, :, mpi_knum(1, icore) ), mpi_num_grid(icore)*nvar_soil*nsol, temp_mpi_real, &
					& icore, 1, temp_mpi_comm_world, ierr )
			enddo
		endif
		if( myrank/=0 ) then
			call mpi_recv( eachdata_soil(:, :, :), eachgrd*nvar_soil*nsol, temp_mpi_real, &
				& 0, 1, temp_mpi_comm_world, istatus, ierr )
		endif
		call mpi_barrier(temp_mpi_comm_world, ierr)

		! Send climate data to each core
		if( myrank==0 ) then
			do icore = 1, nprocs-1
				call mpi_send( kdata_weather( :, :, mpi_knum(1, icore) ), mpi_num_grid(icore)*nvar_weather*ntime, temp_mpi_real, &
					& icore, 1, temp_mpi_comm_world, ierr )
			enddo
		endif
		if( myrank/=0 ) then
			call mpi_recv( eachdata_weather(:, :, :), eachgrd*nvar_weather*ntime, temp_mpi_real, &
				& 0, 1, temp_mpi_comm_world, istatus, ierr )
		endif
		call mpi_barrier(temp_mpi_comm_world, ierr)

		! Send phenology data to each core
		if( myrank==0 ) then
			do icore = 1, nprocs-1
				call mpi_send( kdata_pheno( :, mpi_knum(1, icore) ), mpi_num_grid(icore)*nvar_pheno, temp_mpi_real, &
					& icore, 1, temp_mpi_comm_world, ierr )
			enddo
		endif
		if( myrank/=0 ) then
			call mpi_recv( eachdata_pheno(:, :), eachgrd*nvar_pheno, temp_mpi_real, &
				& 0, 1, temp_mpi_comm_world, istatus, ierr )
		endif
		call mpi_barrier(temp_mpi_comm_world, ierr)

		! Send CO2 data to each core
		if( myrank==0 ) then
			do icore = 1, nprocs-1
				call mpi_send( kdata_co2( :, mpi_knum(1, icore) ), mpi_num_grid(icore)*nyear, temp_mpi_real, &
					& icore, 1, temp_mpi_comm_world, ierr )
			enddo
		endif
		if( myrank/=0 ) then
			call mpi_recv( eachdata_co2(:, :), eachgrd*nyear, temp_mpi_real, &
				& 0, 1, temp_mpi_comm_world, istatus, ierr )
		endif
		call mpi_barrier(temp_mpi_comm_world, ierr)


		! Send parameter values to each core
		if( myrank==0 ) then
			do icore = 1, nprocs-1
				call mpi_send( kdata_param( :, :, mpi_knum(1, icore) ), mpi_num_grid(icore)*nvar_param*nensemble, temp_mpi_real, &
				& icore, 1, temp_mpi_comm_world, ierr )
			enddo
		endif
		if( myrank/=0 ) then
			call mpi_recv( eachdata_param(:, :, :), eachgrd*nvar_param*nensemble, temp_mpi_real, &
			& 0, 1, temp_mpi_comm_world, istatus, ierr )
		endif
		call mpi_barrier(temp_mpi_comm_world, ierr)

		! Send parameter values to each core
		if( myrank==0 ) then
			do icore = 1, nprocs-1
				call mpi_send( kdata_fix_param(:, mpi_knum(1, icore) ), mpi_num_grid(icore)*nvar_fix_param, temp_mpi_real, &
				& icore, 1, temp_mpi_comm_world, ierr )
			enddo
		endif
		if( myrank/=0 ) then
			call mpi_recv( eachdata_fix_param(:, :), eachgrd*nvar_fix_param, temp_mpi_real, &
			& 0, 1, temp_mpi_comm_world, istatus, ierr )
		endif
		call mpi_barrier(temp_mpi_comm_world, ierr)

		! Send GDP data to each core
		if( myrank==0 ) then
			do icore = 1, nprocs-1
				call mpi_send( kdata_gdp(:, mpi_knum(1, icore) ), mpi_num_grid(icore)*nyear, temp_mpi_real, &
				& icore, 1, temp_mpi_comm_world, ierr )
			enddo
		endif
		if( myrank/=0 ) then
			call mpi_recv( eachdata_gdp(:, :), eachgrd*nyear, temp_mpi_real, &
			& 0, 1, temp_mpi_comm_world, istatus, ierr )
		endif
		call mpi_barrier(temp_mpi_comm_world, ierr)

	end subroutine

end module
