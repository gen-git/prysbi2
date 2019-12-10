module Moutput_param

	use Mdream_param
	use var_divide

	private
	public :: output_param
	contains

	subroutine output_param( sect )

		implicit none
		integer :: sect, ii

		if( sect == 1) then ! Preparation of the output files

			!--- Replace files ---
			write( dream_ofile, "(a,'/param_',a,'.out')" ) &
			& trim(dream_odir_name), eachdata_gcode(igrid)
			open ( 1, file=dream_ofile, status='replace', form='unformatted', &
			& access='direct', recl=dream_nchain*dream_ndim*4 )
			close( 1 )

			write( dream_ofile, "(a,'/x_llk_',a,'.out')" ) &
			& trim(dream_odir_name), eachdata_gcode(igrid)
			open ( 1, file=dream_ofile, status='replace', form='unformatted', &
			& access='direct', recl=dream_nchain*4 )
			close( 1 )

			write( dream_ofile, "(a,'/rhat_',a,'.out')" ) &
			& trim(dream_odir_name), eachdata_gcode(igrid)
			open ( 1, file=dream_ofile, status='replace', form='unformatted', &
			& access='direct', recl=dream_ndim*4 )
			close( 1 )

			!--- Initialization ---
			dream_x_rec     = 1
			dream_x_llk_rec = 1
			dream_rhat_rec  = 1

		end if

		if (sect == 2) then ! Output

			write( dream_ofile, "(a,'/param_',a,'.out')" ) &
			& trim(dream_odir_name), eachdata_gcode(igrid)
			open ( 1, file=dream_ofile, status='old', form='unformatted', &
			& access='direct', recl=dream_nchain*dream_ndim*4 )
			write( 1, rec=dream_x_rec) real( dream_x_all( :, :, i_step ) ) ! ndim, nchain
			close( 1 )
			dream_x_rec = dream_x_rec + 1

			write( dream_ofile, "(a,'/x_llk_',a,'.out')" ) &
			& trim(dream_odir_name), eachdata_gcode(igrid)
			open ( 1, file=dream_ofile, status='old', form='unformatted', &
			& access='direct', recl=dream_nchain*4 )
			write( 1, rec=dream_x_llk_rec) real( dream_x_llk_all( :, i_step ) ) ! nchain
			close( 1 )
			dream_x_llk_rec = dream_x_llk_rec + 1

			write( dream_ofile, "(a,'/rhat_',a,'.out')" ) &
			& trim(dream_odir_name), eachdata_gcode(igrid)
			open ( 1, file=dream_ofile, status='old', form='unformatted', &
			& access='direct', recl=dream_ndim*4 )
			write( 1, rec=1) real( dream_rhat_all( : ) ) ! ndim
			close( 1 )
			dream_rhat_rec = dream_rhat_rec + 1

		end if

	end subroutine output_param
end module Moutput_param
