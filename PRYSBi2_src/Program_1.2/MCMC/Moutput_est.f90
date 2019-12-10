module Moutput_est

	use Mdream_param
	use var_divide

	private
	public :: output_est
	contains

	subroutine output_est( sect )

		implicit none

		integer :: sect, ii, jj, kk

		if( sect == 1) then ! Preparation of the output file

			!--- Replace file---
			write( dream_ofile, "(a,'/est_',a,'.out')" ) &
				 & trim(dream_odir_name), eachdata_gcode(igrid)
			open ( 1, file=dream_ofile, status='replace', access='direct', form='unformatted', &
			& recl=dream_nchain * dream_est_dim * dream_est_n * 4) ! Output with 4 bite
			close( 1 )

			!--- Initialization ---
			dream_est_x_rec = 1

		end if

		if (sect == 2) then ! Output
			! print *, dream_est_x_out( :,:,: )
			write( dream_ofile, "(a,'/est_',a,'.out')" ) &
			& trim(dream_odir_name), eachdata_gcode(igrid)
			open ( 1, file=dream_ofile, status='old', access='direct', form='unformatted', &
				 & recl=dream_nchain * dream_est_dim * dream_est_n * 4) ! Output with 4 bite
			write( 1, rec=dream_est_x_rec) real( dream_est_x_out( :,:,: ) ) ! est_n, est_dim, nchain
			close( 1 )

			dream_est_x_rec = dream_est_x_rec + 1
		end if

	end subroutine output_est
end module Moutput_est
