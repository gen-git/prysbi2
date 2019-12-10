module Mread_setting

	use Mdream_param

	contains

	subroutine read_setting

		implicit none

		! Variable declaration
		integer :: i, j
		character(20) :: c_dummy
		character(7)  :: g_dummy
		real(8) :: d_temp(20)


		! Read the data on prior distribution etc.
		open(1, file='./mcmc_setting_maize_major.dat')
		read(1,*)
		do i = 1, dream_ndim
			read(1,*) c_dummy, &
			& dream_x_typeprior(i), &
			& dream_x_lim(i,1), &
			& dream_x_lim(i,2), &
			& dream_x_avprior(i), &
			& dream_x_sdprior(i)
		end do
		read(1,*)
		read(1,*)
		read(1,*) c_dummy, dream_b
		read(1,*) c_dummy, dream_bdash
		read(1,*) c_dummy, dream_delta
		read(1,*)
		read(1,*)
		read(1,*) c_dummy, dream_odir_name
		read(1,*) c_dummy, dream_ofile_name

		close(1)

	end subroutine read_setting

end module Mread_setting
