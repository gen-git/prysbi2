module specify_grid_analyzed

	implicit none

	private
	public :: s_specify_grid_analyzed

	contains

	subroutine s_specify_grid_analyzed( &
		path_ana,       & !in
		rnan,           & !in
		k2i,            & !out
		k2j,            & !out
		ngrd            & !out
		)

		implicit none

		character(1000), intent(in)  :: path_ana
		real(4),         intent(in)  :: rnan
		integer,         intent(out) :: k2i(320*160)
		integer,         intent(out) :: k2j(320*160)
		integer,         intent(out) :: ngrd


		real(4)         :: grid_specified(320, 160)
		character(1000) :: ifile
		integer         :: i, j, k

		write(ifile,"(a)") trim(path_ana)
		open(1, file=ifile, status='old', access='direct', form='unformatted', recl=320*160*4)
		read(1, rec=1) ((grid_specified(i,j),i=1,320),j=1,160)
		close(1)


		k2i(:) = nint(rnan)
		k2j(:) = nint(rnan)


		k = 1
		do j = 1, 160
			do i = 1, 320
				if( nint(grid_specified(i,j)) == 1 ) then
				k2i(k) = i
				k2j(k) = j
				k = k + 1
				end if
			end do !::: i
		end do !::: j
		ngrd = k - 1

	end subroutine

end module
