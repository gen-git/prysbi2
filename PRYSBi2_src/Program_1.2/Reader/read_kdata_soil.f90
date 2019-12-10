module read_kdata_soil

	implicit none

	private
	public :: s_read_kdata_soil
	contains

	subroutine s_read_kdata_soil( &
		path_soil,  & !in
		nvar_soil,  & !in
		avar_soil,  & !in
		nsol,       & !in
		k2i,        & !in
		k2j,        & !in
		ngrd,       & !in
		rnan,       & !in
		kdata_soil  & !out
		)

		implicit none

		character(1000), intent(in)  :: path_soil
		integer,         intent(in)  :: nvar_soil
		character(8),    intent(in)  :: avar_soil(nvar_soil)
		integer,         intent(in)  :: nsol
		integer,         intent(in)  :: k2i(320*160)
		integer,         intent(in)  :: k2j(320*160)
		integer,         intent(in)  :: ngrd
		real(4),         intent(in)  :: rnan
		real(4),         intent(out) :: kdata_soil(nsol, nvar_soil, ngrd)

		integer :: ivar, isol, i, j, k
		character(1000) :: ifile
		real(4) :: data0(320, 160)


		kdata_soil(:,:,:) = rnan

		do ivar = 1, nvar_soil

		    write(ifile,"(a,'/',a,'.grd')") trim(path_soil), trim(avar_soil(ivar))
		    open(1, file=ifile, status='old', access='direct', form='unformatted', recl=320*160*4)

		    data0(1:320,1:160) = rnan
		    do isol = 1, nsol
				read(1,rec=isol) ((data0(i,j),i=1,320),j=1,160)
				do k = 1, ngrd
					kdata_soil(isol,ivar,k) = data0(k2i(k),k2j(k))
				end do !::: k

			end do !::: isol

		end do !::: ivar

	end subroutine

end module
