module read_kdata_topo

	implicit none

	private
	public :: s_read_kdata_topo

	contains

	subroutine s_read_kdata_topo( &
		path_topo,  & !in
		nvar_topo,  & !in
		avar_topo,  & !in
		k2i,        & !in
		k2j,        & !in
		ngrd,       & !in
		rnan,       & !in
		kdata_topo, & !out
		kdata_gcode & !out
		)

		implicit none

		character(1000), intent(in)  :: path_topo
		integer,         intent(in)  :: nvar_topo
		character(8),    intent(in)  :: avar_topo(:)
		integer,         intent(in)  :: k2i(320*160)
		integer,         intent(in)  :: k2j(320*160)
		integer,         intent(in)  :: ngrd
		real(4),         intent(in)  :: rnan
		real(4),         intent(out) :: kdata_topo(nvar_topo, ngrd)
		character(7),    intent(out) :: kdata_gcode(ngrd)

		real(4)      :: xcode(ngrd), ycode(ngrd)

		integer :: i, j, k, ivar, ix, iy
		character(1000) :: ifile
		real(4) :: data0(320, 160)

		xcode(:) = rnan
		ycode(:) = rnan
		kdata_topo(:,:) = rnan
		kdata_gcode(:) = 'XXX-XXX'


		do ivar = 1, nvar_topo
			write(ifile,"(a,'/',a,'.grd')") trim(path_topo), trim(avar_topo(ivar))
			open(1, file=ifile, status='old', access='direct', form='unformatted', recl=320*160*4)
			data0(:,:) = rnan
			read(1, rec=1) ((data0(i,j),i=1,320),j=1,160)
			do k = 1, ngrd
				kdata_topo(ivar,k) = data0(k2i(k),k2j(k))
			end do !::: k
			close(1)
		enddo !::: ivar


		do ivar = 1, nvar_topo
			if( trim(avar_topo(ivar))=='xcode' ) ix = ivar
			if( trim(avar_topo(ivar))=='ycode' ) iy = ivar
		enddo
		xcode(:) = kdata_topo(ix,:)
		ycode(:) = kdata_topo(iy,:)

		
		do k = 1, ngrd
			write(kdata_gcode(k),"(i3.3,'-',i3.3)") nint(xcode(k)), nint(ycode(k))
		end do !::: i

	end subroutine

end module
