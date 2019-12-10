module read_kdata_pheno

	implicit none

	private
	public :: s_read_kdata_pheno

	contains

	subroutine s_read_kdata_pheno( &
		path_pheno, & !in
		nvar_pheno, & !in
		avar_pheno, & !in
		tgt_crop,   & !in
		k2i,        & !in
		k2j,        & !in
		ngrd,       & !in
		rnan,       & !in
		kdata_pheno & !out
		)

		implicit none

		character(1000), intent(in)  :: path_pheno
		integer,         intent(in)  :: nvar_pheno
		character(14),   intent(in)  :: avar_pheno(:)
		character(20),   intent(in)  :: tgt_crop
		integer,         intent(in)  :: k2i(320*160)
		integer,         intent(in)  :: k2j(320*160)
		integer,         intent(in)  :: ngrd
		real(4),         intent(in)  :: rnan
		real(4),         intent(out) :: kdata_pheno(nvar_pheno, ngrd)

		integer :: i, j, k, ivar, ix, iy
		character(1000) :: ifile
		character(20)   :: temp_tgt_crop
		real(4) :: data0(320, 160)

		kdata_pheno(:,:) = rnan


		temp_tgt_crop = tgt_crop
		if( trim(tgt_crop) == 'maize' ) temp_tgt_crop = 'maize_major'
		if( trim(tgt_crop) == 'rice' )  temp_tgt_crop = 'rice_major'


		do ivar = 1, nvar_pheno
			write(ifile,"(a,'/',a,'_',a,'.grd')") trim(path_pheno), trim(temp_tgt_crop), trim(avar_pheno(ivar))
			open(1, file=ifile, status='old', access='direct', form='unformatted', recl=320*160*4)
			data0(:,:) = rnan
			read(1, rec=1) ((data0(i,j),i=1,320),j=1,160)
			do k = 1, ngrd
				kdata_pheno(ivar,k) = data0(k2i(k),k2j(k))
			end do !::: k
			close(1)
		enddo !::: ivar

	end subroutine

end module
