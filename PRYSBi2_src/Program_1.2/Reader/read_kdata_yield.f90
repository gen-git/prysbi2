module read_kdata_yield

	implicit none

	private
	public :: s_read_kdata_yield
	contains

	subroutine s_read_kdata_yield( &
		path_yield,  & !in
		tgt_version, & !in
		tgt_crop,    & !in
		iyrstt,      & !in
		iyrend,      & !in
		k2i,         & !in
		k2j,         & !in
		ngrd,        & !in
		rnan,        & !in
		kdata_yield  & !out
		)

		implicit none

		character(1000), intent(in)  :: path_yield
		character(20),   intent(in)  :: tgt_version
		character(20),   intent(in)  :: tgt_crop
		integer,         intent(in)  :: iyrstt
		integer,         intent(in)  :: iyrend
		integer,         intent(in)  :: k2i(320*160)
		integer,         intent(in)  :: k2j(320*160)
		integer,         intent(in)  :: ngrd
		real(4),         intent(in)  :: rnan
		real(4),         intent(out) :: kdata_yield(iyrstt:iyrend, 8, ngrd) ! Output data

		integer :: ivar, iyr, i, j, k, temp_iyrstt, temp_iyrend
		character(1000) :: ifile
		real(4) :: data0(320, 160)
		character(20)   :: temp_tgt_crop


		kdata_yield(:, :, :) = rnan

		temp_tgt_crop = tgt_crop

        if( iyrend < 1982 .or. 2006 < iyrstt) then
            kdata_yield(:, :, :) = rnan
        else
            temp_iyrstt = iyrstt
            temp_iyrend = iyrend
            if( iyrstt < 1961 ) temp_iyrstt = 1961
            if( 2010 < iyrend ) temp_iyrend = 2010

            do iyr = temp_iyrstt, temp_iyrend
                write(ifile,"(a,'/version_',a,'/',a,'/gcytd_yield_',a,'_',a,'_',i4,'.grd')") &
                & trim(path_yield), trim(tgt_version), trim(temp_tgt_crop), trim(tgt_version), trim(temp_tgt_crop), iyr
                open(1, file=ifile, status='old', access='direct', form='unformatted', recl=320*160*4)

                data0(1:320,1:160) = rnan
                do ivar = 1, 8

                    read(1,rec=ivar) ((data0(i,j),i=1,320),j=1,160)
                    do k = 1, ngrd
                        kdata_yield(iyr,ivar,k) = data0(k2i(k),k2j(k))
                    enddo !::: k

                end do !::: ivar

            enddo !::: iyr
        endif

	end subroutine

end module
