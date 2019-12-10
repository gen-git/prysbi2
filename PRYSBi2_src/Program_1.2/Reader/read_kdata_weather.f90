module read_kdata_weather

	implicit none

	private
	public :: s_read_kdata_weather
	contains

	subroutine s_read_kdata_weather( &
		path_weather,  & !in
		nvar_weather,  & !in
		avar_weather,  & !in
		iyrstt,        & !in
		iyrend,        & !in
		ntime,         & !in
		k2i,           & !in
		k2j,           & !in
		ngrd,          & !in
		rnan,          & !in
		kdata_weather  & !out
		)

		implicit none

		character(1000), intent(in)  :: path_weather
		integer,         intent(in)  :: nvar_weather
		character(8),    intent(in)  :: avar_weather(nvar_weather)
		integer,         intent(in)  :: iyrstt
		integer,         intent(in)  :: iyrend
		integer,         intent(in)  :: ntime
		integer,         intent(in)  :: k2i(320*160)
		integer,         intent(in)  :: k2j(320*160)
		integer,         intent(in)  :: ngrd
		real(4),         intent(in)  :: rnan
		real(4),         intent(out) :: kdata_weather(ntime, nvar_weather, ngrd)

		integer :: ivar, iday, itime, iyr, i, j, k
		integer :: nday
		character(1000) :: ifile
		real(4) :: data0(320, 160)


		kdata_weather(:, :, :) = rnan

		do ivar = 1, nvar_weather

			write(*,*) '   Reading ', trim(avar_weather(ivar))

			itime = 1
			do iyr = iyrstt, iyrend
      			write(ifile,"(a,'/',a,'/',a,i4,'.grd')") trim(path_weather), trim(avar_weather(ivar)), trim(avar_weather(ivar)), iyr
		    	open(1, file=ifile, status='old', access='direct', form='unformatted', recl=320*160*4)

		    	data0(1:320,1:160) = rnan
                if(iyr/=2100) then
                    if(mod(iyr,4)==0) nday = 366
                    if(mod(iyr,4)/=0) nday = 365
                endif
                if(iyr==2100) then
                    nday = 365
                endif

		    	do iday = 1, nday

					read(1,rec=iday) ((data0(i,j),i=1,320),j=1,160)
					do k = 1, ngrd
						kdata_weather(itime,ivar,k) = data0(k2i(k),k2j(k))
					enddo !::: k
					itime = itime + 1

				end do !::: iday

			enddo !::: iyr

		enddo !::: ivar

	end subroutine

end module
