module read_kdata_co2

	implicit none

	private
	public :: s_read_kdata_co2

	contains

	subroutine s_read_kdata_co2( &
		path_co2, & !in
		iyrstt,   & !in
		iyrend,   & !in
		ngrd,     & !in
		rnan,     & !in
		kdata_co2 & !out
		)

		implicit none

		character(1000), intent(in)  :: path_co2
		integer,         intent(in)  :: iyrstt
		integer,         intent(in)  :: iyrend
		integer,         intent(in)  :: ngrd
		real(4),         intent(in)  :: rnan
		real(4),         intent(out) :: kdata_co2(iyrstt:iyrend, ngrd)

		integer :: ists, tmpyr, iyr
		real(4) :: tmpco2
		character(1000) :: ifile

		kdata_co2(:,:) = rnan

		write(ifile,"(a,'/co2.dat')") trim(path_co2)
		open(1, file=ifile, status='old')
		read(1, *)
		do
			read(1, *, iostat=ists) tmpyr, tmpco2
			if( ists /= 0 ) exit
			do iyr = iyrstt, iyrend
				if(iyr == tmpyr) then
					kdata_co2(iyr,:) = tmpco2
					exit
				endif
			enddo
		enddo
		close(1)

		! Check missing value
		do iyr = iyrstt, iyrend
			if( kdata_co2(iyr,1)== rnan ) then
				write(*,*) '! Error: CO2 data is not sufficient'
				stop
			endif
		enddo

	end subroutine

end module
