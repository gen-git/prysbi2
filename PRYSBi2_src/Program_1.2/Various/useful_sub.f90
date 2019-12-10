module useful_sub

	implicit none

	private
	public :: s_ymd2itime, s_count_ntime, s_ymd2doy
	contains

	! s_ymd2itime
	! s_count_ntime
	! s_ymd2doy

	subroutine s_ymd2itime( &
		iyrstt, & ! The year of starting calculation
		iyrend, & ! The year of ending calculation
		tgt_yr, & ! Target year
		tgt_mn, & ! Target month
		tgt_dy, & ! Target date
		itime   & ! Target time
		)

		implicit none

		integer, intent(in)  :: iyrstt
		integer, intent(in)  :: iyrend
		integer, intent(in)  :: tgt_yr
		integer, intent(in)  :: tgt_mn
		integer, intent(in)  :: tgt_dy
		integer, intent(out) :: itime

		integer, parameter :: ndyu(12) = (/31,29,31,30,31,30,31,31,30,31,30,31/)
		integer, parameter :: ndyn(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)
		integer :: ndy(12)

		integer :: iyr, imn, nday

		if(iyrend > iyrstt) then
			itime = 0
			do iyr = iyrstt, iyrend - 1
				if(mod(iyr,4)==0) itime = itime + 366
				if(mod(iyr,4)/=0) itime = itime + 365
			enddo
			iyr = iyrend
			if(mod(iyr,4)==0) ndy(:) = ndyu(:)
			if(mod(iyr,4)/=0) ndy(:) = ndyn(:)
			if(tgt_mn/=1) then
				do imn = 1, tgt_mn - 1
					itime = itime + ndy(imn)
				enddo
				itime = itime + tgt_dy
			else
				itime = itime + tgt_dy
			endif
		else
			itime = 0
			iyr = iyrend
			if(mod(iyr,4)==0) ndy(:) = ndyu(:)
			if(mod(iyr,4)/=0) ndy(:) = ndyn(:)
			if(tgt_mn/=1) then
				do imn = 1, tgt_mn - 1
					itime = itime + ndy(imn)
				enddo
				itime = itime + tgt_dy
			else
				itime = itime + tgt_dy
			endif
		endif

	end subroutine

	subroutine s_count_ntime( &
		iyrstt, & ! The year of starting calculation
		iyrend, & ! The year of ending calculation
		ntime   & ! Total date
		)

		implicit none

		integer, intent(in)  :: iyrstt
		integer, intent(in)  :: iyrend
		integer, intent(out) :: ntime

		integer :: iyr, itime

		itime = 0
		do iyr = iyrstt, iyrend
			if(iyr/=2100) then
				if(mod(iyr,4)==0) itime = itime + 366
				if(mod(iyr,4)/=0) itime = itime + 365
            endif
            if(iyr==2100) then
                itime = itime + 365
            endif
		enddo
		ntime = itime

	end subroutine

	subroutine s_ymd2doy( &
		leapcode, &
		iyr,      &
		imn,      &
		idy,      &
		idoy      &
		)

		! writen by Iizumi

		implicit none

		integer, intent(in)  :: leapcode !::: Leap year code [1=on, 0=off]
		integer, intent(in)  :: iyr      !::: Year [YYYY]
		integer, intent(in)  :: imn      !::: Month [1-12]
		integer, intent(in)  :: idy      !::: Day [1-31]
		integer, intent(out) :: idoy     !::: DOY [1-366]

		integer, save :: ndyu(12) = (/31,29,31,30,31,30,31,31,30,31,30,31/)
		integer, save :: ndyn(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)
		integer :: jmn, ndy

		!:::::::::::::::::::::::::::::::::::::::::::::::::::::::
		!::: Module for converting YYYYMMDD to DOY
		!:::::::::::::::::::::::::::::::::::::::::::::::::::::::

		if( imn.eq.1 ) then
			idoy = idy
		else
			idoy = 0
			do jmn = 1, imn-1
				if( mod(iyr,4).eq.0 ) then
					if( leapcode.eq.1 ) ndy = ndyu(jmn)
					if( leapcode.eq.0 ) ndy = ndyn(jmn)
				else
					ndy = ndyn(jmn)
				end if
				idoy = idoy + ndy
			end do
			idoy = idoy + idy
		end if

	end subroutine

end module
