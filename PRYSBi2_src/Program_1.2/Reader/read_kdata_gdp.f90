module read_kdata_gdp

implicit none

private
public :: s_read_kdata_gdp
contains

subroutine s_read_kdata_gdp( &
    path_gdp,         & !in
    iyrstt,           & !in
    iyrend,           & !in
    k2i,              & !in
    k2j,              & !in
    ngrd,             & !in
    rnan,             & !in
    kdata_gdp         & !out
    )

    implicit none

    character(1000), intent(in)  :: path_gdp
    integer,         intent(in)  :: iyrstt
    integer,         intent(in)  :: iyrend
    integer,         intent(in)  :: k2i(320*160)
    integer,         intent(in)  :: k2j(320*160)
    integer,         intent(in)  :: ngrd
    real(4),         intent(in)  :: rnan
    real(4),         intent(out) :: kdata_gdp(iyrstt:iyrend, ngrd)

    integer :: iyr, i, j, k
    character(1000) :: ifile
    real(4) :: data0(320, 160)


    kdata_gdp(:, :) = rnan

    do iyr = iyrstt, iyrend

             write(ifile,"(a,'/gdp_billion_',i4,'.grd')") trim(path_gdp), iyr
            open(1, file=ifile, status='old', access='direct', form='unformatted', recl=320*160*4)

            data0(1:320,1:160) = rnan

            read(1,rec=1) ((data0(i,j),i=1,320),j=1,160)
            do k = 1, ngrd
                kdata_gdp(iyr, k) = data0(k2i(k),k2j(k))
            enddo !::: k

    enddo !::: iyr

end subroutine

end module
