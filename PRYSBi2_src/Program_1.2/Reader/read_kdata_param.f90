module read_kdata_param

	implicit none

	private
	public :: s_read_kdata_param
    public :: s_read_kdata_fix_param
    contains

	subroutine s_read_kdata_param( &
		path_param,    & !in
		nvar_param,    & !in
		avar_param,    & !in
		nensemble,     & !in
		tgt_crop,      & !in
		k2i,           & !in
		k2j,           & !in
		ngrd,          & !in
		rnan,          & !in
		kdata_param    & !out
		)

		implicit none

		character(1000), intent(in)  :: path_param
		integer,         intent(in)  :: nvar_param
		character(3),    intent(in)  :: avar_param(nvar_param)
		integer,         intent(in)  :: nensemble
		character(20),   intent(in)  :: tgt_crop
		integer,         intent(in)  :: k2i(320*160)
		integer,         intent(in)  :: k2j(320*160)
		integer,         intent(in)  :: ngrd
		real(4),         intent(in)  :: rnan
		real(4),         intent(out) :: kdata_param(nensemble, nvar_param, ngrd)

		integer :: ivar, iensemble, i, j, k
		character(1000) :: ifile
		real(4) :: data0(320, 160)


		kdata_param(:, :, :) = rnan

		do ivar = 1, nvar_param

			write(*,*) '   Reading parameter ', trim(avar_param(ivar))

            write(ifile,"(a,'/grd_param_list_all_',a,'_',a,'.grd')") trim(path_param), trim(avar_param(ivar)), trim(tgt_crop)
            open(1, file=ifile, status='old', access='direct', form='unformatted', recl=320*160*4)

            data0(1:320,1:160) = rnan
            do iensemble = 1, nensemble

                read(1,rec=iensemble) ((data0(i,j),i=1,320),j=1,160)
                do k = 1, ngrd
                    kdata_param(iensemble,ivar,k) = data0(k2i(k),k2j(k))
                enddo !::: k

            end do !::: iensemble

		enddo !::: ivar

	end subroutine

  subroutine s_read_kdata_fix_param( &
      path_fix_param,    & !in
      nvar_fix_param,    & !in
      avar_fix_param,    & !in
      tgt_crop,          & !in
      k2i,               & !in
      k2j,               & !in
      ngrd,              & !in
      rnan,              & !in
      kdata_fix_param    & !out
      )

  implicit none

  character(1000), intent(in)  :: path_fix_param
  integer,         intent(in)  :: nvar_fix_param
  character(3),    intent(in)  :: avar_fix_param(nvar_fix_param)
  character(20),   intent(in)  :: tgt_crop
  integer,         intent(in)  :: k2i(320*160)
  integer,         intent(in)  :: k2j(320*160)
  integer,         intent(in)  :: ngrd
  real(4),         intent(in)  :: rnan
  real(4),         intent(out) :: kdata_fix_param(nvar_fix_param, ngrd) ! Output data

  integer :: ivar, i, j, k
  character(1000) :: ifile
  real(4) :: data0(320, 160)


  kdata_fix_param(:, :) = rnan

  do ivar = 1, nvar_fix_param

      write(*,*) '   Reading parameter ', trim(avar_fix_param(ivar))

      write(ifile,"(a,'/grd_param_list_all_',a,'_',a,'.grd')") trim(path_fix_param), trim(avar_fix_param(ivar)), trim(tgt_crop)
      open(1, file=ifile, status='old', access='direct', form='unformatted', recl=320*160*4)

      data0(1:320,1:160) = rnan
      read(1,rec=1) ((data0(i,j),i=1,320),j=1,160)
      do k = 1, ngrd
          kdata_fix_param(ivar,k) = data0(k2i(k),k2j(k))
      enddo !::: k

  enddo !::: ivar

  end subroutine

end module
