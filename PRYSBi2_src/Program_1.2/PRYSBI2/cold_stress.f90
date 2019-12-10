module cold_stress

use spm_param

implicit none

private
public :: s_cold_heat_stress
public :: s_cold_heat_weight
contains

subroutine s_cold_heat_weight( &
                              cold_heat_type, &
                              dae, &
                              fphu, &
                              tx, &
                              tn, &
                              tm_weighting, &
                              tm_preweight)

	implicit none

  integer, intent(in)    :: cold_heat_type
	integer, intent(in)    :: dae
	real(8), intent(in)    :: fphu
	real(8), intent(in)    :: tx
  real(8), intent(in)    :: tn
	real(8), intent(inout) :: tm_weighting(366)
	real(8), intent(inout) :: tm_preweight(366)

	real(8), parameter :: pi = 3.14159265
	real(8) :: var

	if( dae > 0 ) then

		var = (cold_fphu_sd*100d0)**2
		tm_weighting(dae) = exp( -(fphu*100d0 - cold_fphu_av*100d0)**2 / (2d0*var) )

    if(cold_heat_type == 1) then
      tm_preweight(dae) = tx
    endif
    if(cold_heat_type == 2) then
      tm_preweight(dae) = tn
    endif
	else

		tm_weighting(:) = 0d0
		tm_preweight(:) = 0d0

	endif

end subroutine s_cold_heat_weight
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------
subroutine s_cold_heat_stress(  cold_heat_type, &
                                tm_weighting, &
                                tm_preweight, &
                                cold_heat_change)

	implicit none

  integer, intent(in)  :: cold_heat_type
	real(8), intent(in)  :: tm_weighting(366)
	real(8), intent(in)  :: tm_preweight(366)
	real(8), intent(out) :: cold_heat_change

	real(8) :: cold_tm_xxxx, cold_tm_yyyy, temp_coef1

  ! Weighted average
	cold_tm_xxxx = sum(tm_weighting(:) * tm_preweight(:)) / sum( tm_weighting(:) )

  ! When including NA or Nan
  if(isnan(cold_tm_xxxx)) then
    if(cold_heat_type == 1) then
      cold_tm_xxxx = 30.0 + cold_heat_coef2
    endif
    if(cold_heat_type == 2) then
      cold_tm_xxxx = 20.0 + cold_heat_coef2
    endif
  endif

  ! Heat stress
  if(cold_heat_type == 1) then
    cold_tm_yyyy = min(0.0, cold_tm_xxxx - (30.0 + cold_heat_coef2))
  endif
  ! Cold stress
  if(cold_heat_type == 2) then
    cold_tm_yyyy = min(0.0, (20.0 + cold_heat_coef2) - cold_tm_xxxx)
  endif

  ! heat and cold stress
  cold_heat_change = 1.0 + cold_heat_coef1 * cold_tm_yyyy
  cold_heat_change = max(0.0, cold_heat_change)
  cold_heat_change = min(1.0, cold_heat_change)

  if( cold_heat_coef1 == -9999.0 .or. cold_heat_coef2 == -9999.0 ) then
    cold_heat_change = 0.0
  endif

end subroutine s_cold_heat_stress
!---------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------

end module cold_stress
