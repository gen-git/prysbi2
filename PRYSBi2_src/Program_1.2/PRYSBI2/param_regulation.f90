module param_regulation

!----------------------------------------------------------------
! In this subroutine, down regulation effect of CO2 fertilization
! is calculated.
!
! When not considering the down regulation effect, the parameters
! (e.g. co2_dr_vcmax25) should be set at zero.
!----------------------------------------------------------------
use spm_param

implicit none

private
public :: s_param_regulation
contains

subroutine s_param_regulation( ca )

	implicit none

	real(8), intent(in) :: ca

	adjust_vcmax25       = max( 1.0d-5, 1.0d0 + co2_dr_vcmax25       * ( ca/c1 - 1.0d0 ) )
	adjust_jmax25        = max( 1.0d-5, 1.0d0 + co2_dr_jmax25        * ( ca/c1 - 1.0d0 ) )
	adjust_sla           = max( 1.0d-5, 1.0d0 + co2_dr_sla           * ( ca/c1 - 1.0d0 ) )
	adjust_rd25_fac      = max( 1.0d-5, 1.0d0 + co2_dr_rd25_fac      * ( ca/c1 - 1.0d0 ) )

	! prg_theta = 0.99
	!
	! prg_a = co2_dr_vcmax25
	! prg_phi  = - prg_a / c1
	! prg_amax = - prg_a * (600.0/c1 - 1.0)
	! prg_rst = 1.0 - (prg_phi*(ca-c1)+prg_amax-sqrt((prg_phi*(ca-c1)+prg_amax)**2-4.0* &
	! & prg_theta*prg_phi*prg_amax*(ca-c1))) / (2.0*prg_theta)
	! adjust_vcmax25 = prg_rst
	!
	! prg_a = co2_dr_jmax25
	! prg_phi  = - prg_a / c1
	! prg_amax = - prg_a * (600.0/c1 - 1.0)
	! prg_rst = 1.0 - (prg_phi*(ca-c1)+prg_amax-sqrt((prg_phi*(ca-c1)+prg_amax)**2-4.0* &
	! & prg_theta*prg_phi*prg_amax*(ca-c1))) / (2.0*prg_theta)
	! adjust_jmax25 = prg_rst
	!
	! prg_a = co2_dr_sla
	! prg_phi  = - prg_a / c1
	! prg_amax = - prg_a * (600.0/c1 - 1.0)
	! prg_rst = 1.0 - (prg_phi*(ca-c1)+prg_amax-sqrt((prg_phi*(ca-c1)+prg_amax)**2-4.0* &
	! & prg_theta*prg_phi*prg_amax*(ca-c1))) / (2.0*prg_theta)
	! adjust_sla = prg_rst
	!
	! prg_a = co2_dr_rd25_fac
	! prg_phi  = - prg_a / c1
	! prg_amax = - prg_a * (600.0/c1 - 1.0)
	! prg_rst = 1.0 - (prg_phi*(ca-c1)+prg_amax-sqrt((prg_phi*(ca-c1)+prg_amax)**2-4.0* &
	! & prg_theta*prg_phi*prg_amax*(ca-c1))) / (2.0*prg_theta)
	! adjust_rd25_fac = prg_rst

end subroutine s_param_regulation

end module param_regulation
