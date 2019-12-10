module maintenance

use spm_param

implicit none

private
public :: s_maintenance
contains

! In this subroutine, maintenance respirations of root and shoot are calculated.
! However, 'shoot' does not includes leaves. Maintenance respiration of leaves is calculated
! in the other subroutine.

subroutine s_maintenance( &
	tm,              & !in  Average temperature                                            (deg C)
	nsol,            & !in  Number of soil layers                                          (-)
	sol_tmp,         & !in  Average temperature of each soil layer                         (deg C)
	root_bio_l,      & !in  Root biomass of each soil layer                                (kg dry/ha)
	ag_bio,          & !in  Above ground biomass                                           (kg dry/ha)
	rd_root_c,       & !out Maintenance respiration of root                                (kg C/ha/day)
	rd_shoot_c,      & !out Maintenance respiration of shoot                               (kg C/ha/day)
	rd_root,         & !out Maintenance respiration of root                                (kg dry/ha/day)
	rd_shoot) 	       !out Maintenance respiration of shoot                               (kg dry/ha/day)
	
	implicit none
	
	real(8), intent(in)  :: tm
	integer, intent(in)  :: nsol
	real(8), intent(in)  :: sol_tmp(nsol)
	real(8), intent(in)  :: root_bio_l(nsol)
	real(8), intent(in)  :: ag_bio
	real(8), intent(out) :: rd_root_c
	real(8), intent(out) :: rd_shoot_c
	real(8), intent(out) :: rd_root
	real(8), intent(out) :: rd_shoot
	
	integer :: i
	real(8) :: q_coef
	real(8) :: rd_rise
	
	
	!----- Calculate root respiration -----
	rd_root = 0d0
	rd_root_c = 0d0
	do i = 1, nsol

	  q_coef = (sol_tmp(i) - trd_base)/10d0
	  rd_rise = q_base**q_coef/(1d0 + exp( trd_root1*(sol_tmp(i) - trd_root2) ))
	  rd_root_c = rd_root_c + rd_rise * rd_m_root_rate * root_bio_l(i) * c_rate

	enddo
	
	rd_root = rd_root_c / c_rate


	!----- Calculate the respiration of shoot -----
	q_coef = (tm - trd_base)/10d0
  	rd_rise = q_base**q_coef/(1d0 + exp( trd_shoot1*(tm - trd_shoot2) ))
    rd_shoot_c = rd_rise * rd_m_shoot_rate * ag_bio * c_rate
    rd_shoot = rd_shoot_c / c_rate

end subroutine s_maintenance

end module maintenance
