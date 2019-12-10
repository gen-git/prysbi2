module shoot_structure

use spm_param

implicit none

private
public :: s_shoot_structure
contains

! This subruintine calculates shoot structure.
! According to GDD and biomass, leaf ratio is calculated.
! According to leaf ratio, leaf biomass and biomass of shoot except leaves are calculated.
! LAI is also calculated according to SLA.

subroutine s_shoot_structure( &
	lai_growth_type, & !in    Switch determinig LAI growth type                      (-)
	biomass,         & !in    Total biomass                                          (kg dry/ha)
	fgdd,            & !in    Fraction of GDD to total GDD needed for maturity (0-1) (-)
	lg_days,         & !in    Leaf growth day                                        (day)
	froot,           & !inout Root ratio                                             (-)
	cht,             & !out   Canopy height                                          (m)
	ag_bio,          & !out   Abobe ground biomass                                   (kg dry/ha)
	leaf_bio,        & !out   Leaf biomass                                           (kg dry/ha)
	shoot_el_bio,    & !out   Biomass of shoot except leaves                         (kg dry/ha)
	lai)               !out   LAI                                                    (ha/ha)
	
	implicit none

	integer, intent(in)    :: lai_growth_type
	real(8), intent(in)    :: biomass
	real(8), intent(in)    :: fgdd
	integer, intent(in)    :: lg_days
	real(8), intent(inout) :: froot
	real(8), intent(out)   :: cht
	real(8), intent(out)   :: ag_bio
	real(8), intent(out)   :: leaf_bio
	real(8), intent(out)   :: shoot_el_bio
	real(8), intent(out)   :: lai
                                     
	real(8) :: fleaf, flai, leaf1, leaf2, xx
	real(8) :: n_per_biomass
    
    
	!----- Calculate above ground biomass -----
  	froot  = ( 1d0 - min( 0.8d0, 0.6d0 +  0.2d0 * fgdd ) )
	ag_bio = ( 1d0 - froot ) * biomass
    
    
	!----- Calculate leaf biomass -----
	if( lai_growth_type == 0) then
		fleaf = clf_ratio1*lg_days + clf_ratio2
        fleaf = max(fleaf, 0d0)
	endif
	
	if( lai_growth_type == 1) then
        fleaf = 1.0d0 / (1.0d0 + exp( clf_ratio1*lg_days + clf_ratio2 ))
	endif
	
    if( lai_growth_type == 2) then
        fleaf = clf_ratio3 * fgdd - clf_ratio3
        fleaf = max(0d0, fleaf)
    endif

    leaf_bio     = fleaf * ag_bio
	shoot_el_bio = (1.0d0 - fleaf) * ag_bio
	
	
	!----- Calculate LAI -----
	lai = leaf_bio * (sla + sla_change * lg_days) * adjust_sla
	
	!----- Preparation for calculation of canopy height -----
	xx    = log(frgrw1/laimx1 - frgrw1)
	leaf2 = (xx - log(frgrw2/laimx2 - frgrw2)) / (frgrw2 - frgrw1)
	leaf1 = xx + leaf2*frgrw1
	
	
	!----- Calculate canopy height -----
	flai = fgdd / ( fgdd + exp( leaf1 - leaf2 * fgdd ) )
	cht  = cht_max * sqrt( flai )
	
	
	!----- Escape from error -----
	if(cht < 0.0d0) cht = 0.0d0
	if(ag_bio < 0.0d0) ag_bio = 0.0d0
	if(leaf_bio < 0.0d0) leaf_bio = 0.0d0
	if(shoot_el_bio < 0.0d0) shoot_el_bio = 0.0d0
	if(lai < 0.0d0) lai=0.01d0
	if(lai > 8.0d0) lai=8.0d0
	
end subroutine s_shoot_structure

end module shoot_structure
