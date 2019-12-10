module calc_biomass

use spm_param

implicit none

private
public :: s_calc_biomass
contains

subroutine s_calc_biomass( &
	lai_growth_type, & !in    Control switch
	cmass,           & !in    Net assimilation rate                                    (kg C/ha/day)
	rd_root_c,       & !in    Root respiration                                         (kg C/ha/day)
	rd_shoot_c,      & !in    Respiration of shoot except leaves                       (kg C/ha/day)
	fgdd,            & !in    Fraction of GDD to total GDD needed for maturation (0-1) (-)
	lg_days,         & !in    Leaf growth day                                          (day)
	biomass,         & !inout Total biomass                                            (kg dry/ha)
	leaf_bio,        & !inout Leaf biomass                                             (kg dry/ha)
	leaf_bio_loss,   & !out   Leaf biomass loss                                        (kg dry/ha)
	rd_g_all_c,      & !out   Growth respiration                                       (kg C/ha/day)
	ag_bio,          & !out   Above ground biomass                                     (kg dry/ha)
	dbio)              !out   Biomass change                                           (kg dry/ha/day)
	
	implicit none

	integer, intent(in)    :: lai_growth_type
	real(8), intent(in)    :: cmass
	real(8), intent(in)    :: rd_root_c
	real(8), intent(in)    :: rd_shoot_c
	real(8), intent(in)    :: fgdd
	integer, intent(in)    :: lg_days
	real(8), intent(inout) :: biomass
	real(8), intent(inout) :: leaf_bio
	real(8), intent(out)   :: leaf_bio_loss
	real(8), intent(out)   :: rd_g_all_c
	real(8), intent(out)   :: ag_bio
	real(8), intent(out)   :: dbio

	real(8) :: biomass_pre, leaf_bio_pre, total_rd_c, dbio_p, dbio_c, fleaf

	!----- Save biomass and leaf biomass -----
	biomass_pre  = biomass
	leaf_bio_pre = leaf_bio

	!----- Calculate potential biomass growth -----
	total_rd_c  = rd_root_c + rd_shoot_c      ! Total respiraiton except leaf (kg C/ha/day)
	dbio_p      = cmass - total_rd_c          ! Potential biomass growth (kg C/ha/day)

	!----- Calculate actual biomass growth considering growth respiration -----
	if(dbio_p >  0) then
		dbio_c = dbio_p / (1d0 + rd_g_all_rate) ! Actual biomass growth (kg C/ha/day)
		dbio_c = dbio_c
	endif
	if(dbio_p <= 0) then
		dbio_c = dbio_p
	endif

	!----- Calculate biomass before considering leaf falling -----
	biomass = biomass + dbio_c/c_rate

	!----- Calculate above ground biomass -----
	ag_bio = ( 1d0 - ( 0.4d0 - 0.2d0*fgdd) ) * biomass

	!----- Calculate leaf biomass -----
	if( lai_growth_type == 1) then
		fleaf = 1.0d0 / (1.0d0 + exp( clf_ratio1*lg_days + clf_ratio2 ))
    elseif( lai_growth_type == 2) then
        fleaf = clf_ratio3 * fgdd - clf_ratio3
	else
		write(*,*) 'Error: parameter lai_growth_type should be specified'
		stop
	endif
	
	leaf_bio = fleaf * ag_bio

	!----- Calculate leaf falling -----
	leaf_bio_loss = leaf_bio - leaf_bio_pre
	if( leaf_bio_loss > 0d0 ) leaf_bio_loss = 0d0

	!----- Calculate final biomass -----
	biomass  = biomass + leaf_bio_loss
	dbio = biomass - biomass_pre
	
	if(biomass < 0.0d0) biomass = 0.01d0
	
	!----- Calculate growth respiration rate -----
	rd_g_all_c = max( 0.0d0, rd_g_all_rate * dbio_c )
	
	
end subroutine s_calc_biomass

end module calc_biomass
