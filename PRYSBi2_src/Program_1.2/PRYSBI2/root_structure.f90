module root_structure

use spm_param

implicit none

private
public :: s_root_structure
contains

! This subroutine calculate root structure.

subroutine s_root_structure( &
	biomass,    & !in  Total biomass                                            (kg dry/ha)
	fgdd,       & !in  Fraction of gdd to total gdd needed for maturation (0-1) (-)
	nsol,       & !in  Number of soil layers                                    (-)
	sol_z,      & !in  Soil depth of the bottom of each layer                   (mm)
	root_depth, & !out Root depth                                               (mm)
	root_bio,   & !out Root biomass                                             (kg dry/ha)
	root_bio_l)   !out Root biomass of each layer                               (kg dry/ha)
	
	implicit none
	
	real(8), intent(in)  :: biomass
	real(8), intent(in)  :: fgdd
	integer, intent(in)  :: nsol
	real(8), intent(in)  :: sol_z(nsol)
	real(8), intent(out) :: root_depth
	real(8), intent(out) :: root_bio
	real(8), intent(out) :: root_bio_l(nsol)
	
	integer :: flag_soil      ! Flag showing that there are additional soil layers below
	real(8) :: tgt_depth      ! Target depth
	real(8) :: cum_root(nsol) ! Cumulative root fraction
	real(8) :: froot          ! Fraction of root to total biomass (0-1)
	real(8) :: froot_l(nsol)  ! Fraction of root of each layer (0-1)
	
	integer :: i


	!----- Calculate root depth -----
	if( fgdd <= 0.4d0 ) then
	    root_depth = 2.5d0 * fgdd * max_rd
	endif
	if( fgdd >  0.4d0 ) then
		root_depth = max_rd
	endif
	
    if( root_depth >= max_rd ) root_depth = max_rd
    if( root_depth <= 10.0d0 ) root_depth = 10.0d0


	!----- Calculate root biomass -----
	froot = max( 0.2d0, 0.4d0 - 0.2d0*fgdd )
	root_bio   = biomass * froot


	!----- Calculate root distribution -----
	flag_soil     = 1
	froot_l(:)    = 0d0
	root_bio_l(:) = 0d0
	do i = 1, nsol
	
		if( flag_soil == 0 ) exit

		if( root_depth <= sol_z(i) ) then
			tgt_depth = root_depth
			flag_soil = 0
		endif
		if( root_depth > sol_z(i) ) then
			tgt_depth = sol_z(i)
		endif

		cum_root(i) = 1.0d0 / (1.0d0 - exp(-rdp) ) * ( 1.0d0 - exp( -rdp * tgt_depth / root_depth ) )
		
		if(i == 1) then
			froot_l(i) = cum_root(i)
			root_bio_l(i) = froot_l(i) * root_bio
		endif
		if(i >= 2) then
			froot_l(i) = cum_root(i) - cum_root(i-1)
			root_bio_l(i) = froot_l(i) * root_bio
		endif

	enddo
	
end subroutine s_root_structure

end module root_structure
