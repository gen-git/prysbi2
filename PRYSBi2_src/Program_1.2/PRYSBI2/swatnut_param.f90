module swatnut_param

implicit none

!----- Parameters -----
real(8) :: rsdco_pl   ! plant residue decomposition coefficient
real(8) :: sdnco      ! Denitrification threshold: fraction of field
real(8) :: cmn        ! Rate factor for humus mineralization
real(8) :: nactfr     ! Nitrogen active pool fraction
real(8) :: cdn        ! Coefficient of denitrification
real(8) :: cecf       ! Volatilization CEC factor
real(8) :: anion_excl ! Fraction of porosity from which anions are excluded
real(8) :: nperco     ! Nitrate percolation coefficient (0-1)
real(8) :: psp        ! Phosphorus availability index
real(8) :: bk         ! Phosphorus stabilization coef
real(8) :: phoskd     ! Phosphorus soil partitioning coefficient
real(8) :: n_updis    ! Nitrogen uptake distribution parameter
real(8) :: nfrth      ! Threshold of N stress
real(8) :: p_updis    ! Nitrogen uptake distribution parameter
real(8) :: pfrth      ! Threshold of N stress
real(8) :: pperco     ! Phosphorus percolation coefficient (0-1)
real(8) :: rcn_sub    ! Concentration of nitrogen in the rainfall
real(8) :: pltnfr1    ! Biomass|nitrogen uptake parameter #1
real(8) :: pltnfr2    ! Biomass|nitrogen uptake parameter #2
real(8) :: pltnfr3    ! Biomass|nitrogen uptake parameter #3
real(8) :: pltnfrav   ! Normal fraction of nitrogen in the plant biomass near maturity
real(8) :: pltpfr1    ! Biomass|Phosphorus uptake parameter #1
real(8) :: pltpfr2    ! Biomass|Phosphorus uptake parameter #2
real(8) :: pltpfr3    ! Biomass|Phosphorus uptake parameter #3
real(8) :: pltpfrav   ! Normal fraction of phosphorus in the plant biomass near maturity
integer :: cons_nfix  ! Consider nitrogen fixation (1) or not (0)


!----- Values caluclated later -----
real(8) :: bio_n1     ! Shape parameters for plant nitrogen uptake equation (calculated by ascrv)
real(8) :: bio_n2     ! Shape parameters for plant nitrogen uptake equation (calculated by ascrv)
real(8) :: bio_p1     ! Shape parameters for plant phosphorus uptake equation (calculated by ascrv)
real(8) :: bio_p2     ! Shape parameters for plant phosphorus uptake equation (calculated by ascrv)

end module swatnut_param
