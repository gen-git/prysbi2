module spm_param_fix

use spm_param
use control_param
implicit none
private
public :: s_spm_param_fix
contains

subroutine s_spm_param_fix

implicit none

integer :: check_flag


!--- Fix number of soil layer ---
nsol = common_nsol

hi_change_type   = 0    ! HI changes with year (1) or not change (0)
base_year        = 2006 ! Base year for the calculation of HI change with year
cons_swatnut     = 0    ! Calculate swatnut subroutine (1) or not (2) ! ON for AgGRID phase2 simulation 2016/05/08c
cons_rothc       = 0    ! Calculate RothC model (1) or not (2)
cons_strsn       = 0    ! Consider nitrogen stress of SWAT type (1) or not (0)　! ON for AgGRID phase2 simulation 2016/05/08c


!--- Fix crop parameters ---
check_flag = 0

if( target_plant=='soybean' ) then

check_flag = 1

lai_growth_type =  2          ! Leaf growth type
cold_heat_type  =  0          ! Consider cold stress type 1 (1), type 2 (2), or not (0)
path            =  1          ! C3 (1) or C4 (2)                                              (-)
init_bio        =  50.0d0     ! Initial biomass                                               (kg dry/ha) (Original)
tgdd            =  1380d0     ! Total heat unit (gdd) needed for maturity                     (deg C) (Original)
l_emerge        =  7.170d0    ! Emerging time                                                 (day) (Arif et al. 2008) Soybean
tmbs            =  8.0d0      ! Base temperature                                              (deg C) (Yin & van Laar 2005) Soybean
max_rd          =  1700d0     ! Maximum root depth                                            (mm) (Neitsch et al. 2005) Soybean
rdp             =  10.0d0     ! Root distribution parameter                                   (-) (Neitsch et al. 2005) C3, C4
clf_ratio1      =  0.02507d0  ! Coefficient 1 of leaf ratio funciton  (-)                     (-) (Green energy 1987) Soybean
clf_ratio2      = -1.162d0    ! Coefficient 2 of leaf ratio funciton  (-)                     (-) (Green energy 1987) Soybean
clf_ratio3      = -0.757d0    ! Coefficient 3 of leaf ratio funciton  (-)                     (-) (Green energy 1987) Soybean
sla             =  0.0025     ! SLA                                                           (ha/kg) (Green energy 1987) Soybean
sla_change      =  0.0d0      ! SLA change rate                                               (ha/kg/day) (Yin & van Laar 2005) Soybean
frgrw1          =  0.15d0     ! Shape parameter 1 for canopy height                           (-) (Neitsch et al. 2005) Soybean
frgrw2          =  0.50d0     ! Shape parameter 2 for canopy height                           (-) (Neitsch et al. 2005) Soybean
laimx1          =  0.05d0     ! Shape parameter 3 for canopy height                           (-) (Neitsch et al. 2005) Soybean
laimx2          =  0.95d0     ! Shape parameter 4 for canopy height                           (-) (Neitsch et al. 2005) Soybean
cht_max         =  0.80d0     ! Maximum canopy height                                         (m) (Neitsch et al. 2005) Soybean
rd_m_root_rate  =  0.00582d0  ! Maintenance respiration coefficient of root                   (g C/g C) (Lokupitiya et al. 2009) (x16/44) Soybean
rd_m_shoot_rate =  0.00352d0  ! Maintenance respiration coefficient of shoot                  (g C/g C) (Lokupitiya et al. 2009) (x16/44) Soybean
rd_g_all_rate   =  0.154d0    ! Growth respiration coefficient                                (g C/g C) (Lokupitiya et al. 2009) (x16/44) Soybean
q_base          =  2.0d0      ! Q10 parameter of maintenance respiration                      (-) (Sellers et al. 1996) C3, C4
trd_base        =  20d0       ! Shape parameter temperature sensitivity of root               (-) (Lokupitiya et al. 2009) C3, C4
trd_root1       =  1.3d0      ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
trd_root2       =  55d0       ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
trd_shoot1      =  1.3d0      ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
trd_shoot2      =  55d0       ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
c_rate          =  1d0/2.2d0  ! Fraction of Carbon to biomass                                 (kg C/kg dry) (Original) C3, C4
b_dash          =  0.01d0     ! Parameter of Ball model                                       (mol/m2/s) (Sellers et al. 1996) C3
mm              =  9.0d0      ! Parameter of Ball model                                       (-) (Sellers et al. 1996) C3
kapa            = -9999.0d0   ! Suppression coefficient of rubisco controling photosynthesis  (-)
cold_heat_coef1 = -9999.0d0   ! Parameter for cold stress type 1 (Slope of the model)         (kg/10a/deg C)
cold_heat_coef2 =  0.0        ! Parameter for cold stress type 1 (intercept of the model)     (kg/10a/deg C)
cold_fphu_av    =  0.5d0      ! Average fgdd considering cold stress type 1                   (-) (Original)
cold_fphu_sd    =  0.2d0      ! S.D. of fgdd of temperature weighting of cold stress type 1   (-) (Original)
basehimx        =  0.31d0     ! Base Harvest index                                            (-) (Neitsch et al. 2005) Soybean
yield_correct   =  1.0d0      ! Adjustment parameter of harvest index                         (-) (Original)
ic_himx         =  0.0d0      ! Change rate of harvest index                                  (rate/year) (Original)
c1              =  373.250d0  ! Base CO2 concentration used for parameter adjustment          (ppm) (Original)
phi             =  0.08d0     ! Intrinsic quantum efficiency                                  (mol/mol) (Sellers et al. 1996) C3
kmc25           =  405.0d0    ! Michaelis-Menten constant for CO2 at 25 deg C                 (μ mol/mol) (Kaschuk et al. 2012) C3
kmo25           =  278000.0d0 ! Michaelis-Menten constant for O2 at 25 deg C                  (μ mol/mol) (Kaschuk et al. 2012) C3
ekmc            =  79430.0d0  ! Activation energy for kmc                                     (J/mol) (Kaschuk et al. 2012) C3
ekmo            =  36380.0d0  ! Activation energy for kmo                                     (J/mol) (Kaschuk et al. 2012) C3
evcmax          =  65330.0d0  ! Acviation energy for Vcmax                                    (J/mol) (Kaschuk et al. 2012) C3
rd25_fac        =  0.015d0    ! Respiration factor                                            (-) (Sellers et al. 1996) C3
erd             =  46390.0d0  ! Acviation energy for Rd                                       (J/mol) (Kaschuk et al. 2012) C3
ejmax           =  37000.0d0  ! Acviation energy for J                                        (J/mol) (Kaschuk et al. 2012) C3
djmax           =  200000.0d0 ! Deactivation energy for J                                     (J/mol) (Kaschuk et al. 2012) C3
sjmax           =  650.0d0    ! Entropy term for J                                            (J/K/mol) (Kaschuk et al. 2012) C3
cup_cfj         =  0.9d0      ! Coupling coefficient for J                                    (-) (Medlyn et al. 2002) C3
vtop            =  100.0d0    ! Average Rubisco capacity at top of the canopy                (μmol/m2/s)  (Sellers et al. 1996) C3
leaf_w          =  0.1d0      ! Leaf width                                                    (m) (Original) Soybean
kw              =  1.0d0      ! Wind speed extinction coefficient                             (-) (Original)
ag_resid_rate   =  0.05d0     ! Rate of resids to above ground biomass inputed to the soil    (-) (Original)
co2_dr_vcmax25  = -0.25d0     ! Vcmax adjustment prameter                                     (-) (Original)
co2_dr_jmax25   = -0.15d0     ! Jmax adjustment prameter                                      (-) (Original)
co2_dr_sla      =  0.0d0      ! SLA adjustment prameter                                       (-) (Original)
co2_dr_rd25_fac =  0.0d0      ! rd25_fac adjustment parameter                                 (-) (Original)
adjust_np_stress=  1.0d0      ! NP stress adjuster                                            (-) (Original)
adjust_e        =  1.0d0      ! evcmax and ejmax adjuster                                     (-) (Original)
fgdd_correct    =  1.0d0      ! yield correction coefficient when fgdd < 1.0                  (-) (Original)
ms_ext_coef     =  0.65d0     ! extinction coefficient                                        (-) (Neitsch et al. 2005) Soybean
endif


if( target_plant=='wheat_winter' ) then

check_flag = 1

lai_growth_type =  2          ! Leaf growth type
cold_heat_type  =  0          ! Consider cold stress type 1 (1), type 2 (2), or not (0)
path            =  1          ! C3 (1) or C4 (2)                                              (-)
init_bio        =  50.0d0     ! Initial biomass                                               (kg dry/ha)  (Original)
tgdd            =  1000d0     ! Total heat unit (gdd) needed for maturity                     (deg C) (Original)
l_emerge        =  7.00d0     ! Emerging time                                                 (day) (Original)
tmbs            =  0.0d0      ! Base temperature                                              (deg C) (Yin & van Laar 2005 p.47)  Wheat
max_rd          =  1300d0     ! Maximum root depth                                            (mm) (Neitsch et al. 2005) Winter wheat
rdp             =  10.0d0     ! Root distribution parameter                                   (-) (Neitsch et al. 2005) C3, C4
clf_ratio1      =  0.0134d0   ! Coefficient 1 of leaf ratio funciton  (-)                     (-) (Green Energy 1987) Spring wheat
clf_ratio2      = -1.229d0    ! Coefficient 1 of leaf ratio funciton  (-)                     (-) (Green Energy 1987) Spring wheat
clf_ratio3      = -0.7873d0   ! Coefficient 3 of leaf ratio funciton  (-)                     (-) (Green energy 1987) Winter wheat
sla             =  0.0028d0   ! SLA                                                           (ha/kg) (Yin & van Laar 2005) Wheat
sla_change      =  0.0d0      ! SLA change rate                                               (ha/kg/day)
frgrw1          =  0.05d0     ! Shape parameter 1 for canopy height                           (-) (Neitsch et al. 2005) Winter wheat
frgrw2          =  0.45d0     ! Shape parameter 2 for canopy height                           (-) (Neitsch et al. 2005) Winter wheat
laimx1          =  0.05d0     ! Shape parameter 3 for canopy height                           (-) (Neitsch et al. 2005) Winter wheat
laimx2          =  0.95d0     ! Shape parameter 4 for canopy height                           (-) (Neitsch et al. 2005) Winter wheat
cht_max         =  0.90d0     ! Maximum canopy height                                         (m) (Neitsch et al. 2005) Winter wheat
rd_m_root_rate  =  0.00581d0  ! Maintenance respiration coefficient of root                   (g C/g C) (Lokupitiya et al. 2009) (x16/44) Wheat
rd_m_shoot_rate =  0.00267d0  ! Maintenance respiration coefficient of shoot                  (g C/g C) (Lokupitiya et al. 2009) (x16/44) Wheat
rd_g_all_rate   =  0.08036d0  ! Growth respiration coefficient                                (g C/g C) (Lokupitiya et al. 2009) (x16/44) Wheat
q_base          =  2.0d0      ! Q10 parameter of maintenance respiration                      (-) (Sellers et al. 1996) C3, C4
trd_base        =  20d0       ! Shape parameter temperature sensitivity of root               (-) (Lokupitiya et al. 2009) C3, C4
trd_root1       =  1.3d0      ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
trd_root2       =  55d0       ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
trd_shoot1      =  1.3d0      ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
trd_shoot2      =  55d0       ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
c_rate          =  1d0/2.2d0  ! Fraction of Carbon to biomass                                 (kg C/kg dry) (Original) C3, C4
b_dash          =  0.01d0     ! Parameter of Ball model                                       (mol/m2/s) (Sellers et al. 1996) C3
mm              =  9.0d0      ! Parameter of Ball model                                       (-) (Sellers et al. 1996) C3
kapa            = -9999.0d0    ! Suppression coefficient of rubisco controling photosynthesis  (-)
cold_heat_coef1 = -9999.0d0    ! Parameter for cold stress type 1 (Slope of the model)         (kg/10a/deg C)
cold_heat_coef2 =  0.0        ! Parameter for cold stress type 1 (intercept of the model)     (kg/10a/deg C)
cold_fphu_av    =  0.5d0      ! Average fgdd considering cold stress type 1                   (-) (Original)
cold_fphu_sd    =  0.2d0      ! S.D. of fgdd of temperature weighting of cold stress type 1   (-) (Original)
basehimx        =  0.40d0     ! Base Harvest index                                            (-) (Neitsch et al. 2005) Winter wheat
yield_correct   =  1.0d0      ! Adjustment parameter of harvest index                         (-) (Original)
ic_himx         =  0.0d0      ! Change rate of harvest index                                  (rate/year)  (Original)
c1              =  373.250d0  ! Base CO2 concentration used for parameter adjustment          (ppm) (Original)
phi             =  0.08d0     ! Intrinsic quantum efficiency                                  (mol/mol) (Sellers et al. 1996) C3
kmc25           =  405.0d0    ! Michaelis-Menten constant for CO2 at 25 deg C                 (μ mol/mol)  (Kaschuk et al. 2012) C3
kmo25           =  278000.0d0 ! Michaelis-Menten constant for O2 at 25 deg C                  (μ mol/mol)  (Kaschuk et al. 2012) C3
ekmc            =  79430.0d0  ! Activation energy for kmc                                     (J/mol) (Kaschuk et al. 2012) C3
ekmo            =  36380.0d0  ! Activation energy for kmo                                     (J/mol) (Kaschuk et al. 2012) C3
evcmax          =  65330.0d0  ! Acviation energy for Vcmax                                    (J/mol) (Kaschuk et al. 2012) C3
rd25_fac        =  0.015d0    ! Respiration factor                                            (-) (Sellers et al. 1996) C3
erd             =  46390.0d0  ! Acviation energy for Rd                                       (J/mol) (Kaschuk et al. 2012) C3
ejmax           =  37000.0d0  ! Acviation energy for J                                        (J/mol) (Kaschuk et al. 2012) C3
djmax           =  200000.0d0 ! Deactivation energy for J                                     (J/mol) (Kaschuk et al. 2012) C3
sjmax           =  650.0d0    ! Entropy term for J                                            (J/K/mol) (Kaschuk et al. 2012) C3
cup_cfj         =  0.9d0      ! Coupling coefficient for J                                    (-) (Medlyn et al. 2002) C3
vtop            =  100.0d0    ! Average Rubisco capacity at top of the canopy                (μmol/m2/s)  (Sellers et al. 1996) C3
leaf_w          =  0.01d0     ! Leaf width                                                    (m) (Original) Wheat
kw              =  3.0d0      ! Wind speed extinction coefficient                             (-) (Original)
ag_resid_rate   =  0.05d0     ! Rate of resids to above ground biomass inputed to the soil    (-) (Original)
co2_dr_vcmax25  = -0.27d0     ! Vcmax adjustment prameter                                     (-) (Ainthworth & Long 2005) C3 (x 2.11)
co2_dr_jmax25   = -0.11d0     ! Jmax adjustment prameter                                      (-) (Ainthworth & Long 2005) C3 (x 2.11)
co2_dr_sla      =  0.0d0      ! SLA adjustment prameter                                       (-) (Original)
co2_dr_rd25_fac =  0.0d0      ! rd25_fac adjustment parameter                                 (-) (Original)
adjust_np_stress=  1.0d0      ! NP stress adjuster                                            (-) (Original)
adjust_e        =  1.0d0      ! evcmax and ejmax adjuster                                     (-) (Original)
fgdd_correct    =  1.0d0      ! yield correction coefficient when fgdd < 1.0                  (-) (Original)
ms_ext_coef     =  0.65d0     ! extinction coefficient                                        (-) (Neitsch et al. 2005) Winter Wheat
endif


if( target_plant == 'maize' .or. target_plant == 'maize_major' .or. target_plant == 'maize_second') then

check_flag = 1

lai_growth_type =  2          ! Leaf growth type
cold_heat_type  =  0          ! Consider cold stress type 1 (1), type 2 (2), or not (0)
path            =  2          ! C3 (1) or C4 (2)                                              (-)
init_bio        =  50.0d0     ! Initial biomass                                               (kg dry/ha) (Original)
tgdd            =  1780d0     ! Total heat unit (gdd) needed for maturity                     (deg C) (Original)
l_emerge        =  7.00d0     ! Emerging time                                                 (day) (Original)
tmbs            =  8.0d0      ! Base temperature                                              (deg C) (Yin & van Laar 2005 p.47) Maize
max_rd          =  2000d0     ! Maximum root depth                                            (mm) (Neitsch et al. 2005) Maize
rdp             =  10.0d0     ! Root distribution parameter                                   (-) (Neitsch et al. 2005) C3, C4
clf_ratio1      =  0.03891d0  ! Coefficient 1 of leaf ratio funciton  (-)                     (-) (Green energy 1987) Maize
clf_ratio2      = -2.117d0    ! Coefficient 2 of leaf ratio funciton  (-)                     (-) (Green energy 1987) Maize
clf_ratio3      = -0.86829d0  ! Coefficient 3 of leaf ratio funciton  (-)                     (-) (Green energy 1987) Maize
sla             =  0.0022d0   ! SLA                                                           (ha/kg) (Yin & van Laar 2005) Maize
sla_change      =  0.0d0      ! SLA change rate                                               (ha/kg/day)
frgrw1          =  0.15d0     ! Shape parameter 1 for canopy height                           (-) (Neitsch et al. 2005) Maize
frgrw2          =  0.50d0     ! Shape parameter 2 for canopy height                           (-) (Neitsch et al. 2005) Maize
laimx1          =  0.05d0     ! Shape parameter 3 for canopy height                           (-) (Neitsch et al. 2005) Maize
laimx2          =  0.95d0     ! Shape parameter 4 for canopy height                           (-) (Neitsch et al. 2005) Maize
cht_max         =  2.50d0     ! Maximum canopy height                                         (m) (Neitsch et al. 2005) Maize
rd_m_root_rate  =  0.00581d0  ! Maintenance respiration coefficient of root                   (g C/g C) (Lokupitiya et al. 2009) (x16/44) Maize
rd_m_shoot_rate =  0.00352d0  ! Maintenance respiration coefficient of shoot                  (g C/g C) (Lokupitiya et al. 2009) (x16/44) Maize
rd_g_all_rate   =  0.08218d0  ! Growth respiration coefficient                                (g C/g C) (Lokupitiya et al. 2009) (x16/44) Maize
q_base          =  2.0d0      ! Q10 parameter of maintenance respiration                      (-) (Sellers et al. 1996) C3, C4
trd_base        =  20d0       ! Shape parameter temperature sensitivity of root               (-) (Lokupitiya et al. 2009) C3, C4
trd_root1       =  1.3d0      ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
trd_root2       =  55d0       ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
trd_shoot1      =  1.3d0      ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
trd_shoot2      =  55d0       ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
c_rate          =  1d0/2.2d0  ! Fraction of Carbon to biomass                                 (kg C/kg dry) (Original) C3, C4
b_dash          =  0.04d0     ! Parameter of Ball model                                       (mol/m2/s) (Sellers et al. 1996) C4
mm              =  4.0d0      ! Parameter of Ball model                                       (-) (Sellers et al. 1996) C4
kapa            =  0.00746d0  ! Suppression coefficient of rubisco controling photosynthesis  (-) (Stich et al. 2003) C4
cold_heat_coef1 = -9999.0d0    ! Parameter for cold stress type 1 (Slope of the model)         (kg/10a/deg C)
cold_heat_coef2 =  0.0        ! Parameter for cold stress type 1 (intercept of the model)     (kg/10a/deg C)
cold_fphu_av    =  0.7d0      ! Average fgdd considering cold stress type 1                   (-) (Original)
cold_fphu_sd    =  0.2d0      ! S.D. of fgdd of temperature weighting of cold stress type 1   (-) (Original)
basehimx        =  0.50d0     ! Base Harvest index                                            (-) (Neitsch et al. 2005) Maize
yield_correct   =  1.0d0      ! Adjustment parameter of harvest index                         (-) (Original)
ic_himx         =  0.0d0      ! Change rate of harvest index                                  (rate/year) (Original)
c1              =  373.250d0  ! Base CO2 concentration used for parameter adjustment          (ppm) (Original)
phi             =  0.05d0     ! Intrinsic quantum efficiency                                  (mol/mol) (Sellers et al. 1996) C3
kmc25           =  405.0d0    ! Michaelis-Menten constant for CO2 at 25 deg C                 (μ mol/mol) (Kaschuk et al. 2012) C3
kmo25           =  278000.0d0 ! Michaelis-Menten constant for O2 at 25 deg C                  (μ mol/mol) (Kaschuk et al. 2012) C3
ekmc            =  79430.0d0  ! Activation energy for kmc                                     (J/mol) (Kaschuk et al. 2012) C3
ekmo            =  36380.0d0  ! Activation energy for kmo                                     (J/mol) (Kaschuk et al. 2012) C3
evcmax          =  65330.0d0  ! Acviation energy for Vcmax                                    (J/mol) (Kaschuk et al. 2012) C3
rd25_fac        =  0.025d0    ! Respiration factor                                            (-) (Sellers et al. 1996) C4
erd             =  46390.0d0  ! Acviation energy for Rd                                       (J/mol) (Kaschuk et al. 2012) C3
ejmax           =  37000.0d0  ! Acviation energy for J                                        (J/mol) (Kaschuk et al. 2012) C3
djmax           =  200000.0d0 ! Deactivation energy for J                                     (J/mol) (Kaschuk et al. 2012) C3
sjmax           =  650.0d0    ! Entropy term for J                                            (J/K/mol) (Kaschuk et al. 2012) C3
cup_cfj         =  0.9d0      ! Coupling coefficient for J                                    (-) (Medlyn et al. 2002) C3
vtop            =  30.0d0     ! Average Rubisco capacity at top of the canopy                 (μmol/m2/s) (Sellers et al. 1996) C4
leaf_w          =  0.10d0     ! Leaf width                                                    (m) (Original) Maize
kw              =  1.0d0      ! Wind speed extinction coefficient                             (-) (Original)
ag_resid_rate   =  0.05d0     ! Rate of resids to above ground biomass inputed to the soil    (-) (Original)
co2_dr_vcmax25  =  0.0d0      ! Vcmax adjustment prameter                                     (-) (Original) ! 2016/05/22 change
co2_dr_jmax25   =  0.0d0      ! Jmax adjustment prameter                                      (-) (Original) ! 2016/05/22 change
co2_dr_sla      =  0.0d0      ! SLA adjustment prameter                                       (-) (Original)
co2_dr_rd25_fac =  0.0d0      ! rd25_fac adjustment parameter                                 (-) (Original)
adjust_np_stress=  1.0d0      ! NP stress adjuster                                            (-) (Original)
adjust_e        =  1.0d0      ! evcmax and ejmax adjuster                                     (-) (Original)
fgdd_correct    =  1.0d0      ! yield correction coefficient when fgdd < 1.0                  (-) (Original)
ms_ext_coef     =  0.65d0     ! extinction coefficient                                        (-) (Neitsch et al. 2005) Maize
endif


if( target_plant=='wheat_spring' ) then

check_flag = 1

lai_growth_type =  2          ! Leaf growth type
cold_heat_type  =  0          ! Consider cold stress type 1 (1), type 2 (2), or not (0)
path            =  1          ! C3 (1) or C4 (2)                                              (-)
init_bio        =  50.0d0     ! Initial biomass                                               (kg dry/ha) (Original)
tgdd            =  1200d0     ! Total heat unit (gdd) needed for maturity                     (deg C) (Original)
l_emerge        =  7.00d0     ! Emerging time                                                 (day) (Original)
tmbs            =  0.0d0      ! Base temperature                                              (deg C) (Yin & van Laar 2005 p.47) Wheat
max_rd          =  2000d0     ! Maximum root depth                                            (mm) (Neitsch et al. 2005) Spring wheat
rdp             =  10.0d0     ! Root distribution parameter                                   (-) (Neitsch et al. 2005) C3, C4
clf_ratio1      =  0.0269d0   ! Coefficient 1 of leaf ratio funciton  (-)                     (-) (Green Energy 1987) Spring wheat
clf_ratio2      = -0.038d0    ! Coefficient 1 of leaf ratio funciton  (-)                     (-) (Green Energy 1987) Spring wheat
clf_ratio3      = -0.4933d0   ! Coefficient 3 of leaf ratio funciton  (-)                     (-) (Green energy 1987) Spring wheat
sla             =  0.0028d0   ! SLA                                                           (ha/kg) (Yin & van Laar 2005) Wheat
sla_change      =  0.0d0      ! SLA change rate                                               (ha/kg/day)
frgrw1          =  0.15d0     ! Shape parameter 1 for canopy height                           (-) (Neitsch et al. 2005) Spring wheat
frgrw2          =  0.50d0     ! Shape parameter 2 for canopy height                           (-) (Neitsch et al. 2005) Spring wheat
laimx1          =  0.05d0     ! Shape parameter 3 for canopy height                           (-) (Neitsch et al. 2005) Spring wheat
laimx2          =  0.95d0     ! Shape parameter 4 for canopy height                           (-) (Neitsch et al. 2005) Spring wheat
cht_max         =  0.90d0     ! Maximum canopy height                                         (m) (Neitsch et al. 2005) Spring wheat
rd_m_root_rate  =  0.00581d0  ! Maintenance respiration coefficient of root                   (g C/g C) (Lokupitiya et al. 2009) (x16/44) Wheat
rd_m_shoot_rate =  0.00267d0  ! Maintenance respiration coefficient of shoot                  (g C/g C) (Lokupitiya et al. 2009) (x16/44) Wheat
rd_g_all_rate   =  0.08036d0  ! Growth respiration coefficient                                (g C/g C) (Lokupitiya et al. 2009) (x16/44) Wheat
q_base          =  2.0d0      ! Q10 parameter of maintenance respiration                      (-) (Sellers et al. 1996) C3, C4
trd_base        =  20d0       ! Shape parameter temperature sensitivity of root               (-) (Lokupitiya et al. 2009) C3, C4
trd_root1       =  1.3d0      ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
trd_root2       =  55d0       ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
trd_shoot1      =  1.3d0      ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
trd_shoot2      =  55d0       ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
c_rate          =  1d0/2.2d0  ! Fraction of Carbon to biomass                                 (kg C/kg dry) (Original) C3, C4
b_dash          =  0.01d0     ! Parameter of Ball model                                       (mol/m2/s) (Sellers et al. 1996) C3
mm              =  9.0d0      ! Parameter of Ball model                                       (-) (Sellers et al. 1996) C3
kapa            = -9999.0d0    ! Suppression coefficient of rubisco controling photosynthesis  (-)
cold_heat_coef1 = -9999.0d0    ! Parameter for cold stress type 1 (Slope of the model)         (kg/10a/deg C)
cold_heat_coef2 =  0.0        ! Parameter for cold stress type 1 (intercept of the model)     (kg/10a/deg C)
cold_fphu_av    =  0.5d0      ! Average fgdd considering cold stress type 1                   (-) (Original)
cold_fphu_sd    =  0.2d0      ! S.D. of fgdd of temperature weighting of cold stress type 1   (-) (Original)
basehimx        =  0.42d0     ! Base Harvest index                                            (-) (Neitsch et al. 2005) Spring wheat
yield_correct   =  1.0d0      ! Adjustment parameter of harvest index                         (-) (Original)
ic_himx         =  0.0d0      ! Change rate of harvest index                                  (rate/year) (Original)
c1              =  373.250d0  ! Base CO2 concentration used for parameter adjustment          (ppm) (Original)
phi             =  0.08d0     ! Intrinsic quantum efficiency                                  (mol/mol) (Sellers et al. 1996) C3
kmc25           =  405.0d0    ! Michaelis-Menten constant for CO2 at 25 deg C                 (μ mol/mol) (Kaschuk et al. 2012) C3
kmo25           =  278000.0d0 ! Michaelis-Menten constant for O2 at 25 deg C                  (μ mol/mol) (Kaschuk et al. 2012) C3
ekmc            =  79430.0d0  ! Activation energy for kmc                                     (J/mol) (Kaschuk et al. 2012) C3
ekmo            =  36380.0d0  ! Activation energy for kmo                                     (J/mol) (Kaschuk et al. 2012) C3
evcmax          =  65330.0d0  ! Acviation energy for Vcmax                                    (J/mol) (Kaschuk et al. 2012) C3
rd25_fac        =  0.015d0    ! Respiration factor                                            (-) (Sellers et al. 1996) C3
erd             =  46390.0d0  ! Acviation energy for Rd                                       (J/mol) (Kaschuk et al. 2012) C3
ejmax           =  37000.0d0  ! Acviation energy for J                                        (J/mol) (Kaschuk et al. 2012) C3
djmax           =  200000.0d0 ! Deactivation energy for J                                     (J/mol) (Kaschuk et al. 2012) C3
sjmax           =  650.0d0    ! Entropy term for J                                            (J/K/mol) (Kaschuk et al. 2012) C3
cup_cfj         =  0.9d0      ! Coupling coefficient for J                                    (-) (Medlyn et al. 2002) C3
vtop            =  100.0d0    ! Average Rubisco capacity at top of the canopy                (μmol/m2/s)  (Sellers et al. 1996) C3
leaf_w          =  0.01d0     ! Leaf width                                                    (m) (Original) Wheat
kw              =  3.0d0      ! Wind speed extinction coefficient                             (-) (Original)
ag_resid_rate   =  0.05d0     ! Rate of resids to above ground biomass inputed to the soil    (-) (Original) [wnated]
co2_dr_vcmax25  = -0.27d0     ! Vcmax adjustment prameter                                     (-) (Ainthworth & Long 2005) C3 (x 2.11)
co2_dr_jmax25   = -0.11d0     ! Jmax adjustment prameter                                      (-) (Ainthworth & Long 2005) C3 (x 2.11)
co2_dr_sla      =  0.0d0      ! SLA adjustment prameter                                       (-) (original)
co2_dr_rd25_fac =  0.0d0      ! rd25_fac adjustment parameter                                 (-) (Original)
adjust_np_stress=  1.0d0      ! NP stress adjuster                                            (-) (Original)
adjust_e        =  1.0d0      ! evcmax and ejmax adjuster                                     (-) (Original)
fgdd_correct    =  1.0d0      ! yield correction coefficient when fgdd < 1.0                  (-) (Original)
ms_ext_coef     =  0.65d0     ! extinction coefficient                                        (-) (Neitsch et al. 2005) Spring Wheat
endif


if( target_plant == 'rice' .or. target_plant == 'rice_major' .or. target_plant == 'rice_second') then

check_flag = 1

lai_growth_type =  2          ! Leaf growth type
cold_heat_type  =  0          ! Consider cold stress type 1 (1), type 2 (2), or not (0)
path            =  1          ! C3 (1) or C4 (2)                                              (-)
init_bio        =  50.0d0     ! Initial biomass                                               (kg dry/ha) (Original)
tgdd            =  1580d0     ! Total heat unit (gdd) needed for maturity                     (deg C) (Original)
l_emerge        =  7.00d0     ! Emerging time                                                 (day) (Original)
tmbs            =  8.0d0      ! Base temperature                                              (deg C) (Yin & van Laar 2005 p.47) Rice
max_rd          =  900d0      ! Maximum root depth                                            (mm) (Neitsch et al. 2005) Rice
rdp             =  10.0d0     ! Root distribution parameter                                   (-) (Neitsch et al. 2005) C3, C4
clf_ratio1      =  0.021348d0 ! Coefficient 1 of leaf ratio funciton  (-)                     (-) (Green Energy 1987) Spring wheat
clf_ratio2      = -0.437d0    ! Coefficient 1 of leaf ratio funciton  (-)                     (-) (Green Energy 1987) Spring wheat
clf_ratio3      = -0.56510d0  ! Coefficient 3 of leaf ratio funciton  (-)                     (-) (Green energy 1987) Rice
sla             =  0.0023d0   ! SLA                                                           (ha/kg) (Yin & van Laar 2005) Rice
sla_change      =  0.0d0      ! SLA change rate                                               (ha/kg/day)
frgrw1          =  0.30d0     ! Shape parameter 1 for canopy height                           (-) (Neitsch et al. 2005) Spring wheat
frgrw2          =  0.70d0     ! Shape parameter 2 for canopy height                           (-) (Neitsch et al. 2005) Spring wheat
laimx1          =  0.01d0     ! Shape parameter 3 for canopy height                           (-) (Neitsch et al. 2005) Spring wheat
laimx2          =  0.95d0     ! Shape parameter 4 for canopy height                           (-) (Neitsch et al. 2005) Spring wheat
cht_max         =  0.80d0     ! Maximum canopy height                                         (m) (Neitsch et al. 2005) Spring wheat
rd_m_root_rate  =  0.00581d0  ! Maintenance respiration coefficient of root                   (g C/g C) (Lokupitiya et al. 2009) (x16/44) Wheat
rd_m_shoot_rate =  0.00267d0  ! Maintenance respiration coefficient of shoot                  (g C/g C) (Lokupitiya et al. 2009) (x16/44) Wheat
rd_g_all_rate   =  0.08036d0  ! Growth respiration coefficient                                (g C/g C) (Lokupitiya et al. 2009) (x16/44) Wheat
q_base          =  2.0d0      ! Q10 parameter of maintenance respiration                      (-) (Sellers et al. 1996) C3, C4
trd_base        =  20d0       ! Shape parameter temperature sensitivity of root               (-) (Lokupitiya et al. 2009) C3, C4
trd_root1       =  1.3d0      ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
trd_root2       =  55d0       ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
trd_shoot1      =  1.3d0      ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
trd_shoot2      =  55d0       ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
c_rate          =  1d0/2.2d0  ! Fraction of Carbon to biomass                                 (kg C/kg dry) (Original) C3, C4
b_dash          =  0.01d0     ! Parameter of Ball model                                       (mol/m2/s) (Sellers et al. 1996) C3
mm              =  9.0d0      ! Parameter of Ball model                                       (-) (Sellers et al. 1996) C3
kapa            = -9999.0d0    ! Suppression coefficient of rubisco controling photosynthesis  (-)
cold_heat_coef1 = -9999.0d0    ! Parameter for cold stress type 1 (Slope of the model)         (kg/10a/deg C)
cold_heat_coef2 =  0.0        ! Parameter for cold stress type 1 (intercept of the model)     (kg/10a/deg C)
cold_fphu_av    =  0.5d0      ! Average fgdd considering cold stress type 1                   (-) (Original)
cold_fphu_sd    =  0.2d0      ! S.D. of fgdd of temperature weighting of cold stress type 1   (-) (Original)
basehimx        =  0.50d0     ! Base Harvest index                                            (-) Neitsch et al. 2005) Rice
yield_correct   =  1.0d0      ! Adjustment parameter of harvest index                         (-) (Original)
ic_himx         =  0.0d0      ! Change rate of harvest index                                  (rate/year) (Original)
c1              =  373.250d0  ! Base CO2 concentration used for parameter adjustment          (ppm) (Original)
phi             =  0.08d0     ! Intrinsic quantum efficiency                                  (mol/mol) (Sellers et al. 1996) C3
kmc25           =  405.0d0    ! Michaelis-Menten constant for CO2 at 25 deg C                 (μ mol/mol) (Kaschuk et al. 2012) C3
kmo25           =  278000.0d0 ! Michaelis-Menten constant for O2 at 25 deg C                  (μ mol/mol) (Kaschuk et al. 2012) C3
ekmc            =  79430.0d0  ! Activation energy for kmc                                     (J/mol) (Kaschuk et al. 2012) C3
ekmo            =  36380.0d0  ! Activation energy for kmo                                     (J/mol) (Kaschuk et al. 2012) C3
evcmax          =  65330.0d0  ! Acviation energy for Vcmax                                    (J/mol) (Kaschuk et al. 2012) C3
rd25_fac        =  0.015d0    ! Respiration factor                                            (-) (Sellers et al. 1996) C3
erd             =  46390.0d0  ! Acviation energy for Rd                                       (J/mol) (Kaschuk et al. 2012) C3
ejmax           =  37000.0d0  ! Acviation energy for J                                        (J/mol) (Kaschuk et al. 2012) C3
djmax           =  200000.0d0 ! Deactivation energy for J                                     (J/mol) (Kaschuk et al. 2012) C3
sjmax           =  650.0d0    ! Entropy term for J                                            (J/K/mol) (Kaschuk et al. 2012) C3
cup_cfj         =  0.9d0      ! Coupling coefficient for J                                    (-) (Medlyn et al. 2002) C3
vtop            =  100.0d0    ! Average Rubisco capacity at top of the canopy                 (μmol/m2/s)  (Sellers et al. 1996) C3
leaf_w          =  0.01d0     ! Leaf width                                                    (m) (Original) Rice
kw              =  3.0d0      ! Wind speed extinction coefficient                             (-) (Original)
ag_resid_rate   =  0.05d0     ! Rate of resids to above ground biomass inputed to the soil    (-) (Original)
co2_dr_vcmax25  = -0.27d0     ! Vcmax adjustment prameter                                     (-) (Ainthworth & Long 2005) C3 (x 2.11)
co2_dr_jmax25   = -0.11d0     ! Jmax adjustment prameter                                      (-) (Ainthworth & Long 2005) C3 (x 2.11)
co2_dr_sla      =  0.0d0      ! SLA adjustment prameter                                       (-) (Original)
co2_dr_rd25_fac =  0.0d0      ! rd25_fac adjustment parameter                                 (-) (Original)
adjust_np_stress=  1.0d0      ! NP stress adjuster                                            (-) (Original)
adjust_e        =  1.0d0      ! evcmax and ejmax adjuster                                     (-) (Original)
fgdd_correct    =  1.0d0      ! yield correction coefficient when fgdd < 1.0                  (-) (Original)
ms_ext_coef     =  0.65d0     ! extinction coefficient                                        (-) (Neitsch et al. 2005) Rice
endif


if( target_plant == 'sorghum' .or. target_plant == 'sorghum_major' .or. target_plant == 'sorghum_second') then

check_flag = 1

lai_growth_type =  2          ! Leaf growth type
cold_heat_type  =  0          ! Consider cold stress type 1 (1), type 2 (2), or not (0)
path            =  2          ! C3 (1) or C4 (2)                                              (-)
init_bio        =  50.0d0     ! Initial biomass                                               (kg dry/ha) (Original)
tgdd            =  1780d0     ! Total heat unit (gdd) needed for maturity                     (deg C) (Original)
l_emerge        =  7.00d0     ! Emerging time                                                 (day) (Original)
tmbs            =  8.0d0      ! Base temperature                                              (deg C) (Yin & van Laar 2005 p.47) Maize
max_rd          =  2000d0     ! Maximum root depth                                            (mm) (Neitsch et al. 2005) Sorghum Hay
rdp             =  10.0d0     ! Root distribution parameter                                   (-) (Neitsch et al. 2005) C3, C4
clf_ratio1      =  0.03891d0  ! Coefficient 1 of leaf ratio funciton  (-)                     (-) (Green energy 1987) Maize
clf_ratio2      = -2.117d0    ! Coefficient 2 of leaf ratio funciton  (-)                     (-) (Green energy 1987) Maize
clf_ratio3      = -0.86829d0  ! Coefficient 3 of leaf ratio funciton  (-)                     (-) (Green energy 1987) Maize
sla             =  0.0022d0   ! SLA                                                           (ha/kg) (Yin & van Laar 2005) Maize
sla_change      =  0.0d0      ! SLA change rate                                               (ha/kg/day)
frgrw1          =  0.15d0     ! Shape parameter 1 for canopy height                           (-) (Neitsch et al. 2005) Sorghum Hay
frgrw2          =  0.50d0     ! Shape parameter 2 for canopy height                           (-) (Neitsch et al. 2005) Sorghum Hay
laimx1          =  0.05d0     ! Shape parameter 3 for canopy height                           (-) (Neitsch et al. 2005) Sorghum Hay
laimx2          =  0.95d0     ! Shape parameter 4 for canopy height                           (-) (Neitsch et al. 2005) Sorghum Hay
cht_max         =  1.50d0     ! Maximum canopy height                                         (m) (Neitsch et al. 2005) Sorghum Hay
rd_m_root_rate  =  0.00581d0  ! Maintenance respiration coefficient of root                   (g C/g C) (Lokupitiya et al. 2009) (x16/44) Maize
rd_m_shoot_rate =  0.00352d0  ! Maintenance respiration coefficient of shoot                  (g C/g C) (Lokupitiya et al. 2009) (x16/44) Maize
rd_g_all_rate   =  0.08218d0  ! Growth respiration coefficient                                (g C/g C) (Lokupitiya et al. 2009) (x16/44) Maize
q_base          =  2.0d0      ! Q10 parameter of maintenance respiration                      (-) (Sellers et al. 1996) C3, C4
trd_base        =  20d0       ! Shape parameter temperature sensitivity of root               (-) (Lokupitiya et al. 2009) C3, C4
trd_root1       =  1.3d0      ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
trd_root2       =  55d0       ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
trd_shoot1      =  1.3d0      ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
trd_shoot2      =  55d0       ! Shape parameter temperature sensitivity of root               (-) (Sellers et al. 1996) C3, C4
c_rate          =  1d0/2.2d0  ! Fraction of Carbon to biomass                                 (kg C/kg dry) (Original) C3, C4
b_dash          =  0.04d0     ! Parameter of Ball model                                       (mol/m2/s) (Sellers et al. 1996) C4
mm              =  4.0d0      ! Parameter of Ball model                                       (-) (Sellers et al. 1996) C4
kapa            =  0.00746d0  ! Suppression coefficient of rubisco controling photosynthesis  (-) (Stich et al. 2003) C4
cold_heat_coef1 = -9999.0d0    ! Parameter for cold stress type 1 (Slope of the model)         (kg/10a/deg C)
cold_heat_coef2 =  0.0        ! Parameter for cold stress type 1 (intercept of the model)     (kg/10a/deg C)
cold_fphu_av    =  0.7d0      ! Average fgdd considering cold stress type 1                   (-) (Original)
cold_fphu_sd    =  0.2d0      ! S.D. of fgdd of temperature weighting of cold stress type 1   (-) (Original)
basehimx        =  0.50d0     ! Base Harvest index                                            (-) (Neitsch et al. 2005) Original
yield_correct   =  1.0d0      ! Adjustment parameter of harvest index                         (-) (Original)
ic_himx         =  0.0d0      ! Change rate of harvest index                                  (rate/year) (Original)
c1              =  373.250d0  ! Base CO2 concentration used for parameter adjustment          (ppm) (Original)
phi             =  0.05d0     ! Intrinsic quantum efficiency                                  (mol/mol) (Sellers et al. 1996) C3
kmc25           =  405.0d0    ! Michaelis-Menten constant for CO2 at 25 deg C                 (μ mol/mol) (Kaschuk et al. 2012) C3
kmo25           =  278000.0d0 ! Michaelis-Menten constant for O2 at 25 deg C                  (μ mol/mol) (Kaschuk et al. 2012) C3
ekmc            =  79430.0d0  ! Activation energy for kmc                                     (J/mol) (Kaschuk et al. 2012) C3
ekmo            =  36380.0d0  ! Activation energy for kmo                                     (J/mol) (Kaschuk et al. 2012) C3
evcmax          =  65330.0d0  ! Acviation energy for Vcmax                                    (J/mol) (Kaschuk et al. 2012) C3
rd25_fac        =  0.025d0    ! Respiration factor                                            (-) (Sellers et al. 1996) C4
erd             =  46390.0d0  ! Acviation energy for Rd                                       (J/mol) (Kaschuk et al. 2012) C3
ejmax           =  37000.0d0  ! Acviation energy for J                                        (J/mol) (Kaschuk et al. 2012) C3
djmax           =  200000.0d0 ! Deactivation energy for J                                     (J/mol) (Kaschuk et al. 2012) C3
sjmax           =  650.0d0    ! Entropy term for J                                            (J/K/mol) (Kaschuk et al. 2012) C3
cup_cfj         =  0.9d0      ! Coupling coefficient for J                                    (-) (Medlyn et al. 2002) C3
vtop            =  30.0d0     ! Average Rubisco capacity at top of the canopy                 (μmol/m2/s) (Sellers et al. 1996) C4
leaf_w          =  0.10d0     ! Leaf width                                                    (m) (Original) Original
kw              =  1.0d0      ! Wind speed extinction coefficient                             (-) (Original)
ag_resid_rate   =  0.05d0     ! Rate of resids to above ground biomass inputed to the soil    (-) (Original)
co2_dr_vcmax25  =  0.0d0      ! Vcmax adjustment prameter                                     (-) (Original)
co2_dr_jmax25   =  0.0d0      ! Jmax adjustment prameter                                      (-) (Original)
co2_dr_sla      =  0.0d0      ! SLA adjustment prameter                                       (-) (Original)
co2_dr_rd25_fac =  0.0d0      ! rd25_fac adjustment parameter                                 (-) (Original)
adjust_np_stress=  1.0d0      ! NP stress adjuster                                            (-) (Original)
adjust_e        =  1.0d0      ! evcmax and ejmax adjuster                                     (-) (Original)
fgdd_correct    =  1.0d0      ! yield correction coefficient when fgdd < 1.0                  (-) (Original)
ms_ext_coef     =  0.65d0     ! extinction coefficient                                        (-) (Neitsch et al. 2005) Sorghum Hay
endif


!--- Check error ---
if( check_flag == 0 ) then
write(*,*) 'Error: target_plant is not specified'
stop
endif


!--- Read namelist ---
if( read_spm_param_namelist == 1 ) then
open(1, file=prysbi_namelist_file)
read(1, spm_namelist)
close(1)
endif


end subroutine s_spm_param_fix

end module spm_param_fix
