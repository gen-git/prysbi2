module var_read_all

  implicit none

  character(3)    :: avar_param(4)     = (/'1  ','2  ','3  ','4  '/)

  character(1000) :: path_ana
  character(1000) :: path_topo
  character(1000) :: path_soil
  character(1000) :: path_weather
  character(1000) :: path_yield
  character(1000) :: path_pheno
  character(1000) :: path_co2
  character(1000) :: path_gdp
  character(1000) :: path_param
  character(1000) :: path_fix_param
  character(20)   :: tgt_version
  character(20)   :: tgt_crop
  character(8)    :: avar_topo(7)      = (/'elev    ','garea   ','landmask','lati    ','long    ','xcode   ','ycode   '/)
  character(8)    :: avar_soil(6)      = (/'solz    ','solbd   ','solawc  ','solalb  ','solcbn  ','solclay '/)
  character(8)    :: avar_weather(6)   = (/'tmaxsfc ','tminsfc ','precsfc ','dswrfsfc','rhsfc   ','windsfc '/)
  character(14)   :: avar_pheno(2)     = (/'planting      ', 'maturity      '/)
  character(3)    :: avar_fix_param(1) = (/'1  '/)

  real(4)         :: rnan
  integer         :: nsol
  integer         :: iyrstt
  integer         :: iyrend

  integer         :: ntime
  integer         :: nyear
  integer         :: k2i(320*160)
  integer         :: k2j(320*160)
  integer         :: ngrd
  integer         :: nvar_soil
  integer         :: nvar_topo
  integer         :: nvar_weather
  integer         :: nvar_pheno
  integer         :: nvar_param
  integer         :: nvar_fix_param
  integer         :: nensemble

  integer         :: sim_flag

  real(4),      allocatable :: kdata_topo(:,:)
  character(7), allocatable :: kdata_gcode(:)
  real(4),      allocatable :: kdata_weather(:,:,:)
  real(4),      allocatable :: kdata_yield(:,:,:)
  real(4),      allocatable :: kdata_soil(:,:,:)
  real(4),      allocatable :: kdata_pheno(:,:)
  real(4),      allocatable :: kdata_co2(:,:)
  real(4),      allocatable :: kdata_param(:,:,:)
  real(4),      allocatable :: kdata_fix_param(:,:)
  real(4),      allocatable :: kdata_gdp(:,:)

  namelist/read_all_list/rnan, nsol, iyrstt, iyrend, &
  & path_ana, path_topo, path_soil, path_weather, path_pheno, path_yield, path_co2, &
  & tgt_version, tgt_crop, avar_topo, avar_soil, avar_weather, avar_pheno, &
  & avar_param, nensemble, path_param, path_fix_param, path_gdp

end module
