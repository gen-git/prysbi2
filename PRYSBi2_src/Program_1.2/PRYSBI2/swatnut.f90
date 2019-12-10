module swatnut

use swatnut_param
use swatnut_param_fix

implicit none

private
public :: s_swatnut
contains

subroutine s_swatnut( &
	cons_rothc       , & !in    Consider RothC model (1) or not (0)
	soilc            , & !in    Soil carbon of each compartment (t/ha)
	nsol             , & !in    Number of soil layers
	sol_tmp          , & !in    Soil temperature in each layer (deg C)
	sol_st           , & !in    Amount of water in each soil layer at the day (mm)
	sol_fc           , & !in    Field capacity of each soil layer (mm)
	sol_wpmm         , & !in    Wilting point of each soil layer (mm)
	sol_z            , & !in    Length from the top of the soil to the botom of the each soil layer (mm)
	prec             , & !in    Precipitation depth (at the ground level) (mm)
	sepbtm           , & !in    Percolation from bottom of entire soil profile (mm)
	surfq            , & !in    Accumulated runoff or rainfall excess (mm)
	sol_bd           , & !in    Bulk density of each soil layer (Mg/m^3)
	conv_wt          , & !in    factor which converts kg/kg soil to kg/ha
	fgdd             , & !in    Fraction of growing degree days to GDD needed for maturity
	biomass          , & !in    Biomass of the plant (kg dry/ha)
	dbio             , & !in    Biomass generated on current day (kg dry/ha)
	root_depth       , & !in    Current rooting depth (mm)
	sol_prk          , & !in    Percolation from each soil layer (mm)
	sol_ul           , & !in    Water content of each layer at satulation (mm)
	sol_aorgn        , & !inout Amount of nitrogen stored in humus in each layer (kg/ha)
	sol_cbn          , & !inout Percent organic carbon in each soil layer (%)
	sol_fon          , & !inout Amount of organic N stored in the fresh        in each layer (kg/ha)
	sol_fop          , & !inout Amount of organic P stored in the fresh        in each layer (kg/ha)
	sol_no3          , & !inout Amount of mineral N stored in the nitrate pool in each layer (kg/ha)
	sol_orgn         , & !inout Amount of organic N stored in the stable pool  in each layer (kg/ha)
	sol_orgp         , & !inout Amount of organic P stored in the stable pool  in each layer (kg/ha)
	sol_rsd          , & !inout Amount of organic matter in each soil layer (kg/ha)
	sol_nh3          , & !inout Amount of mineral N stored in the ammonium     in each layer (kg/ha)
	sol_actp         , & !inout Amount of organic P stored in the active pool  in each layer (kg/ha)
	sol_solp         , & !inout Amount of mineral P stored in the solution     in each layer (kg/ha)
	sol_stap         , & !inout Amount of mineral P stored in the stable pool  in each layer (kg/ha)
	plantn           , & !inout Amount of N in plant biomass (kg/ha)
	plantp           , & !inout Amount of P in plant biomass (kg/ha)
	strsn            , & !out   Nitrogen stress (0: heigh stress, 1: low stress)
	strsp            , & !out   Phosphorus stress (0: heigh stress, 1: low stress)
	wdntl,             & !out Amount of nitrogen lost by denitrification (kg/ha)
	hmn_tot,           & !out Amount of nitrogen mineralized from sol_aorgn in soil profile (kg/ha)
	hmp_tot,           & !out Amount of phosphorus mineralized from sol_orgp in soil profile (kg/ha)
	rmn_tot,           & !out Amount of nitrogen decomposed from sol_fon in soil profile (kg/ha)
	rmp_tot            ) !out Amount of phosphorus docomposed from sol_fop in soil profile (kg/ha)
	
	
	implicit none
	
	integer, intent(in)    :: cons_rothc
	real(8), intent(in)    :: soilc(5)
	integer, intent(in)    :: nsol
	real(8), intent(in)    :: sol_tmp(nsol)
	real(8), intent(in)    :: sol_st(nsol)
	real(8), intent(in)    :: sol_fc(nsol)
	real(8), intent(in)    :: sol_wpmm(nsol)
	real(8), intent(in)    :: sol_z(nsol)
	real(8), intent(in)    :: prec
	real(8), intent(in)    :: sepbtm
	real(8), intent(in)    :: surfq
	real(8), intent(in)    :: sol_bd(nsol)
	real(8), intent(in)    :: conv_wt(nsol)
	real(8), intent(in)    :: fgdd
	real(8), intent(in)    :: biomass
	real(8), intent(in)    :: dbio
	real(8), intent(in)    :: root_depth
	real(8), intent(in)    :: sol_prk(nsol)
	real(8), intent(in)    :: sol_ul(nsol)
	real(8), intent(inout) :: sol_aorgn(nsol)
	real(8), intent(inout) :: sol_cbn(nsol)
	real(8), intent(inout) :: sol_fon(nsol)
	real(8), intent(inout) :: sol_fop(nsol)
	real(8), intent(inout) :: sol_no3(nsol)
	real(8), intent(inout) :: sol_orgn(nsol)
	real(8), intent(inout) :: sol_orgp(nsol)
	real(8), intent(inout) :: sol_rsd(nsol)
	real(8), intent(inout) :: sol_nh3(nsol)
	real(8), intent(inout) :: sol_actp(nsol)
	real(8), intent(inout) :: sol_solp(nsol)
	real(8), intent(inout) :: sol_stap(nsol)
	real(8), intent(inout) :: plantn
	real(8), intent(inout) :: plantp
	real(8), intent(out)   :: strsn
	real(8), intent(out)   :: strsp
	real(8), intent(out)   :: wdntl
	real(8), intent(out)   :: hmn_tot
	real(8), intent(out)   :: hmp_tot
	real(8), intent(out)   :: rmn_tot
	real(8), intent(out)   :: rmp_tot
	
	
	real(8) :: rnit, rvol
	
	
	!----- Calculate N and P dynamics in soil -----
	if( cons_rothc == 0 ) then
	
	call Mnminrl( &
	nsol, &                  !in    No. of soil layer
	rsdco_pl, &              !in    plant residue decomposition coefficient [param]
	sdnco, &                 !in    Denitrification threshold: fraction of field [param]
	cmn, &                   !in    Rate factor for humus mineralization [param]
	nactfr, &                !in    Nitrogen active pool fraction [param]
	cdn, &                   !in    Coefficient of denitrification [param]
	sol_tmp, &               !in    Average temperature of soil layer (deg C)
	sol_st, &                !in    Amount of water in soil layer at the day (mm)
	sol_fc, &                !in    Amount of water available to plants in soil layer at fc (mm) [force]
	sol_aorgn, &             !inout Amount of nitrogen stored in humus (kg/ha)
	sol_cbn, &               !inout Percent organic carbon in soil layer (%)
	sol_fon, &               !inout Amount of nitrogen stored in the fresh (kg/ha)
	sol_fop, &               !inout Amount of phosphorus stored in the fresh (kg/ha)
	sol_no3, &               !inout Amount of nitrogen stored in the soil layer (kg/ha)
	sol_orgn, &              !inout Organic N concentration in soil layer (kg/ha)
	sol_orgp, &              !inout Organic P concentration in soil layer (kg/ha)
	sol_rsd, &               !inout Amount of organic matter in the soil layer (kg/ha)
	sol_solp, &              !inout Amount of phosohorus stored in solution (kg/ha)  
	wdntl,                 & !out Amount of nitrogen lost by denitrification (kg/ha)
	hmn_tot,               & !out Amount of nitrogen mineralized from sol_aorgn in soil profile (kg/ha)
	hmp_tot,               & !out Amount of phosphorus mineralized from sol_orgp in soil profile (kg/ha)
	rmn_tot,               & !out Amount of nitrogen decomposed from sol_fon in soil profile (kg/ha)
	rmp_tot)                 !out Amount of phosphorus docomposed from sol_fop in soil profile (kg/ha)
	
	endif
	
	if( cons_rothc == 1) then
	
	call Mnminrl_rev( &
	soilc, &                 !in    Soil carbon of each compartment (t/ha)
	nsol, &                  !in    No. of soil layer
	rsdco_pl, &              !in    plant residue decomposition coefficient [param]
	sdnco, &                 !in    Denitrification threshold: fraction of field [param]
	cmn, &                   !in    Rate factor for humus mineralization [param]
	nactfr, &                !in    Nitrogen active pool fraction [param]
	cdn, &                   !in    Coefficient of denitrification [param]
	sol_tmp, &               !in    Average temperature of soil layer (deg C)
	sol_st, &                !in    Amount of water in soil layer at the day (mm)
	sol_fc, &                !in    Amount of water available to plants in soil layer at fc (mm) [force]
	sol_aorgn, &             !inout Amount of nitrogen stored in humus (kg/ha)
	sol_cbn, &               !inout Percent organic carbon in soil layer (%)
	sol_fon, &               !inout Amount of nitrogen stored in the fresh (kg/ha)
	sol_fop, &               !inout Amount of phosphorus stored in the fresh (kg/ha)
	sol_no3, &               !inout Amount of nitrogen stored in the soil layer (kg/ha)
	sol_orgn, &              !inout Organic N concentration in soil layer (kg/ha)
	sol_orgp, &              !inout Organic P concentration in soil layer (kg/ha)
	sol_rsd, &               !inout Amount of organic matter in the soil layer (kg/ha)
	sol_solp, &              !inout Amount of phosohorus stored in solution (kg/ha)  
	wdntl,                 & !out Amount of nitrogen lost by denitrification (kg/ha)
	hmn_tot,               & !out Amount of nitrogen mineralized from sol_aorgn in soil profile (kg/ha)
	hmp_tot,               & !out Amount of phosphorus mineralized from sol_orgp in soil profile (kg/ha)
	rmn_tot,               & !out Amount of nitrogen decomposed from sol_fon in soil profile (kg/ha)
	rmp_tot)                 !out Amount of phosphorus docomposed from sol_fop in soil profile (kg/ha)
	
	endif
	
	!----- Calculate nitrification and NH3 aerification -----
	call Mnitvol( &
	nsol, &                  !in No. of soil layer
	cecf, &                  !in Volatilization CEC factor
	sol_fc, &                !in Amount of water available to plants in soil layer at fc (mm) [force]
	sol_st, &                !in Amount of water in soil layer at the day (mm)
	sol_tmp, &               !in Average temperature of soil layer (deg C)
	sol_wpmm, &              !in Wilting point (mm) [force]
	sol_z, &                 !in Depth to bottom of soil layer (mm) [force]
	sol_nh3, &               !inout Amount of nitrogen stored in the ammonium (kg/ha)
	sol_no3, &               !inout Amount of nitrogen stored in the soil layer (kg/ha)
	rnit, &                  !out amount of NH4 that was nitrated
	rvol )                   !out amount of NH4 that was volatilized
	
	
	!----- Phosphorus relared process -----
	call Mpminrl( &
	nsol, &                  !in No. of soil layer
	psp, &                   !in Phosphorus availability index [param]
	bk,&                     !in Phosphorus stabilization coef [param]
	sol_actp, &              !inout Aamount of phosphorus stored in the active pool (kg/ha)
	sol_solp, &              !inout Amount of phosohorus stored in solution (kg/ha)
	sol_stap )               !inout Amount of phosphorus in the soil layer stored in the stable pool (kg/ha)
	
	
	!----- Add N to soil from rain -----
	call Mnrain( &
	prec, &                  !in Precipitation (mm)
	rcn_sub, &	             !in Concentration of nitrogen in the rainfall (mg/L) [param]
	sol_no3(1) )             !inout Amount of nitrogen stored in the soil layer (kg/ha)
	
	
	!----- Calculate N percolation -----
	call Mnlch( &
	nsol, &                  !in No. of soil layer
	anion_excl, &            !in Fraction of porosity from which anions are excluded [param]
	nperco, &                !in Nitrate percolation coefficient (0-1) [param]
	surfq, &                 !in Surface runoff generated on day (mm) [param]
	sol_prk, &               !in Percolation from soil layer (mm)
	sol_ul, &                !in Amount of water held in the soil layer at saturation (mm) [force]
	sepbtm, &                !in Percolation from bottom of soil profile (mm)
	sol_no3 )                !inout Amount of nitrogen stored in the soil layer (kg/ha)
	
	
	!----- Phosphorus movement -----
	call Msolp( &
	nsol, &                  !in No. of soil layer
	conv_wt, &               !in Factor which converts kg/kg soil to kg/ha
	phoskd, &                !in Phosphorus soil partitioning coefficient
	pperco, &                !in Phosphorus percolation coefficient (0-1)
	surfq, &                 !in Surface runoff generated on day (mm) [param]
	sol_bd, &                !in Balk density (Mg/m^3) [force]
	sol_prk, &               !in Percolation from soil layer (mm)
	sol_z, &                 !in Depth to bottom of soil layer (mm) [force]
	sol_solp )               !inout Amount of phosohorus stored in solution (kg/ha)
	
	
	!----- Calculate N stress -----
	call Mnup( &
	pltnfr1, &               !in Biomass|nitrogen uptake parameter #1 [param]
	pltnfr3, &               !in Biomass|nitrogen uptake parameter #3 [param]
	bio_n1, &                !in Shape parameters for plant nitrogen uptake equation
	bio_n2, &                !in Shape parameters for plant nitrogen uptake equation
	n_updis, &               !in Nitrogen uptake distribution parameter [param]
	nfrth, &                 !in Threshold of N stress [param]
	fgdd, &                  !in Rate of phu to tphu
	biomass, &               !in Biomass of plant (kg/ha)
	dbio, &		             !in Biomass generated on current day (kg/ha)
	nsol, &                  !in No. of soil layer
	sol_z, &                 !in Depth to bottom of soil layer (mm) [force]
	root_depth, &            !in Current rooting depth (mm)
    fgdd, &                  !in Ratio of fgdd to tgdd (-)
    sol_st,  &               !in Amount of water in each soil layer at the day (mm)
    sol_fc, &                !in Field capacity of each soil layer (mm)
    cons_nfix, &             !in Consider Nigrogen fixation (1) or not (0)
	plantn, &                !inout Amount of nitrogen in plant biomass (kg/ha)
	sol_no3, &               !inout Amount of nitrogen stored in the soil layer (kg/ha)
	strsn )                  !out N stress factor
	
	!----- Calculate P stress -----
	call Mpup( &
	pltpfr1, &               !in Biomass|nitrogen uptake parameter #1 [param]
	pltpfr3, &               !in Biomass|nitrogen uptake parameter #3 [param]
	bio_p1, &                !in Shape parameters for plant nitrogen uptake equation
	bio_p2, &                !in Shape parameters for plant nitrogen uptake equation
	p_updis, &               !in Nitrogen uptake distribution parameter [param]
	pfrth, &                 !in Threshold of N stress [param]
	fgdd, &                  !in Rate of phu to tphu
	biomass, &               !in Biomass of plant (kg/ha)
	dbio, &		             !in Biomass generated on current day (kg/ha)
	nsol, &                  !in No. of soil layer
	sol_z, &                 !in Depth to bottom of soil layer (mm) [force]
	root_depth, &            !in Current rooting depth (mm)
	plantp, &                !inout Amount of nitrogen in plant biomass (kg/ha)
	sol_solp, &              !inout Amount of nitrogen stored in the soil layer (kg/ha)
	strsp )                  !out N stress factor
	
	
end subroutine s_swatnut
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Mnminrl(                         & 
                    sol_nly,                & !in No. of soil layer
                    rsdco_pl,               & !in plant residue decomposition coefficient [param]
                    sdnco,                  & !in Denitrification threshold: fraction of field [param]
                    cmn,                    & !in Rate factor for humus mineralization [param]
                    nactfr,                 & !in Nitrogen active pool fraction [param]
                    cdn,                    & !in Coefficient of denitrification [param]
                    sol_tmp,                & !in Average temperature of soil layer (deg C)
                    sol_st,                 & !in Amount of water in soil layer at the day (mm)
                    sol_fc,                 & !in Amount of water available to plants in soil layer at fc (mm) [force]
                    sol_aorgn,              & !inout Amount of nitrogen stored in humus (kg/ha)
                    sol_cbn,                & !inout Percent organic carbon in soil layer (%)
                    sol_fon,                & !inout Amount of nitrogen stored in the fresh (kg/ha)
                    sol_fop,                & !inout Amount of phosphorus stored in the fresh (kg/ha)
                    sol_no3,                & !inout Amount of nitrogen stored in the soil layer (kg/ha)
                    sol_orgn,               & !inout Organic N concentration in soil layer (kg/ha)
                    sol_orgp,               & !inout Organic P concentration in soil layer (kg/ha)
                    sol_rsd,                & !inout Amount of organic matter in the soil layer (kg/ha)
                    sol_solp,               & !inout Amount of phosohorus stored in solution (kg/ha)                      
					wdntl,                  & !out Amount of nitrogen lost by denitrification (kg/ha)
					hmn_tot,                & !out Amount of nitrogen mineralized from sol_aorgn in soil profile (kg/ha)
					hmp_tot,                & !out Amount of phosphorus mineralized from sol_orgp in soil profile (kg/ha)
					rmn_tot,                & !out Amount of nitrogen decomposed from sol_fon in soil profile (kg/ha)
					rmp_tot                 & !out Amount of phosphorus docomposed from sol_fop in soil profile (kg/ha)
                    )                         

	implicit none
	
    integer , intent(in)    :: sol_nly
    real(8) , intent(in)    :: rsdco_pl
    real(8) , intent(in)    :: sdnco
    real(8) , intent(in)    :: cmn
    real(8) , intent(in)    :: nactfr
    real(8) , intent(in)    :: cdn
    real(8) , intent(in)    :: sol_tmp(sol_nly)
    real(8) , intent(in)    :: sol_st(sol_nly)
    real(8) , intent(in)    :: sol_fc(sol_nly)
    real(8) , intent(inout) :: sol_aorgn(sol_nly)
    real(8) , intent(inout) :: sol_cbn(sol_nly)
    real(8) , intent(inout) :: sol_fon(sol_nly)
    real(8) , intent(inout) :: sol_fop(sol_nly)
    real(8) , intent(inout) :: sol_no3(sol_nly)
    real(8) , intent(inout) :: sol_orgn(sol_nly)
    real(8) , intent(inout) :: sol_orgp(sol_nly)
    real(8) , intent(inout) :: sol_rsd(sol_nly)
    real(8) , intent(inout) :: sol_solp(sol_nly)
	real(8) , intent(out)   :: wdntl
	real(8) , intent(out)   :: hmn_tot
	real(8) , intent(out)   :: hmp_tot
	real(8) , intent(out)   :: rmn_tot
	real(8) , intent(out)   :: rmp_tot

    integer :: k, kk
    real(8) :: rmn1, rmp, xx, csf, rwn, hmn, hmp, r4, cnr, cnrf, cpr
    real(8) :: cprf, ca, decr, rdc, cdg, sut, wdn

    !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    !::: this subroutine estimates daily nitrogen and phosphorus       :::
    !::: mineralization and immobilization considering fresh organic   :::
    !::: material (plant residue) and active and stable humus material :::
    !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


	wdntl = 0.0d0
	hmn_tot = 0.0d0
	hmp_tot = 0.0d0
	rmn_tot = 0.0d0
	rmp_tot = 0.0d0
    do k = 1, sol_nly

	  if( k == 1) then

		  !--- Calc Nitrogen in the fresh organic pool ---
		  sol_fon(k) = 0.0015d0 * sol_rsd(k)
		  sol_fop(k) = 0.0003d0 * sol_rsd(k)

	  endif
	  
      if( sol_tmp(k) > 0.0d0 ) then

        !--- compute soil water factor ---
        sut = sol_st(k) / sol_fc(k)  ! nutrient cycling water factor
        sut = min( 1.0d0, sut )
        sut = max( 0.05d0, sut )

        !--- compute soil temperature factor ---
        xx = sol_tmp(k)
        cdg = 0.9d0 * xx / ( xx + exp( 9.93d0 - 0.312d0 * xx ) ) + 0.1d0 ! nutrient cycling temperature factor
        cdg = max( 0.1d0, cdg )

        !--- compute combined factor ---
        xx = cdg * sut
        if( xx < 0.0d0 ) xx = 0.0d0
        if( xx > 1.0d6 ) xx = 1.0d6
        csf = sqrt(xx)


        !--- compute flow from active to stable pools ---
		!--- Active N <--> Stable N
        rwn = 0.1d-4 * ( sol_aorgn(k) * ( 1.0d0 / nactfr - 1.0d0 ) - sol_orgn(k) )
        if( rwn > 0.0d0 ) then
          rwn = min( rwn, sol_aorgn(k) )
        else
          rwn = -( min( abs(rwn), sol_orgn(k) ) )
        endif
        sol_orgn(k) = max( 1.0d-6, sol_orgn(k) + rwn )
        sol_aorgn(k) = max( 1.0d-6, sol_aorgn(k) - rwn )


        !--- compute humus mineralization on active organic n ---
		!--- NO3 <-- Active N
        hmn = cmn * csf * sol_aorgn(k)
        hmn = min( hmn, sol_aorgn(k) )
        sol_aorgn(k) = max( 1.0d-6, sol_aorgn(k) - hmn )
        sol_no3(k) = sol_no3(k) + hmn

		hmn_tot = hmn_tot + hmn !GS

        !--- compute humus mineralization on active organic p ---
		!--- Solution P <-- Active P
        xx = sol_orgn(k) + sol_aorgn(k)
        if( xx > 1.0d-6 ) then
          hmp = 1.4d0 * hmn * sol_orgp(k) / xx
        else
          hmp = 0.0d0
        end if
        hmp = min( hmp, sol_orgp(k) )
        sol_orgp(k) = sol_orgp(k) - hmp
        sol_solp(k) = sol_solp(k) + hmp

		hmp_tot = hmp_tot + hmp !GS

        !! compute residue decomp and mineralization of 
        !! fresh organic n and p (upper two layers only)
        rmn1 = 0.0d0
        rmp = 0.0d0
        if( k == 1 ) then

		  !----- Calc C:N and C:P, and N cycling factor -----
          r4 = 0.58d0 * sol_rsd(k)
          if( sol_fon(k) + sol_no3(k) > 1.0d-4 ) then
            cnr = r4 / ( sol_fon(k) + sol_no3(k) )
            if( cnr > 500.0d0 ) cnr = 500.0d0
            cnrf = exp( -0.693d0 * ( cnr - 25.0d0 ) / 25.0d0 )
          else
            cnrf = 1.0d0
          end if

          if( sol_fop(k) + sol_solp(k) > 1.0d-4 ) then
            cpr = r4 / ( sol_fop(k) + sol_solp(k) )
            if( cpr > 5000.0d0 ) cpr = 5000.0d0
            cprf = exp( -0.693d0 * ( cpr - 200.0d0 ) / 200.0d0 )
          else
            cprf = 1.0d0
          end if
          ca = min( cnrf, cprf, 1.0d0 )

		  !----- Calc mineralization from the residue fresh organic N -----
          decr = rsdco_pl * ca * csf
          decr = max( 0.01d0, decr )
          decr = min( decr, 1.0d0 )

		  !--- residue decomposition ---
          sol_rsd(k) = max( 1.0d-6, sol_rsd(k) )
          rdc = decr * sol_rsd(k)
          sol_rsd(k) = sol_rsd(k) - rdc
          if( sol_rsd(k) < 0.0d0 ) sol_rsd(k) = 0.0d0

		  !--- Fresh N decomposition
          rmn1 = decr * sol_fon(k)
          sol_fon(k) = max( 1.0d-6, sol_fon(k) )
          sol_fon(k) = sol_fon(k) - rmn1

		  !--- Fresh P decomposition ---
          sol_fop(k) = max( 1.0d-6, sol_fop(k) )
          rmp = decr * sol_fop(k)
          sol_fop(k) = sol_fop(k) - rmp

		  !--- NO3 <-- Fresh N
          sol_no3(k) = sol_no3(k) + 0.8d0 * rmn1
          !--- Active N <-- Fresh N
          sol_aorgn(k) = sol_aorgn(k) + 0.2d0 * rmn1

		  !--- Solution P <-- Fresh P
          sol_solp(k) = sol_solp(k) + 0.8d0 * rmp
		  !--- Active P <-- Fresh P
          sol_orgp(k) = sol_orgp(k) + 0.2d0 * rmp

          rmn_tot = rmn_tot + rmn1 !GS
          rmp_tot = rmp_tot + rmp  !GS

        end if


        !! compute denitrification
		!--- NO3 --> N2O or N2
		sut = sol_st(k) / sol_fc(k)
        if( sut >= sdnco ) then
          wdn = sol_no3(k) * ( 1.0d0 - exp( -cdn * cdg * sol_cbn(k) ) )
        else
          wdn = 0.0d0
        end if
        sol_no3(k) = sol_no3(k) - wdn

		wdntl = wdntl + wdn

      end if
    end do

end subroutine Mnminrl
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Mnrain(                          & !::: 
                   precipday,               & !::: In
                   rcn_sub,                 & !::: In
                   sol_no3                  & !::: Inout
                   )                          !::: 

	implicit none
	
    real(8) , intent(in)    :: precipday
    real(8) , intent(in)    :: rcn_sub
    real(8) , intent(inout) :: sol_no3(1)

    real(8) :: no3pcp

    !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    !::: this subroutine adds nitrate from rainfall to the soil profile
    !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    !! calculate nitrate in precipitation
    no3pcp = 0.01d0 * rcn_sub * precipday
    sol_no3(1) = sol_no3(1) + no3pcp

end subroutine Mnrain
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Mnitvol(                         & ! 
                    sol_nly,                & ! In
                    cecf,                   & ! In
                    sol_fc,                 & ! In
                    sol_st,                 & ! In
                    sol_tmp,                & ! In
                    sol_wpmm,               & ! In
                    sol_z,                  & ! In
                    sol_nh3,                & ! Inout
                    sol_no3,                & ! Inout
					rnit,                   & ! Out
					rvol                    & ! Out
                    )                         ! 
                    
implicit none

integer , intent(in)    :: sol_nly
real(8) , intent(in)    :: cecf
real(8) , intent(in)    :: sol_fc(sol_nly)
real(8) , intent(in)    :: sol_st(sol_nly)
real(8) , intent(in)    :: sol_tmp(sol_nly)
real(8) , intent(in)    :: sol_wpmm(sol_nly)
real(8) , intent(in)    :: sol_z(sol_nly)
real(8) , intent(inout) :: sol_nh3(sol_nly)
real(8) , intent(inout) :: sol_no3(sol_nly)
real(8) , intent(out)   :: rnit
real(8) , intent(out)   :: rvol

integer :: j, k
real(8)  :: sw25, swwp, swf, xx, dmidl, dpf, akn, akv, rnv, tf

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!::: this subroutine estimates daily mineralization (NH3 to NO3) :::
!::: and volatilization of NH3                                   :::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


do k = 1, sol_nly

		!--- Calc nitrification/volatilization temperature factor ---
		tf = 0.41d0 * ( sol_tmp(k) - 5.0d0 ) / 10.0d0

		if( sol_nh3(k) > 0.0d0 .and. tf >= 0.001d0 ) then


		!--- Calc volatilization soil water factor ---
        sw25 = 0.0d0
        swwp = 0.0d0
        sw25 = sol_wpmm(k) + 0.25d0 * sol_fc(k)
        swwp = sol_wpmm(k) + sol_st(k)
        if (swwp < sw25) then
          swf = 0.0d0
          swf = (swwp - sol_wpmm(k)) /(sw25 - sol_wpmm(k))
        else
          swf = 1.0
        endif

		sw25 = -0.5d0 * sol_wpmm(k) + 0.25d0 * sol_fc(k)
		swwp = sol_wpmm(k) + sol_st(k)


		if( swwp < sw25 ) then
			swf = ( swwp - sol_wpmm(k) ) / ( 0.25d0*sol_fc(k) )
		else
			swf = 1.0d0
		endif


		!--- Calc volatilization depth factor ---
		if( k == 1 ) then
			xx = 0.0d0
		else
			xx = sol_z(k-1)
		endif


		dmidl = ( sol_z(k) + xx ) / 2.0d0
		dpf = 1.0d0 - dmidl / ( dmidl + exp( 4.706d0 - 0.0305d0 * dmidl ) )



		!--- Cqlc total amount of ammonium lost ---
		akn = tf * swf
		akv = tf * dpf * cecf
		rnv = sol_nh3(k) * ( 1.0d0 - exp( -akn - akv ) )

		rnit = 1.0d0 - exp( -akn )
		rvol = 1.0d0 - exp( -akv )

		!! calculate nitrification (NH3 => NO3)
		if( rvol + rnit > 1.0d-6 ) then
			rvol = rnv * rvol / ( rvol + rnit )

			rnit = rnv - rvol
			if( rnit < 0.0d0 ) rnit = 0.0d0
			sol_nh3(k) = max( 1.0d-6, sol_nh3(k) - rnit )
		endif


		if( sol_nh3(k) < 0.0d0 ) then
			rnit = rnit + sol_nh3(k)
			sol_nh3(k) = 0.0d0
		endif

		sol_no3(k) = sol_no3(k) + rnit

		!! calculate ammonia volatilization
		sol_nh3(k) = max( 1.0d-6, sol_nh3(k) - rvol )

		if( sol_nh3(k) < 0.0d0 ) then
			rvol = rvol + sol_nh3(k)
			sol_nh3(k) = 0.0d0
		endif

	end if

end do

end subroutine Mnitvol
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Mnlch(                           & !::: 
                  sol_nly,                  & !::: In
                  anion_excl,               & !::: In
                  nperco,                   & !::: In
                  surfq,                    & !::: In
                  sol_prk,                  & !::: In
                  sol_ul,                   & !::: In
                  sepbtm,                   & !::: In
                  sol_no3                   & !::: Inout
                  )                           !::: 

	implicit none

    integer, intent(in)    :: sol_nly
    real(8) , intent(in)    :: anion_excl
    real(8) , intent(in)    :: nperco
    real(8) , intent(in)    :: surfq
    real(8) , intent(in)    :: sol_prk(sol_nly)
    real(8) , intent(in)    :: sol_ul(sol_nly)
    real(8) , intent(in)    :: sepbtm
    real(8) , intent(inout) :: sol_no3(sol_nly)

    integer :: jj
    real(8) :: sro, ssfnlyr, vv, vno3, co, cosurf, ww, surqno3, percnlyr

    !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    !::: this subroutine simulates the loss of nitrate via surface runoff, :::
    !::: lateral flow, and percolation out of the profile                  :::
    !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    percnlyr = 0.0d0

    do jj = 1, sol_nly

      !! add nitrate leached from layer above
      sol_no3(jj) = sol_no3(jj) + percnlyr

      !! determine concentration of nitrate in mobile water
      if( jj.eq.1 ) then
        sro = surfq
      else
        sro = 0.0d0
      end if
      vv = sol_prk(jj) + sro + dble( 1.0d-10 )
      ww = -vv / ( ( 1.0d0 - anion_excl ) * sol_ul(jj) )
      vno3 = sol_no3(jj) * ( 1.0d0 - exp( ww ) )
      co = max( vno3 / vv, 0.0d0 )

      !! calculate nitrate in ground water runoff
      cosurf = 0.0d0
      cosurf = nperco * co
      if( jj.eq.1 ) then
        surqno3 = surfq * cosurf
        surqno3 = min( surqno3, sol_no3(jj) )
        sol_no3(jj) = sol_no3(jj) - surqno3
      else if( jj.eq.sol_nly ) then
        surqno3 = surqno3 * 0.001d0
        surqno3 = min( surqno3, sol_no3(jj) )
        sol_no3(jj) = sol_no3(jj) - surqno3
      endif

      !! calculate nitrate in percolate
      percnlyr = co * sol_prk(jj)
      percnlyr = min( percnlyr, sol_no3(jj) )
      sol_no3(jj) = sol_no3(jj) - percnlyr

    end do

end subroutine Mnlch
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Mpminrl(                         & !::: 
                    sol_nly,                & !::: In
                    psp,                    & !::: In
                    bk,                     & !::: In
                    sol_actp,               & !::: Inout
                    sol_solp,               & !::: Inout
                    sol_stap                & !::: Inout
                    )                         !::: 

	implicit none

    integer,           intent(in)    :: sol_nly
    real(8) , intent(in)    :: psp
    real(8) , intent(in)    :: bk
    real(8) , intent(inout) :: sol_actp(sol_nly)
    real(8) , intent(inout) :: sol_solp(sol_nly)
    real(8) , intent(inout) :: sol_stap(sol_nly)

    integer :: l
    real(8) :: rto, rmn1, roc

    !::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    !::: this subroutine computes p flux between the labile, active mineral :::
    !::: and stable mineral p pools.                                        :::
    !::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    rto = psp / ( 1.0d0 - psp )

    do l = 1, sol_nly
      rmn1 = ( sol_solp(l) - sol_actp(l) * rto )
      if( rmn1 > 0.d00 ) rmn1 = rmn1 * 0.1d0
      rmn1 = min( rmn1, sol_solp(l) )

      roc = bk * ( 4.0d0 * sol_actp(l) - sol_stap(l) )
      if( roc < 0.0d0 ) roc = roc * 0.1d0
      roc = min( roc, sol_actp(l) )

      sol_stap(l) = sol_stap(l) + roc
      if( sol_stap(l) < 0.0d0 ) sol_stap(l) = 0.0d0

      sol_actp(l) = sol_actp(l) - roc + rmn1
      if( sol_actp(l) < 0.0d0 ) sol_actp(l) = 0.0d0

      sol_solp(l) = sol_solp(l) - rmn1
      if( sol_solp(l) < 0.0d0 ) sol_solp(l) = 0.0d0

    end do

end subroutine Mpminrl
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Msolp(                           & !::: 
                  sol_nly,                  & !::: In
                  conv_wt,                  & !::: In
                  phoskd,                   & !::: In
                  pperco,                   & !::: In
                  surfq,                    & !::: In
                  sol_bd,                   & !::: In
                  sol_prk,                  & !::: In
                  sol_z,                    & !::: In
                  sol_solp                  & !::: Inout
                  )                           !::: 

	implicit none

    integer, intent(in)    :: sol_nly
    real(8) , intent(in)    :: conv_wt(sol_nly)
    real(8) , intent(in)    :: phoskd
    real(8) , intent(in)    :: pperco
    real(8) , intent(in)    :: surfq
    real(8) , intent(in)    :: sol_bd(sol_nly)
    real(8) , intent(in)    :: sol_prk(sol_nly)
    real(8) , intent(in)    :: sol_z(sol_nly)
    real(8) , intent(inout) :: sol_solp(sol_nly)

    real(8) :: xx, vap, surqsolp

    !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    !::: this subroutine calculates the amount of phosphorus lost from the soil  :::
    !::: profile in runoff and the movement of soluble phosphorus from the first :::
    !::: to the second layer via percolation                                     :::
    !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    !! compute soluble P lost in surface runoff
    xx = sol_bd(1) * sol_z(1) * phoskd
    surqsolp = sol_solp(1) * surfq / xx !::: units ==> surqsolp = [kg/ha * mm] / [t/m^3 * mm * m^3/t] = kg/ha
    surqsolp = min( surqsolp, sol_solp(1) )
    surqsolp = max( surqsolp, 0.0d0 )
    sol_solp(1) = sol_solp(1) - surqsolp

    !! compute soluble P leaching
    vap = sol_solp(1) * sol_prk(1) / ( ( conv_wt(1) / 1000.0d0 ) * pperco )
    vap = min( vap, 0.5d0 * sol_solp(1) )
    sol_solp(1) = sol_solp(1) - vap
    if( sol_nly >= 2 ) then
      sol_solp(2) = sol_solp(2) + vap
    end if

end subroutine Msolp
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Mnup(                           & !::: 
                 pltnfr1,                  & !::: In
                 pltnfr3,                  & !::: In
                 bio_n1,                   & !::: In
                 bio_n2,                   & !::: In
                 n_updis,                  & !::: In
                 tmpnfrth,                 & !::: In
                 phuacc,                   & !::: In
                 bio_ms,                   & !::: In
                 dbio,                     & !::: In
                 sol_nly,                  & !::: In
                 sol_z,                    & !::: In
                 root_depth,               & !::: In
                 fgdd,                     & !::: In
                 sol_st,                   & !::: In
                 sol_fc,                   & !::: In
                 cons_nfix,                & !::: In
                 plantn,                   & !::: Inout
                 sol_no3,                  & !::: Inout
                 strsn                     & !::: Out
                 )                           !::: 

	implicit none

	real(8) , intent(in)    :: pltnfr1
	real(8) , intent(in)    :: pltnfr3
	real(8) , intent(in)    :: bio_n1
	real(8) , intent(in)    :: bio_n2
	real(8) , intent(in)    :: n_updis
	real(8) , intent(in)    :: tmpnfrth
	real(8) , intent(in)    :: phuacc
	real(8) , intent(in)    :: bio_ms
	real(8) , intent(in)    :: dbio
	integer , intent(in)    :: sol_nly
	real(8) , intent(in)    :: sol_z(sol_nly)
	real(8) , intent(in)    :: root_depth
	real(8) , intent(in)    :: fgdd
	real(8) , intent(in)    :: sol_st(sol_nly)
	real(8) , intent(in)    :: sol_fc(sol_nly)
	integer , intent(in)    :: cons_nfix
	real(8) , intent(inout) :: plantn
	real(8) , intent(inout) :: sol_no3(sol_nly)
	real(8) , intent(out)   :: strsn

	integer :: l, ir
	real(8) :: uobn, un2, unmx, uno3l, gx, pltfr_n, uno3d, pltnfr
	real(8) :: xx, nfr, nplnt
	real(8) :: pre_bio !GS
	real(8) :: f_gr, f_no3, f_sw, n_fix, n_demand, no3 !GS

	!::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	!::: This subroutine calculates plant nitrogen uptake :::
	!::::::::::::::::::::::::::::::::::::::::::::::::::::::::

	uobn = 1.0d0 - exp( -n_updis )

	if( phuacc < 1.0d-6 ) then

		strsn  = 1.0d0
		plantn = 0.0d0 !GS

	else

		pltnfr = ( pltnfr1 - pltnfr3 ) * ( 1.0d0 - phuacc / &
		& ( phuacc + exp( bio_n1 - bio_n2 * phuacc ) ) ) + pltnfr3

		if( plantn == 0.0d0 ) plantn = pltnfr * bio_ms !GS for the start of development

		un2 = pltnfr * bio_ms

		uno3d = un2 - plantn ! Calculate plant nitrogen deficiency
		if( uno3d < 0.0d0 ) uno3d = 0.0d0
		uno3d = min( 4.0d0 * pltnfr * dbio, uno3d )

		strsn = 1.0d0
		if( uno3d < 1.0d-6 ) return

		ir = 0
		nplnt = 0.0d0
		do l = 1, sol_nly
			if( ir > 0 ) exit

			if( root_depth <= sol_z(l) ) then
			  gx = root_depth
			  ir = 1
			else
			  gx = sol_z(l)
			endif

			unmx = uno3d * ( 1.0d0 - exp( -n_updis * gx / root_depth ) ) / uobn  !::: Max N can be uptake from soil layer

			uno3l = unmx - nplnt
			if( uno3l < 0.0d0 ) uno3l = 0.0d0
			uno3l = min( uno3l, sol_no3(l) )
			nplnt = nplnt + uno3l 
			sol_no3(l) = sol_no3(l) - uno3l

		enddo

		plantn = min(un2, plantn + nplnt)
		
		
		!----- Nitrogen fixation ----- GS (see Neitsch et al. 2009 p.303-)
		n_demand = un2 - plantn
		
		if(n_demand > 0.0d0 .and. cons_nfix == 1) then

			if( fgdd <= 0.15d0 ) f_gr = 0.0d0
			if( 0.15d0 < fgdd .and. fgdd <= 0.30d0 ) f_gr = 6.67d0*fgdd - 1.0d0
			if( 0.30d0 < fgdd .and. fgdd <= 0.55d0 ) f_gr = 1.0d0
			if( 0.55d0 < fgdd .and. fgdd <= 0.75d0 ) f_gr = 3.75d0 - 5.0d0*fgdd
			if( 0.75d0 < fgdd ) f_gr = 0.0d0

			no3 = sum(sol_no3(:))
			if( no3 <= 100.0d0 ) f_no3 = 1.0d0
			if( 100.0d0 < no3 .and. no3 <= 300.0d0 ) f_no3 = 1.5d0 - 0.0005d0*no3
			if( 300.0d0 <= no3 ) f_no3 = 0.0d0

			f_sw = sum(sol_st(:))/sum(sol_fc(:))

			n_fix = n_demand * f_gr * min(f_sw, f_no3, 1.0d0)
			
			plantn = min(un2, plantn + n_fix)
			
		endif
		
		!! compute nitrogen stress
		nfr = plantn / un2
		if( nfr > 1.0d0 ) nfr = 1.0d0
		strsn = 200.0d0 * ( nfr - tmpnfrth )
		strsn = strsn / ( strsn + exp( 3.535d0 - 0.02597d0 * strsn ) )
		if( strsn > 1.0d0 ) strsn = 1.0d0
		if( strsn < 0.0d0 ) strsn = 0.0d0

	endif

	if(un2 < plantn) plantn = un2 !GS
	
end subroutine Mnup
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Mpup( &
                 pltpfr1,                  & !::: In
                 pltpfr3,                  & !::: In
                 bio_p1,                   & !::: In
                 bio_p2,                   & !::: In
                 p_updis,                  & !::: In
                 pfrth,                    & !::: In
                 phuacc,                   & !::: In
                 bio_ms,                   & !::: In
                 dbio,                     & !::: In
                 sol_nly,                  & !::: In
                 sol_z,                    & !::: In
                 root_depth,               & !::: In
                 plantp,                   & !::: Inout
                 sol_solp,                 & !::: Inout
                 strsp                     & !::: Out
                 )                           !::: 

		implicit none

		real(8) , intent(in)    :: pltpfr1
		real(8) , intent(in)    :: pltpfr3
		real(8) , intent(in)    :: bio_p1
		real(8) , intent(in)    :: bio_p2
		real(8) , intent(in)    :: p_updis
		real(8) , intent(in)    :: pfrth
		real(8) , intent(in)    :: phuacc
		real(8) , intent(in)    :: bio_ms
		real(8) , intent(in)    :: dbio
		integer , intent(in)    :: sol_nly
		real(8) , intent(in)    :: sol_z(sol_nly)
		real(8) , intent(in)    :: root_depth
		real(8) , intent(inout) :: plantp
		real(8) , intent(inout) :: sol_solp(sol_nly)
		real(8) , intent(out)   :: strsp

		integer :: j, icrop, l, ir
		real(8) :: up2, uapd, upmx, uapl, gx, pltfr_p, uobp
		real(8) :: u1, u2, uu, pplnt
		real(8) :: pre_bio

		!GS-->
		if( phuacc < 1.0d-6 ) then
			plantp = 0.0d0
			strsp  = 1.0d0
		else
		!<--GS
		
			uobp = 1.0d0 - exp( -p_updis )
			pltfr_p = (pltpfr1 - pltpfr3) * (1.0d0 - phuacc / (phuacc + exp(bio_p1 - bio_p2 * phuacc))) + pltpfr3

			if( plantp == 0.0d0 ) plantp = pltfr_p * bio_ms !GS for the start of development

			up2 = 0.0d0
			uapd = 0.0d0
			up2 = pltfr_p * bio_ms

!			if( plantp == 0.0d0 ) plantp = up2 !GS
			
!			if (up2 < plantp) up2 = plantp !GS
			
			uapd = up2 - plantp
!			uapd = min(4.0d0 * pltpfr3 * dbio, uapd)
			uapd = min(4.0d0 * pltfr_p * dbio, uapd)
			uapd = 1.5 * uapd !! luxury p uptake

			strsp = 1.0d0
			ir = 0
			if (uapd < 1.0d-6) return

			pplnt = 0.0d0
			do l = 1, sol_nly
				if (ir > 0) exit

				gx = 0.0d0
				if (root_depth <= sol_z(l)) then
				    gx = root_depth
				    ir = 1
				else
				    gx = sol_z(l)
				end if

				upmx = 0.0d0
				uapl = 0.0d0
				upmx = uapd * (1.0d0 - exp(-p_updis * gx / root_depth)) / uobp
				uapl = min(upmx - pplnt, sol_solp(l))
				pplnt = pplnt + uapl
				sol_solp(l) = sol_solp(l) - uapl

			end do
			if (pplnt < 0.0d0) pplnt = 0.0d0

			plantp = min( up2, plantp + pplnt)

			!! compute phosphorus stress
			uu = 0.0d0
			u1 = plantp
			u2 = up2

			uu = 200.0d0 * (u1 / (u2 + 0.0001d0) - pfrth)

			if (uu <= 0.0d0) then
			uu = 0.0d0
			else
			if (uu < 99.0d0) then
			    uu = uu / (uu + exp(3.535d0 - 0.02597d0 * uu))
			else
			    uu = 1.0d0
			endif
			end if

			if (u2 <= 1.0d-6) uu = 1.0d0

			strsp = uu
		endif
		
		if(up2<plantp) plantp = up2 !GS

end subroutine Mpup
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
subroutine Mnminrl_rev(                         & 
					soilc,                  & !in Soil Carbon of each compornent (t/ha)
                    sol_nly,                & !in No. of soil layer
                    rsdco_pl,               & !in plant residue decomposition coefficient [param]
                    sdnco,                  & !in Denitrification threshold: fraction of field [param]
                    cmn,                    & !in Rate factor for humus mineralization [param]
                    nactfr,                 & !in Nitrogen active pool fraction [param]
                    cdn,                    & !in Coefficient of denitrification [param]
                    sol_tmp,                & !in Average temperature of soil layer (deg C)
                    sol_st,                 & !in Amount of water in soil layer at the day (mm)
                    sol_fc,                 & !in Amount of water available to plants in soil layer at fc (mm) [force]
                    sol_aorgn,              & !inout Amount of nitrogen stored in humus (kg/ha)
                    sol_cbn,                & !inout Percent organic carbon in soil layer (%)
                    sol_fon,                & !inout Amount of nitrogen stored in the fresh (kg/ha)
                    sol_fop,                & !inout Amount of phosphorus stored in the fresh (kg/ha)
                    sol_no3,                & !inout Amount of nitrogen stored in the soil layer (kg/ha)
                    sol_orgn,               & !inout Organic N concentration in soil layer (kg/ha)
                    sol_orgp,               & !inout Organic P concentration in soil layer (kg/ha)
                    sol_rsd,                & !inout Amount of organic matter in the soil layer (kg/ha)
                    sol_solp,               & !inout Amount of phosohorus stored in solution (kg/ha)
					wdntl,                  & !out Amount of nitrogen lost by denitrification (kg/ha)
					hmn_tot,                & !out Amount of nitrogen mineralized from sol_aorgn in soil profile (kg/ha)
					hmp_tot,                & !out Amount of phosphorus mineralized from sol_orgp in soil profile (kg/ha)
					rmn_tot,                & !out Amount of nitrogen decomposed from sol_fon in soil profile (kg/ha)
					rmp_tot                 & !out Amount of phosphorus docomposed from sol_fop in soil profile (kg/ha)
                    )                         

	implicit none
	
	real(8) , intent(in)    :: soilc(5)
    integer , intent(in)    :: sol_nly
    real(8) , intent(in)    :: rsdco_pl
    real(8) , intent(in)    :: sdnco
    real(8) , intent(in)    :: cmn
    real(8) , intent(in)    :: nactfr
    real(8) , intent(in)    :: cdn
    real(8) , intent(in)    :: sol_tmp(sol_nly)
    real(8) , intent(in)    :: sol_st(sol_nly)
    real(8) , intent(in)    :: sol_fc(sol_nly)
    real(8) , intent(inout) :: sol_aorgn(sol_nly)
    real(8) , intent(inout) :: sol_cbn(sol_nly)
    real(8) , intent(inout) :: sol_fon(sol_nly)
    real(8) , intent(inout) :: sol_fop(sol_nly)
    real(8) , intent(inout) :: sol_no3(sol_nly)
    real(8) , intent(inout) :: sol_orgn(sol_nly)
    real(8) , intent(inout) :: sol_orgp(sol_nly)
    real(8) , intent(inout) :: sol_rsd(sol_nly)
    real(8) , intent(inout) :: sol_solp(sol_nly)
	real(8) , intent(out)   :: wdntl
	real(8) , intent(out)   :: hmn_tot
	real(8) , intent(out)   :: hmp_tot
	real(8) , intent(out)   :: rmn_tot
	real(8) , intent(out)   :: rmp_tot

    integer :: k, kk
    real(8) :: xx, csf, rwn, r4, cnr, cnrf, cpr, hmn, hmp, rmn1, rmp, wdn
    real(8) :: cprf, ca, decr, rdc, cdg, sut, totaln, totalc, cn_rate

    !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    !::: this subroutine estimates daily nitrogen and phosphorus       :::
    !::: mineralization and immobilization considering fresh organic   :::
    !::: material (plant residue) and active and stable humus material :::
    !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


	wdntl = 0.0d0
	hmn_tot = 0.0d0
	hmp_tot = 0.0d0
	rmn_tot = 0.0d0
	rmp_tot = 0.0d0
    do k = 1, sol_nly


	  !--- Calc Nitrogen in the fresh organic pool --- GS
	  sol_fon(k) = 0.0015d0 * sol_rsd(k) !GS In SWAT this equation allowed only in layer 1, however, in this version,
	                                     !GS we allowed it in deeper layer because of long term profection.
	  sol_fop(k) = 0.0003d0 * sol_rsd(k) !GS


      if( sol_tmp(k) > 0.0d0 ) then

        !--- compute soil water factor ---
        sut = sol_st(k) / sol_fc(k)  ! nutrient cycling water factor
        sut = min( 1.0d0, sut )
        sut = max( 0.05d0, sut )

        !--- compute soil temperature factor ---
        xx = sol_tmp(k)
        cdg = 0.9d0 * xx / ( xx + exp( 9.93d0 - 0.312d0 * xx ) ) + 0.1d0 ! nutrient cycling temperature factor
        cdg = max( 0.1d0, cdg )

        !--- compute combined factor ---
        xx = cdg * sut
        if( xx < 0.0d0 ) xx = 0.0d0
        if( xx > 1.0d6 ) xx = 1.0d6
        csf = sqrt(xx)


        !--- compute flow from active to stable pools ---
		!--- Active N <--> Stable N
        rwn = 0.1d-4 * ( sol_aorgn(k) * ( 1.0d0 / nactfr - 1.0d0 ) - sol_orgn(k) )
        if( rwn > 0.0d0 ) then
          rwn = min( rwn, sol_aorgn(k) )
        else
          rwn = -( min( abs(rwn), sol_orgn(k) ) )
        endif
        sol_orgn(k) = max( 1.0d-6, sol_orgn(k) + rwn )
        sol_aorgn(k) = max( 1.0d-6, sol_aorgn(k) - rwn )


        !--- compute humus mineralization on active organic n --- GS version !!!
		!--- NO3 <-- Active N
		totaln = sol_no3(k) + sol_aorgn(k) + sol_orgn(k) + sol_fon(k) !GS
		totalc = sum(soilc(1:5)) * 1000.0d0 !GS
		cn_rate = totalc/totaln !GS
		
		if(cn_rate < 20.0d0) then !GS
		
        hmn = cmn * csf * sol_aorgn(k)
        hmn = min( hmn, sol_aorgn(k) )
        sol_aorgn(k) = max( 1.0d-6, sol_aorgn(k) - hmn )
        sol_no3(k) = sol_no3(k) + hmn
        
        else !GS
        
        hmn = 0.0d0 !GS
        
        endif !GS

		hmn_tot = hmn_tot + hmn !GS

        !--- compute humus mineralization on active organic p ---
		!--- Solution P <-- Active P
        xx = sol_orgn(k) + sol_aorgn(k)
        if( xx > 1.0d-6 ) then
          hmp = 1.4d0 * hmn * sol_orgp(k) / xx
        else
          hmp = 0.0d0
        end if
        hmp = min( hmp, sol_orgp(k) )
        sol_orgp(k) = sol_orgp(k) - hmp
        sol_solp(k) = sol_solp(k) + hmp

		hmp_tot = hmp_tot + hmp !GS

        !! compute residue decomp and mineralization of 
        !! fresh organic n and p (upper two layers only)
        rmn1 = 0.0d0
        rmp = 0.0d0
!GS        if( k == 1 ) then

		  !----- Calc C:N and C:P, and N cycling factor ----- GS version !!!
          if(k >  1) r4 = 0.58d0 * sol_rsd(k) !GS
          if(k == 1) r4 = sum(soilc(1:2))*1000.0d0 !GS C:N ratio is calculated using the result of RothC
          
          if( sol_fon(k) + sol_no3(k) > 1.0d-4 ) then
            cnr = r4 / ( sol_fon(k) + sol_no3(k) )
            if( cnr > 500.0d0 ) cnr = 500.0d0
            cnrf = exp( -0.693d0 * ( cnr - 25.0d0 ) / 25.0d0 )
          else
            cnrf = 1.0d0
          end if

          if( sol_fop(k) + sol_solp(k) > 1.0d-4 ) then
            cpr = r4 / ( sol_fop(k) + sol_solp(k) )
            if( cpr > 5000.0d0 ) cpr = 5000.0d0
            cprf = exp( -0.693d0 * ( cpr - 200.0d0 ) / 200.0d0 )
          else
            cprf = 1.0d0
          end if
          ca = min( cnrf, cprf, 1.0d0 )

		  !----- Calc mineralization from the residue fresh organic N -----
          decr = rsdco_pl * ca * csf
          decr = max( 0.01d0, decr )
          decr = min( decr, 1.0d0 )

		  !--- residue decomposition ---
          sol_rsd(k) = max( 1.0d-6, sol_rsd(k) )
          rdc = decr * sol_rsd(k)
          sol_rsd(k) = sol_rsd(k) - rdc
          if( sol_rsd(k) < 0.0d0 ) sol_rsd(k) = 0.0d0

		  !--- Fresh N decomposition
          rmn1 = decr * sol_fon(k)
          sol_fon(k) = max( 1.0d-6, sol_fon(k) )
          sol_fon(k) = sol_fon(k) - rmn1

		  !--- Fresh P decomposition ---
          sol_fop(k) = max( 1.0d-6, sol_fop(k) )
          rmp = decr * sol_fop(k)
          sol_fop(k) = sol_fop(k) - rmp

		  !--- NO3 <-- Fresh N
          sol_no3(k) = sol_no3(k) + 0.8d0 * rmn1
          !--- Active N <-- Fresh N
          sol_aorgn(k) = sol_aorgn(k) + 0.2d0 * rmn1

		  !--- Solution P <-- Fresh P
          sol_solp(k) = sol_solp(k) + 0.8d0 * rmp
		  !--- Active P <-- Fresh P
          sol_orgp(k) = sol_orgp(k) + 0.2d0 * rmp
          
          rmn_tot = rmn_tot + rmn1 !GS
          rmp_tot = rmp_tot + rmp  !GS
          
!GS        end if

        !! compute denitrification
		!--- NO3 --> N2O or N2
		sut = sol_st(k) / sol_fc(k)
        if( sut >= sdnco ) then
          wdn = sol_no3(k) * ( 1.0d0 - exp( -cdn * cdg * sol_cbn(k) ) )
        else
          wdn = 0.0d0
        end if
        sol_no3(k) = sol_no3(k) - wdn

		wdntl = wdntl + wdn

      end if
    end do

end subroutine Mnminrl_rev
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

end module swatnut
