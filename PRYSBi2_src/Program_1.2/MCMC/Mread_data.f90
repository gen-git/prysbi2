module Mread_data

	!---------------------------------------------------------------------
	! In this subroutine, the variables needed for simulation is assigned.
	!
	! The data is already read in "main_mcmc", and
	! This subroutine is called only once in "dream" subroutine.
	!
	! In case of modification of the program, this subroutine should
	! be changed according to the model concept.
	!---------------------------------------------------------------------
	use environ_param
	use var_read_all
	use var_divide
	use Mdream_param

	implicit none

	contains

	subroutine read_data

		implicit none

		integer :: tgt_var, ivar, ndoy
		integer :: ii, iyr, idoy
		real(8) :: tmpsx, tmpdaylength

		integer :: isol, k
		integer :: check_error

		real(8) :: dg, xx, xxxx, wt1, zdst, dgg
		real(8) :: pormm, sumpor, sol_avpor

		!--- Climate data and parameter ---

		! Allocation
		allocate( tm(ntime) )
		allocate( tx(ntime) )
		allocate( tn(ntime) )
		allocate( pr(ntime) )
		allocate( sr(ntime) )
		allocate( rh(ntime) )
		allocate( ws(ntime) )
		allocate( ca(iyrstt:iyrend) )
		allocate( plnt_day(iyrstt:iyrend) )
		allocate( hvst_day(iyrstt:iyrend) )
		allocate( doy(ntime) )
		allocate( year(ntime) )
		allocate( finp(ntime) )
		allocate( obs_yield(iyrstt:iyrend) )
		allocate( params(nensemble, nvar_param) )
		allocate( fix_params(nvar_fix_param) )
		allocate( gdp(iyrstt:iyrend) )

		! Parameter values
		params(1:nensemble, 1:nvar_param) = dble( eachdata_param(1:nensemble, 1:nvar_param, igrid) )

		! Fixed parameter values
		fix_params(1:nvar_fix_param) = dble( eachdata_fix_param(1:nvar_fix_param, igrid) )

		! GDP
		gdp(iyrstt:iyrend) = dble( eachdata_gdp(iyrstt:iyrend, igrid) )

		! Temperature
		do ivar = 1, nvar_weather
			if( trim(avar_weather(ivar))=='tmaxsfc' ) tgt_var = ivar
		enddo
		tx(1:ntime) = dble( eachdata_weather(1:ntime, tgt_var, igrid) )

		do ivar = 1, nvar_weather
			if( trim(avar_weather(ivar))=='tminsfc' ) tgt_var = ivar
		enddo
		tn(1:ntime) = dble( eachdata_weather(1:ntime, tgt_var, igrid) )

		tm(:) = (tx(:) + tn(:))/2.0d0

		! Precipitation
		do ivar = 1, nvar_weather
			if( trim(avar_weather(ivar))=='precsfc' ) tgt_var = ivar
		enddo
		pr(1:ntime) = dble( eachdata_weather(1:ntime, tgt_var, igrid) )

		! Solar radiation
		do ivar = 1, nvar_weather
			if( trim(avar_weather(ivar))=='dswrfsfc' ) tgt_var = ivar
		enddo
		sr(1:ntime) = dble( eachdata_weather(1:ntime, tgt_var, igrid) )

		! Humidity
		do ivar = 1, nvar_weather
			if( trim(avar_weather(ivar))=='rhsfc' ) tgt_var = ivar
		enddo
		rh(1:ntime) = dble( eachdata_weather(1:ntime, tgt_var, igrid) )

		! Wind speed
		do ivar = 1, nvar_weather
			if( trim(avar_weather(ivar))=='windsfc' ) tgt_var = ivar
		enddo
		ws(1:ntime) = dble( eachdata_weather(1:ntime, tgt_var, igrid) )

		! Calculation of average temperature
		ta = 0.0d0
		do ii = 1, ntime
			ta = ta + tm(ii)
		enddo
		ta = ta / dble(ntime)

		! CO2
		ca(iyrstt:iyrend) = dble( eachdata_co2(iyrstt:iyrend, igrid) )

		! Planting day
		do ivar = 1, nvar_pheno
			if( trim(avar_pheno(ivar))=='planting' ) tgt_var = ivar
		enddo
		plnt_day(iyrstt:iyrend) = dble( eachdata_pheno(tgt_var, igrid) )

		! Harvest day
		do ivar = 1, nvar_pheno
			if( trim(avar_pheno(ivar))=='maturity' ) tgt_var = ivar
		enddo
		hvst_day(iyrstt:iyrend) = dble( eachdata_pheno(tgt_var, igrid) )

		! Latitude
		do ivar = 1, nvar_topo
			if( trim(avar_topo(ivar))=='lati' ) tgt_var = ivar
		enddo
		lati = dble( eachdata_topo(tgt_var, igrid) )

		! Altitude
		do ivar = 1, nvar_topo
			if( trim(avar_topo(ivar))=='elev' ) tgt_var = ivar
		enddo
		elev = dble( eachdata_topo(tgt_var, igrid) )

		! Calculation of maximum solar radiation
		do ii = 1, 366 ! Calculate daily total solar radiation
			call Mhtop(ii, lati, tmpsx, tmpdaylength)
			sx(ii) = tmpsx
		enddo

		! List of year and DOY
		ii = 1
		do iyr = iyrstt, iyrend
			ndoy = 365
			if( iyr/=2100 .and. mod(iyr, 4) == 0 ) ndoy = 366

			do idoy = 1, ndoy
				year(ii) = iyr
				doy(ii) = idoy
				ii = ii + 1
			enddo
		enddo

		! Fixed values
		oa      = 250000.0d0
		press   = 101325.0d0
		uzz     = 10.0d0
		finp(:) = 0.0d0

		! Yield observation
		obs_yield(iyrstt:iyrend) = dble( eachdata_yield(iyrstt:iyrend, 5, igrid) )


		!--- Soil ---

		! Allocation
		allocate( sol_z(nsol) )
		allocate( sol_tmp(nsol) )
		allocate( sol_st(nsol) )
		allocate( sol_fc(nsol) )
		allocate( sol_wpmm(nsol) )
		allocate( sol_bd(nsol) )
		allocate( conv_wt(nsol) )
		allocate( sol_ul(nsol) )
		allocate( sol_clay(nsol) )
		allocate( soil_resid_l(nsol) )
		allocate( sol_aorgn(nsol) )
		allocate( sol_cbn(nsol) )
		allocate( sol_fon(nsol) )
		allocate( sol_fop(nsol) )
		allocate( sol_no3(nsol) )
		allocate( sol_orgn(nsol) )
		allocate( sol_orgp(nsol) )
		allocate( sol_rsd(nsol) )
		allocate( sol_nh3(nsol) )
		allocate( sol_actp(nsol) )
		allocate( sol_solp(nsol) )
		allocate( sol_stap(nsol) )

		allocate( sol_prk(nsol) )
		allocate( sol_alb(nsol) )
		allocate( sol_shc(nsol) )

		allocate( sol_awc(nsol) )
		allocate( sol_wp(nsol) )
		allocate( sol_up(nsol) )
		allocate( sol_por(nsol) )

		allocate( root_bio_l(nsol) )

		! Soil depth
		do ivar = 1, nvar_soil
			if( trim(avar_soil(ivar))=='solz' ) tgt_var = ivar
		enddo
		sol_z(1:nsol) = dble( eachdata_soil(1:nsol, tgt_var, igrid) )

		! Bulk density
		do ivar = 1, nvar_soil
			if( trim(avar_soil(ivar))=='solbd' ) tgt_var = ivar
		enddo
		sol_bd(1:nsol) = dble( eachdata_soil(1:nsol, tgt_var, igrid) )

		! Available water capacity
		do ivar = 1, nvar_soil
			if( trim(avar_soil(ivar))=='solawc' ) tgt_var = ivar
		enddo
		sol_awc(1:nsol) = dble( eachdata_soil(1:nsol, tgt_var, igrid) )

		! Albed
		do ivar = 1, nvar_soil
			if( trim(avar_soil(ivar))=='solalb' ) tgt_var = ivar
		enddo
		sol_alb(1:nsol) = dble( eachdata_soil(1:nsol, tgt_var, igrid) )

		! Soil carbon
		do ivar = 1, nvar_soil
			if( trim(avar_soil(ivar))=='solcbn' ) tgt_var = ivar
		enddo
		sol_cbn(1:nsol) = dble( eachdata_soil(1:nsol, tgt_var, igrid) )

		! Soil clay
		do ivar = 1, nvar_soil
			if( trim(avar_soil(ivar))=='solclay' ) tgt_var = ivar
		enddo
		sol_clay(1:nsol) = dble( eachdata_soil(1:nsol, tgt_var, igrid) )

		! Check NA values
		check_error = 0
		do isol = 1, nsol
			if( sol_bd(isol)   < 0.0d0 ) check_error = 1
			if( sol_awc(isol)  < 0.0d0 ) check_error = 1
			if( sol_cbn(isol)  < 0.0d0 ) check_error = 1
			if( sol_clay(isol) < 0.0d0 ) check_error = 1
		enddo
		if( check_error == 1 ) then
			write(*,*) 'Soil data have missing value'
			stop
		endif

 		!----- Calc soil water potential -----
		dg = 0.0d0
		xx = 0.0d0
		sol_avbd  = 0.0d0
		sol_sumfc = 0.0d0
		sol_sumul = 0.0d0
		sumpor = 0.0d0
		do k = 1, nsol
			pormm = 0.0d0
			dg = sol_z(k) - xx
			sol_wp(k) = 0.4d0 * sol_clay(k) * sol_bd(k) * 0.01d0 ! estimate wilting point Eq. 2:3.1.5 (2009)
			sol_wpmm(k) = sol_wp(k) * dg

			if( sol_wp(k) <= 0.0d0 ) sol_wp(k) = 0.005d0

			sol_up(k) = sol_wp(k) + sol_awc(k)/dg ! field capacty
			sol_por(k) = 1.0d0 - sol_bd(k) / 2.65d0 ! Eq. 2:3.1.3 (2009)
			if( sol_up(k) >= sol_por(k) ) then
				sol_up(k) = sol_por(k) - 0.05d0
				sol_wp(k) = sol_up(k) - sol_awc(k) / dg
				if( sol_wp(k) <= 0.0d0 ) then
					sol_up(k) = sol_por(k) * 0.75d0
					sol_wp(k) = sol_por(k) * 0.25d0
				endif
			endif
			pormm = sol_por(k) * dg
			sumpor = sumpor + pormm
			sol_ul(k) = ( sol_por(k) - sol_wp(k) ) * dg  ! Amount of water held in the soil layer at saturation (mm)
			sol_fc(k) = dg * ( sol_up(k) - sol_wp(k) ) ! Amount of water available to plants in soil layer at fc (mm)
			sol_st(k) = sol_fc(k) * 0.8d0
			sol_sumfc = sol_sumfc + sol_fc(k)
			sol_sumul = sol_sumul + sol_ul(k)
			xx = sol_z(k)

			! Calculate travel time for percolaton
			sol_shc(k) = exp(1.078d0 + 0.661d0*log(sol_por(k))) ! This eq was added by Sakurai (Jarvis et al. 2002)
		enddo
		sol_avpor = sumpor / sol_z(nsol)
		sol_avbd = 2.65 * (1.0 - sol_avpor)


		!----- Calclate initial soil nutrient condition -----

		! zero clear
		do k = 1, nsol
			sol_sw       = 0.0d0
			sol_tmp(k)   = 0.0d0
			sol_aorgn(k) = 0.0d0
			sol_no3(k)   = 0.0d0
			sol_nh3(k)   = 0.0d0
			sol_orgn(k)  = 0.0d0
			sol_orgp(k)  = 0.0d0
			sol_rsd(k)   = 0.0d0
			sol_solp(k)  = 0.0d0
		enddo

		do k = 1, nsol
			sol_tmp(k) = ta
			sol_sw = sol_sw + sol_st(k)
		enddo

		sol_rsd(1) = 500.0d0

		!! calculate initial nutrient contents of layers, profile and
		!! average in soil for the entire watershed
		!! convert mg/kg (ppm) to kg/ha

		!----- Calc Fresh N and Fresh P (kg/ha) -----
		sol_fop(1) = sol_rsd(1) * 0.0003d0
		sol_fop(2) = 0.0d0

		sol_fon(1) = sol_rsd(1) * 0.0055d0
		sol_fon(2) = 0.0d0
		xxxx = 0.0d0
		do k = 1, nsol

			!----- Weight for ransrattion -----
			dgg = ( sol_z(k) - xxxx )
			wt1 = sol_bd(k) * dgg / 100.0d0             !! mg/kg => kg/kg

			conv_wt(k) = 1.0d6 * wt1                    !! kg/kg => kg/ha

			!----- Set initial NO3- -----
			if( sol_no3(k) <= 0.0d0 ) then
				zdst = exp( -sol_z(k) / 1000.0d0 )
				sol_no3(k) = 10.0d0 * zdst * 0.7d0
			endif
			sol_no3(k) = sol_no3(k) * wt1               !! mg/kg => kg/ha

			!----- Set initial Active N and Stable N -----
			if( sol_orgn(k) > 0.0001d0 ) then
				sol_orgn(k) = sol_orgn(k) * wt1         !! mg/kg => kg/ha
			else
				!! assume C:N ratio of 14:1
				sol_orgn(k) = 10000.0d0 * ( sol_cbn(k) / 14.0d0 ) * wt1

			endif
			sol_aorgn(k) = sol_orgn(k) * 0.02d0
			sol_orgn(k) = sol_orgn(k) * ( 1.0d0 - 0.02d0 )

			!----- Set initial Organic P (Active OP and Stable OP) -----
			if( sol_orgp(k) > 0.0001d0 ) then
				sol_orgp(k) = sol_orgp(k) * wt1          !! mg/kg => kg/ha
			else
				sol_orgp(k) = 0.125d0 * sol_orgn(k)
			endif

			!----- Set initial Sol P -----
			if( sol_solp(k) > 0.0001d0 ) then
				sol_solp(k) = sol_solp(k) * wt1          !! mg/kg => kg/ha
			else
				!! assume initial concentration of 25 mg/kg
				sol_solp(k) = 25.0d0 * wt1
			endif

			!----- Set initial Active MP and Stable MP -----
			sol_actp(k) = sol_solp(k) * ( 1.0d0 - 0.4d0 ) / 0.4d0
			sol_stap(k) = 4.0d0 * sol_actp(k)

			xxxx = sol_z(k)

		enddo

		if( dream_simulate_version == 2 ) then
			! Output observed yield
			write( dream_ofile, "(a,'/obs_',a,'.out')" ) &
				 & trim(dream_odir_name), eachdata_gcode(igrid)
			open ( 1, file=dream_ofile, status='replace', access='direct', form='unformatted', &
			& recl=size(obs_yield) * 4) ! Output with 4 bite
			write( 1, rec=1) real( obs_yield )
			close( 1 )
		endif

	end subroutine read_data

	subroutine Mhtop(  & !:::
				  doy, & !::: In
				  lat, & !::: In
				  h0,  & !::: Out
				  dayl & !::: Out
				  )      !:::

		implicit none

		integer, intent(in)    :: doy  !::: Day of year (day)
		real(8), intent(in)    :: lat  !::: Latitude (deg.)
		real(8), intent(out)   :: h0   !::: Daily total solar radiation at the top of atmosphere (MJ/m^2/day)
		real(8), intent(out)   :: dayl !::: Day length (hours)

		real(8)  :: sd, dd, latsin, latcos, ch, h, ys, yc

		sd = asin( 0.4d0 * sin( (dble(doy) - 82.0d0 ) / 57.30d0 ) ) !::: 360/2*pi = 57.30
		dd = 1.0d0 + 0.033d0 * cos( dble(doy) / 57.30d0 ) !::: calculate the relative distance of the earth from the sun.

		latsin = sin( lat / 57.30d0 )
		latcos = cos( lat / 57.30d0 )

		ch = -latsin * tan( sd ) / latcos

		if( ch > 1.0d0 ) then !::: ch will be >= 1. if latitude exceeds +/- 66.5 deg in winter
			h = 0.0d0
		elseif ( ch >= -1.0d0 ) then
			h = acos( ch )
		else
			h = 3.1416d0 !::: latitude exceeds +/- 66.5 deg in summer
		endif

		dayl = 7.6394d0 * h

		ys = latsin * sin( sd ) !::: Calculate Potential (maximum) Radiation. equation 2.2.7 in SWAT manual
		yc = latcos * cos( sd )
		h0 = 30.0d0 * dd * ( h * ys + yc * sin( h ) )

	end subroutine Mhtop

end module Mread_data
