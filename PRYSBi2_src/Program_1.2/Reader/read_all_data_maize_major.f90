module read_all_data

	use read_kdata_soil
	use read_kdata_weather
	use read_kdata_yield
	use read_kdata_topo
	use read_kdata_pheno
	use read_kdata_co2
	use read_kdata_gdp
	use read_kdata_param
	use specify_grid_analyzed
	use var_read_all
	use Mdream_param
	use useful_sub

	implicit none

	contains

	subroutine s_read_all_data

		implicit none

		character(1000) :: ifile
		integer         :: temp_ierr

    sim_flag = 1
    if( dream_simulate_version == 2 ) then
      sim_flag = 0
    endif

    write(ifile, "('./Namelist/setting_namelist_maize_major.txt')")
    open (1, file=ifile)
    read (1, read_all_list)
    close(1)

		nvar_soil      = size(avar_soil)
		nvar_topo      = size(avar_topo)
		nvar_weather   = size(avar_weather)
		nvar_pheno     = size(avar_pheno)
		nvar_param     = size(avar_param)
		nvar_fix_param = size(avar_fix_param)

		! Calculate ntime
		call s_count_ntime( &
      iyrstt, & ! in
      iyrend, & ! in
      ntime   & ! out
      )

		! Calculate nyear
		nyear = iyrend - iyrstt + 1

		call s_specify_grid_analyzed( &
			path_ana,       & !in
			rnan,           & !in
			k2i,            & !out
			k2j,            & !out
			ngrd            & !out
			)

		! Allocation
		allocate( kdata_topo      (1:nvar_topo, 1:ngrd) )
		allocate( kdata_gcode     (1:ngrd) )
		allocate( kdata_weather   (1:ntime, 1:nvar_weather, 1:ngrd) )
		allocate( kdata_yield     (iyrstt:iyrend, 8, 1:ngrd) )
		allocate( kdata_soil      (1:nsol, 1:nvar_soil, 1:ngrd) )
		allocate( kdata_pheno     (1:nvar_pheno, 1:ngrd) )
		allocate( kdata_co2       (iyrstt:iyrend, 1:ngrd) )
		allocate( kdata_param     (1:nensemble, 1:nvar_param, 1:ngrd) )
		allocate( kdata_fix_param (1:nvar_fix_param, 1:ngrd) )
		allocate( kdata_gdp       (iyrstt:iyrend, 1:ngrd) )

		write(*,*) 'Reading topology data...'
		call s_read_kdata_topo( &
			path_topo,  & !in
			nvar_topo,  & !in
			avar_topo,  & !in
			k2i,        & !in
			k2j,        & !in
			ngrd,       & !in
			rnan,       & !in
			kdata_topo, & !out
			kdata_gcode & !out
			)

		write(*,*) 'Reading soil data...'
		call s_read_kdata_soil( &
			path_soil,  & !in
			nvar_soil,  & !in
			avar_soil,  & !in
			nsol,       & !in
			k2i,        & !in
			k2j,        & !in
			ngrd,       & !in
			rnan,       & !in
			kdata_soil  & !out
			)

		write(*,*) 'Reading weather data...'
		call s_read_kdata_weather( &
			path_weather,  & !in
			nvar_weather,  & !in
			avar_weather,  & !in
			iyrstt,        & !in
			iyrend,        & !in
			ntime,         & !in
			k2i,           & !in
			k2j,           & !in
			ngrd,          & !in
			rnan,          & !in
			kdata_weather  & !out
			)

		if( sim_flag /= 1 ) then
			write(*,*) 'Reading yield data...'
			call s_read_kdata_yield( &
				path_yield,  & !in
				tgt_version, & !in
				tgt_crop,    & !in
				iyrstt,      & !in
				iyrend,      & !in
				k2i,         & !in
				k2j,         & !in
				ngrd,        & !in
				rnan,        & !in
				kdata_yield  & !out
				)
		endif

		write(*,*) 'Reading phenology data...'
		call s_read_kdata_pheno( &
			path_pheno, & !in
			nvar_pheno, & !in
			avar_pheno, & !in
			tgt_crop,   & !in
			k2i,        & !in
			k2j,        & !in
			ngrd,       & !in
			rnan,       & !in
			kdata_pheno & !out
			)

		write(*,*) 'Reading co2 data...'
		call s_read_kdata_co2( &
			path_co2, & !in
			iyrstt,   & !in
			iyrend,   & !in
			ngrd,     & !in
			rnan,     & !in
			kdata_co2 & !out
			)


    if( dream_simulate_version == 3 ) then
      write(*,*) 'Reading param data...'
      call s_read_kdata_param( &
        path_param,    & !in
        nvar_param,    & !in
        avar_param,    & !in
        nensemble,     & !in
        tgt_crop,      & !in
        k2i,           & !in
        k2j,           & !in
        ngrd,          & !in
        rnan,          & !in
        kdata_param    & !out
        )

    endif

    if( dream_simulate_version == 2 .or. dream_simulate_version == 3 ) then
      if( trim(avar_fix_param(1)) /= "Nan" ) then
        write(*,*) 'Reading fix param data...'
        call s_read_kdata_fix_param( &
            path_fix_param,    & !in
            nvar_fix_param,    & !in
            avar_fix_param,    & !in
            tgt_crop,          & !in
            k2i,               & !in
            k2j,               & !in
            ngrd,              & !in
            rnan,              & !in
            kdata_fix_param    & !out
            )
      endif

      write(*,*) 'Reading gdp data...'
      call s_read_kdata_gdp( &
        path_gdp,         & !in
        iyrstt,           & !in
        iyrend,           & !in
        k2i,              & !in
        k2j,              & !in
        ngrd,             & !in
        rnan,             & !in
        kdata_gdp         & !out
        )

    endif

	end subroutine

end module
