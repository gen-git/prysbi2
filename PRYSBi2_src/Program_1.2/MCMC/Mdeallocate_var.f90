module Mdeallocate_var

	use environ_param

	implicit none

	contains

	subroutine deallocate_var

		implicit none

		deallocate( tm )
		deallocate( tx )
		deallocate( tn )
		deallocate( pr )
		deallocate( sr )
		deallocate( rh )
		deallocate( ws )
		deallocate( ca )
		deallocate( plnt_day )
		deallocate( hvst_day )
		deallocate( doy )
		deallocate( year )
		deallocate( finp )
		deallocate( obs_yield )

		deallocate( sol_z )
		deallocate( sol_tmp )
		deallocate( sol_st )
		deallocate( sol_fc )
		deallocate( sol_wpmm )
		deallocate( sol_bd )
		deallocate( conv_wt )
		deallocate( sol_ul )
		deallocate( sol_clay )
		deallocate( soil_resid_l )
		deallocate( sol_aorgn )
		deallocate( sol_cbn )
		deallocate( sol_fon )
		deallocate( sol_fop )
		deallocate( sol_no3 )
		deallocate( sol_orgn )
		deallocate( sol_orgp )
		deallocate( sol_rsd )
		deallocate( sol_nh3 )
		deallocate( sol_actp )
		deallocate( sol_solp )
		deallocate( sol_stap )

		deallocate( sol_prk )
		deallocate( sol_alb )
		deallocate( sol_shc )

		deallocate( sol_awc )
		deallocate( sol_wp )
		deallocate( sol_up )
		deallocate( sol_por )

		deallocate( root_bio_l )

		deallocate( params )
		deallocate( fix_params )
		deallocate( gdp )

	end subroutine

end module
