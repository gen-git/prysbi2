module Mallocate_dream_param

	use Mdream_param
		
	implicit none
	
	contains

	subroutine allocate_dream_param
	
		implicit none
				
!		allocate( dream_x_typeprior( dream_ndim ) )
!		allocate( dream_x_lim( dream_ndim, 2 ) )
!		allocate( dream_x_avprior( dream_ndim ) )
!		allocate( dream_x_sdprior( dream_ndim ) )
!		allocate( dream_cr( dream_nchain ) )
!		allocate( dream_change( dream_ndim ) )
!		allocate( dream_diff( dream_ndim ) )
!		allocate( dream_inlimit( dream_ndim ) )
!		allocate( dream_x( dream_ndim, dream_nchain ) )
!		allocate( dream_z( dream_ndim, dream_nchain ) )
!		allocate( dream_x_llk( dream_nchain ) )
!		allocate( dream_z_llk( dream_nchain ) )
		allocate( dream_est_x( dream_est_n, dream_est_dim, dream_nchain ) )
		allocate( dream_est_z( dream_est_n, dream_est_dim, dream_nchain ) )
!		allocate( dream_alpha( dream_nchain ) )
!		allocate( dream_rhat( dream_ndim ) )
!		allocate( dream_ohm( dream_nchain ) )
!		allocate( dream_stop( dream_ndim ) )
		allocate( dream_pm( dream_ncr ) )
		allocate( dream_dm( dream_ncr ) )
!		allocate( dream_x_all( dream_ndim, dream_nchain, 2 ) )
!		allocate( dream_x_llk_all( dream_nchain, dream_nstep ) )
!		allocate( dream_rhat_all( dream_ndim, dream_nstep ) )
		allocate( dream_est_x_out( dream_est_n, dream_est_dim, dream_nchain ) )
!		allocate( temp_x( dream_ndim ) )
!		allocate( temp_gelman( dream_nchain, dream_nstep ) ) !2013/07/11d
		
	end subroutine

end module
