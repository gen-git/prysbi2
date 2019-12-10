module cast_setting

	use Mdream_param
	use var_divide

	implicit none

	contains

	subroutine s_cast_setting

		implicit none

		! Cast the variables relating to the MCMC setting (e.g. prior information)
		call mpi_bcast(dream_x_typeprior(:) , dream_ndim  , temp_mpi_double_precision, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(dream_x_lim(:,:)     , dream_ndim*2, temp_mpi_double_precision, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(dream_x_avprior(:)   , dream_ndim  , temp_mpi_double_precision, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(dream_x_sdprior(:)   , dream_ndim  , temp_mpi_double_precision, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(dream_b              , 1           , temp_mpi_double_precision, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(dream_bdash          , 1           , temp_mpi_double_precision, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(dream_delta          , 1           , temp_mpi_integer         , 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(dream_odir_name      , 1000        , temp_mpi_character       , 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(dream_ofile_name     , 1000        , temp_mpi_character       , 0, temp_mpi_comm_world, ierr)
		call mpi_barrier( temp_mpi_comm_world, ierr )

	end subroutine

end module
