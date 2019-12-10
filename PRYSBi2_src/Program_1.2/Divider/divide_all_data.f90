module divide_all_data

	! In this module, the variables containing the information about basic setup
	! are casted to each spatial grid.
	
	use var_read_all
	use var_divide
	use cast_basic_info
	use divide_grid
	use path_kdata

	implicit none

	contains

	subroutine s_divide_all_data

		implicit none

		call s_cast_basic_info
		call mpi_barrier( temp_mpi_comm_world, ierr )

		call s_divide_grid
		call mpi_barrier( temp_mpi_comm_world, ierr )

		call s_path_kdata
		call mpi_barrier( temp_mpi_comm_world, ierr )

	end subroutine

end module
