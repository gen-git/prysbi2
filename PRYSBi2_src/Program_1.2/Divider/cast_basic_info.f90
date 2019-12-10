module cast_basic_info

	use var_read_all
	use var_divide
	use control_param
	use Mdream_param

	implicit none

	contains

	subroutine s_cast_basic_info

		implicit none

		! Cast variables relating to basic setup (the information mainly in setting_namelist.txt)
		call mpi_bcast(ngrd,           1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(ntime,          1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(nyear,          1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(iyrstt,         1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(iyrend,         1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(nvar_topo,      1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(nvar_weather,   1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(nvar_soil,      1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(nvar_pheno,     1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(nsol,           1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(nensemble,      1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(nvar_param,     1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(nvar_fix_param, 1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)

		! Cast variables relating to MCMC setting (the information mainly in dream_namelist.txt)
		call mpi_bcast(dream_nstep,           1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(dream_nburn,           1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(dream_minstep,         1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(dream_param_interval,  1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(dream_est_interval,    1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(dream_gelman_interval, 1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(dream_ncr,             1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(dream_est_dim,         1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(dream_est_n,           1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(dream_nchain,          1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(dream_ndim,            1, temp_mpi_integer, 0, temp_mpi_comm_world, ierr)

		! Cast variables relating to crop model (the information mainly in control_namelist.txt)
		call mpi_bcast(read_spm_param_namelist,   1, temp_mpi_integer,   0, temp_mpi_comm_world, ierr)
		call mpi_bcast(target_plant,            100, temp_mpi_character, 0, temp_mpi_comm_world, ierr)
		call mpi_bcast(common_nsol,               2, temp_mpi_integer,   0, temp_mpi_comm_world, ierr)
		call mpi_bcast(ctl_adjust_plnt,           2, temp_mpi_integer,   0, temp_mpi_comm_world, ierr)
		call mpi_bcast(ctl_cut_strsw,             2, temp_mpi_integer,   0, temp_mpi_comm_world, ierr)
		call mpi_bcast(ctl_nspin,                 2, temp_mpi_integer,   0, temp_mpi_comm_world, ierr)
		call mpi_bcast(ctl_ca,                    2, temp_mpi_integer,   0, temp_mpi_comm_world, ierr)
		call mpi_bcast(ctl_adapt,                 2, temp_mpi_integer,   0, temp_mpi_comm_world, ierr)

	end subroutine

end module
