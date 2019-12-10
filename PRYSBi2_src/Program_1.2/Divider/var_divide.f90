module var_divide

	implicit none

	integer :: ierr, nprocs, myrank

	integer :: temp_mpi_integer
	integer :: temp_mpi_comm_world
	integer :: temp_mpi_real
	integer :: temp_mpi_double_precision
	integer :: temp_mpi_character
	integer :: temp_mpi_status_size

	integer,      allocatable :: mpi_num_grid(:)
	integer,      allocatable :: mpi_knum(:,:)

	real(4),      allocatable :: eachdata_topo(:,:)
	real(4),      allocatable :: eachdata_weather(:,:,:)
	real(4),      allocatable :: eachdata_yield(:,:,:)
	real(4),      allocatable :: eachdata_soil(:,:,:)
	real(4),      allocatable :: eachdata_pheno(:,:)
	real(4),      allocatable :: eachdata_co2(:,:)
	real(4),      allocatable :: eachdata_param(:,:,:)
	real(4),      allocatable :: eachdata_fix_param(:,:)
	real(4),      allocatable :: eachdata_gdp(:,:)
	character(7), allocatable :: eachdata_gcode(:)


	integer :: eachgrd


	real(8) :: each_tphu_prior_av
	real(8) :: each_tphu_prior_sd

	integer :: igrid

end module
