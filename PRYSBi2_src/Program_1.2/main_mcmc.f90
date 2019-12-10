program main_mcmc

	use var_read_all          ! Variable declaration for reading data by core 0 (dir Reader)
	use var_divide            ! Variable declaration for MPI (dir Divider)
	use read_all_data         ! Read data (dir Reader)
	use divide_all_data       ! Calulate the allocation of grids for each core (dir Divider)
	use cast_setting          ! Cast setting (dir Divider)
	use Mallocate_dream_param ! Allocation for MCMC (dir MCMC)
	use Mread_dreamlist       ! Read namelist for MCMC (dir MCMC)
	use Mread_setting         ! Read setting for MCMC such as the prior distributions (dir MCMC)
	use Mdeallocate_var       ! Deallocate (dir MCMC)
	use Mdream                ! Program for MCMC (dir MCMC)

	implicit none
  real(8) :: cpu_time1, cpu_time2

	include 'mpif.h'

	! Preparation of MPI
	call mpi_init(ierr)
	call mpi_comm_size(mpi_comm_world, nprocs, ierr)
	call mpi_comm_rank(mpi_comm_world, myrank, ierr)

	! Declaration of start
	if(myrank==0) then
	write(*,*) '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
	write(*,*) '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
	write(*,*) '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
	write(*,*) '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
	write(*,*) '>>>>>>>>>>>>>>>>>>>>>>>'
	write(*,*) 'Compiled ->>> Start'
	call cpu_time(cpu_time1)
	endif

	! Save imformation for MPI (These variables are declared in ./Divider/var_divide.f90)
	temp_mpi_integer          = mpi_integer
	temp_mpi_comm_world       = mpi_comm_world
	temp_mpi_real             = mpi_real
	temp_mpi_double_precision = mpi_double_precision
	temp_mpi_character        = mpi_character
	temp_mpi_status_size      = mpi_status_size

	! Read all data (only core 0) (Subroutine is in ./Reader/read_all_data.f90)
	! Call setting_namelist.txt fot loading data
	if(myrank == 0) then
		call s_read_all_data
	endif
	call mpi_barrier( temp_mpi_comm_world, ierr )

  ! Declaration
	if(myrank==0) then
	write(*,*) '--------------'
	if( dream_simulate_version == 2) then
		write(*,*) 'Excute MCMC'
	endif
	if( dream_simulate_version /= 2) then
		write(*,*) 'Excute simulation'
	endif
	write(*,*) '--------------'
	call cpu_time(cpu_time1)
	endif

	! Read namelist (only core 0) (Subroutine is in ./MCMC/read_dreamlist.f90)
	! Read settings for MCMC and names of crops for PRYSBI2
	! Read dream_namelist.txt and control_namelist.txt
	if(myrank == 0) then
		call read_dreamlist
	endif
	call mpi_barrier( temp_mpi_comm_world, ierr )


	! Cast all data and the information about basic setup to each spatial grid (Subroutine is in ./Divider/divide_all_data.f90)
	! The data of prior distribution isn't able to read (need to share the imformation of dream_namelist)
	! Cast the data from s_read_all_data and the imformation of namelist from read_dreamlist to each grid
	call s_divide_all_data
	call mpi_barrier( temp_mpi_comm_world, ierr )

	! Allocate the array of variables for DREAM (Subroutine is in ./MCMC/Mallocate_dream_param)
	call allocate_dream_param
	call mpi_barrier( temp_mpi_comm_world, ierr )

	! Read the data of prior distribution etc. (only core 0) (Subroutine is in ./MCMC/Mread_setting.f90)
	! Read mcmc_setting.dat
	if(myrank == 0) then
	call read_setting
	endif
	call mpi_barrier( temp_mpi_comm_world, ierr )

	! Cast the data of prior distribution etc. to each spatial grid (Subroutine is in ./Devider/cast_setting.f90)
	call s_cast_setting

	! Declaration of ending preparation
	if(myrank==0) then
		write(*,*) 'End preparation'
		write(*,*) 'Start calculation>>>'
	endif

	! Run MCMC by each assigned grid (Do nothing if core 0)
	! dream is in ./MCMC/Mdream.f90, deallocate_var is in ./MCMC/deallocate_var
	if(myrank /= 0 .and. eachgrd > 0) then
		do igrid = 1, eachgrd ! igrid and eachgrd are in var_divide
			call dream
			call deallocate_var
		enddo
	endif
  call mpi_barrier( temp_mpi_comm_world, ierr )

	! Declaration of ending calculation
	if(myrank==0) then
		write(*,*) '<<<End calculation'
		write(*,*) 'Calculation has successfully finished!!!'
		call cpu_time(cpu_time2)
		write(*,*) cpu_time2 - cpu_time1, 's was needed.'
	endif

	! Finalize MPI
	call mpi_finalize(ierr)

end program
