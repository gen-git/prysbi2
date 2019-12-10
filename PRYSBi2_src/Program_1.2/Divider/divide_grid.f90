module divide_grid

	use var_read_all
	use var_divide

	implicit none

	contains

	subroutine s_divide_grid

		implicit none

		! In this subroutine following two are calculated.
		! 1. mpi_num_grid(:) : vector which specify the nmber of grids each core calculates.
		! 2. mpi_knum(:,:)   : matrix which specify the grid No.s each core calculates.

		!--- Variable decaration ---
		integer :: i1, i2, i3, k
		integer :: i_core, i_each
		integer :: temp_num_grid
		integer :: ncalc_grid

		!--- Decide the number of grids for each core (except for core 0) ---
		ncalc_grid = nprocs - 1
		if( ncalc_grid==0 ) then
			write(*,*) '! Error: This program assumes excution by multiple cores.'
			stop
		endif

		! allocation
		allocate( mpi_num_grid(ncalc_grid) )

		! Calculate the number of grids
		i1 = int( ngrd / ncalc_grid )
		i2 = mod( ngrd, ncalc_grid )

		do i_core = 1, ncalc_grid

			if( i_core /= 0 ) then
				if( i_core <= i2 ) mpi_num_grid(i_core) = i1 + 1
				if( i_core >  i2 ) mpi_num_grid(i_core) = i1
			endif

		enddo

		! allocation
		if(i2 > 0) then
			allocate( mpi_knum (i1+1, ncalc_grid) )
		else
			allocate( mpi_knum (i1, ncalc_grid) )
		endif

		! Set target grid numbers (serial umber) for each core
		mpi_knum(:,:)  = int(rnan)
		k = 1
		do i_core = 1, ncalc_grid

			temp_num_grid = mpi_num_grid(i_core)
			do i_each = 1, temp_num_grid

				mpi_knum(i_each, i_core)  = k
				k = k + 1

			enddo

		enddo

		if(myrank==0) then
			write(*,"(i4,a)") ncalc_grid, ' cores will calculate'
			write(*,"(a,i6,a)") ' each core has about ', size( mpi_knum(:,1) ), 'grids'
		endif

	end subroutine

end module
