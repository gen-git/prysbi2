module Mgelman_rubin

	private
	public :: gelman_rubin
	contains

	subroutine gelman_rubin(x, rhat)

	implicit none

	real(8) :: x(:,:) ! x( number of chains, number of steps )
	real(8) :: rhat   ! Gelman Rubin's diagnostics (not squared)
					  ! If rhat < 1.2, MCMC is converged.

	integer :: m, n, i, j
	real(8), allocatable :: theta_j(:), pre_w(:)
	real(8) :: theta, w, b, v

	m = size(x, 1)
	n = size(x, 2)

	allocate( theta_j(m), pre_w(m) )

	theta_j(:) = 0d0
	do i = 1, m
		theta_j(i) = sum( x( i, : ) ) / dble(n)
	end do

	theta = sum( theta_j(1:m) ) / dble(m)

	pre_w(:) = 0d0
	do i = 1, m
		pre_w(i) = sum( ( x( i, : ) - theta_j(i) )**2 )
	end do

	w = 1d0/( dble(m)*(dble(n)-1d0) ) * sum( pre_w(1:m) )

	b = dble(n)/( dble(m) - 1d0 ) * sum( ( theta_j(1:m) - theta )**2 )

	v = ( 1d0 - 1d0/dble(n) ) * w + 1d0/dble(n) * b

	rhat = abs( v / (w + 1d-15) )

	deallocate( theta_j, pre_w )

	end subroutine gelman_rubin
end module Mgelman_rubin
