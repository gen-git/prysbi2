module Mrunif

	!-----------------------------------------------
	! Subroutine for generating random variable from
	! uniform distribution.
	!-----------------------------------------------
	private

	public :: runif


	contains

		subroutine runif ( lower, upper, ru )

			use mt
			implicit none

			real*8, intent(in)  :: lower, upper
			real*8, intent(out) :: ru

				ru = grnd()
				ru = ru * (upper - lower) + lower

		end subroutine runif
end module Mrunif
