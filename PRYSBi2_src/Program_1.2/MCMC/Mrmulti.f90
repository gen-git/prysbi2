module Mrmulti

	private

	public :: rmulti

	contains

		subroutine rmulti(weight, sample)

			use mt
			implicit none

			real(8), intent(in) :: weight(:) ! weights
			integer, intent(out) :: sample(:) ! sampled values

			integer :: sizes
			integer :: i, n
			integer :: tgt_i
			integer :: i_sizes
			real(8) :: prob(size(weight))
			real(8) :: acum_prob(size(weight))
			real(8) :: u

			n = size(weight)
			sizes = size(sample)
			prob = weight / sum(weight)

			do i = 1, n
				if(i == 1) acum_prob(i) = prob(i)
				if(i /= 1) acum_prob(i) = acum_prob(i-1) + prob(i)
			enddo

			do i_sizes = 1, sizes
				tgt_i = n
				u = grnd()
				do i = 1, n
					if(u < acum_prob(i)) then
						tgt_i = i
						exit
					endif
				enddo
				sample(i_sizes) = tgt_i
			enddo

		end subroutine rmulti
end module Mrmulti
