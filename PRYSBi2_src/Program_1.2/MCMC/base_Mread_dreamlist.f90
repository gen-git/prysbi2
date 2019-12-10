module Mread_dreamlist

	use Mdream_param  ! (dir MCMC)
	use control_param ! (dir PRYSBI2)

	implicit none

	contains

	subroutine read_dreamlist

		implicit none

		character(1000) :: ifile

		write(ifile, "('./Namelist/dream_namelist.txt')")
		open (1, file=ifile)
		read (1, dream_list)
		close(1)

		write(ifile, "('./Namelist/control_namelist_cropcrop.txt')")
		open (1, file=ifile)
		read (1, control_list)
		close(1)

	end subroutine

end module
