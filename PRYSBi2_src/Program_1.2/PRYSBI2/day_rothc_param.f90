module day_rothc_param

implicit none 

real(8)   ::  Wparam( 4 ) ! Parameter adjusting water effect (basically Wparam = 1.0)
real(8)   ::  Lparam( 4 ) ! Parameter adjusting basic decomposition rate (basically Lparam = 1.0)
real(8)   ::  Rparam( 4 ) ! Parameter determining temperature sensitivity (in RothC Rparam = 106.0)

namelist /day_rothc_namelist/ Wparam, Lparam, Rparam

end module
