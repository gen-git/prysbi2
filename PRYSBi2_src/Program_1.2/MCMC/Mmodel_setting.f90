module Mmodel_setting

	use spm_param_fix
	use swatnut_param_fix
	use day_rothc_param_fix
	use swatwt_param_fix

	implicit none
	
	contains
	
	subroutine model_setting
	
		implicit none
			
		call s_spm_param_fix
		call s_swatnut_param_fix
		call s_day_rothc_param_fix
		call s_swatwt_param_fix
	
	end subroutine model_setting
	
end module Mmodel_setting
