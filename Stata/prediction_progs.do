cap prog drop get_all_preds
prog define get_all_preds
syntax varname, model_code(passthru) pred_var_name(passthru) [predict_function(passthru) predict_args(passthru)]
	
	tempfile data_with_preds
	

	
	qui levelsof  `varlist', local(folds)
	tokenize `folds'
	foreach fold in `folds' {
		di "Fold: " `fold'
		preserve
		get_hold_out_pred `varlist', hold_out_study(`fold') `model_code' `pred_var_name' `predict_args'
		set trace on
		keep if `varlist' == `fold'
		if `fold' == `1' {
			save  `data_with_preds'
		}
		else {
			append using `data_with_preds'
			save `data_with_preds', replace
		}
		restore
		set trace off
	}
	di "loading data"
	use `data_with_preds', clear
end



cap prog drop get_hold_out_pred
prog define get_hold_out_pred
syntax varname, hold_out_study(integer) model_code(string) pred_var_name(string) [predict_function(string) predict_args(string)]

	if "`predict_function'" == "" {
		local predict_function predict
	}
	
	
	`model_code' if `varlist' != `hold_out_study'
	if "`predict_args'" == != "" local predict_args , `predict_args'
	`predict_function' `pred_var_name'* if `varlist' == `hold_out_study' `predict_args'
end

