cap prog drop predict_multiwave_gsem

prog define predict_multiwave_gsem
syntax varname, predictor_waves(numlist) out_wave(integer) id_var(varname) pred_var(name)
	local predictor_waves_csv = subinstr("`predictor_waves'", " ", ", ",.)
	di "`predictor_waves_csv'"
	
	
	tempvar all random_part fixed_part fixed_outcome sd_random random_part_all
	tempvar 
	tempvar 
	
	predict `all' if inlist(`varlist', `predictor_waves_csv')
	predict `fixed_part' if inlist(`varlist', `predictor_waves_csv'),  ///
		conditional(fixedonly)
	gen `random_part' = `all' - `fixed_part'
	
	bysort `id_var': egen `sd_random' = sd(`random_part')
	di "Checking variablily of random part within IDs (should be v close to zero)"
	su `sd_random'
	if r(sd) > 0.0001  {
		di as error "prediction of random part of the model varies within ids"
		error
	}
	su `random_part'
	bysort `id_var': egen `random_part_all' = mean(`random_part') 

	su `random_part' `random_part_all'
	
	predict `fixed_outcome' if `varlist' == `out_wave', conditional(fixedonly)
	gen `pred_var' = `fixed_outcome' + `random_part_all'
	
end

predict_multiwave_gsem time, predictor_waves(1 2 3) out_wave(4) id_var(id)

	di "Number with predictor outcomes"
	count if sdq_emot_p != .
	tempvar random_part 
	predict `random_part' , latent
	tempfile tempfile
	codebook `random_part'
	keep if relative_wave ==0
	count if sdq_emot_p !=.
	save `tempfile'

	restore
	
	merge m:1 studyid ID using `tempfile', keepusing(`random_part')
	drop _merge
	tempvar fixed
	predict `fixed', fixedonly
	gen `namelist' = `fixed' + `random_part'

*Predictions - multi-timepoint

* Fit model
* Drop outcome data
* Get random part prediction - what to do with study ID when random???? - estimate with baseline only
* Restore data
* restrict to outcome only
*Predict fixed part and add latent part.

* What is stored vs what is estimated 
* How does this work if making a prediction for a new individual.
* Model needs to be calibrated for study.

*Results: 



* in the longitduninal setting it is possible to estimate a study ID using baseline data only. So why not in the 


*Obtaining predictions from a gsem with random intercept only, data available for a number of waves
cap prog drop my_predict
prog define my_predict
syntax namelist, predictor_waves(integer)
	preserve
	keep if relative_wave <=0  & relative_wave >=  1- `predictor_waves' 
	di "Number with predictor outcomes"
	count if sdq_emot_p != .
	tempvar random_part 
	predict `random_part' , latent
	tempfile tempfile
	codebook `random_part'
	keep if relative_wave ==0
	count if sdq_emot_p !=.
	save `tempfile'

	restore
	
	merge m:1 studyid ID using `tempfile', keepusing(`random_part')
	drop _merge
	tempvar fixed
	predict `fixed', fixedonly
	gen `namelist' = `fixed' + `random_part'
end

*Obtaining predictions from a gsem with random intercept and slope only, data available for a number of waves

cap prog drop my_predict_rand_slope
prog define my_predict_rand_slope
syntax namelist, predictor_waves(integer)
	preserve
	keep if relative_wave <=0  & relative_wave >=  1- `predictor_waves' 
	di "Number with predictor outcomes"
	count if sdq_emot_p != .
	tempvar rand_int rand_slope
	predict `rand_int', latent(M1[ID])
	predict `rand_slope', latent(M2[ID])
	tempfile tempfile
	keep if relative_wave ==0 // needs to be upgraded with some sort of pickone code so it selects people with different relative waves
	save `tempfile'

	restore
	
	merge m:1 studyid ID using `tempfile', keepusing(`rand_int' `rand_slope')
	drop _merge
	tempvar fixed
	predict `fixed', fixedonly
	gen `namelist' = `fixed' + `rand_int' + age_cent*`rand_slope'
end
