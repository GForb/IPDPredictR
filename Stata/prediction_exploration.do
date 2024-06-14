clear
set obs 400
gen latent = rnormal(0,1)
gen shared_error = rnormal(0, 0.5)
gen study = round(_n/100)
gen id = _n
expand 4
bysort id: gen time = _n
gen age = time + rnormal(0, 0.25)

gen y1 = latent  + rnormal(0,1) +age*1 -study/2
gen y2 = latent  + rnormal(0,1) + age*0.5 + 0.5 -study/4
gen y3 = latent  + rnormal(0,2) + age*0.25 + 2 - study 


gsem (y1 <- i.study age M1[id]@1)

predict_multiwave_gsem time, predictor_waves(1 2 3) out_wave(4) id_var(id) pred_var(pred_multi3)


predict pred_default*
su pred_default* if study ==4


predict pred_marginal*, marginal
su pred_marginal*

predict pred_fixed*, conditional(fixedonly)
su pred_fixed* if study ==4



predict pred_latent*, latent
su pred_latent*

forvalues i = 1 (1) 3 {
	gen my_predict`i' = pred_fixed`i' + pred_latent
	gen check`i' = my_predict`i' - pred_default`i'
	gen error`i' = pred_default`i' - y`i'

 }
 
bysort study: su pred_default* my* check*

bysort study: su error*

get_hold_out_pred study, ///
	hold_out_study(4) ///
	model_code(gsem (y1 <- i.study M1@1) (y2 <- i.study M1@1) (y3 <- i.study M1@1)) ///
	pred_var_name(pred_ado)

get_all_preds study, ///
	model_code(gsem (y1 <- i.study M1@1) (y2 <- i.study M1@1) (y3 <- i.study M1@1)) ///
	pred_var_name(pred_all)
	
bysort study: su pred_ado* pred_all* pred_default* 




*Q does the inclusion of a study intercept change the value of the latent variable