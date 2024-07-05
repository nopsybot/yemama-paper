predict_impute_compl_vi <- function(model_frame,dat_imp_setup) {
  
  # 3. Predict missing (and nonmissing) SDs ####
  predicted <- metafor::predict.rma(
    model_frame$result$full, 
    newmods = dat_imp_setup[
      compliance_reported==TRUE, .(compl_m,"I(compl_m^2)"=compl_m^2)
    ] %>% as.matrix(), 
    intercept = TRUE,
    addx = FALSE
  )$pred
  
  # 4. Impute missing and corr.-ln(SDs) and transform to SDs ####
  # order important
  dat_imputed <- dat_imp_setup[
    compliance_reported==TRUE, 
    compl_ln_sd_corr_pred := predicted # All predicted values
  ][
    compliance_reported==TRUE, 
    `:=`(
    compl_ln_sd_corr_imp_yn = compliance_reported & 
      is.na(compl_sd) & !is.na(compl_ln_sd_corr_pred),
    compl_ln_sd_corr_imp = fcoalesce(compl_ln_sd_corr,compl_ln_sd_corr_pred)
    )
  ][
    compliance_reported==TRUE & compl_ln_sd_corr_imp_yn==TRUE
    , compl_sd_imp_only := exp(compl_ln_sd_corr_pred)
  ][
    compliance_reported==TRUE,
    `:=`(
      compl_sd_imp = data.table::fcoalesce(compl_sd,compl_sd_imp_only),
      compl_sd_format = forcats::fct_unify( # unify format factor
        list(
          compl_sd_format,
          compl_ln_sd_corr_imp_yn %>% 
            iif("quadratic meta-regression",NA_character_) %>% factor()
        )
      ) %>% data.table::fcoalesce()
    )
  ][
    compliance_reported==TRUE,
    compl_vi := compl_sd_imp^2/compl_sample_size
  ][]
  
  return(dat_imputed)
  
}




