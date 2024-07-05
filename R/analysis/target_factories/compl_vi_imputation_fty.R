compl_vi_imputation_fty <- function() {
  
  tar_plan(
    
    dat_imp_setup = preproc_imp_setup(dat),
    
    compl_vi_model_frame = meta_regress_compl_vi(dat_imp_setup),
    
    dat_imputed = predict_impute_compl_vi(compl_vi_model_frame,dat_imp_setup),
    
    imputation_fig_fty()
  )
}
