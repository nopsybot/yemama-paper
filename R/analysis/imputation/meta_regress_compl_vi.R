meta_regress_compl_vi <- function(dat_imp_setup) {
  
  model_frame <- data.table::data.table(
    name = c("linear","full"),
    formula = paste0(
      "compl_ln_sd_corr ~ ", c("compl_m","compl_m + I(compl_m^2)")
    ) %>% 
      lapply(as.formula)
  ) %>% 
    collapse::mtt(
      result = lapply(
        formula,
        \(frml) metafor::rma(
          frml, vi = compl_ln_sd_corr_vi, 
          data = dat_imp_setup[compliance_reported==TRUE & !is.na(compl_sd),][]
        ) 
      ) %>% setNames(name)
    )
  
  return(model_frame)
}
