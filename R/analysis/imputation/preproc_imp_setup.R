preproc_imp_setup <- function(dat) {
  
  # 1. Create bias corrected ln SDs ####
  # and their sampling variance for studies with present SDs
  dat[
    compliance_reported==TRUE & !is.na(compl_sd),
    `:=`(
      compl_ln_sd_corr = log(compl_sd) + (1/(2*(compl_sample_size-1))),
      compl_ln_sd_corr_vi = 1/(2*(compl_sample_size-1))
    )
  ][]
}
