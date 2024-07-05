pooled_es_fty <- function(
    dat_imputed = targets::tar_read(dat_imputed)
) {
  
  pooled_plan <-  data.table::data.table(
    name = c("compliance","retention","acceptance"),
    prefix = c("compl","retent","accept")) %>% 
    mtt(
      yi_var = paste0(prefix,c("_m",".asin",".asin")),
      vi_var = paste0(prefix,"_vi"),
      select_var = paste0(name,"_reported"),
      ni_var = c("compl_sample_size","retention_n","acceptance_n"),
      across(c(yi_var,vi_var,select_var,ni_var),.fns = rlang::syms)
    ) %>% 
    mtt(
      .yi_var_str = paste0(prefix,c("_m",".asin",".asin")),
      .xscale_trans_fun = list(
        \(x) x,metafor::transf.arcsin,metafor::transf.arcsin
      ),
      .x_lab = c(
      "Compliance rate (%)",
      "Retention rate (arcsine scale)",
      "Acceptance rate (arcsine scale)"
      ),
      .xlab_trans_fun = list(
        \(x) x*100,metafor::transf.iarcsin,metafor::transf.iarcsin
      )
    )
  
  arcsin_raw <- tar_plan(
    
    dat_arcsin = dat_imputed[
      ,c("accept.asin","retent.asin") := lapply(
        .SD,\(x) metafor::transf.arcsin(x/100)
      ),
      .SDcols = c("acceptance_rate_extr","retention_rate_extr")
    ][
      ,c("accept_vi","retent_vi") := lapply(
        .SD,\(x) 1/(4*x)
      ),
      .SDcols = c("acceptance_n","retention_n")
    ][],
    pooled_compliance_cutoff_use = metafor::rma(
      yi = compl_m, vi = compl_vi, mods = compl_cutoff_use, method = "REML",
      data = dat_arcsin[compliance_reported==TRUE,]
    )
  )
  
  pooled_inclusive_results <- pooled_inclusive_fty(pooled_plan)
  
  influence_results <- influencer_detection_fty(
    pooled_inclusive_results$influencers
  ) 
  
  pooled_unfluenced_results <- pooled_unfluenced_fty(pooled_plan)
  
  sensitivity_pooled <- pooled_sensitivity_fty(
    pooled_tarlist = list(
      pooled_inclusive_results$pooled,
      pooled_inclusive_results$pooled_mv,
      pooled_unfluenced_results$pooled_1unfluenced,
      pooled_unfluenced_results$pooled_6unfluenced
    ),
    anova_tarlist = pooled_inclusive_results$anova_full_nol3,
    varcomp_tarlist = pooled_inclusive_results$pooled_mv_varcomp
  )
  
  return(
    list(
      arcsin_raw,
      pooled_inclusive_results,
      influence_results,
      pooled_unfluenced_results,
      sensitivity_pooled
    )
  )
}
