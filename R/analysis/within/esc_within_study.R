esc_within_study <- function(dat_imputed = tar_load(dat_imputed)) {

  # 1. Conversion of compliance units #####
  # Mapped argument list
  diff_pfx <- c("diffgen","diff2","diff3")
  
  diffs <- purrr::map2(
    .x = diff_pfx,
    .y = list(c("m_f","sd_f","m_m","sd_m"),
         c("m1","sd1","m2","sd2"),
         c("m1","sd1","m2","sd2")),
    \(x,y) list(vars = paste0(x,"_",y), units = paste0(x,"_units"))
  ) %>% setNames(diff_pfx)
  # Loop-pipe
  dat.prc <- dat_imputed %>%
    { x <- . ; for (i in seq_along(diffs)){
      x <- convert_compl_units(x,diffs[[i]]$vars,diffs[[i]]$units)
    }; x }
  
  grp1_ifx <- c("_f",1,1)
  grp2_ifx <- c("_m",2,2)
  corr_pfx <- c("corr_age","corr_base","corr3","corr4")
  
  # 2. Map ES-conversion per reported differences ####
  dat_within_diff <- purrr::pmap(
    .l = list(
      pfx = diff_pfx,
      es_type = paste0(diff_pfx,"_es_type"),
      es = paste0(diff_pfx,"_es"),
      grp1m_var = paste0(diff_pfx,"_m",grp1_ifx,".prc"),
      grp1sd_var = paste0(diff_pfx,"_sd",grp1_ifx,".prc"),
      grp1n_var = paste0(diff_pfx,"_n",grp1_ifx),
      grp2m_var = paste0(diff_pfx,"_m",grp2_ifx,".prc"),
      grp2sd_var = paste0(diff_pfx,"_sd",grp2_ifx,".prc"),
      grp2n_var = paste0(diff_pfx,"_n",grp2_ifx)
    ),
    .f = esc_diff_calc, dat.prc
  )
  
  # 3. Map ES-conversion per reported correlate category ####
  dat_within_corr <- purrr::pmap(
    list(
      pfx=corr_pfx,
      es_type = paste0(corr_pfx,"_es_type"), es = paste0(corr_pfx,"_es"),
      coeff = paste0(corr_pfx,"_coef"), n = paste0(corr_pfx,"_n") 
    ),
    .f = esc_corr_calc, dat.prc
  )
    
  # 4. Join and split special frequent comparisons (racial and clinical) ####
  dat_within <- Reduce(
    f = \(x,y) join(x,y,on = "record_id",validate = "m:1",), 
    x = list(dat.prc,dat_within_diff,dat_within_corr) %>% purrr::list_flatten()
  ) %>% 
    split_rgx_comparison(rgx = "white") %>% 
    split_rgx_comparison(rgx = "clin")
  
  
return(dat_within)


}
