esc_diff_calc <- function(
    data, es_type, es ,
    grp1m_var, grp1sd_var, grp1n_var, grp2m_var, grp2sd_var, grp2n_var,
    ysd_var = "compl_sd_imp",
    totaln_var = "compl_sample_size",
    pfx
){

  esc_vars <- c("esc","se","var","ci.lo","ci.hi","w","totaln","measure","info")
  
  data[
    !is.na(es_type),
    { if (es_type=="group means and SDs")
      res <- esc::esc_mean_sd(
        grp1m = grp1m_var, grp1sd = grp1sd_var, grp1n = grp1n_var,
        grp2m = grp2m_var, grp2sd = grp2sd_var, grp2n = grp2n_var,
        es.type = "g"
      ) %>% get_elem("study",invert=TRUE)
    
    else if (es_type=="group means, overall SD")
      res <- esc::esc_mean_sd(
        grp1m = grp1m_var, grp1n = grp1n_var,
        grp2m = grp2m_var, grp2n = grp2n_var,
        totalsd = ysd_var*100, es.type = "g"
      ) %>% get_elem("study",invert=TRUE)
    
    else if (es_type=="OR")
      res <- esc::convert_or2d(
        or = as.numeric(es), se = 1, totaln = totaln_var,
        es.type = "g"
      ) %>% get_elem("study",invert=TRUE)
    
    else if (es_type=="t-score")
      res <- esc::esc_t(
        t = as.numeric(es),totaln = totaln_var,
        es.type = "g"
      ) %>% get_elem("study",invert=TRUE)
    
    else if (es_type=="Chi-square")
      res <- esc::esc_chisq(
        chisq = as.numeric(es),totaln = totaln_var,
        es.type = "g"
      ) %>% get_elem("study",invert=TRUE)
    
    else if (es_type=="r-point-biseral")
      res <- esc::esc_rpb(
        r = as.numeric(es), totaln = totaln_var,
        es.type = "g"
      ) %>% get_elem("study",invert=TRUE)
    
    else if (es_type=="B (unstandardized)")
      res <- esc::esc_B(
        b = as.numeric(es), sdy = ysd_var*100, 
        grp1n = grp1n_var, grp2n = grp2n_var, es.type = "g"
      ) %>% get_elem("study",invert=TRUE)
    
    else if (es_type=="F-test")
      res <- esc::esc_f(
        f = as.numeric(es), totaln = totaln_var, 
        es.type = "g"
      ) %>% get_elem("study",invert=TRUE)
    
    res$totaln <- as.integer(res$totaln);
    res %>% setNames(paste0(pfx,"_",names(.)) %>% str_replace("_es$","_esc"))
    
    },
    
    by = "record_id",
    
    env = list(
      es_type = es_type, es = es,
      grp1m_var = grp1m_var, grp1sd_var = grp1sd_var, grp1n_var = grp1n_var,
      grp2m_var = grp2m_var, grp2sd_var = grp2sd_var, grp2n_var = grp2n_var,
      ysd_var = ysd_var, totaln_var = totaln_var, pfx = I(pfx),
      esc_vars = I(esc_vars))
  ][]
}
