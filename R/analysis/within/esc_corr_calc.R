esc_corr_calc <- function(
    data, 
    es_type = "corr_age_es_type", 
    es = "corr_age_es", 
    pfx="corr_age",
    n = "corr_age_n", 
    coeff = "corr_age_coef"
) {
  
  
  data[
    !is.na(es_type),
    { if(is.na(n))
      n <- compl_sample_size
    
    if( es_type=="OR" )
      res <- data.table(
        pfx_r = esc::pearsons_r(or = as.numeric(es))
      ) %>%
        mtt(pfx_z = esc::convert_r2z(pfx_r), pfx_z_se = 1/sqrt(n-3))
    
    if(es_type == "t-score"){
      res <- data.table(
        pfx_r = esc::esc_t(t = as.numeric(es),totaln = n,es.type = "r")[[1]]
        ) %>% 
        mtt(pfx_z = esc::convert_r2z(pfx_r), pfx_z_se = 1/sqrt(n-3))
    }
    
    if(es_type == "r-product-moment"){
      res <- data.table(pfx_r = as.numeric(coeff)) %>% 
        mtt(pfx_z = esc::convert_r2z(pfx_r), pfx_z_se = 1/sqrt(n-3))
    }
    
    if(es_type == "B"){
      res <- data.table(
        pfx_r = esc::esc_B(
          b = as.numeric(es),sdy = compl_sd_imp*100,
          grp1n = ceiling(n/2),grp2n = floor(n/2),
          es.type = "r"
        )[[1]]
      ) %>% 
        mtt(pfx_z = esc::convert_r2z(pfx_r), pfx_z_se = 1/sqrt(n-3))
    }
    
    if(es_type == "F-test"){
      res <- data.table(
        pfx_r = esc::esc_f(f = as.numeric(es),totaln = n,es.type = "r")[[1]]
      ) %>% 
        mtt(pfx_z = esc::convert_r2z(pfx_r), pfx_z_se = 1/sqrt(n-3))
    }
    res},
    by = record_id,
    env = list(
      es_type = es_type,
      es = es,
      n = n,
      coeff = coeff,
      pfx = pfx,
      pfx_r = paste0(pfx,"_r"),
      pfx_z = paste0(pfx,"_z"),
      pfx_z_se = paste0(pfx,"_z_se")
    )
  ]
  }