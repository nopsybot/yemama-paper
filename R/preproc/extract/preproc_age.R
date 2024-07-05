preproc_age <- function(extracts) {
  
  vec_mln_S1_mean <- function(min_val,med_val,max_val,n_val){
    Map(
      f = \(mi,me,ma,en) estmeansd::mln.mean.sd(
        min.val = mi, med.val=me, max.val=ma, n=en
      )$est.mean,
      min_val,med_val,max_val,n_val
    ) %>% as.numeric()
  }
  
  extracts[# Priority 1: reported mean age
      !is.na(age_m),`:=`(age_format = "av. age reported", age = age_m)
    ][# Priority 2: Imputing via median + enrolled range
      is.na(age_m) & !panyNA(age_md,age_enr_min,age_enr_max,retention_n),
      `:=`(
        age_format = "Median and enrolled Range",
        age = vec_mln_S1_mean(
          min_val = age_enr_min, med_val = age_md, max_val = age_enr_max,
          n = retention_n
        )
      )
    ][# Priority 3: Imputing via median + eligible range
      is.na(age) & !panyNA(age_md,age_elig_min,age_elig_max,retention_n),
      `:=`(
        age_format = "Median and eligible Range",
        age = vec_mln_S1_mean(
          min_val = age_elig_min, med_val = age_md, max_val = age_elig_max,
          n = retention_n
        )
      )
    ][# Priority 4: Imputing enrolled midpoint
      is.na(age) & !panyNA(age_enr_min,age_enr_max),
      `:=`(
        age_format = "Midpoint of enrolled Range",
        age = pmean(age_enr_min,age_enr_max)
      )
    ][# Priority 4: Imputing eligible midpoint
      is.na(age) & !panyNA(age_elig_min,age_elig_max),
      `:=`(
        age_format = "Midpoint of eligibe Range",
        age = pmean(age_elig_min,age_elig_max)
      )
    ][
      is.na(age) & !is.na(age_context), `:=`(
        age_format = "Deduced from context information",
        age = age_context
      )
    ][
      age_vs_emaage==2 & !is.na(years_ema_later),
      `:=`(
        age_format = paste(age_format,"(added time until EMA)"),
        age = age + years_ema_later
      )
    ][] %>% 
    mtt(
      age = age %>% 
        setLabels("Mean sample age") %>% 
        setLabels(attrn="feature","sample"),
      
      age_min = fcoalesce(age_enr_min,as.double(age_elig_min)),
      age_max = fcoalesce(age_enr_max,as.double(age_elig_max))
    )
  
}
