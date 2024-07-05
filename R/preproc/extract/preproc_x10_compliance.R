preproc_x10_compliance <- function(
    extracts = targets::tar_read(extracts)
) {
  
  prc_vars <- c("compl_tot_prc","compl_raw_m","compl_raw_sd","compl_raw_md","compl_raw_q1",
                "compl_raw_q3","compl_raw_min","compl_raw_max")
  
 extracts[
    ,(prc_vars) := lapply(.SD,\(x) x/100),.SDcols = (prc_vars)
  ][
    ,`:=`(
      prompt_tot_n = fcoalesce(prompt_tot_calc,prompt_tot,compl_pcount_sent) %>% 
        setLabels("Total n° of planned prompts") %>% 
        setLabels(attrn="feature","design"),
      
      acceptance_rate_extr = acceptance_rate_extr %>% 
        setLabels("Acceptance rate") %>% 
        setLabels(attrn = "feature","outcome"),
      
      retention_rate_extr = retention_rate_extr %>% 
        setLabels("Retention rate") %>% 
        setLabels(attrn = "feature","outcome")
    )
  ][
    # , prompt_pfreq := kit::nif(!is.na(prompt_freq),prompt_freq)
  ][
    , compl_m_format := nif(
      !panyNA(compl_raw_m),"av. percentage",
      !panyNA(compl_tot_prc),"overall percentage",
      !panyNA(compl_tot_resp,compl_tot_sent),"overall responses",
      
      !panyNA(compl_pcount_m,prompt_tot_n),"av. protocol prompt count",
      !panyNA(compl_pdaycount_m,prompt_dfreq),"av. daily prompt count",
      !panyNA(compl_day_m,n_ema_days),"av. full-day-compliance",
      
      !panyNA(compl_raw_min,compl_raw_q1,compl_raw_md,compl_raw_q1,compl_raw_max,compl_sample_size),"median percentage (Range + IQR)",
      !panyNA(compl_raw_min,compl_raw_md,compl_raw_max,compl_sample_size),"median percentage (Range)",
      !panyNA(compl_raw_q1,compl_raw_md,compl_raw_q3,compl_sample_size),"median percentage (IQR)",
      
      !panyNA(compl_pcount_min,compl_pcount_q1,compl_pcount_md,compl_pcount_q3,compl_pcount_max,compl_sample_size,prompt_tot_n),"median protocol count (Range + IQR)",
      !panyNA(compl_pcount_min,compl_pcount_md,compl_pcount_max,compl_sample_size,prompt_tot_n),"median protocol count (Range)",
      !panyNA(compl_pcount_q1,compl_pcount_md,compl_pcount_q3,compl_sample_size,prompt_tot_n),"median protocol count (IQR)",
      
      !panyNA(compl_pdaycount_min,compl_pdaycount_q1,compl_pdaycount_md,compl_pdaycount_q3,compl_pdaycount_max,compl_sample_size,prompt_dfreq),"Quantile daily count (Range + IQR)",
      !panyNA(compl_pdaycount_min,compl_pdaycount_md,compl_pdaycount_max,compl_sample_size,prompt_dfreq),"median daily count (Range)",
      !panyNA(compl_pdaycount_q1,compl_pdaycount_md,compl_pdaycount_q3,compl_sample_size,prompt_dfreq),"median daily count (IQR)",
      
      !panyNA(compl_day_min,compl_day_q1,compl_day_md,compl_day_q3,compl_day_max,compl_sample_size,n_ema_days),"median full-day-compliance (Range + IQR)",
      !panyNA(compl_day_min,compl_day_md,compl_day_max,compl_sample_size,n_ema_days),"median full-day-compliance (Range)",
      !panyNA(compl_day_q1,compl_day_md,compl_day_q3,compl_sample_size,n_ema_days),"median full-day-compliance (IQR)",
      
      default = NA_character_
    ) %>% factor(levels = c(
      "av. percentage","overall percentage","overall responses",
      "av. protocol prompt count","av. daily prompt count","av. full-day-compliance",
      "median_percentage (Range + IQR)","median percentage (Range)","median percentage (IQR)",
      "median protocol count (Range + IQR)","median protocol count (Range)","median protocol count (IQR)",
      "median daily count (Range + IQR)","median daily count (Range)","median daily count (IQR)",
      "median full-day-compliance (Range + IQR)","median full-day-compliance (Range)","median full-day-compliance (IQR)"
    ))
  ][
    , compl_m := nif(
      !panyNA(compl_raw_m),compl_raw_m,
      !panyNA(compl_tot_prc),compl_tot_prc,
      !panyNA(compl_tot_resp,compl_tot_sent),compl_tot_resp/compl_tot_sent,
      !panyNA(compl_pcount_m,prompt_tot_n),compl_pcount_m/prompt_tot_n,
      !panyNA(compl_pdaycount_m,prompt_dfreq),compl_pdaycount_m/prompt_dfreq,
      !panyNA(compl_day_m,n_ema_days),compl_day_m/n_ema_days,
      default = NA_real_
    )  %>% 
      setLabels("Mean compliance rate") %>% 
      setLabels(attrn="feature","outcome")
  ][
    , compl_sd_format := nif(
      !panyNA(compl_raw_sd),"SD percentage",
      
      !panyNA(compl_pcount_sd,prompt_tot_n),"SD protocol prompt count",
      !panyNA(compl_pdaycount_sd,prompt_dfreq),"SD daily prompt count",
      !panyNA(compl_day_sd,n_ema_days),"SD full-day-compliance",
      
      !panyNA(compl_raw_min,compl_raw_q1,compl_raw_md,compl_raw_q3,compl_raw_max,compl_sample_size),"Quantile percentage (Range + IQR)",
      !panyNA(compl_raw_min,compl_raw_md,compl_raw_max,compl_sample_size),"Quantile percentage (Range)",
      !panyNA(compl_raw_q1,compl_raw_md,compl_raw_q3,compl_sample_size),"Quantile percentage (IQR)",
      
      !panyNA(compl_pcount_min,compl_pcount_q1,compl_pcount_md,compl_pcount_q3,compl_pcount_max,compl_sample_size,prompt_tot_n),"Quantile protocol count (Range + IQR)",
      !panyNA(compl_pcount_min,compl_pcount_md,compl_pcount_max,compl_sample_size,prompt_tot_n),"Quantile protocol count (Range)",
      !panyNA(compl_pcount_q1,compl_pcount_md,compl_pcount_q3,compl_sample_size,prompt_tot_n),"Quantile protocol count (IQR)",
      
      !panyNA(compl_pdaycount_min,compl_pdaycount_q1,compl_pdaycount_md,compl_pdaycount_q3,compl_pdaycount_max,compl_sample_size,prompt_dfreq),"Quantile daily count (Range + IQR)",
      !panyNA(compl_pdaycount_min,compl_pdaycount_md,compl_pdaycount_max,compl_sample_size,prompt_dfreq),"Quantile daily count (Range)",
      !panyNA(compl_pdaycount_q1,compl_pdaycount_md,compl_pdaycount_q3,compl_sample_size,prompt_dfreq),"Quantile daily count (IQR)",
      
      !panyNA(compl_day_min,compl_day_q1,compl_day_md,compl_day_q3,compl_day_max,compl_sample_size,n_ema_days),"Quantile full-day-compliance (Range + IQR)",
      !panyNA(compl_day_min,compl_day_md,compl_day_max,compl_sample_size,n_ema_days),"Quantile full-day-compliance (Range)",
      !panyNA(compl_day_q1,compl_day_md,compl_day_q3,compl_sample_size,n_ema_days),"Quantile full-day-compliance (IQR)",
      
      !panyNA(compl_raw_min,compl_raw_m,compl_raw_max,compl_sample_size),"Quantile percentage (Range + mean)",
      
      default = NA_character_
    ) %>% factor(levels = c(
      "SD percentage",
      "SD protocol prompt count","SD daily prompt count","SD full-day-compliance",
      "Quantile percentage (Range + IQR)","Quantile percentage (Range)","Quantile percentage (IQR)",
      "Quantile protocol count (Range + IQR)","Quantile protocol count (Range)","Quantile protocol count (IQR)",
      "Quantile daily count (Range + IQR)","Quantile daily count (Range)","Quantile daily count (IQR)",
      "Quantile full-day-compliance (Range + IQR)","Quantile full-day-compliance (Range)","Quantile full-day-compliance (IQR)"
      ,"Quantile percentage (Range + mean)"
    ))
  ][
    , compl_sd := nif(
      !panyNA(compl_raw_sd),compl_raw_sd,
      
      !panyNA(compl_pcount_sd,prompt_tot_n),compl_pcount_sd/prompt_tot_n,
      !panyNA(compl_pdaycount_sd,prompt_dfreq),compl_pdaycount_sd/prompt_dfreq,
      !panyNA(compl_day_sd,n_ema_days),compl_day_sd/n_ema_days,
      default = NA_real_
    ) 
    
  ][# S1 Median percentage (Range) 
    record_id=="Borus_2013_1912_sb", `:=`(
      compl_m = estmeansd::mln.mean.sd(min.val = compl_raw_min, med.val = compl_raw_md, max.val = compl_raw_max, n = compl_sample_size)$est.mean,
      compl_sd = estmeansd::mln.mean.sd(min.val = compl_raw_min, med.val = compl_raw_md, max.val = compl_raw_max, n = compl_sample_size)$est.sd
    )
  ][ # S2 Median percentage (IQR)
    record_id=="Corwin_2023_8377_dyn_vr", `:=`(
      compl_m = estmeansd::mln.mean.sd(q1.val = compl_raw_q1, med.val = compl_raw_md, q3.val = compl_raw_q3, n = compl_sample_size)$est.mean,
      compl_sd = estmeansd::mln.mean.sd(q1.val = compl_raw_q1, med.val = compl_raw_md, q3.val = compl_raw_q3, n = compl_sample_size)$est.sd
    )
  ][ # S2 Median percentage (IQR)
    record_id=="Corwin_2023_8377_flat_vr", `:=`(
      compl_m = estmeansd::mln.mean.sd(q1.val = compl_raw_q1, med.val = compl_raw_md, q3.val = compl_raw_q3, n = compl_sample_size)$est.mean,
      compl_sd = estmeansd::mln.mean.sd(q1.val = compl_raw_q1, med.val = compl_raw_md, q3.val = compl_raw_q3, n = compl_sample_size)$est.sd
    )
  ][
    record_id=="Heathcote_2022_15_vr", # S1 Quantile percentage (Range)
    compl_sd := estmeansd::mln.mean.sd(min.val = compl_raw_min, med.val = compl_raw_md, max.val = compl_raw_max, n = compl_sample_size)$est.sd
  ][
    record_id=="Mayne_2023_8149_vr", # S1 Quantile percentage (Range)
    compl_sd := estmeansd::mln.mean.sd(min.val = compl_raw_min, med.val = compl_raw_md, max.val = compl_raw_max, n = compl_sample_size)$est.sd
  ][
    record_id=="Bakshi_2017_1376_sb", `:=`(# S2 Quantile full-day-compliance (Range)
    compl_m = estmeansd::mln.mean.sd(min.val = compl_day_min,q1.val = compl_day_q1, med.val = compl_day_md, q3.val = compl_day_q3, max.val = compl_day_max, n = compl_sample_size)$est.mean/n_ema_days,
    compl_sd = estmeansd::mln.mean.sd(min.val = compl_day_min,q1.val = compl_day_q1, med.val = compl_day_md, q3.val = compl_day_q3, max.val = compl_day_max, n = compl_sample_size)$est.sd/n_ema_days
    )  # Like with range but with mean instead of median
  ][
    record_id=="Alfven_2010_2141", # S2 Quantile full-day-compliance (Range)
    compl_sd := estmeansd::mln.mean.sd(
      min.val = compl_raw_min, med.val = compl_raw_m, max.val = compl_raw_max, n = compl_sample_size
    )$est.sd
  ][# Like with range but with mean instead of median
    record_id=="Connelly_2010_2138_vr", # S2 Quantile full-day-compliance (Range)
    compl_sd := estmeansd::mln.mean.sd(
      min.val = compl_raw_min, med.val = compl_raw_m, max.val = compl_raw_max, n = compl_sample_size
    )$est.sd
  ][# Like with range but with mean instead of median
    record_id=="Cordier_2016_1592_vr", # S2 Quantile full-day-compliance (Range)
    compl_sd := estmeansd::mln.mean.sd(
      min.val = compl_raw_min, med.val = compl_raw_m, max.val = compl_raw_max, n = compl_sample_size
    )$est.sd
  ][# Like with range but with mean instead of median
    record_id=="Dunton_2012_2019_sb", # S2 Quantile full-day-compliance (Range)
    compl_sd := estmeansd::mln.mean.sd(
      min.val = compl_raw_min, med.val = compl_raw_m, max.val = compl_raw_max, n = compl_sample_size
    )$est.sd
  ][# Like with range but with mean instead of median
    record_id=="Dunton_2015_1644", # S2 Quantile full-day-compliance (Range)
    compl_sd := estmeansd::mln.mean.sd(
      min.val = compl_raw_min, med.val = compl_raw_m, max.val = compl_raw_max, n = compl_sample_size
    )$est.sd
  ][# Like with range but with mean instead of median
    record_id=="Garcia_2014_1963_vr", # S2 Quantile full-day-compliance (Range)
    compl_sd := estmeansd::mln.mean.sd(
      min.val = compl_raw_min, med.val = compl_raw_m, max.val = compl_raw_max, n = compl_sample_size
    )$est.sd
  ][# Like with range but with mean instead of median
    record_id=="Kennedy_2022_8753_vr", # S2 Quantile full-day-compliance (Range)
    compl_sd := estmeansd::mln.mean.sd(
      min.val = compl_raw_min, med.val = compl_raw_m, max.val = compl_raw_max, n = compl_sample_size
    )$est.sd
  ][# Like with range but with mean instead of median
    record_id=="Milojevich_2023_8120_vr", # S2 Quantile full-day-compliance (Range)
    compl_sd := estmeansd::mln.mean.sd(
      min.val = compl_raw_min, med.val = compl_raw_m, max.val = compl_raw_max, n = compl_sample_size
    )$est.sd
  ][# Like with range but with mean instead of median
    record_id=="Murray_2023_8101_vr", # S2 Quantile full-day-compliance (Range)
    compl_sd := estmeansd::mln.mean.sd(
      min.val = compl_raw_min, med.val = compl_raw_m, max.val = compl_raw_max, n = compl_sample_size
    )$est.sd
  ][# Like with range but with mean instead of median
    record_id=="Rah_2006_2283_vr", # S2 Quantile full-day-compliance (Range)
    compl_sd := estmeansd::mln.mean.sd(
      min.val = compl_raw_min, med.val = compl_raw_m, max.val = compl_raw_max, n = compl_sample_size
    )$est.sd
  ][# Like with range but with mean instead of median
    record_id=="Tang_2022_8553_vr", # S2 Quantile full-day-compliance (Range)
    compl_sd := estmeansd::mln.mean.sd(
      min.val = compl_raw_min, med.val = compl_raw_m, max.val = compl_raw_max, n = compl_sample_size
    )$est.sd
  ][# Like with range but with mean instead of median
    record_id=="Turri_2023_7947_vr", # S2 Quantile full-day-compliance (Range)
    compl_sd := estmeansd::mln.mean.sd(
      min.val = compl_raw_min, med.val = compl_raw_m, max.val = compl_raw_max, n = compl_sample_size
    )$est.sd
  ][
    compliance_reported==TRUE, compl_m_as_med := compl_m_format=="Quantile percentage (Range + mean)"
  ][]
 
 }
