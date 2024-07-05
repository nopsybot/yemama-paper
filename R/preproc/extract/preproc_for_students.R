preproc_for_students <- function(extracts,dict) {
 extracts %>% 
    attach_dict_labels(dict = dict) %>% 
    preproc_x06_ema_design() %>% 
    preproc_x10_compliance() %>% 
    collapse::fselect(
      record_id,recnum,primary_record_id,auth_year,
      title,year,journal,doi,apa_ref,study_name,study_abbr,
      sample_country1,sample_country2,sample_country3,sampling_start,
      sampling_end, n_groups,
      
      age_m,age_sd,age_md,age_enr_max,age_enr_min,age_elig_min,
      age_vs_emaage,years_ema_later,edu_highest,edu_sco_yrs_min,edu_sco_yrs_max,
      prc_female,ses,nonwhite_prc,
      
      clin_sample___1,clin_sample___2,clin_sample___3,clin_sample___4,
      clin_domain___1,clin_domain___2,clin_domain____99,
      diag_txt,diag_txt_harmonized,clin_diag_group,diag_comorbidity,
      treat_status___1, treat_status___2,treat_status___3,treat_status___4,                    
      treat_setting___1,treat_setting___2,treat_setting___3, treat_setting___4,
      treat_domain___1,treat_domain___2,treat_domain___3,treat_domain___4,
      
      design,design_interv,controlled,design_control,randomized,blinded,                             
      emi_used,multicenter,n_centers,
      
      recruit_cntxt___1,recruit_cntxt___2,recruit_cntxt___3,                   
      recruit_cntxt___4,recruit_cntxt___5,recruit_cntxt___6,                   
      recruit_cntxt_other,
      
      acceptance_reported,acceptance_rate_extr,
      retention_reported,retention_rate_extr,
      
      n_ema_days,n_break_days,
      multiwave, n_waves,wave_dur,wave_intervall,                      
      tot_part_dur,ass_wkend,ass_holidays, n_days_noscol,
      
      ema_sampling___1,ema_sampling___2,ema_sampling___3,
      sc_schedule___1,sc_schedule___2,sc_schedule___3,sc_schedule___4,
      prompt_stability, prompt_freq, prompt_vary_av_calc,prompt_freq_min, 
      prompt_freq_max, prompt_tot_calc, prompt_tot,prompt_interval_txt, 
      prompt_earliest, prompt_latest, 
      trigger_who___1, trigger_who___2, trigger_who___3, trigger_txt,
      
      prevent_miss_type___1,prevent_miss_type___2,prevent_miss_type___3,
      prevent_miss_type___4,prevent_miss_type___5, 
      prevent_miss_other, snooze_max_times, snooze_max_dur, no_snooze___1,
      
      item_n_vary,
      item_n_vary_rules___1,item_n_vary_rules___2,item_n_vary_rules___3,
      item_n_vary_rules___4,
      item_n,media_augmented,gamification,
      resp_dur_m, resp_dur_sd,
      
      dev_type___1, dev_type___2, dev_type___3, dev_type___4, dev_type___5, 
      dev_name, platform_type___1, platform_type___2, platform_type___3, 
      platform_type___4, platform_name, dev_own___1, dev_own___2, dev_own___3, 
      obj_addon, 
      obj_addon_domain___1, obj_addon_domain___2, obj_addon_domain___3, 
      obj_addon_domain___4, obj_addon_domain___5, obj_addon_domain___6, 
      obj_addon_domain___7, obj_addon_domain___8, obj_addon_domain___9, 
      obj_addon_domain___10, obj_addon_domain___11, obj_addon_domain___12, 
      obj_addon_domain___13, obj_addon_domain___14, obj_addon_domain___15, 
      sensor_type, 
      obj_dev_type___1, obj_dev_type___2, obj_dev_type___3, obj_dev_type___4, 
      obj_dev_type___5, obj_dev_type_else_txt, 
      obj_dev_name,
      
      target_main___1, target_main___2, 
      target_health___1, target_health___2, target_health___3, 
      target_health___4, target_health___5, target_health___6, 
      target_health___7,
      target_main___3, target_main___4, 
      target_main___5, target_main___6, target_main___7, target_main___8, 
      target_main___9, target_icd_txt,target_else_txt,
      
      compliance_reported,compl_m,compl_m_format, compl_sd,compl_sd_format,
      compl_cutoff_use,
      compl_cutoff_type___1,compl_cutoff_type___2,compl_cutoff_type___3,
      compl_cutoff_type___4,compl_cutoff_type___5,
      compl_cutoff,compl_cutoff_unit,compl_cutoff_days,compl_cutoff_other,
      
      incentives,inc_type_txt,
      incent_plan___1,incent_plan___2,incent_plan___3,incent_plan___4,
      incent_plan___5,incent_plan___6,
      incent_type___1,incent_type___2,incent_type___3,incent_type___4,
      incent_type___5,
      incent_currency,inc_val_pot_max,
      
      feedback,feedback_type___1,feedback_type___2,feedback_type___3,
      feedback_type___4,
      feedback_granularity,feedback_inference,
      
      ema_train_yn,ema_train_type___1,ema_train_type___2,ema_train_type___3,
      ema_demo_type___1,ema_demo_type___2,ema_demo_type___3,ema_demo_type___4,
      
      part_care_type___1,part_care_type___2,part_care_type___3,
      part_care_type___4,part_care_type___5,part_care_type___6,
      par_car_type_else, miss_inquiry,
      
      parent_involvement___1,parent_involvement___2,parent_involvement___3
    )
}
