fselect_longlist <- function(extracts) {
  
  extracts %>% 
    collapse::fselect(
      # x01. Bibliographic information ####
      record_id,grplab, study_id_dep, recnum,primary_record_id,auth_year,authyear,
      title,year,journal,doi,apa_ref,study_name,study_abbr,
      first_pub,n_pub, inWen2017,
      
      # x02. Study identification ####
      any_outcome,
      sample_country1,sample_country2,sample_country3,
      sampling_start,sampling_end,n_groups,
      
      # x03. Sample characteristics ####
      ##   a. Socio-demographics ####
      age_format,age, age_min,age_max, female.prc,ses,nonwhite.prc,
      ##   b. Clinical status ####
      clin_sample.mix, clin_sample.strict_bin,
      ##   c. Diagnostic groups ####
      clin_sompsy.mix,clin_sompsy.mixna,clin_diag_group,clin_diag_groups.lump,
      diag_txt,diag_txt_harmonized,diag_comorbidity,
      ##   d. Treatment characteristics ####
      treat_status.mix,treat_status.any_vs_na,
      treat_setting.mix,treat_setting.inout_nomix,
      
      # x04. General design ####
      design,design_interv,controlled,design_control,randomized,blinded,                             
      emi_used,multicenter,n_centers,
      
      # x05. Recruitment - Acceptance - Retention ####
      recruit_cntxt,
      acceptance_reported, acceptance_rate_extr, acceptance_n,
      retention_reported, retention_rate_extr, retention_n,
      
      # x06. EMA design ####
      ##   a. Study length ####
      n_ema_days,tot_part_dur,
      multiwave, n_waves,wave_dur,wave_intervall,ass_wkend,ass_holidays, 
      n_days_noscol,
      ##   b. EMA sampling schemes ####
      ema_sampling, added_event_sampling, 
      ##   c. Prompting ####
      prompt_stability, 
      prompt_freq, prompt_dfreq,
      random_signaling, personalized_signaling,
      prompt_tot_n, prompt_vary_av_calc, prompt_tot_calc, prompt_tot,
      prompt_earliest, prompt_latest, 
      ##   d. Event-contingent ####
      trigger_who,
      ##   e. Momentary missingness prevention ####
      prevent_miss_n,prevent_miss_type, snooze_max_dur,
      no_snooze,
      
      # x07. EMA Questionnaire ####
      item_n_vary,item_n_vary_rules, item_n,
      # media_augmented,gamification,
      enhancement, any_augmented_vs_nonrep, illustrated, gamificated,
      resp_dur_m_sec,
      
      # x08. Technical features ####
      dev_type, dev_name, platform_type, dev_own, obj_addon, 
      obj_addon_domain___1, obj_addon_domain___2,
      
      # x09. Assessment targets ####
      target_main_n,target_health_n,target_detail_n,symptom_ema,
      
      # x10. Compliance Outcomes ####
      compliance_reported,compl_m,compl_m_format,compl_sd,compl_sd_format,
      compl_m_as_med,
      compl_sample_size,
      compl_cutoff_use,compl_cutoff_strict_bin,compl_cutoff_type,compl_cutoff,
      compl_cutoff_imp0,
      
      # x11. Incentivization ####
      ##   a. incentives/remuneration ####
      incentives,incent_compl,incent_uncompl,incent_compliance,
      incent_plan_anyuncompl,incent_plan,
      incent_type,incent_monetary,incent_nonmonetary,
      incent_currency.conv,inc_val_pot_max.usd,
      ##   b. feedback ####
      feedback_vs_nonrep,
      ##   c. training ####
      ema_train_vs_nonrep,ema_train_type,
      ##   d. participant care ####
      part_care_vs_nonrep,part_care_anycontact,part_care_minact,
      miss_inquiry_vs_nonrep,
      ##   e. parent involvement ####
      parent_involvement,
      
      # WITHIN-STUDY EFFECT SIZES
      diffgen_sig, diffgen_units, diffgen_es_type,
      diffgen_m_f,diffgen_sd_f,diffgen_n_f,
      diffgen_m_m,diffgen_sd_m,diffgen_n_m,
      diffgen_es,diffgen_signed,
      diff2_sig, diff2_units, diff2_es_type,
      diff2_grp1,diff2_m1,diff2_sd1,diff2_n1,
      diff2_grp2,diff2_m2,diff2_sd2,diff2_n2,
      diff2_grp3,diff2_grp4,
      diff2_es,diff2_signed,
      diff3_sig, diff3_units, diff3_es_type,
      diff3_grp1,diff3_m1,diff3_sd1,diff3_n1,
      diff3_grp2,diff3_m2,diff3_sd2,diff3_n2,
      diff3_grp3,diff3_grp4,
      diff3_es,diff3_signed,
      corr_age_sig, corr_age_es_type,
      corr_age_n, corr_age_coef, corr_age_es,
      corr_base_sig, corr_base_var, corr_base_scale, corr_base_es_type,
      corr_base_n, corr_base_coef, corr_base_es,
      corr3_sig, corr3_var, corr3_scale, corr3_es_type,
      corr3_n, corr3_coef, corr3_es,
      corr4_sig, corr4_var, corr4_scale, corr4_es_type,
      corr4_n, corr4_coef, corr4_es,
      corr5_sig, corr5_var, corr5_scale, corr5_es_type,
      corr5_n, corr5_coef, corr5_es
    )
  
  
}
