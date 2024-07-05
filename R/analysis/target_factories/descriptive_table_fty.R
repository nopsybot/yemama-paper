descriptive_table_fty <- function() {
  
  tar_plan(
    diag_breakup = tabulate_clin_groups(dat_imputed),
    
    diag_breakup_gt = gt_style_diag_breakup(diag_breakup),
    
    study_dat = dat_imputed %>%
      gby("primary_record_id") %>% 
      fsummarise(
        across(
          .cols = .c(
            first_pub,n_groups,enhancement,incent_type,feedback_vs_nonrep,
            ema_train_type,parent_involvement,clin_sample.mix
          ),
          .fn = mix_fct
        ),
        sample_country1 = ffirst(sample_country1,na.rm = TRUE),
        retention_n = round(fsum(retention_n*(retention_rate_extr/100), na.rm = TRUE)), # %>% setLabels("Sample size"),
        age = fmean(age,w = retention_n, na.rm = TRUE),
        female.prc = fmean(female.prc,w = retention_n,na.rm = TRUE),
        n_ema_days = fmean(n_ema_days,w = retention_n,na.rm=TRUE),
        prompt_dfreq = fmean(prompt_dfreq,w = retention_n,na.rm=TRUE),
        random_signaling = fmax(random_signaling,na.rm = TRUE),
        item_n = fmax(item_n,na.rm = TRUE),
        resp_dur_m_sec = fmean(resp_dur_m_sec,retention_n, na.rm = TRUE),
        inc_val_pot_max.usd = fmean(inc_val_pot_max.usd,w = retention_n, na.rm = TRUE),
        acceptance_rate_extr = fmean(acceptance_rate_extr,w = acceptance_n, na.rm = TRUE),
        retention_rate_extr = fmean(retention_rate_extr,w = retention_n, na.rm = TRUE),
        compl_m = fmean(compl_m*100,w = compl_sample_size, na.rm = TRUE)
      ) %>% 
      fungroup(),
    
    bins_plan = list(
      in_breaks = list(
        first_pub = seq(2005,2020,by = 5),
        retention_n = c(50,100,300,600),
        age = c(5,10,15),
        female.prc = c(25,50,75),
        n_ema_days = c(1,8,15,29,60),
        prompt_dfreq = c(1,3,6,11),
        item_n = c(10,20,40,60),
        inc_val_pot_max.usd = c(20,50,100,200),
        acceptance_rate_extr = c(40,60,80,90),
        retention_rate_extr = c(40,60,80,90),
        compl_m = c(40,60,80,90)
      ),
      out_names = .c(
        first_pub,sample_size,age,gender,n_days,p_day,item_n,mon_val,
        accept,retent,compl
      ) %>% paste0(".bins")
    ) %>% 
      qDT(keep.attr = FALSE) %>% 
      mtt(var = names(in_breaks)),

    study_grp_bins_desc = add_vars(
        study_dat %>% 
          desc_bins_cats(bins_plan) %>% 
          frename(N_study = N,prc_study=prc),
        dat_imputed %>% 
          mtt(compl_m = compl_m*100) %>% 
          desc_bins_cats(bins_plan) %>% 
          fselect(N_group = N,prc_group = prc)
      ),
    
    binned_desc_gt = gt_style_binned_desc(
      study_grp_bins_desc,
      N = nrow(dat_imputed)
    ),
    
    tar_map(
      values = data.frame(formats = c(html = "html", docx = "docx", pdf = "pdf")),
      tar_file(
        binned_desc_gt_file,
        fs::path(
          "pipelines/analysis/tables/table_1",paste0("binned_desc_gt.",formats)
        ) %>% as.character() %T>% 
          gt::gtsave(data = binned_desc_gt,filename = .)
        )
    ),
    
    newly_added_samples = dat_arcsin %>% 
      fsubset(inWen2017!="Wen 2017" & year < 2016) %>% 
      fselect(
        primary_record_id,age,age_max,prompt_dfreq,n_ema_days,compl_m,
        compl_sample_size
      ),
    newly_added_studies = newly_added_samples %$% fnunique(primary_record_id),
    
    newly_added_samples_oldelig = newly_added_samples %>% 
      fsubset(
        (age_max <= 18 | is.na(age_max))  & 
          (prompt_dfreq >= 2 | is.na(prompt_dfreq))
        # for meta-analytic inclusion 
        # & !panyNA(gv(.,.c(prompt_dfreq,n_ema_days,compl_m,compl_sample_size)))
      ),
    newly_added_sudies_oldelig = newly_added_samples_oldelig %$% 
      fnunique(primary_record_id),
    
  )
  
}
  
