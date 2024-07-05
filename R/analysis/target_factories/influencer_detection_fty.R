influencer_detection_fty <- function(influencer_tarlist) {
  
  list(
  
    tar_combine(
      allfluencers,
      influencer_tarlist,
      command = list(!!!.x) %>% 
        rbindlist(idcol = "infl_outcome") %>% 
        fsubset(inf=="*" | is.inf_zi | is.inf_cookd) %>% 
        mtt(infl_outcome = str_remove(infl_outcome,"influencers_"))
    ),
    
    tar_target(
      dat_influencers,
      dat_arcsin %>% 
        fsubset(record_id %in% allfluencers$slab) %>% 
        gv(.c(record_id,year,sample_country1,recruit_cntxt,n_ema_days,
              prompt_dfreq,item_n,resp_dur_m_sec,
              any_augmented_vs_nonrep,obj_addon,incent_compliance, 
              inc_val_pot_max.usd, ema_train_vs_nonrep,
              part_care_minact,parent_involvement,compl_m,compl_m_format,
              compl_sample_size,compl_cutoff_strict_bin,compl_cutoff,
              retention_n,retention_rate_extr
        )) %>% 
        join(
          allfluencers[,.(infl_outcome,slab,rstudent,is.inf_cookd)],
          on = c("record_id" = "slab")
        )
    )
  )
}