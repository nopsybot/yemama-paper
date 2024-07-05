preproc_x06_ema_design <- function(extracts) {
  
  extracts %>% 
    mtt(
      n_ema_days = n_ema_days %>% 
        setLabels("Number of EMA days") %>% 
        setLabels(attrn = "feature","design"),
      
      multiwave = multiwave %>% as.logical() %>% 
        iif("single wave","multiwave") %>% 
        factor(levels = c("single wave","multiwave")) %>% 
        setLabels("Multi-wave design") %>% 
        setLabels(attrn="feature","design"),
      
      tot_part_dur =  iif( # ok (90 explicit extractions),
        !is.na(tot_part_dur_days),as.numeric(tot_part_dur_days),n_ema_days
      ) %>% 
        setLabels(attrn="feature","design"),
      
      ema_sampling = nif(
        ema_sampling___1==1 & ema_sampling___3==0,
        "no event-contingent",
        ema_sampling___1==1 & ema_sampling___3==1,
        "including event-contingent",
        ema_sampling___1==0 & ema_sampling___2==0 & ema_sampling___3==1,
        "only event-contingent",
        ema_sampling___1==0 & ema_sampling___2==1 & ema_sampling___3==0,
        "only intervall-contingent",
        psum(gvr(.,"ema_sampling___[1-3]"))==0,
        NA_character_
      ) %>% factor(
        levels = c("no event-contingent","including event-contingent",
                   "only event-contingent","only intervall-contingent")
      ) %>% 
        setLabels(attrn="feature","design"),
      
      added_event_sampling = nif(
        ema_sampling___1==1 & ema_sampling___3==0, "no event-contingent EMAs",
        ema_sampling___1==1 & ema_sampling___3==1, "event-contingent add-on"
      ) %>% factor(
        levels = c("no event-contingent EMAs","event-contingent add-on")
      ) %>% setLabels("Event-contingent EMAs as add-on") %>% 
        setLabels(attrn="feature","design"),
      
      random_signaling = nif(
        sc_schedule___1==1 | sc_schedule___2==1 & 
          !(sc_schedule___3==1 | sc_schedule___4==1),"fixed sampling", 
        sc_schedule___3==1 | sc_schedule___4==1, "(semi)-random sampling" 
      ) %>% 
        factor(levels = c("fixed sampling","(semi)-random sampling")) %>% 
        setLabels("Sampling scheme") %>% 
        setLabels(attrn="feature","design"),
      
      personalized_signaling = nif(
        sc_schedule___2==1,"personalized",
        psum(gvr(.,"sc_schedule___[134]"))>0,"generic fixed or random"
      ) %>% factor( levels = c("personalized","generic fixed or random") ) %>% 
        setLabels("Personalization of signaling schedule") %>% 
        setLabels(attrn="feature","design"),
      
      prompt_stability = prompt_stability %>%
        forcats::fct_recode(
          levels = !!!levels(prompt_stability) %>% 
            setNames(c("stable","varying"))
      ) %>% 
        setLabels(attrn="feature","design"),
      
      prompt_dfreq_format = nif(
        !is.na(prompt_freq),"as reported",
        !is.na(prompt_freq_calc),"averaged across days",
        is.na(prompt_freq_calc) & !is.na(prompt_tot),"total by days",
        is.na(prompt_freq_calc) & !is.na(compl_pcount_sent),"total by days"
      ) %>% 
        setLabels(attrn="feature","design"),
      
      prompt_dfreq = nif(
        !is.na(prompt_freq),as.numeric(prompt_freq),
        !is.na(prompt_freq_calc),prompt_freq_calc,
        
        is.na(prompt_freq_calc) & !is.na(prompt_tot),
        prompt_tot/n_ema_days,
        is.na(prompt_freq_calc) & !is.na(compl_pcount_sent),
        compl_pcount_sent/n_ema_days
      ) %>% 
        copyAttrib(prompt_freq) %>% 
        setLabels("Prompt frequency") %>% 
        setLabels(attrn="feature","design"),
      
      trigger_who = nif( # freq too low, even if no missings
        trigger_who___1==1 & trigger_who___2==1,"Participant and algorithm",
        trigger_who___1==1 & trigger_who___2==0,"Participant",
        trigger_who___1==0 & trigger_who___2==1,"Algorithm",
        ema_sampling___3==1 & psum(gvr(.,"trigger_who___[1-3]"))==0,
        NA_character_
      ) %>% 
        factor(
          levels=c("Participant","Algorithm","Participant and algorithm")
        ) %>% 
        setLabels(attrn="feature","design"),
      
      prevent_miss_n = psum(gvr(.,"prevent_miss_type___[1-5]")) %>%
        setLabels("N° of momentary missingness prevention strategies") %>%
        setLabels(attrn="feature","design"),
      
      # any_prevent_miss = (prevent_miss_n > 0) %>% 
      #   setLabels(attrn="feature","design"),
      
      prevent_miss_type = nif(
        prevent_miss_n > 1, "multiple", 
        prevent_miss_type___1==1, "acctive snooze", 
        prevent_miss_type___2==1, "passive reminder", 
        prevent_miss_type___3==1, "alarm instructions", 
        prevent_miss_type___4==1, "individual inquiry", 
        prevent_miss_type___5==1, "else", 
        prevent_miss_n == 0, NA_character_
      ) %>% 
        setLabels(attrn="feature","design"),
      
      snooze_max_dur = snooze_max_dur %>% 
        lubridate::hms() %>% 
        lubridate::period_to_seconds() %>% 
        copyAttrib(snooze_max_dur) %>% 
        setLabels(attrn="feature","design"),
      
      no_snooze = no_snooze___1 %>% 
        setLabels("Unlimited response window") %>% 
        setLabels(c("limited or not reported"=0,"explicitely unlimited"=1)) %>% 
        setLabels(attrn="feature","design")
    )
  
}
