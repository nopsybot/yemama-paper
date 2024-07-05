preproc_x07_questionnaire <- function(extracts) {
  
  extracts %>% 
    mtt(
      item_n_vary = factor(
        item_n_vary,
        levels = 0:1,
        labels = c("stable","varying")
        ) %>% 
        setLabels(attrn="feature","design"),
      
      item_n_vary_rules = nif(
        psum(gvr(.,"item_n_vary_rules___[1-4]"))>1,"multiple",
        item_n_vary_rules___1==1, "time-of-day/day-of-week",
        item_n_vary_rules___1==1, "skip-logic",
        item_n_vary_rules___1==1, "personalized",
        item_n_vary_rules___1==1, "planned missingness",
        psum(gvr(.,"item_n_vary_rules___[1-4]"))==0,NA_character_
      ) %>% 
        factor(
          levels = c("time-of-day/day-of-week","skip-logic","personalized",
                     "planned missingness","multiple")
        ) %>% 
        setLabels("Varying item schemes") %>% 
        setLabels(attrn="feature","design"),
      
      item_n = item_n %>% 
        setLabels("Number of items") %>% 
        setLabels(attrn="feature","design"),
      
      illustrated = media_augmented %>% as.logical() %>% 
        iif("illutstrated","text-only") %>% 
        factor(levels = c("illutstrated","text-only")) %>% 
        setLabels("Visual enhancement") %>% 
        setLabels(attrn="feature","design"),
      
      gamificated = gamification %>% as.logical() %>% 
        iif("gamificated","not gamificated") %>% 
        factor(levels = c("gamificated","not gamificated")) %>% 
        setLabels("Use of gamification") %>% 
        setLabels(attrn="feature","design"),
      
      enhancement = nif(
        as.logical(gamification), "gamification",
        as.logical(media_augmented), "visual elements",
        default = "none or not reported"
      ) %>% 
        factor(
          levels = c(
            "gamification", "visual elements", "none or not reported"
          )
        ) %>% 
        setLabels("Enhancement of EMA material") %>% 
        setLabels(attrn="feature","design"),
      
      any_augmented_vs_nonrep = nif(
        media_augmented | gamification,"visual enhancement or gamification",
        default = "no or not reported"
      ) %>% 
        factor( 
          levels = c("no or not reported","visual enhancement or gamification")
        ) %>% 
        setLabels("Enhancement of EMA material (any vs. nonrep)") %>% 
        setLabels("Enhanced material:",attrn="tick_title") %>% 
        set_attr("level_abbr",setNames(c("NNR","VG"),levels(.))) %>% 
        setLabels(
          attributes(.) %>% glue_data("{level_abbr} = {levels}"),
          attrn="abbr_key"
        ) %>% 
        setLabels(attrn="feature","design"),
      
      resp_dur_m_sec = resp_dur_m_time %>% 
        lubridate::hms() %>% 
        lubridate::period_to_seconds() %>% 
        setLabels("Response duration (sec)") %>% 
        setLabels(attrn="feature","design")
    )
    
}