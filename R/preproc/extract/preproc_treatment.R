preproc_treatment <- function(extracts) {
  extracts %>% 
    mtt(
      treat_status.mix = nif(
        psum(gvr(.,"treat_status___[1-4]"))>1,"mixed",
        treat_status___1==1,"receiving treatment",
        treat_status___2==1,"treatment seeking",
        treat_status___3==1,"waitlist",
        treat_status___4==1,"posttreatment",
        default = NA_character_
      ) %>%
        factor(
          levels = c("treatment seeking","waitlist","receiving treatment",
                     "posttreatment","mixed")
        ) %>%
        setLabels("Treatment status") %>%
        setLabels(attrn="feature","sample"),
      # 
      # treat_status.receive_vs_any_nomix = nif(
      #   treat_status___1 & (treat_status___2|treat_status___3), NA_character_,
      #   treat_status___1==1,"receiving treatment",
      #   treat_status___1==0,"pre/post/no treatment",
      #   default = NA_character_
      # ) %>% 
      #   factor(
      #     levels = c("receiving treatment","pre/post/no treatment")
      #   ) %>% 
      #   setLabels("Treatment status (receive vs. any - nomix)") %>% 
      #   setLabels("mixed samples were dropped (k<=8)",attrn = "note") %>% 
      #   setLabels(attrn="feature","sample"),
      
      treat_status.any_vs_na = nif(
        psum(gvr(.,"treat_status___[1-3]"))>0, "pre, peri, or post-treatment",
        psum(gvr(.,"treat_status___[1-3]"))==0, "unrelated"
      ) %>% 
        factor(
          levels = c("unrelated","pre, peri, or post-treatment")
        ) %>% 
        setLabels("EMA in treatment (any vs. no)") %>% 
        setLabels("EMA in treatment:",attrn="tick_title") %>% 
        set_attr("level_abbr",setNames(c("no","PPP"),levels(.))) %>% 
        setLabels(
          attributes(.) %>% glue_data("{level_abbr} = {levels}"),
          attrn="abbr_key"
        ) %>%
        setLabels(attrn="feature","sample"),
      
      treat_setting.mix = nif(
        psum(gvr(.,"treat_status___[1-4]"))>1,"mixed",
        treat_setting___1==1,"inpatient",
        treat_setting___2==1,"outpatient",
        treat_setting___3==1,"remote",
        treat_setting___4==1,"prevention",
        default = NA_character_
      ) %>% 
        factor(
          levels = c("inpatient","outpatient","remote","prevention","mixed")
        ) %>% 
        setLabels("Teatment setting") %>% 
        setLabels(attrn="feature","sample"),
      
      treat_setting.inout_nomix = nif(
        psum(gvr(.,"treat_status___[1-4]"))>1, NA_character_,
        treat_setting___1==1,"inpatient",
        treat_setting___2==1,"outpatient",
        default = NA_character_
      ) %>%
        factor(
          levels = c("inpatient","outpatient")
        ) %>%
        setLabels("Teatment setting (in vs. out - no mix)") %>%
        setLabels("Setting:",attrn="tick_title") %>%
        set_attr("level_abbr",setNames(c("IN","OUT"),levels(.))) %>%
        setLabels(
          attributes(.) %>% glue_data("{level_abbr} = {levels}"),
          attrn="abbr_key"
        ) %>%
        setLabels(
          "remote (k<=2), preventive (k=0), and mixed (k<=8) treatment settings were dropped.",
          attrn = "note"
        ) %>%
        setLabels(attrn="feature","sample")
    )
  
}
