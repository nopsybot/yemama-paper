preproc_clin_sample <- function(extracts) {
  
  extracts %>% 
    mtt(
      #### 1. Clinical sample categories ####
      clin_sample.mix = nif(
        psum(gvr(.,"clin_sample___\\d"))>1,"mixed",
        clin_sample___1==1,"clinical",
        clin_sample___2==1,"at risk",
        clin_sample___3==1,"healthy",
        clin_sample___4==1,"convenience",
        default = NA_character_
      ) %>% factor(
        levels = c("clinical","at risk","healthy","convenience","mixed")
      ) %>%
        setLabels("Clinical status") %>%
        setLabels(attrn="feature","sample"),
      
      # clin_sample.riskna = nif(
      #   psum(gvr(.,"clin_sample___[134]"))>1,"mixed",
      #   clin_sample___1==1,"clinical",
      #   clin_sample___2==1,NA_character_,
      #   clin_sample___3==1,"healthy",
      #   clin_sample___4==1,"convenience",
      #   default = NA_character_
      # ) %>% factor(
      #   levels = c("clinical","healthy","convenience","mixed")
      # ) %>% 
      #   setLabels("Clinical status (no risk)") %>% 
      #   setLabels("At-risk samples were dropped (k<=6)",attrn="note") %>% 
      #   setLabels(attrn="feature","sample"),
      # 
      # clin_sample.riskmixna = nif(
      #   psum(gvr(.,"clin_sample___[134]"))>1,NA_character_,
      #   clin_sample___1==1,"clinical",
      #   clin_sample___2==1,NA_character_,
      #   clin_sample___3==1,"healthy",
      #   clin_sample___4==1,"convenience",
      #   default = NA_character_
      # ) %>% factor(
      #   levels = c("clinical","healthy","convenience")
      # ) %>% 
      #   setLabels("Clinical status (no risk - no mix)") %>% 
      #   setLabels(
      #     "At-risk (k<=6) and mixed (k<=4) samples were dropped",
      #     attrn="note") %>% 
      #   setLabels(attrn="feature","sample"),
      # 
      # clin_sample.riskmixna_nonclin = nif(
      #   psum(clin_sample___1,(clin_sample___3|clin_sample___4))>1,NA_character_,
      #   clin_sample___1==1, "clinical",
      #   clin_sample___2==1,NA_character_,
      #   clin_sample___3|clin_sample___4, "healthy or convenience",
      #   default = NA_character_
      # ) %>% factor(
      #   levels = c("clinical","healthy or convenience")
      # ) %>% 
      #   setLabels("Clinical status (no risk - no mix - nonclin)") %>% 
      #   setLabels(
      #     attrn="note",
      #     paste(
      #       attributes(clin_sample.riskmixna)$note,
      #       "Healthy and convenience samples were collapsed into one category."
      #     )
      #   ) %>% 
      #   setLabels(attrn="feature","sample"),
      # clin_sample.mixna_anyclin = nif(
      #     psum(
      #       (clin_sample___1|clin_sample___2),
      #       clin_sample___3,clin_sample___4
      #     )>1,NA_character_,
      #     clin_sample___1|clin_sample___2, "clinical or at risk",
      #     clin_sample___3==1, "healthy",
      #     clin_sample___4==1, "convenience",
      #     default = NA_character_
      #   ) %>% 
      #   factor(
      #     levels = c("clinical or at risk","healthy","convenience")
      #   ) %>% 
      #   setLabels("Clinical status (clin+risk - no mix)") %>% 
      #   setLabels(
      #     attrn="note",
      #     paste(
      #       attributes(clin_sample.riskmixna)$note,
      #       "Clinical and at risk samples were collapsed into one category."
      #     )
      #   ) %>% 
      #   setLabels(attrn="feature","sample"),
      
      clin_sample.strict_bin = nif(
          psum((clin_sample___1|clin_sample___3))>1,NA_character_,
          clin_sample___1==1, "clinical",
          clin_sample___3==1, "healthy controls",
          default = NA_character_
        ) %>% 
        factor(levels = c("clinical","healthy controls")) %>% 
        setLabels("Clinical status (strictly binary)") %>% 
        setLabels(
          attrn="note","At risk, convenience, and mixed samples were dropped."
        ) %>% 
        setLabels("Clinical status:",attrn="tick_title") %>%
        set_attr("level_abbr",setNames(c("C","HC"),levels(.))) %>% 
        setLabels(
          attributes(.) %>% glue::glue_data("{level_abbr} = {levels}"),
          attrn="abbr_key"
        ) %>%
        setLabels(attrn="feature","sample"),
      
      # clin_sample.inclu_bin = nif(
      #     psum(
      #       (clin_sample___1|clin_sample___2),(clin_sample___3|clin_sample___4)
      #     )>1,NA_character_,
      #     clin_sample___1|clin_sample___2, "clinical or at risk",
      #     clin_sample___3|clin_sample___4==1, "healthy or convenience",
      #     default = NA_character_
      #   ) %>% 
      #   factor(
      #     levels = c("clinical or at risk","healthy or convenience")
      #   ) %>% 
      #   setLabels("Clinical status (most inclusive)") %>% 
      #   setLabels(
      #     attrn="note",
      #     "Clinical and at risk, and healthy and convenience samples were collapsed into one category. Mixed clinical-healthy samples were dropped (K<=7)"
      #   ) %>% 
      #   setLabels(attrn="feature","sample"),
        
      
      clin_sample.bin_strict = clin_sample.mix %>% 
        forcats::fct_na_level_to_value(
          extra_levels = c("mixed","at risk","convenience")
        ) %>% 
        setLabels("Clinical status (strict binary)") %>% 
        setLabels(
          "At risk, convenience, and mixed samples were dropped",
          attrn = "note"
        ) %>% 
        setLabels(attrn="feature","sample"),
      
      clin_sompsy.mix = nif(
        psum(clin_domain___1,clin_domain___2)==2,"mixed",
        clin_domain___2==1,"psychiatric",
        clin_domain___1==1,"somatic",
        default = NA_character_
      ) %>% factor(
        levels = c("somatic","psychiatric","mixed")
      ) %>%
        setLabels("Diagnostic groups (psy-som-mix)") %>%
        setLabels(attrn="feature","sample"),
      
      clin_sompsy.mixna = clin_sompsy.mix %>% 
        forcats::fct_na_level_to_value(extra_levels = "mixed") %>% 
        setLabels("Diagnostic groups (binary - no mix)") %>% 
        setLabels("Diagnosis:",attrn = "tick_title") %>% 
        set_attr("level_abbr",setNames(c("SOM","PSY"),levels(.))) %>% 
        setLabels(
          attributes(.) %>% glue_data("{level_abbr} = {levels}"),
          attrn="abbr_key"
        ) %>%
        setLabels("Mixed diagnostic samples ignored (k<=8)",attrn = "note") %>% 
        setLabels(attrn="feature","sample"),
      
      clin_diag_groups.lump = nif(
        clin_diag_group %in% 1:2,"Substance related disorders (Alcohol, Nicotine, and others)",
        clin_diag_group %in% 6:7,"Sucidal thoughts and Behaviors and Non-suicidal Self-Injury",
        clin_diag_group %in% 8:9,"Anxiety and stress related disorders",
        default = as.character(clin_diag_group)
      ) %>% 
        setLabels("Diagnostic groups") %>% 
        setLabels(
          "Substances and alcohol, STB and NSSRIs, and Anxiety and Stress-related disorders were grouped due to low cell frequencies.",
          attrn="note"
        ) %>% 
        setLabels(attrn="feature","sample")
    )
}