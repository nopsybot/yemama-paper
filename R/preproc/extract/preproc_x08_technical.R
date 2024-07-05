preproc_x08_technical <- function(extracts) {
  
  extracts %>%
    mtt(
      dev_type_n = psum(gvr(.,"dev_type___[1-5]")) %>% 
        setLabels(attrn="feature","design"),
      dev_type = nif(
        dev_type___6==1,"cellphone",
        dev_type_n>1, "multiple",
        dev_type___1==1,"smartphone",
        dev_type___2==1,"handheld device",
        dev_type___5==1,"else",
        dev_type_n==0,NA_character_
      ) %>% factor(
        levels=c("smartphone","handheld device","cellphone","else","multiple")
      ) %>% 
        forcats::fct_na_level_to_value(extra_levels="else") %>% # drop empty else
        setLabels("Type of EMA delivering device") %>% 
        setLabels(attrn="feature","design"),
      
      platform_type = nif(
        platform_type___1==1,"installed app",
        platform_type___2==1,"remote web app",
        platform_type___3==1,"EMA only device", # no single study
        platform_type___5==1,"Short-messaging",
        platform_type___6==1,"Automated voice interface",
        platform_type___4==1,"else", # too low freq
        psum(gvr(.,"platform_type___[1-4]"))==0,NA_character_ # any else NA
      ) %>% 
        factor(
          levels = c("installed app","remote web app","Short-messaging",
                     "Automated voice interface","EMA only device","else")
        ) %>% 
        # drop rare sms and else
        forcats::fct_na_level_to_value(
          extra_levels = c("else","Short-messaging",
                           "Automated voice interface","EMA only device")
        ) %>% 
        setLabels("Type of EMA software deployment") %>% 
        setLabels(attrn="feature","design"),
        
      dev_own = nif(
          psum(gvr(.,"dev_own___[1-3]"))>1,"multiple",
          dev_own___1==1,"participants",
          # dev_own___2==1,"parents", # only one study
          dev_own___3==1,"study device"#,
          # psum(gvr(.,"dev_own___[1-3]"))==0,NA_character_, # any else
        ) %>% 
        factor(
          levels = c("multiple","participants","study device")
        ) %>% 
        setLabels("Device ownership") %>% 
        setLabels(
          "No studies with parent-provided devices (k=0)",attrn="note"
        ) %>% 
        setLabels(attrn="feature","design"),
      
      obj_addon = obj_addon %>% 
        as.logical() %>% 
        iif("add-on sensors","no add-on") %>% 
        factor(levels = c("no add-on","add-on sensors")) %>% 
        setLabels("Parallel sensor recordings") %>%
        set_attr("tick_title","Add-on sensors:") %>% 
        set_attr("level_abbr",setNames(c("no","yes"),levels(.))) %>% 
        setLabels(attrn="feature","design")
    )
}
