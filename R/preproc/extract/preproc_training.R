preproc_training <- function(extracts) {
  
  extracts %>% 
    mtt(
      feedback_vs_nonrep = nif(
        feedback==1,"feedback",
        feedback==0 | is.na(feedback),"no or not reported"
      ) %>% 
        factor(levels = c("no or not reported","feedback")) %>% 
        setLabels("Providing feedback") %>%  
        setLabels("Providing feedback:",attrn = "tick_title") %>%  
        set_attr("level_abbr",setNames(c("no","yes"),levels(.))) %>% 
        setLabels(attrn="feature","design"),
      
      ema_train_vs_nonrep = nif(
        ema_train_yn==0 | is.na(ema_train_yn),"no or not reported",
        ema_train_yn==1,"yes"
      ) %>% factor(
        levels = c("no or not reported","yes")
      ) %>% 
        setLabels("Providing EMA training") %>% 
        setLabels("EMA training:",attrn = "tick_title") %>%  
        set_attr("level_abbr",setNames(c("no","yes"),levels(.))) %>% 
        setLabels(attrn="feature","design"),
      
      ema_train_type_n = psum(gvr(.,"ema_train_type___[1-3]")) %>% 
        setLabels("EMA training") %>% 
        setLabels(attrn="feature","design"),
      
      ema_train_type = nif(
        ema_train_type_n>1,"multiple",
        ema_train_type___1==1,"individually face-to-face",
        ema_train_type___2==1,"grouped sessions",
        ema_train_type___3==1,"remotely (print or online)",
        ema_train_type_n==0,NA_character_
      ) %>% 
        factor(
          levels = c("individually face-to-face","grouped sessions",
                     "remotely (print or online)","multiple")
        ) %>% 
        setLabels("EMA training") %>% 
        setLabels(attrn="feature","design")
    )
  
}
