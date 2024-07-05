preproc_x09_ass_targets <- function(extracts) {
  
  extracts %>% 
    mtt(
      target_main_n = psum(gvr(.,"target_main___[1-9]")) %>% 
        setLabels(attrn="feature","design"),
      
      target_health_n = psum(gvr(.,"target_health___[1-7]")) %>% 
        setLabels(attrn="feature","design"),
      
      target_detail_n = target_main_n - target_main___2 + target_health_n %>% 
        setLabels("N° of assessment topics") %>% 
        setLabels(attrn="feature","design"),
      
      symptom_ema = target_main___3 %>% 
        as.logical() %>% 
        iif("yes","no") %>% 
        factor(levels = c("no","yes")) %>% 
        setLabels("EMA of symptoms") %>% 
        setLabels("EMA of symptoms:",attrn="tick_title") %>% 
        setLabels(attrn="feature","design")
    )
  
}