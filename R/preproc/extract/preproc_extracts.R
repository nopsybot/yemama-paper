preproc_extracts <- function(extracts,dict) {
  
  extracts %>% 
    
    attach_dict_labels(dict = dict) %>% 
    
    preproc_x02_general_info() %>% 
    
    preproc_x03_sample_features() %>% 
    
    preproc_x05_recruitment() %>% 
    
    preproc_x06_ema_design() %>% 
    
    preproc_x07_questionnaire() %>% 
    
    preproc_x08_technical() %>% 
    
    preproc_x09_ass_targets() %>% 
    
    preproc_x10_compliance() %>% 
    preproc_compl_cutoff_type() %T>% 
    check_outcome_availability() %>%
    
    preproc_x11_incentives() %>% 
    
    fselect_longlist()
    
}
