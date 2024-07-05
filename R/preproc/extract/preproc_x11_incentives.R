preproc_x11_incentives <- function(extracts) {
  
  extracts %>% 
    
    preproc_incent_plan() %>% 
    
    preproc_incent_type() %>% 
    
    preproc_incent_val() %>% 
    
    preproc_training() %>% 
    
    preproc_participant_care() %>% 
    
    preproc_parent_involvement()
}