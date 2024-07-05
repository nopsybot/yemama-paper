preproc_x03_sample_features <- function(extracts) {
  
  extracts %>% 
    preproc_age() %>% 
    mtt(
      female.prc = prc_female %>%  # fine, symmetric distribution
        setLabels("Gender (% girls)") %>% 
        setLabels(attrn="feature","sample"),
      nonwhite.prc = nonwhite_prc %>%  # fine, flat distribution
        setLabels("Ethnicity (% non-white)") %>% 
        setLabels(attrn="feature","sample")
      # ignore income, urbanicity, and ses because too rarely reported
    ) %>% 
    preproc_clin_sample() %>% 
    preproc_treatment()
    
}
