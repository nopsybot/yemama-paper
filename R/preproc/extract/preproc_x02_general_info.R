preproc_x02_general_info <- function(extracts) {
  
  extracts %>% 
    mtt(
      any_outcome = compliance_reported|retention_reported|acceptance_reported,
      authyear = str_replace(auth_year,"_"," (") %>% paste0(")"),
      year = year %>% setLabels("Year of Publication"),
      grplab = record_id %>% 
        str_remove("_[:lower:]{2}$") %>% 
        str_extract("(?<=_)[:lower:]+$") %>% 
        str_to_upper() %>% 
        str_replace_na(""),
      
      # TODO: list column for 1st,2nd,3rd country
      sample_country1 = sample_country1 %>% 
        setLabels("Country") %>% 
        setLabels(attrn = "feature","design"),
      
      n_groups = fnobs(record_id,g = gv(.,"primary_record_id"),TRA = "fill") %>% 
        setLabels("Number of groups") %>% 
        setLabels(attrn = "feature","general")
    ) %>% 
    gby(study_id) %>%
    mtt(study_id_dep = {
      if(all(!is.na(group_clustered) & group_clustered==1))
        study_id
      else if (length(study_id)>2)
        paste0(study_id,".",seq_along(study_id))
      else
        study_id
    }) %>%
    fungroup()
}