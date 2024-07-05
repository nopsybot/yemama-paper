preproc_manifest <- function(
    data
) {
  # TODO: how to splice such a list of arguments into nif?
  # extr_vars = paste0("extractor___",c(1:3,6:8))
  # extr_names = c("Konstantin","Assel","Sophie","Vanisha","Talia","Fabienne")
  # nif_args <- matrix(
  #   c(paste0(extr_vars,"==1"),extr_names),
  #   nrow = 2,
  #   byrow = TRUE
  # )

  data %>% 
    collapse::fselect(
      record_id:primary_record_id, 
      incl, prim, final, 
      redcap_event_name:x02_study_identification_complete
    ) %>% 
    collapse::fsubset(
      redcap_event_name=="extraction_arm_1" 
      & (current_group==1 | is.na(current_group))
    ) %>% 
    collapse::mtt(
      extractor = kit::nif(
        extractor___1==1,"Konstantin",
        extractor___2==1,"Assel",
        extractor___3==1,"Sophie",
        extractor___6==1,"Vanisha",
        extractor___7==1,"Talia",
        extractor___8==1,"Fabienne"
      ),
      n_extractors = kit::psum(
        extractor___1, extractor___2, extractor___3,
        extractor___6, extractor___7, extractor___8
      )
    )

}
# combine variables and extractor names into flat alternating list