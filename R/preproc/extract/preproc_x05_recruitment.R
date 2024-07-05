preproc_x05_recruitment <- function(extracts) {
  
  extracts %>% 
    mtt(
      acceptance_n = acceptance_n %>% setLabels("N° of invited people"),
      recruit_cntxt_n = psum(gvr(.,"recruit_cntxt___\\d")),
      recruit_cntxt = nif(
        psum(gvr(.,"recruit_cntxt___[23]"))==2,
        "clinical and school combined",
        recruit_cntxt___2==1 & recruit_cntxt___3==0,
        "clinical settings involved",
        recruit_cntxt___3==1 & recruit_cntxt___2==0,
        "school settings involved",
        recruit_cntxt___5==1 & psum(gvr(.,"recruit_cntxt___[23]"))==0,
        "existing participant pool involved",
        psum(gvr(.,"recruit_cntxt___[14]"))>=1,
        "public/community/online setting",
        recruit_cntxt___6==1,
        "other recruitment setting",
        recruit_cntxt_n == 0,
        NA_character_
      ) %>% 
    setLabels(attrn="feature","design"),
    retention_n = retention_n %>% 
      setLabels("Sample size (enrollment)") %>% 
      setLabels(attrn = "feature","general")
    ) 
  
}
