preproc_compl_cutoff_type <- function(extracts) {
  
  extracts %>% 
    mtt(
      compl_cutoff_type = nif(
        
        psum(gvr(.,"compl_cutoff_type___[3-5]"))>0,
        "only inflated or content providers",
        
        compl_cutoff_type___1==1,"only retained",
        
        psum(gvr(.,"compl_cutoff_type___"))==0 |
          (psum(gvr(.,"compl_cutoff_type___"))==1 & compl_cutoff_type___2==1),
        "all who responded",
        
        default = NA_character_
      ) %>% factor(
        levels = c(
          "all who responded",
          "only retained",
          "only inflated or content providers"
        )
      ),
      compl_cutoff_strict_bin = nif(
        psum(gvr(.,"compl_cutoff_type___[34]"))>0, "inflated compliance",
        compl_cutoff_use==0, "raw compliance",
        default = NA_character_
      ) %>% 
        factor( levels = c("raw compliance","inflated compliance") ) %>% 
        setLabels("Inflated compliance rate"),
        setLabels(attrn="feature","outcome"),
      
      # FIXME!!! results in NULL or NA check here
      compl_cutoff = nif(
        compl_cutoff_unit=="% percent",compl_cutoff/100,
        compl_cutoff_unit=="n° responses",compl_cutoff/prompt_tot_n
      ),
      compl_cutoff_imp0 = iif(
        compliance_reported==TRUE & compl_cutoff_use==0, 0, compl_cutoff
      ) %>% 
        setLabels(paste(attributes(compl_cutoff)$label,"(0 for raw compliance)"))
    )
  
}
