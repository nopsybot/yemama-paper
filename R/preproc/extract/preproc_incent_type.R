preproc_incent_type <- function(extracts) {
  
  extracts %>% 
    mtt(
      incent_type_n = psum(gvr(.,"incent_type___[1-5]")) %>% 
        setLabels(attrn="feature","design"),
      
      incent_monetary = psum(gvr(.,"incent_type___[12]"))>0 %>% 
        setLabels(attrn="feature","design"),
      
      incent_nonmonetary = psum(gvr(.,"incent_type___[3-5]"))>0 %>% 
        setLabels(attrn="feature","design"),
      
      incent_type = nif(
        psum(gvr(.,"incent_type___[12]"))>0 &
          psum(gvr(.,"incent_type___[3-5]"))>0,"both",
        psum(gvr(.,"incent_type___[12]"))>0,"monetary",
        psum(gvr(.,"incent_type___[3-5]"))>0,"non-monetary",
        incent_type_n>0,NA_character_
      ) %>% 
        factor(
          levels=c("monetary","non-monetary","both")
        ) %>% 
        setLabels("Type of incentivization") %>% 
        setLabels(
          "Monetary incentivization consists of cash and gift cards, and non-monetary forms consist of receiving the study device, course credit or other.",
          attrn="note"
        ) %>% 
        setLabels(attrn="feature","design")
    )
  
}
