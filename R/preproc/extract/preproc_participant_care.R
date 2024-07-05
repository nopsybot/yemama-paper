preproc_participant_care <- function(extracts) {
  extracts %>% 
    mtt(
      part_care_type_n = psum(gvr(.,"part_care_type___[1-6]")) %>% 
        setLabels(attrn="feature","design"),
      
      part_care_vs_nonrep = iif(
        part_care_type_n>0,"participant care","not reported"
        ) %>% 
        factor("not reported","participant care") %>% 
        setLabels("Engagement in participant care") %>% 
        setLabels(attrn="feature","design"),
      
      part_care_anycontact = nif(
        psum(gvr(.,"part_care_type___[2-6]"))>0,"contacting participants",
        part_care_type___1==1,"remaining available troughout study"
      ) %>% 
        factor(
          levels = c("remaining available troughout study",
                     "contacting participants")
        ) %>% 
        setLabels("Contacting participants during study") %>% 
        setLabels(attrn="feature","design"),
      
      part_care_minact = nif(
        psum(gvr(.,"part_care_type___[2356]"))>0,"active contact",
        psum(gvr(.,"part_care_type___[14]"))>0,"minimal contact",
        part_care = part_care_type_n==0,NA_character_
      ) %>% 
        factor(
          levels = c("minimal contact","active contact")
        ) %>% 
        setLabels("Intensity of participant care") %>% 
        setLabels(
          "Active contacting consists of research staff providing mid-term meetings, or individual contacting via phone or via mail. Minimal contact is defined as explicit instructions to reach out to research staff or automated messages.",
          attrn="note"
        ) %>% 
        set_attr("tick_title","Participant care:") %>% 
        set_attr("level_abbr",setNames(c("MIN","ACT"),levels(.))) %>% 
        setLabels(
          attributes(.) %>% glue_data("{level_abbr} = {levels}"),
          attrn="abbr_key"
        ) %>% 
        setLabels(attrn="feature","design"),
      
      miss_inquiry_vs_nonrep = nif(
        miss_inquiry==0 | is.na(miss_inquiry), "no or not reported",
        miss_inquiry==1, "yes"
      ) %>% 
        factor(levels = c("no or not reported","yes")) %>% 
        setLabels("Contacting participants during study") %>% 
        setLabels(attrn="feature","design")
    )
}
