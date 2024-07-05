preproc_parent_involvement <- function(extracts) {
  
  extracts %>% 
    mtt(
      parent_involvement = nif(
        parent_involvement___1==1, "parallel parent EMA",
        parent_involvement___2==1, "parents assist child EMA",
        parent_involvement___3==1, "some parent reports",
        default = "no involvement"
      ) %>% 
        factor(
          levels = c("no involvement","some parent reports",
                     "parents assist child EMA","parallel parent EMA"
          )
        ) %>% 
        setLabels("Parent involvement") %>% 
        set_attr("level_abbr",setNames(c("no","PR","PA","PE"),levels(.))) %>% 
        setLabels(
          attributes(.) %>% glue_data("{level_abbr} = {levels}"),
          attrn="abbr_key"
        ) %>% 
        set_attr("level_abbr",setNames(c("no","Parent involvement: PR","PA","PE"),levels(.))) %>% 
        setLabels(attrn="feature","design")
    )
  
}