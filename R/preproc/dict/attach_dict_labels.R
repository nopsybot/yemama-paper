attach_dict_labels <- function(extracts,dict) {
  
extracts %>% 
    rrapply(
      condition = \(x,.xname) .xname %in% dict[!is.na(var_labs),var],
      f = \(x,.xname) collapse::setLabels(
        x, 
        value = dict[var==.xname,var_labs],
        attrn = "label"
      ) 
    ) %>%
    rrapply(
      condition = \(x,.xname) {
        .xname %in% dict[sapply(dict$choice_defs,\(x) all(!is.na(x))),var]
      },
      f = \(x,.xname) collapse::setLabels(
        x,
        value = unlist(dict[var==.xname,choice_defs]), 
        attrn = "labels"
      )
    ) %>% 
    # selectively convert labelled numerics to r-native factors  
    # by excluding yes-no variables, we don't want to be factors
    rrapply(
      condition = \(x,.xname) suppressWarnings({
        !is.null(attributes(x)$labels) & # labelled variables
          sum(as.numeric(attributes(x)$labels),na.rm = TRUE)>1 # but not the 01
          # !str_detect(.xname,"reported") # but not the "[outcome]_reported"
      }),
      f = sjlabelled::as_label
    )
  
}
