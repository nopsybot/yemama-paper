gt_style_binned_desc <- function(study_grp_bins_desc,N){
  
  gt_dat <- study_grp_bins_desc %>% 
    split(by = "var") %>% 
    lapply(
      \(x) rbindlist(
        list(
          label = data.table(row_label = unique(x$label), 
                             var = unique(x$var),
                             feature = unique(x$feature)),
          cat = x %>% mtt(row_label = cat)
        ),
        fill = TRUE,
        idcol = "row_type"
      ) %>% 
        mtt(row_num = seq_along(var))
    ) %>% 
    rbindlist() %>% 
    mtt(
      row_type = factor(row_type,levels = c("feature","label","cat")),
      feature = paste(feature,"characteristics")
    ) %>% 
    split(by = "feature") %>% 
    lapply(
      \(x) rbindlist(
        list(
          feature = data.table(row_type = "feature",
                               row_label = unique(x$feature) %>% 
                                 str_to_sentence(),
                               feature = unique(x$feature),
                               row_num = 0),
          var = x
        ),
        fill = TRUE)
    ) %>% 
    rbindlist() %>% 
    mtt(PLACEHOLDER = NA) %>% 
    colorder(prc_study,PLACEHOLDER,pos = "after")
  
  
  gt_dat %>%   
    gt::gt() %>% 
    gt::tab_spanner(label = "Study level",columns = ends_with("study")) %>% 
    gt::tab_spanner(label = "Group level",columns = ends_with("group")) %>% 
    gt::cols_label(
      row_label = "Characteristic",
      PLACEHOLDER = ""
    ) %>%
    gt::cols_width(PLACEHOLDER ~ gt::px(20)) %>% 
    gt::cols_label(
      starts_with("N") ~ "k",
      starts_with("prc") ~ "%"
    ) %>% 
    
    gt::tab_style(
      locations = gt::cells_body(
        columns = row_label, rows = row_type  %in%  c("feature","label")
      ),
      style = list(gt::cell_text(weight = "bold"))
    ) %>% 
    gt::tab_style(
      locations = gt::cells_body(
        columns = row_label, rows = row_type == "label"
      ),
      style = list(gt::cell_text(indent = gt::pct(5)))
    ) %>% 
    gt::tab_style(
      locations = gt::cells_body(
        columns = row_label, rows = row_type == "cat"
      ),
      style = list(gt::cell_text(indent = gt::pct(10)))
    ) %>% 
    gt::tab_style(
      locations = gt::cells_body(
        columns = row_label,
        rows = row_label == "not reported"
      ),
      style = list(gt::cell_text(style = "italic"))
    ) %>% 
    gt::cols_hide(c(var,row_type,feature,label,row_num,cat)) %>% 
    gt::sub_missing(missing_text = "") %>% 
    gt::tab_header(
      title = glue::glue(
        "Table 1. Descriptive statistics of the included of studies (N={N})"
      )
    ) %>% 
    gt::tab_style(
      locations = gt::cells_body(rows = 1:(nrow(gt_dat)-1)),
      gt::cell_borders(sides = "bottom",style = "hidden")
    ) %>% 
    gt::tab_style(
      locations = gt::cells_title(),
      style = list(
        gt::cell_text(size = "medium"),
        gt::cell_borders(sides="top",style = "hidden")
      )
    )
  
}




