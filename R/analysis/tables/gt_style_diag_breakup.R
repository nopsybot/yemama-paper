gt_style_diag_breakup <- function(diag_breakup) {
  
  diag_breakup %>% 
    gt::gt(
      groupname_col = "clin_sompsy.mix",
      rowname_col = "clin_diag_sompsy_groups"
    ) %>% 
    gt::sub_missing(missing_text = "-") %>% 
    gt::tab_stubhead(vlabels(diag_breakup$clin_diag_sompsy_groups)) %>% 
    gt::tab_style(
        locations = cells_stub(),
        style = list(
          gt::cell_borders(sides = "right",style = "hidden"),
          gt::cell_text(indent = gt::px(20))
        )
    ) %>% 
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(weight = "bold"),
        gt::cell_borders(sides = "bottom",style = "hidden")
      )
    ) %>% 
    gt::summary_rows(
      groups = 1:2,
      columns = starts_with("N_"), fns = list(label = "Sum",fn = "sum"),
      missing_text = ""
    ) %>% 
    gt::grand_summary_rows(
      columns = starts_with("N_"),
      fns = list(label = "Total",fn = "sum"),
      missing_text = ""
    ) %>% 
    gt::tab_style(
      locations = list(
        cells_summary(groups = 1:2),
        cells_stub_summary(groups = 1:2),
        cells_grand_summary(),
        cells_stub_grand_summary()
      ),
      style = list(
        cell_text(style = "italic"),
        cell_borders(sides = "right",style = "hidden")
      )
    ) %>% 
    gt::tab_style(
      locations = list(cells_grand_summary(),cells_stub_grand_summary()),
      style = cell_text(weight = "bold")
    ) %>% 
    gt::tab_header(
      title = "Diagnostic breakup according to clinical domain and clinical status."
    ) %>% 
    gt::tab_options(
      column_labels.font.weight = "bold",
      table_body.hlines.style = "hidden",
      # heading.border.top.style = "hidden",
      heading.align = "left",
      heading.title.font.size = 16
    )
  
}