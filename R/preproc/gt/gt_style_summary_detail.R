gt_style_summary_detail <- function(sum_dt) {
  
  sum_gt <- sum_dt %>% 
    gt::gt(
      groupname_col = "label",
      rowname_col = "count_cat"
    ) %>% 
    # Style column labels ####
    gt::cols_label_with(
      columns = ends_with("reported"),
      fn = \(x) str_remove(x,"_reported") %>% 
        str_to_sentence() %>% 
        paste0(
          "<br>(*k* = ",
          sum_dt[is.na(cat), max(fsum(x,g=var),na.rm=TRUE), env=list(x=x)],
          ")"
        ) %>% gt::md()
    ) %>% 
    gt::tab_style(
      style = gt::cell_text(weight=700,align="center"),
      locations = gt::cells_column_labels()
    ) %>% 
    
    # Indentation ####
    gt::tab_stub_indent(rows = is.na(cat), indent = 2) %>% 
    gt::tab_stub_indent(rows = !is.na(cat), indent = 5) %>% 
    
    # Style grouped rows ####
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(weight = 700),
        gt::cell_fill("skyblue2")
      )
    ) %>% 
    gt::tab_style(
      locations = gt::cells_body(
        columns = contains("reported"), rows = count_cat=="Total"
      ),
      style = list(gt::cell_text(weight = 550),gt::cell_fill("grey80"))
    ) %>% 
    gt::tab_style(
      locations = gt::cells_stub(rows = count_cat=="Total"),
      style = list(gt::cell_text(weight = 550),gt::cell_fill("grey80"))
    ) %>%  
    gt::tab_style(
      locations = gt::cells_body(rows = count_cat=="missing"),
      style=list(gt::cell_text(style = "italic"),gt::cell_fill("grey90"))
    ) %>% 
    gt::tab_style(
      locations = gt::cells_stub(rows = count_cat=="missing"),
      style=list(gt::cell_text(style = "italic"),gt::cell_fill("grey90"))
    ) %>% 
    
    # Conditional colors ####
    gt::tab_style_body(
      columns = contains("reported"), rows = !is.na(cat), fn = \(x) x<10,
      style = gt::cell_fill(color = "orange")
    ) %>% 
    gt::tab_style_body(
      columns = contains("reported"), rows = count_cat=="Total", fn = \(x) x<40,
      style = gt::cell_fill(color = "tomato")
    ) %>% 
    gt::cols_hide(c("var","cat","class","footnote"))
  
  if(fnobs(sum_dt$footnote)>0){
    
    footnotes <- sum_dt[!is.na(footnote),][
      ,.(footnote=funique(footnote)),by="label"
    ]
    
    for(i in 1:nrow(footnotes)){
      sum_gt <- sum_gt %>% 
        gt::tab_footnote(
          footnote = footnotes$footnote[i],
          locations = gt::cells_row_groups(groups = footnotes$label[i])
        )
    }
  }
  
  return(sum_gt)
}
