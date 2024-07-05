ft_style_reg_gt <- function(
    reg_gt = tar_read(acceptance_sample_sum_gt),
    outcome = "acceptance"
){
  
  reg_gt %>% 
    gt:::as.data.frame.gt_tbl() %>% 
    dapply(\(x) str_replace(x,"<br />","")) %>% 
    dapply(\(x) str_replace(x,"&lt;","<")) %>% 
    frename(
      # FIXME: the outcome variable breaks in the overall table with all outcomes
      \(x) str_remove(x,paste0(outcome,"_"))
    ) %>% 
    flextable:::as_flextable.grouped_data(
      col_keys = c("terms","k","b","ci.lb","pval","QM","QMp","R2")
    ) %>% 
    add_footer_lines(
      if(outcome %in% c("acceptance","retention")){
        "Note: Parameters are presented on the arcsine scale. Unlisted predictors are dropped due to less than 40 available studies per predictor, or less than 10 per category."
      }
    ) %>% 
    set_header_labels(
      terms = "Characteristic",
      b = "Estimate ± SE",
      ci.lb = "95% CI"
    ) %>% 
    mk_par(
      part = "header", j = c("pval","QMp"),
      value = as_paragraph(as_i("P"), "-value")
    ) %>% 
    mk_par(
      part = "header", j = "QM",
      value = as_paragraph("Q",as_sub("M")," test (",as_i("df"),")")
    ) %>% 
    mk_par(
      part = "header", j = "R2",
      value = as_paragraph("R",as_sup("2"))
    ) %>% 
    padding(i = ~ model_part=="parameters", j = 1, padding.left = 20) %>% 
    bold(part="header") %>% 
    align(part="header",j = 2:8,align = "center") %>%
    valign(part="header",j = 1:8,valign = "bottom") %>%
    align(part="body",j = 3:4,align = "center") %>%
    height_all(height = 0.5, part = "body", unit = "cm") %>% 
    hrule(rule = "exact", part = "body") %>%
    # line_spacing(space = 0.5, part = "all") %>% 
    padding(padding.top = 1, padding.bottom = 1, part = "body") %>% 
    font(part = "all",fontname = "Calibri") %>% 
    fontsize(part = "all",size = 9) %>%
    set_table_properties(width = 1,layout = "autofit") %>% 
    paginate(group = "label",group_def = "rle")
}