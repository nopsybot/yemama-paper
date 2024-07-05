metareg_tables_fty <- function(
    # sum_obj_str = "all_interaction_sum",
    # gt_tar_pfx = "interactions",
    # outcomes = c("compliance","retention","acceptance"),
    # features = "interaction"
    sum_obj_str = "ALLreg_sum_l__unflu1",
    gt_tar_pfx = "metaregs",
    outcomes = c("acceptance","compliance","retention"),
    features = c("sample","design")
    ) {
  
  sum_gt_plan <- list(
    .outcomes = list(as.list(outcomes)),
    .features = list(as.list(features))
  ) %>% 
    {list(
      all = .,
      single = {expand.grid(
        .outcomes = .$.outcomes[[1]], .features = .$.features[[1]])}
    )} %>% 
    rbindlist(idcol = "selection") %>% 
    lapply(
      \(col) lapply(col,\(row) if(is.list(row)) unlist(row) else row)
    ) %>% 
    qDT() %>% 
    mtt(
      name = iif(
        selection=="all",
        paste0("all_",paste(features,collapse = "_")),
        paste0(.outcomes,"_",.features)
      ),
      sum_obj_str = sum_obj_str,
      sum_obj_sym = rlang::syms(sum_obj_str),
      gt_tar_pfx = gt_tar_pfx,
      sum_gt_str = paste0(name,"_sum_gt"),
      sum_gt_sym = rlang::syms(sum_gt_str),
      sum_gt_html_str = paste0(sum_gt_str,"_html"),
      sum_gt_docx_str = paste0(sum_gt_str,"_docx"),
      sum_gt_df_str = paste0(sum_gt_str,"_df"),
      sum_gt_df_sym = rlang::syms(sum_gt_df_str),
      sum_ft_str = paste0(name,"_sum_ft")
    ) %>% 
    # FIXME: overall table breaks with the flextable style function
    fsubset(selection!="all")
  
  # tar_map(
  sum_gt_tars <- tar_eval(
    values = sum_gt_plan,
    expr = list(
      sum_gt = tar_target_raw(
        sum_gt_str,
        quote(
          gt_style_ALLreg_sum_l(
            sum_obj_sym,
            outcomes = .outcomes,
            features = .features
          )
        )
      ),
      sum_gt_html = tar_target_raw(
       sum_gt_html_str,
        quote(
          here::here(
            paste0("pipelines/analysis/tables/metaregs/",name,"/",sum_gt_str,".html")
          ) %T>% gt::gtsave(sum_gt_sym,filename = .)
        ) %>% as.expression(),
       format = "file"
      ),
      sum_gt_docx = tar_target_raw(
        sum_gt_docx_str,
        quote(here::here(
            paste0("pipelines/analysis/tables/metaregs/",name,"/",sum_gt_str,".docx")
          ) %T>% gt::gtsave(sum_gt_sym,filename = .)) %>% as.expression(),
        format = "file"
      ),
      sum_ft = tar_target_raw(
        sum_ft_str,
        quote(ft_style_reg_gt(sum_gt_sym,.outcomes))
      )
    )
  ) %>% 
    set_names(sum_gt_plan$name)
  
  return(sum_gt_tars)
}
