gt_style_ALLreg_sum_l <- function(
    ALLreg_sum_l = tar_read(ALLreg_sum_l),
    # ALLreg_sum_l = tar_read(all_interaction_sum),
    outcomes = c("compliance","retention","acceptance"),
    features = c("sample","design","interaction")
) {
  
  gt_data <- ALLreg_sum_l %>%
    gvr("^model$",invert = TRUE) %>% 
    # Pivot wider ####
    pivot(
      ids = c("feature","moderator","model_part","terms"),
      names = "outcome",
      how = "wider",
      transpose = "names"
    ) %>% 
    fsubset(feature %in% features) %>% 
    gvr(c("terms","model_part","feature",outcomes)) %>% 
    mtt(
      feature = ifelse(
        feature != "interaction",
        paste(str_to_sentence(feature),"characteristic"),
        "Interaction model"
      )
    ) %>% 
    fsubset(!pallNA(gvr(.,outcomes)))
  
  indent_width = c(0,20)
  if(length(unique(gt_data$feature))>1) {indent_width <- indent_width + 20}
  
  gt_data %>%
    gt::gt(
      groupname_col = {if(length(unique(gt_data$feature))>1)"feature" else NULL}
    ) %>% 
    
    gt::tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) %>%
    gt::tab_style(
      style = cell_text(indent = px(indent_width[1])),
      locations = cells_body(columns = terms,rows = model_part=="performance")
    ) %>% 
    gt::tab_style(
      style = cell_text(indent = px(indent_width[2])),
      locations = cells_body(columns = terms,rows = model_part=="parameters")
    ) %>% 
    
    # Outcome spanners ####
    (\(x){if(length(outcomes)>1) {for (o in outcomes)
      x <- gt::tab_spanner(x,str_to_sentence(o),starts_with(o))
    x } else x})() %>% 
    
    # Percentage estimates
    # gt::sub_large_vals(
    #   columns = ends_with("ci.ub"),
    #   threshold = 99,
    #   large_pattern = "100"
    # ) %>% 
    
    gt::fmt(
      columns = any_of(paste0("compliance_",c("b","se","ci.lb","ci.ub"))),
      fns = \(x) round(x*100,2)
    ) %>% 
    gt::fmt(
      columns = any_of(
        sapply(
          c("retention","acceptance"),
          \(x) paste0(x,"_",c("b","se","ci.lb","ci.ub"))
        ) %>% as.character()
      ),
      fns = \(x) round(x,2)
    ) %>% 
    
    gt::fmt_number(matches("_R2$")) %>%
    gt::sub_small_vals(columns = matches("_R2$"),threshold = 0.01) %>% 
    
    gt::fmt_number( columns = matches("p(val)?$"), decimals = 3) %>% 
    gt::sub_small_vals(columns = matches("p(val)$"),threshold = 0.001) %>% 
    gt::fmt_number(matches("_QM$")) %>%
    
    # Estimate ± SE ####
    (\(x){ for (o in outcomes)
      x <- gt::cols_merge(
        x, columns = paste0(o,c("_b","_se")),
        rows = !is.na(gv(gt_data,paste0(o,"_b"))) %>% as.vector(),
        # pattern="{1}&nbsp;±&nbsp;{2}")
        pattern="{1}±{2}")
    x })() %>% 
    
    # 95% CI ####
  (\(x){for (o in outcomes)
    x <- gt::cols_merge(
      x,columns = paste0(o,c("_ci.lb","_ci.ub")),
      rows = !is.na(gv(gt_data,paste0(o,"_ci.lb"))) %>% as.vector(),
      # pattern = "[{1};&nbsp;{2}]"
      pattern = "[{1};{2}]"
    );
  x})() %>% 
    
    # QM tests
    (\(x){for (o in outcomes)
      x <- gt::cols_merge(
        x,columns = paste0(o,c("_QM","_QMdf")),
        rows = !is.na(gv(gt_data,paste0(o,"_QM"))) %>% as.vector(),
        # pattern = "{1}&nbsp;({2})"
        pattern = "{1} ({2})"
      );
    x})() %>% 
  
    (\(x){for(o in outcomes)
      x <- gt::cols_label_with(
        x,starts_with(o),
        \(x) str_remove(x,paste0(o,"_")))
    x})() %>% 
    
    gt::cols_label(
      terms = ifelse(
        length(unique(gt_data$feature))==1,
        unique(gt_data$feature),
        "Characteristics"
      )
    ) %>%
    
    gt::cols_label(matches("ci.lb$")~ "95%&nbsp;CI",.fn=gt::md) %>% 
    gt::cols_label(matches("_b$")~ "Estimate&nbsp;±&nbsp;SE",.fn=gt::md) %>% 
    gt::cols_label(matches("p(val)?$")~ "*P*&#8209;value",.fn=gt::md) %>% 
    gt::cols_label(matches("QM$")~ "Q~M~&nbsp;test&nbsp;(*df*)",.fn=gt::md) %>% 
    gt::cols_label(matches("_R2$")~ "R<sup>2</sup>",.fn=gt::md) %>% 
    
    gt::sub_values(
      columns = matches("p(val)?$"),
      rows = str_detect(terms,"Intercept"),
      fn = \(x) rep(TRUE,length(x)),
      replacement = "—"
    ) %>% 
    gt::sub_missing(missing_text = "") %>% 
    
    # gt::cols_move(columns = model_part, after = terms) %>% 
    # gt::cols_width(terms ~ px(200)) %>% 
    gt::tab_style(
      locations = cells_body(rows = 1:(nrow(gt_data)-1)),
      style = cell_borders(sides = "bottom",style = "hidden")
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = cell_borders(sides = "bottom",style = "hidden")
    ) %>%
    gt::tab_header(
      title = paste0(
        if(length(features)>1 & length(outcomes)>1){"Metaregression results: "}
        else{NULL},
        if(length(features)<2){
          paste(str_to_sentence(features), "characteristics",
                if(length(outcomes)<3)
                  paste("predicting",and::and(outcomes),"rates"))
        } else {NULL},
        ".")
    ) %>% 
    gt::tab_style(
      locations = cells_column_labels(
        c(starts_with("compliance"),
          starts_with("retention"),
          starts_with("acceptance"))
      ),
      style = cell_text(align = "center")
    ) %>% 
    gt::tab_style(
      locations = cells_column_labels(),
      style = cell_text(weight = "bold")
    ) %>% 
    
    # Outcome specific footnotes ####
    {if(any(outcomes %in% c("retention","acceptance"))){
    gt::tab_footnote(.,
      "Note: Unlisted predictors are dropped due to less than 40 available studies per predictor, or less than 10 per category."
    ) %>% 
    gt::tab_footnote(.,
      locations = cells_column_spanners(any_of(c("Retention","Acceptance"))),
      footnote = "Arcsine transformed values"
    )
    } else {.}} %>% 
    # Column selection ####
    gt::cols_hide(columns = everything()) %>% 
    gt::cols_unhide(
      columns = c(
        terms,
        # model_part,
        ends_with("_k"),
        ends_with("_b"),
        ends_with("_lb"),
        ends_with("_ci.lb"),
        ends_with("_pval"),
        ends_with("_QM"),
        ends_with("_QMp"),
        ends_with("_R2")
      )
    ) %>% 
    # Final align and border options ####
    gt::tab_options(
      ihtml.use_text_wrapping=FALSE,
      heading.align = "left",
      column_labels.font.weight = "bold",
      heading.title.font.size = "18px",
      table.border.top.style = "hidden",
      table.border.bottom.style = "hidden"
    )
  
}

