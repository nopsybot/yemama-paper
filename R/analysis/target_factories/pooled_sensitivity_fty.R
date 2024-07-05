pooled_sensitivity_fty <- function(
    pooled_tarlist,
    anova_tarlist,
    varcomp_tarlist
) {
  list(
    tar_combine(
      pooled_combisum,
      pooled_tarlist,
      command = list(!!!.x) %>% 
        purrr::map(
          \(res) res %>% 
            (\(x) {
              if(class(x)[1]=="rma.uni")
                x %$% data.table(
                  object="rma.uni", k,
                  b = b[1], se, ci.lb, ci.ub, pval,
                  tau2, se.tau2, QE, QEdf = k-1, QEp
                )
              else if(class(x)[1]=="rma.mv")
                x %$% data.table(
                  object="rma.mv", k, g=s.nlevels[1], 
                  b = b[1], se, ci.lb, ci.ub, pval,
                  sigma2.1=sigma2[1], sigma2.2=sigma2[2], QE, QEdf=QEdf, 
                  QEp
                )
            })()
        ) %>% 
        rbindlist( idcol = "model", fill = TRUE ) %>% 
        mtt( outcome = str_extract(model,"(?<=_)[^_]+$")) %>% 
        gby( outcome ) %>% 
        mtt( delta = ffirst( b, g = outcome, TRA =3 )) %>% 
        fungroup()
    ),
    
    tar_target(pooled_combisum_gt, gt_style_sensitivity(pooled_combisum)),
    
    tar_combine(
      pooled_mv_combianova,
      anova_tarlist,
      command = lapply(list(!!!.x),FUN = as.data.frame) %>% 
      rbindlist(idcol = "model") %>% 
        mtt(outcome = str_extract(model,"[^_]+$"))
    ),
    
    tar_combine(
      pooled_mv_combivarcomp,
      varcomp_tarlist,
      command = lapply(list(!!!.x), \(x) get_elem(x,"results")) %>% 
        rbindlist(,idcol = "model") %>% 
        mtt(outcome = str_extract(model,"[^_]+$")) %>% 
        gby(outcome) %>% 
        mtt(level = seq_along(outcome)) %>% 
        fungroup() %>% 
        frename(var_prc = `% of total variance`)
    ),
    
    tar_target(
      pooled_mv_comp_sum,
      pooled_mv_combivarcomp %>% 
        pivot(
          ids = "outcome",values = "var_prc",names = c("level"),how = "wider"
        ) %>% 
        frename(cols = 2:4,\(x) paste0("var_l",x)) %>% 
        mtt(df=3) %>% 
        join(pooled_mv_combianova,how = "right",on = .c(outcome,df))
    ),
    
    tar_target(
      pooled_mv_comp_sum_gt,
      pooled_mv_comp_sum %>% 
        mtt(
          outcome = str_to_sentence(outcome),
          model = iif(df==3,"Three-level","Two-level")
        ) %>% 
        gt(groupname_col = "outcome") %>% 
        fmt_number(columns = c(AIC,BIC,AICc,LRT,logLik,QE),decimals = 2) %>% 
        fmt_number(columns = matches("var_l"),decimals = 2) %>% 
        fmt_number(columns = pval,decimals = 3) %>% 
        sub_small_vals(columns = pval,threshold = 0.001) %>% 
        cols_merge(
          columns = matches("var_l"),
          pattern = "<<({1} + {2} + {3})>>"
        ) %>% 
        cols_move_to_start(model) %>% 
        cols_move_to_end(var_l1) %>% 
        tab_header("Multi-level model comparisons") %>% 
        tab_style(
          locations = cells_row_groups(
            groups = c("Retention","Acceptance")
          ),
          style = cell_borders(sides = "top",style = "hidden")
        ) %>% 
        cols_label(
          model = "Model",
          var_l1 = "Variance components<br>(L1 + L2 + L3; %)",
          pval = "*P*-value",
          .fn = gt::md
        ) %>% 
        sub_missing(columns = c(LRT,pval)) %>% 
        tab_style(
          locations = cells_body(columns = model),
          style = cell_text(indent = px(20))
        ) %>% 
        tab_options(
          row_group.border.bottom.style = "hidden",
          table_body.hlines.style = "hidden",
          heading.align = "left",
          column_labels.border.bottom.style = "solid",
          table.border.top.style = "hidden",
          row_group.font.weight = "bold",
          column_labels.font.weight = "bold"
        )
    ),
    
    tar_target(
      pooled_compl_minus5, dat_arcsin[
        !str_detect(record_id,"Tutel") & compliance_reported==TRUE,
      ][
        , compl_m_m5 := nif(
          compl_cutoff_strict_bin=="inflated compliance", compl_m - 0.1,
          default = compl_m
        )
      ][] %>% 
        metafor::rma.uni(
          yi = compl_m_m5,
          vi = compl_vi,
          slab = record_id, 
          data = .
        )
    ),
    tar_target(
      weights_minus5, pooled_compl_minus5 %>% 
        weights() %>% 
        {data.frame(record_id = names(.),weights = .)} %>% 
        join(
          dat_arcsin[!str_detect(record_id,"Tutel") & compliance_reported==TRUE,][
            , raw := compl_cutoff_strict_bin!="inflated compliance" | is.na(compl_cutoff_strict_bin)
          ][]
            ,
          on = "record_id"
        ) %>% 
        gby(raw) %>% 
        fsummarize(weights = sum(weights)) %>% 
        fungroup()
    )
    
  )
}