meta_regressions_eval_fty <- function(
    mod_plan = readRDS("pipelines/preproc/_targets/objects/mod_pruned_transf"),
    add_filter_expr = NULL,
    tar_sfx = NULL
) {
  
  if(!is.null(add_filter_expr)){
    add_filter_str = deparse(add_filter_expr) %>% 
      paste0(collapse = "")
    if(is.null(tar_sfx)){
      tar_sfx <- str_extract_all(add_filter_str,"^[^ =]+|fluence") %>% 
        unlist() %>% 
        paste0(collapse = "_")
    }
  }
  
  metareg_plan <- plan_metaregs(
    mod_plan,
    add_filter_expr=add_filter_expr,
    tar_sfx = tar_sfx
  )
  
  combine_sum_l <- function(x) {
    data.table::rbindlist(x, fill=TRUE, idcol="model") %>% 
    collapse::colorder(
      model,outcome,moderator,model_part,k,terms,b,se,pval,ci.lb,ci.ub,QM
    ) %>% 
    collapse::colorder(R2,pos = "end")
  }
  combine_sum_w <- function(x) {
    data.table::rbindlist(x, fill=TRUE, idcol="model")
  }
  
  list(
    reg = tar_eval(
      values = metareg_plan,
      tar_target_raw(
          reg_tarname,
          quote(
            metafor::rma.uni(
              yi = yi_var,
              vi = vi_var,
              mods = mod_var,
              slab = record_id,
              ni = ni_var,
              method = "REML",
              data = dat_arcsin[i_expr,]
            )
          )
      )
    ) %>% setNames(metareg_plan$reg_tarname),
    sum_w = tar_eval(
      values = metareg_plan %>% frename(.outcome = outcome,.feature = feature),
      tar_target_raw(
        sum_w_tarname, 
        quote(
          get_params_wide(reg_tarsym)[
            ,`:=`(outcome=.outcome,feature = .feature)
          ][]
        )
      )
    ) %>% setNames(metareg_plan$reg_tarname),
    sum_l = tar_eval(
      values = metareg_plan %>% frename(.outcome = outcome,.feature = feature),
      tar_target_raw(
        sum_l_tarname, quote(get_params_long(reg_tarsym)[
          ,`:=`(
            outcome=.outcome,
            feature = .feature
          )
        ][])
      )
    ) %>% setNames(metareg_plan$reg_tarname)
  ) %>% 
    append(
      tar_eval(
        values = data.table(list_name = c("sum_l","sum_w")) %>%
          mtt(
            cmb_tarname = if(!is.null(tar_sfx)){
              paste0("ALLreg_",list_name,"__",tar_sfx)
            } else {paste0("ALLreg_",list_name)},
            cmb_fun = list(combine_sum_l,combine_sum_w)
          ),
        tar_combine_raw(
          cmb_tarname, .[[list_name]], command = quote(cmb_fun(list(!!!.x)))
        )
      )
    ) %>% append(
      tar_eval(
        values = data.table(
          dep_ALLsum_tarsym = ifelse(
            !is.null(tar_sfx),
            paste0("ALLreg_sum_w__",tar_sfx),
            "ALLreg_sum_w"
          ) %>% as.symbol(),
          glimpse_tarname = ifelse(
            !is.null(tar_sfx),
            paste0("ALLreg_glimpse__",tar_sfx),
            "ALLreg_glimpse"
          )
        ),
        tar_target_raw(
          glimpse_tarname,
          quote(simplify_results_w(dep_ALLsum_tarsym))
        )
      )
    ) %>%
    setNames(
      names(.) %>%
        (\(x) {x[4:6] <- c("ALLreg_sum_l","ALLreg_sum_w","ALLreg_glimpse");x})()
    )
  
}
