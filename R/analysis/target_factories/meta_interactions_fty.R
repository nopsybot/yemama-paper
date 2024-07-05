meta_interactions_fty <- function(){
  
  interaction_plan <- list(
    design = .c( prompt_dfreq.log, n_ema_days.log, item_n.log,
                 inc_val_pot_max.usd.log),
    sample = .c(clin_sample.strict_bin,clin_sompsy.mixna,female.prc,
                nonwhite.prc,age),
    reported = paste0(c("compliance","retention","acceptance"),"_reported")
  ) %>% 
    expand.grid(stringsAsFactors = FALSE) %>% 
    qDT() %>% 
    fsubset(design!="") %>% 
    mtt(
      .outcome = str_remove(reported,"_reported"),
      pfx = .outcome %>% str_extract("(compl)|(retent)|(accept)"),
      mod_str = glue::glue("~{sample}*{design}"),
      mod_frm = lapply(mod_str,as.formula),
      res_tarstr = glue::glue("reg_{pfx}__{sample}_x_{design}"),
      res_tarsym = lapply(res_tarstr,as.symbol),
      sum_tarstr = glue::glue("sum_{pfx}__{sample}_x_{design}"),
      sum_tarsym = rlang::syms(sum_tarstr),
      yi_sym = nif(
        pfx=="compl","compl_m",
        pfx=="retent","retent.asin",
        pfx=="accept","accept.asin"
      ) %>% rlang::syms(),
      vi_sym = nif(
        pfx=="compl","compl_vi",
        pfx=="retent","retent_vi",
        pfx=="accept","accept_vi"
      ) %>% rlang::syms(),
      ni_sym = nif(
        pfx=="compl","compl_sample_size",
        pfx=="retent","retention_n",
        pfx=="accept","acceptance_n"
      ) %>% rlang::syms()
    )
  
  meta_interacts <- list(
    interaction_res = tar_eval(
      values = interaction_plan,
      tar_target_raw(
        res_tarstr,
        quote(
          metafor::rma.uni(
            yi = yi_sym,
            vi = vi_sym,
            mods = mod_frm,
            slab = record_id,
            ni = ni_sym,
            method = "REML",
            data = dat_arcsin[
              rep==TRUE & 
                record_id %!in% allfluencers[str_detect(slab,'Tutelman'),slab],
              , env = list(rep=reported)
            ]
          )
        )
      )
    ) %>% set_names(interaction_plan$res_tarstr),
    interaction_sum = tar_eval(
      values = interaction_plan,
      tar_target_raw(
        sum_tarstr,
        quote(
          get_params_long(res_tarsym)[
            ,`:=`(outcome=.outcome,feature = "interaction")
          ]
        )
      )
    ) %>% set_names(interaction_plan$sum_tarstr)
  )
  
  interact_cmb_tar <- tar_combine(
    all_interaction_sum,
    meta_interacts$interaction_sum,
    command = list(!!!.x) %>% 
      data.table::rbindlist(fill=TRUE, idcol="model") %>% 
      collapse::colorder(
        model,outcome,moderator,model_part,k,terms,b,se,pval,ci.lb,ci.ub,QM
      ) %>% 
      collapse::colorder(R2,pos = "end") %>% 
      gby(model) %>% 
      mtt(
        k_min = fmin(as.numeric(k),na.rm=TRUE),
        k_max = fmax(as.numeric(k),na.rm=TRUE)
      ) %>% 
      fungroup() %>% 
      fsubset(k_min>=10 & k_max >= 40)
  )
  
  return(list(meta_interacts,interact_cmb_tar))
  
}