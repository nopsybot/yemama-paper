meta_regs_fty <- function(mod_plan) {
  
  tar_plan(
    
    meta_regressions_eval_fty(
      mod_plan
    ),
    
    meta_regressions_eval_fty(
      mod_plan, 
      add_filter_expr = quote(
        record_id %!in% allfluencers[str_detect(slab,'Tutelman'),slab]
      ),
      tar_sfx = "unflu1"
    ),
    
    bubble_plots_fty(),
    
    meta_regressions_eval_fty(
      mod_plan,
      add_filter_expr = quote(
        compl_cutoff_use==0 & record_id %!in% allfluencers[str_detect(slab,"Tutelman"),slab]
      ),
      tar_sfx = "unflu1_uncut"
    ),
    
    tar_map(
      values = data.table(
        outcome = c("compliance","retention","acceptance")
      ) %>% 
        mtt(
          pfx = str_extract(outcome,"(compl)|(retent)|(accept)"),
          yi_sym = rlang::syms(.c(compl_m,retent.asin,accept.asin)),
          vi_sym = rlang::syms(.c(compl_vi,retent_vi,accept_vi)),
          ni_sym = rlang::syms(.c(compl_sample_size,retention_n,acceptance_n)),
          i_expr = paste0(outcome,"_reported==TRUE") %>% lapply(str2lang),
          name = paste0(pfx,"__year")
        ),
      names = "name",
      tar_target_raw(
        "reg",
        quote(
          metafor::rma.uni(
            yi = yi_sym,
            vi = vi_sym,
            mods = year,
            slab = record_id,
            ni = ni_sym,
            method = "REML",
            data = dat_arcsin[i_expr,]
          )
        )
      ),
      tar_target_raw(
        "reg_days_",
        quote(
          metafor::rma.uni(
            yi = yi_sym,
            vi = vi_sym,
            mods = ~ year * n_ema_days.log,
            slab = record_id,
            ni = ni_sym,
            method = "REML",
            data = dat_arcsin[i_expr,]
          )
        )
      )
    ),
    
    metareg_tables_fty(
      sum_obj_str = "ALLreg_sum_l__unflu1",
      gt_tar_pfx = "metaregs",
      outcomes = c("compliance","retention","acceptance"),
      features = c("sample","design")
    ),

    metareg_plots_fty(),
    
    meta_interactions_fty(),
    
    metareg_tables_fty(
      sum_obj_str = "all_interaction_sum",
      gt_tar_pfx = "interactions",
      outcomes = c("compliance","retention","acceptance"),
      features = "interaction"
    ),
    
    pred_compl__year = c(2006,2023) %>% {
      cbind(
        year = .,
        predict.rma(
          reg_compl__year,
          newmods = .,
        ) %>% as.data.table())},
    
    pred_retent__n_ema_days_log = c(7,14,21,28,63,100,600) %>% {
      cbind(
        n_ema_days = .,
        predict.rma(
          reg_retent__n_ema_days_log__unflu1,
          newmods = c(log(.)),
          transf = transf.iarcsin
        ) %>% as.data.table())},
    
    pred_accept__item_n_log = c(5,10,20,40,50) %>% {
      cbind(
        item_n = .,
        predict.rma(
          reg_accept__item_n_log__unflu1,
          newmods = c(log(c(5,10,20,40,50))),
          transf = transf.iarcsin
        ) %>% as.data.table()
      )},
    
    reg_compl__inWen2017 = metafor::rma.uni(
      yi = compl_m,
      vi = compl_vi,
      mods = ~ inWen2017,
      slab = record_id,
      ni = compl_sample_size,
      method = "REML",
      data = dat_arcsin[
        compliance_reported==TRUE & 
          record_id %!in% allfluencers[str_detect(slab,'Tutelman'),slab],
      ]
    ),
    
    reg_compl__afterWen = metafor::rma.uni(
      yi = compl_m,
      vi = compl_vi,
      mods = ~ afterWen,
      slab = record_id,
      ni = compl_sample_size,
      method = "REML",
      data = dat_arcsin[
        compliance_reported==TRUE & 
          record_id %!in% allfluencers[str_detect(slab,'Tutelman'),slab],
      ] %>% mtt(afterWen = year >= 2016)
    ),
    
  )
  
}