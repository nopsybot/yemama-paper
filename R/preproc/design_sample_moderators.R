design_sample_moderators <- function() {
  list(
    design = c(
      "n_ema_days",
      "prompt_dfreq",
      "item_n",
      "resp_dur_m_sec",
      "any_augmented_vs_nonrep",
      "obj_addon",
      "incent_compliance",
      "inc_val_pot_max.usd",
      "ema_train_vs_nonrep",
      "part_care_minact",
      "parent_involvement"
    ),
    
    c("compl_cutoff_strict_bin","compl_cutoff"),
    
    sample = c(
      "age","female.prc",
      "nonwhite.prc",
      "clin_sample.strict_bin",
      "clin_sompsy.mixna",
      "treat_status.any_vs_na",
      "treat_setting.inout_nomix",
      "symptom_ema"
    )
  ) %>%
    unlist() %>% 
    data.table(moderator = .,feature = names(.) %>% str_remove("\\d{1,2}$"))
}
