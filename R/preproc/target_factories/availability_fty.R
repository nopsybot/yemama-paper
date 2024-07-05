availability_fty <- function() {
  
  # LIST of preprocessed variable types.
  # Categoticals should be factors and numerics tolerate integer
  # meta_dat[,.SD,.SDcols = mod_shortlist] %>%
  #   collapse::vclasses() %>%
  #   BY(x=names(.),g=.,FUN=list)
  
  tar_plan(
    
    mod_list = design_sample_moderators() %T>% 
      {tar_assert_in(
        .$moderator,
        names(meta_dat),
        paste("moderator names not in meta_dat:",x[x %!in% names(meta_dat)])
      )},
    
    
    mod_pruned = { prune_modlist(
        data = meta_dat,
        modlist = mod_list$moderator,
        outcomes = paste0(c("compliance","retention","acceptance"), "_reported"),
        return = "modname_table",
        drop_all_false = FALSE
      )[
        str_detect(moderator,"cutoff"),
        `:=`(retention_reported=NA,acceptance_reported=NA)
      ][] %>% 
        join(mod_list,on="moderator")},
    
    available_dt = count_available_data(
      meta_dat, moderators = mod_list$moderator
    ),
    available_gt = {available_dt %>% 
      gt::gt() %>% 
      gt::data_color(
        columns=c("Compliance","Retention","Acceptance"),
        method = "bin",
        bins = c(0,39,300),
        palette = c("tomato","greenyellow")
      )},
    
    available_detail_gt = gt_summarize_detail(
      data = meta_dat,
      outcomes = paste0(c("compliance","retention","acceptance"), "_reported"),
      # modvars = gvr(meta_dat,"clin_", "names")
      # modvars = mod_shortlist
      modvars = mod_pruned$moderator
    )
    
  )
}
