desc_bins_cats <- function(data,bins_plan) {
  
  data %>% 
    mtt(
      across(
        .cols = bins_plan$var, 
        .names = bins_plan$out_names,
        .apply = FALSE,
        .fns = \(x) purrr::map2(
          .x = x, 
          .y = bins_plan$in_breaks,
          .f = \(a,b) custom_cut(a, in_breaks = b) 
        )
      )
    ) %>%
    gv(c(
      bins_plan$out_names,
      .c(clin_sample.mix,random_signaling,enhancement,incent_type,
         feedback_vs_nonrep,ema_train_type,parent_involvement)
    )) %>% 
    purrr::map(
      \(x) qtab(x,na.exclude=FALSE) %>% 
        as.data.table(keep.rownames = TRUE) %>% 
        rnm(cat = x) %>% 
        mtt(
          label = vlabels(x), 
          feature = vlabels(x,attrn="feature"),
          prc = (N*100/length(x)) %>% round(1)
        )
    ) %>% 
    rbindlist(idcol = "var") %>% 
    gby(var) %>% 
    mtt(
      cat = {
        if (all(var  %in% bins_plan$out_names))
          c(#first(cat) %>% str_replace("^.+ -","≤"), cat[2:(length(na_rm(cat))-1)], # In case you want to rewrite the lowest bin
            cat[1:(length(na_rm(cat))-1)],
            last(na_rm(cat)) %>% str_remove("- .+$") %>% 
              str_trim() %>%
              {paste("≥",.)},
            cat[is.na(cat)])
        else cat
      },
      cat = replace_na(cat,"not reported")
    ) %>% 
    fungroup() %>% 
    colorder(var,feature,label,cat,N,prc) %>% 
    mtt(
      var = var %>% factor(
        levels = .c(
          first_pub.bins,# sample_country1, 
          sample_size.bins,age.bins,gender.bins,
          clin_sample.mix,
          n_days.bins,p_day.bins,random_signaling,
          item_n.bins,enhancement,
          incent_type,mon_val.bins,
          feedback_vs_nonrep,ema_train_type,parent_involvement,
          accept.bins,retent.bins,compl.bins
        )
      ),
      feature = feature %>% factor(
        levels = c("general","sample","design","outcome")
      )
    ) %>% 
    roworder(var)
}

