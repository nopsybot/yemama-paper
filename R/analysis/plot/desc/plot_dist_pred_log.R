plot_dist_pred_log <- function(
    
  dat_arcsin = tar_read(dat_arcsin),
  mod_list = tar_read(mod_list),
  trans.rgx = "\\.(log)|(exp)$"
){
  
  preds <- dat_arcsin %>% 
    gv(names(.) %in% mod_list$moderator) %>% 
    gv(sapply(.,\(x) is.numeric(x)),"names") %>%
    str_subset("cutoff",negate = TRUE) %>% 
    {cbind(gv(dat_arcsin,str_remove(.,trans.rgx),"names"),.)} %>% 
    qDT() %>% 
    set_names(c("nat","log")) %>% 
    mtt(
      base = nat,
      unplot = iif(log==nat,nat,NA_character_)
    ) %>% 
    pivot(
      ids = c("unplot","base"),
      names = list(value = "mod",variable = "scale")) %>% 
    mtt(
      lab = sapply(base,\(x) vlabels(dat_arcsin[[x]])),
      scaling = iif(scale=="nat","Natural scale","log transformed")
    )
 
  preds %>% 
    mtt(
      single_plot = list(
        x_var = mod,
        x_lab = scaling,
        var_lab = lab,
        unplot = !is.na(unplot) & scale=="log"
      ) %>% 
        purrr::pmap(
          .l = .,
          .f = \(x_var,x_lab,var_lab,unplot) dat_arcsin %>% 
            fsubset(any_outcome) %>% {
              if(unplot){
                ggplot() +
                  scale_x_continuous(
                    n.breaks = 5,
                    sec.axis = sec_axis(
                      transform = I, name = var_lab, guide = guide_none()
                    )) +
                  papaja::theme_apa() +
                  theme(axis.title.x = element_text(face = "bold"))
              } else {
              ggplot(.,aes(x = .[[x_var]])) +
                geom_histogram(
                  aes(y = after_stat(ndensity)),
                  bins = 30,
                  alpha = 0.3) +
                geom_density(
                  aes(y = after_stat(ndensity)),
                  bounds = c(min(.[[x_var]],na.rm = TRUE),
                             max(.[[x_var]],na.rm = TRUE)),
                  outline.type = "full"
                ) +
                # scale_y_continuous(expand = c(0,0,0.05,0.05)) +
                labs(x = x_lab, y = "Normalized density") +
                guides(
                  x = guide_axis(cap = "both"),
                  y = guide_axis(cap = "both")
                ) +
                scale_x_continuous(
                  n.breaks = 5,
                  breaks = \(l) seq(
                    min(.[[x_var]],na.rm=TRUE),
                    max(.[[x_var]],na.rm=TRUE),
                    length.out = 5) %>% 
                    {if(.[1]<1 & .[5]<10) round(.,1) 
                      else if(.[5]>100) round(.,-1) 
                      else if(.[5]>100) round(.,-1) 
                      else round(.)},
                  sec.axis = sec_axis(
                    transform = I, name = var_lab, guide = guide_none()
                  )) +
                papaja::theme_apa() +
                theme(axis.title.x = element_text(face = "bold"))
              } 
            }),
      single_plot = purrr::pmap(
        list(p=single_plot,s=scale,m=mod),
          \(p,s,m) if(s=="nat") {p + 
            ggplot2::geom_text(
              inherit.aes = FALSE,
              data = data.frame(
                skew = datawizard::skewness(dat_arcsin[[m]])$Skewness %>% 
                  round(2)
                ) %>% 
                mtt(
                  text = paste0("Skew = ",skew),
                  x = iif(skew<0,-Inf,Inf),
                  hjust = iif(skew<0,-0.5,1.5),
                  color = iif(abs(skew)>3,"tomato3","black")
                ),
              ggplot2::aes(x=x,y=Inf,label = text,hjust = hjust,color = color),
              vjust = 1.5
            ) +
              ggplot2::scale_color_identity()} else {p}
      )
    ) %$% 
    wrap_plots(
      single_plot,
      ncol = 2,byrow = FALSE) +
    plot_layout(
      axes = "collect_y",
      axis_titles = "collect",guides = "collect")
    
}
