plot_dist_outcome_asin <- function(
    
  dat_arcsin = tar_read(dat_arcsin),
  
  outcomes = c(
    "acceptance_rate_extr",
    "accept.asin",
    "compl_m",
    "retention_rate_extr",
    "retent.asin"
    ),
  
  filt_vars = c(
    "acceptance_reported",
    "acceptance_reported",
    "compliance_reported",
    "retention_reported",
    "retention_reported"
  ),
  
  scalings = c(
    rep("Natural scale in %",3),rep("Arcsine transformed",2)
  )[c(1,4,2,3,5)]
) {
  
  dat_arcsin <- dat_arcsin %>% mtt(
    compl_m = iif(compliance_reported==TRUE,compl_m*100,compl_m)
  )
  
  list(outcome = outcomes, filt = filt_vars, scaling = scalings) %>% 
    purrr::pmap(
      .l = .,
      .f = \(outcome,filt,scaling) dat_arcsin[
        filt_var==TRUE,,env = list(filt_var = filt)
      ] %>% {
        ggplot(.,aes(x = .[[outcome]])) +
          geom_histogram(
            aes(y = after_stat(ndensity)),
            bins = 30,
            alpha = 0.3) +
          geom_density(
            aes(y = after_stat(ndensity)),
            bounds = c(min(.[[outcome]],na.rm = TRUE),
                       max(.[[outcome]],na.rm = TRUE)),
            outline.type = "full"
          ) +
          scale_y_continuous(expand = c(0,0,0.05,0.05)) +
          scale_x_continuous(
            n.breaks = 5,
            sec.axis = sec_axis(
              transform = I,
              name = vlabels(gv(.,outcome)) %>% str_remove(" rate"),
              guide = guide_none()
            )) +
          labs(x = scaling) +
          guides(
            x = guide_axis(cap = "both"),
            y = guide_axis(cap = "both")
          ) +
          papaja::theme_apa() +
          theme(axis.title.x = element_text(face = "bold"))
      }) %>%
    set_names(vlabels(gv(dat_arcsin,outcomes)) %>% str_remove(" rate")) %>% 
    append(
      list(
        ggplot() + 
          scale_x_continuous(
            sec.axis = sec_axis(
              transform = I,
              name = names(.)[3],
              guide = guide_none()
            )) +
          papaja::theme_apa() +
          theme(axis.title.x = element_text(face = "bold"))
      ),
      after = 3
    ) %>% 
    
    purrr::modify_at(.at = c(1,2,5,6),.f = \(p) p + labs(y = "")) %>% 
    purrr::modify_at(.at = 3,.f = \(p) p + labs(y = "Normalized density")) %>% 
    
    purrr::modify_at(
      .at = c(1,3,5),
      .f = \(p) p + 
        coord_cartesian(xlim = c(-5,105)) #+
        # scale_x_continuous(breaks = seq(0,100,25))
    ) %>% 
    purrr::modify_at(
      .at = c(2,6),
      .f = \(p) p + 
        # labs(title = "") +
        coord_cartesian(xlim = c(-0.1,1.8))# +
        # scale_x_continuous(breaks = seq(0,1.6,0.4))
    ) %>% 
    wrap_plots(
      ncol = 2,byrow = TRUE) +
    plot_layout(
      axes = "collect_y",
      axis_titles = "collect",guides = "collect")
    
}
