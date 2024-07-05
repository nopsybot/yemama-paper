outcome_mod_vis_miss <- function(
    data = tar_read(dat),
    outcome = "compl_m",
    mod_vars = tar_read(mod_list)$moderator
) {
  
  all_vars <- c("record_id",outcome,mod_vars)
  
  plot_data <- data %>% 
    gv(all_vars) %>% 
    mtt(row = 1:nrow(.)) %>%
    mtt(across(gv(.,c(outcome,mod_vars),"names"),\(x) is.na(x))) %>%
    mtt(p_nobs = psum(gv(.,c(outcome,mod_vars)),na.rm=TRUE)) %>%
    mtt(out_obs = get_elem(.,outcome)) %>% 
    pivot(ids = .c(row,record_id,p_nobs,out_obs), how = "longer",factor = "names") %>%
    mtt(v_nobs = fsum(value,g=variable,TRA = "fill",na.rm=TRUE)) %>% 
    mtt(across(.c(out_obs,p_nobs),factor)) %>%
    mtt(out_obs.x.p_nobs = finteraction(out_obs,p_nobs)) %>%
    mtt(
      variable = variable %>% 
        forcats::fct_reorder(v_nobs) %>% 
        forcats::fct_relevel(outcome,after = 0L)
    ) %>%
    roworder(out_obs.x.p_nobs) %>%
    mtt(record_id = forcats::fct_inorder(record_id) %>% as.numeric())
  
  total_labs <- plot_data$value %>% qtab() %>% {./nrow(plot_data)} %>% 
    sjmisc::prcn() %>% setNames(c("available","missing")) %>% 
    data.frame(prc=.) %>% 
    glue::glue_data("{rownames(.)} ({prc})") %>% 
    as.character() %>% 
    setNames(c("FALSE","TRUE"))
  
  labs <- data %>% gv(levels(plot_data$variable)) %>% vlabels() %>% 
    purrr::modify_in(1,\(x) glue::glue("**{x}**"))
  
  prc_labs <- data %>% gv(levels(plot_data$variable)) %>% 
    sapply(\(x) fnobs(x)*100/nrow(.)) %>% formatC(1,width = 3,format="f") %>% 
    paste("%") %>% 
    purrr::modify_in(1,\(x) glue::glue("**{x}**"))
  
  plot_data %>% 
    ggplot2::ggplot(
      ggplot2::aes(
      x=record_id,
      y=forcats::fct_rev(variable) %>% as.integer(),
      fill = value)
    ) + 
    ggplot2::geom_raster() +
    ggplot2::scale_x_continuous(
      # sec.axis = ggplot2::dup_axis(name = ""),
      breaks = seq(min(plot_data$row),max(plot_data$row),length.out=4) %>%
        round() %>%
        {.[2:3]<-round(.[2:3],-2);.},
      expand = c(0.01,0.007)
    ) +
    
    ggplot2::scale_y_continuous(
      breaks = seq_along(c(outcome,mod_vars)),
      minor_breaks = NULL,
      labels = rev(labs),
      name = "Variable",
      sec.axis = ggplot2::dup_axis(
        labels = rev(prc_labs),
        name="% available"),
      expand = c(0,0)
    ) +
    ggplot2::scale_fill_grey(
      start = 0.4,end = 0.8,
      labels = total_labs,
      name = "Overall"
      ) +
    labs(x = "Study samples") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "top",
      legend.key.spacing.x = unit(2,"lines"),
      panel.spacing.y = unit(0,"lines"),
      axis.ticks.x.bottom = element_line(colour = "grey80",linewidth = 0.4),
      axis.ticks.length.x = unit(0.5,"lines"),
      axis.ticks.length.y = unit(0.25,"lines"),
      axis.text.y.left = ggtext::element_markdown(),
      axis.text.y.right = ggtext::element_markdown(hjust = 1)
    )
}
