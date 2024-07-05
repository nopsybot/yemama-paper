plot_log_corr <- function(
    data = tar_read(dat_imputed),
    xvar = "n_ema_days.log",
    yvar = "prompt_dfreq.log",
    xlab = NULL,
    ylab = NULL
){
  
  xlab <- xlab %||% vlabels(gv(data,xvar)) %||% ""
  ylab <- ylab %||% vlabels(gv(data,yvar)) %||% ""

  xmax.nat <- data %>% gv(xvar) %>% max(na.rm = TRUE) %>% exp() %>% 
    {ifelse(.<100,round(.,-1),round(.,-2))}
  ymax.nat <- data %>% gv(yvar) %>% max(na.rm = TRUE) %>% exp() %>% 
    {ifelse(.<100,round(.,-1),round(.,-2))}
  
  if(xmax.nat<100)
    x_breaks = sapply(c(20,2,1),\(x) xmax.nat/x)%>% c(1,10) %>% sort()
  else
    x_breaks = sapply(c(100,10,1),\(x) xmax.nat/x) %>% c(10^(0:2)) %>% sort()
  
  if(ymax.nat<100)
    y_breaks = sapply(c(20,2,1),\(x) ymax.nat/x) %>% c(1,10) %>% sort()
  else
    y_breaks = sapply(c(100,10,1),\(x) ymax.nat/x) %>% c(10^(0:2)) %>% sort()
  
  exp10_seq_to <- \(to) sapply(1:9,\(x) x*10^(0:4)) %>% {.[.<=to]} %>% sort()
  italize_if <- \(x,p) sapply(x,\(v) ifelse(p(v),paste0("<em>",v,"</em>"),v)) 
  
  res_str <- paste("~",yvar,"+",xvar) %>% 
    as.formula() %>% 
    cor.test(formula = .,data=data) %>% 
    report::report_statistics()
  
  p1 <- data %>% 
    correlation::cor_test(xvar,yvar) %>% 
    correlation::visualisation_recipe(
      # smooth = list(color = "grey20")
      ) %>% 
    plot() +
    scale_x_continuous(
      breaks = log(x_breaks),
      minor_breaks = exp10_seq_to(xmax.nat) %>% log(),
      labels = \(x) exp(x) %>% round() %>% italize_if(\(v) str_detect(v,"3"))
    ) +
    scale_y_continuous(
      breaks = y_breaks %>% log(),
      minor_breaks = exp10_seq_to(ymax.nat) %>% log(),
      labels = \(x) exp(x) %>% round() %>% italize_if(\(v) v %!in% 10^(0:2))
    ) +
    labs(
      subtitle = res_str,
      x = xlab, 
      y = ylab
    ) +
    guides(
      x = guide_axis(cap = "both",minor.ticks = TRUE),
      y = guide_axis(cap = "both",minor.ticks = TRUE)
    ) +
    papaja::theme_apa() +
    theme(
      plot.title.position = "panel",
      plot.subtitle = element_text(hjust = 1,vjust = -2,size = 12),
      axis.text.x.bottom = ggtext::element_markdown(),
      axis.text.y.left = ggtext::element_markdown()
    )
  
  dens_2d <- data %>% 
    gv(c(xvar,yvar)) %>% 
    dapply(exp) %>% 
    ggplot(
      aes(
        x = .[[xvar]], #data[[str_remove(xvar,"\\.log")]],
        y = .[[yvar]] #data[[str_remove(yvar,"\\.log")]]
      )
    ) +
    # geom_area(data = data.frame(x = 1:300,y = 0.34^seq(-4,7,length.out = 300)+4), aes(x=x,y=y), alpha = 0.2) +
    geom_density2d(
      binwidth = c(0.00001),
      show.legend = FALSE,
      color = "grey50",
      alpha = 0.5
    ) +
    geom_text(
      data = data.frame(
        x=0.5 * max(data[[str_remove(xvar,"\\.log")]],na.rm=TRUE),
        y=0.5 * max(data[[str_remove(yvar,"\\.log")]],na.rm=TRUE),
        label = str_wrap("2D-density on natural scale",width = 15)
      ),
      aes(x=x,y=y,label = label),
      size = 4
    ) +
    stat_density2d_filled(
      geom = "raster",
      # bins = 1,
      aes( fill = after_stat(density),alpha = after_stat(density)),
      show.legend = FALSE,
      contour = FALSE
    ) +
    # geom_point(size = 1.5,shape = 21,alpha = 1) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_alpha_binned(range = c(0,1)) +
    scale_fill_binned(high = "black",low ="white") +
    theme_void() +
    theme(plot.background = element_rect(colour = "black"))
  
  p1 + 
    patchwork::inset_element(
      dens_2d,
      left = 0.7,bottom = 0.7,right = 0.98,top = 0.98
    )
  
    
  
}
