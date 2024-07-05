plot_funneldat <- function(
    funnel_plotdat = tar_read(funnel_plotdat_retention),
    regres = tar_read(regtest_res_retention),
    x_breaks = waiver(),
    x_minorbreaks = waiver(),
    x_labels = waiver(),
    x_lab = "x"
) {
  
  funnel_plotdat %>% 
    ggplot2::ggplot(ggplot2::aes(x=x,y=y)) +
    # Label dots?
    # ggplot2::geom_text(data = funnel_plotdat %>% fsubset(y==max(y)),ggplot2::aes(label = slab),vjust = 0.5,hjust = 1.05) +
  ggplot2::geom_vline(
    xintercept = mean(funnel_plotdat$x), 
    color = "grey",
    linetype = "dashed",
    linewidth = 1
  ) +
  ggplot2::geom_point(shape = 16, alpha = 0.7) +
    geom_line(
      data = data.frame(
        x = coef(regres$fit)[1] + 
          coef(regres$fit)[2]*(1:max(funnel_plotdat$y)),
        y = 1:max(funnel_plotdat$y)
        ),
      color = "tomato",
      linewidth = 1
      )+
  ggplot2::scale_x_continuous(
    breaks = x_breaks,
    minor_breaks = x_minorbreaks,
    labels = x_labels
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, funnel_plotdat$y %>% {max(.)+sd(.)/2})
  ) +
  guides(
    x = guide_axis(cap = "both", minor.ticks = TRUE),
    y = guide_axis(cap = "both")
  ) +
  ggplot2::labs(
    x = x_lab,
    y = "Sample size"
  ) +
  papaja::theme_apa()
  
  }