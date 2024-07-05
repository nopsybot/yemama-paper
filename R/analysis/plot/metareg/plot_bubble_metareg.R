plot_bubble_metareg <- function(
    # metareg = tar_read(reg_accept__item_n_log__unflu1)
    # metareg = tar_read(reg_retent__n_ema_days_log__unflu1)
    metareg = tar_read(reg_compl_year)
) {
  
  slope_levels <- c(
    "Point estimate","95% confidence interval","95% prediction interval"
  )
  
  # 1. Variable names and labels -----------------------------------------------
  mod_var <- metareg$call %>% 
    as.list() %>% 
    get_elem("mods") %>% 
    as.character() %>% 
    str_subset("~",negate = TRUE)
  
  mod_varlabel <- metareg$data %>% gv(mod_var) %>% vlabels()
  
  ni_var <- metareg$call %>% 
    as.list() %>% 
    get_elem("ni") %>% 
    as.character()
  
  yi_var <- metareg$call %>% 
    as.list() %>% 
    get_elem("yi") %>% 
    as.character()
  yi_varlabel <- metareg$data %>% gv(yi_var) %>% vlabels()
  
  # 2. Predicted slope and intervals -------------------------------------------
  slope_dat = seq(
    min(metareg$data[[mod_var]],na.rm = TRUE),
    max(metareg$data[[mod_var]],na.rm = TRUE),
    length.out = 100
  ) %>% 
    {cbind(x_var = .,as.data.table(predict(metareg,newmods = .)))} %>%
    fselect(-se)
  
  slope_dat_l = slope_dat %>% 
    pivot(ids = "x_var",  how = "longer") %>% 
    mtt(
      pred_type = nif(
        str_detect(variable,"ci"), "ci",
        str_detect(variable,"pi"), "pi",
        default = "pred"
      ),
      pi = iif(
        str_detect(variable,"pi"),
        "95% Prediction interval",
        "Point estimate"
      ) %>% factor(levels = c("Point estimate","95% Prediction interval"))
    )
  
  # 4. Transformed x-axis ------------------------------
  if(str_detect(mod_var,"log")){
    xmax.nat <- metareg$data %>% gv(mod_var) %>% max(na.rm = TRUE) %>% exp() %>% 
      {ifelse(.<100,round(.,-1),round(.,-2))}
    if(xmax.nat<100) {
      x_breaks = sapply(c(20,2,1),\(x) xmax.nat/x)%>% c(1,10) %>% sort()
    } else {
      x_breaks = sapply(c(100,10,1),\(x) xmax.nat/x) %>% c(10^(0:2)) %>% sort()
    }
    
    exp10_seq_to <- \(to) sapply(1:9,\(x) x*10^(0:4)) %>% {.[.<=to]} %>% sort()
    italize_if <- \(x,p) sapply(x,\(v) ifelse(p(v),paste0("<em>",v,"</em>"),v)) 
    
    scale_x <- scale_x_continuous(
      limits = c(min(slope_dat$x_var),max(slope_dat$x_var)),
      breaks = log(x_breaks),
      minor_breaks = exp10_seq_to(xmax.nat) %>% log(),
      labels = \(x) exp(x) %>% round() %>% 
        italize_if(
          \(v) str_detect(v,xmax.nat %>% as.character() %>% str_sub(1,1))
        )
    )
  }else{
    scale_x <- scale_x_continuous(
      limits = c(min(slope_dat$x_var),max(slope_dat$x_var)),
      breaks = seq(min(slope_dat$x_var),max(slope_dat$x_var),length.out = 4) %>% 
        as.integer(),
      minor_breaks = seq(min(slope_dat$x_var),max(slope_dat$x_var),1) %>% 
        as.integer()
    )
  }
  
  # 5. Transformed y-axis ------------------------------------------------------
  if(str_detect(yi_var,"asin")){
    scale_y <- scale_y_continuous(
      limits = c(0,1) %>% metafor::transf.arcsin(),
      breaks = seq(0,1,0.2) %>% metafor::transf.arcsin(),
      minor_breaks = seq(0,1,0.1) %>% metafor::transf.arcsin(),
      labels = \(x) (metafor::transf.iarcsin(x)*100) %>% paste("%"),
      oob = scales::oob_keep
    )
    
  } else {
    scale_y <- scale_y_continuous(
      limits = c(0,1),
      breaks = seq(0,1,0.2),
      labels = \(x) (x*100) %>% paste("%"),
      oob = scales::oob_keep
    )
  }
  
  # 6. Plot --------------------------------------------------------------------
  metareg %$% {
    data %>%
      
      ## 6.0 Data prep ---------------------------------------------------------
    fsubset(!panyNA(.[,.SD,.SDcols = c(mod_var,yi_var,ni_var)])) %>%
      mtt(weight = weights(metareg)) %>% 
      
      ## 6.1 plot --------------------------------------------------------------
    ggplot(aes(x=.[[mod_var]],y=.[[yi_var]])) +
      
      ## 6.2 95% confidence interval -------------------------------------------
    geom_ribbon(
      inherit.aes = FALSE,
      data = slope_dat %>% 
        mtt(slope = factor("95% confidence interval",levels = slope_levels)),
      aes(
        ymin = ci.lb, 
        ymax = ci.ub, 
        x = x_var, 
        fill = slope),
      alpha = 0.3
    ) +
      
      ## 6.3 95% prediction interval -------------------------------------------
    geom_ribbon(
      inherit.aes = FALSE,
      data = slope_dat %>% 
        mtt(slope = factor("95% prediction interval",levels = slope_levels)),
      aes(
        ymin = pi.lb, 
        ymax = pi.ub, 
        x = x_var, 
        color = slope
      ),
      alpha = 0,
      linetype = "dashed"
    ) +
      
      ## 6.4 Bubbles -----------------------------------------------------------
    geom_point(
      aes(size = weight),
      shape = 16,
      alpha = 0.2,
      show.legend = FALSE
    ) +
      scale_size(range = c(1,2)) +
      
      ## 6.5 Point estimate ----------------------------------------------------
    geom_line(
      data = slope_dat %>% 
        mtt(slope = factor("Point estimate",levels = slope_levels)),
      aes(x=x_var,y=pred,color = slope)
    ) + 
      scale_fill_manual(
        name = NULL,
        values = c(
          "Point estimate" = "dodgerblue3",
          "95% prediction interval" = "turquoise",
          "95% confidence interval" = "turquoise"
        ),
        aesthetics = c("colour","fill"),
        guide = guide_legend(reverse = TRUE)
      ) +
      
      ## 6.6 Position scales ---------------------------------------------------
    scale_y + scale_x + #coord +
      guides(
        x = guide_axis(cap = "both",minor.ticks = TRUE),
        y = guide_axis(cap = "both",minor.ticks = TRUE)
      ) +
      
      ## 6.7 Labels ------------------------------------------------------------
    labs(
      subtitle = if(str_detect(yi_var,"asin")) {
        glue::glue(
          "Arcsine slope: b = {round(b[2,1],3)}, SE = {round(se[2],3)}, [{round(ci.lb[2],3)}; {round(ci.ub[2],3)}], p = {round(pval[2],4)}, R<sup>2</sup> = {round(R2,1)}%"
        )
      } else {
        glue::glue(
          "Percentage slope: b = {round(b[2,1]*100,2)}, SE = {round(se[2]*100,2)}, [{round(ci.lb[2]*100,2)}; {round(ci.ub[2]*100,2)}], p = {round(pval[2],4)}, R<sup>2</sup> = {round(R2,1)}%"
        ) 
      },
      x = mod_varlabel,
      y = yi_varlabel %>% 
        {if(str_detect(yi_var,"asin")) paste(.,"(arcsine scale)") else .},
    ) +
      
      ## 6.8 Theme -------------------------------------------------------------
    papaja::theme_apa() +
      theme(
        plot.title.position = "panel",
        plot.subtitle = ggtext::element_textbox_simple(
          halign = 0.5,size = 12
        ),
        axis.text.x.bottom = ggtext::element_markdown(),
        axis.text.y.left = ggtext::element_markdown(),
        legend.position = "bottom",
        legend.direction = "horizontal",legend.byrow = TRUE
      )
  }
}

