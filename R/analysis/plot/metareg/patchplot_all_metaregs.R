patchplot_all_metaregs <- function(
    # data = tar_read(ALLreg_sum_l__unflu1_uncut),
    data = tar_read(ALLreg_sum_l__unflu1),
    fignum = 2,
    cap.title = "Metaregression estimates.<br>",
    cap.note = "",
    custom.asin.cartxlims = NULL,
    custom.asin.breaks = NULL,
    ffont = "Arial"
) {
  
  data %>% 
    fsubset(outcome %in% c("acceptance","retention")) %>%  
    fsubset(b==min(b,na.rm=TRUE)) %$%  
    (b-se) %>% 
    round(1)
  
  data %>% 
    fsubset(!is.na(effect) & feature != "") %>%
    mtt(
      effect = effect %>% 
        {iif(str_detect(.,"[:\\(]") | !str_detect(.," vs\\. "),
             {str_c("<strong>",.)} %>%
               str_replace_all(.,pattern=c(":"=":</strong>","\\("="</strong>\\(")),
             . )} %>% 
        forcats::fct_inorder() %>% 
        forcats::fct_rev(),
      b = iif(outcome  == "compliance",b*100,b),
      se = iif(outcome  == "compliance",se*100,se),
      ci.lb = iif(outcome  == "compliance",ci.lb*100,ci.lb),
      ci.ub = iif(outcome  == "compliance",ci.ub*100,ci.ub),
      outcome = iif(
        outcome=="compliance",
        str_c(outcome,"<br><span style='font-size: 8pt;'>(%)</span>"),
        str_c(outcome,"<br><span style='font-size: 8pt;'>(arcsine scale)</span>")
      ) %>% 
        str_to_sentence() %>% # NOTE: Order of outcomes is defined here
        factor(
          levels = purrr::map_chr(
            .x = c("ccept","ompl","etent"),
            .f = \(x) str_subset(unique(.),x))
        ),
      sig = nif(
        # pval < 0.001, "p < .001",
        pval < 0.005, "*P*<.005",
        pval < 0.05, "*P*<.05",
        default = "*P*≥.05"
      ) %>% factor(levels = c("*P*<.005","*P*<.05","*P*≥.05")),
      # abbr_key = abbr_key %>% 
      #   rrapply(f = \(x) str_subset(x,"^no = ",negate = TRUE)),
      # NOTE: Order of features is defined here
      feature = factor(feature, levels = c("sample","design"))
    ) %>%
    split(by=c("feature")) %>%
    rev() %>% 
    lapply(\(d) mtt(d,effect = forcats::fct_drop(effect))) %>% 
    lapply(\(l) split(l,by = "outcome",sort = TRUE)) %>% 
    purrr::flatten() %>% 
    # Apply core plotting function
    purrr::map(
      \(d) plot_metaregs(d, .drop_missing_mods=FALSE, ffont = ffont)
    ) %>% 
    # Dynamic labelling and generic theme elements
    purrr::map2(names(.),\(p,t) p+labs(x=t)) %>% 
    purrr::map(
      \(p) p + theme(
        plot.caption = element_blank(),
        axis.title.x.bottom = ggtext::element_markdown(),
        panel.grid.major.x = element_line(),
        panel.grid.major.y = element_line()
      )
    ) %>%
    
    # For upper plot panels, remove x-axis labels and legends
    purrr::modify_at(
      .at = 1:3,
      \(p) p + 
        labs(x="") +
        guides(x=guide_none(), shape = guide_none(), fill = guide_none()) + 
        theme(
          plot.margin = margin(1,1,0,1)
        )
    ) %>%
    
    # middle panel column
    # For compliance, cartesian limits 
    purrr::modify_at(
      .at = c(2,5), \(p) p + coord_cartesian(xlim = c(-10,20))
    ) %>%
    
    # Drop y-axis for all but leftmost panels
    purrr::modify_at(
      .at = c(2,3,5,6),
      \(p) p + guides(y = guide_none())
    ) %>% 
    
    # Arcsine outcomes, cartesian limits
    {if(!is.null(custom.asin.cartxlims)){
      purrr::modify_at(
        ., .at = c(1,3,4,6), 
        \(p) p + coord_cartesian(xlim = custom.asin.cartxlims)
      )
    } else {.}} %>% 
    
    # Arcsine outcomes, cartesian x-breaks
    {if(!is.null(custom.asin.breaks)){
      purrr::modify_at(
      ., .at = c(1,3,4,6), 
      \(p) p + scale_x_continuous(
        breaks = custom.asin.breaks,
        guide = guide_axis(minor.ticks = TRUE)
      ) +
        theme(panel.grid.minor.x = element_line())
    )
    } else {.}} %>% 
    
    # For lower row panels, remove top margin
    purrr::modify_at(
      .at = 4:6,\(p) p + 
        theme(
          plot.margin = margin(0,1,1,1),
          axis.text.x.bottom = element_text(hjust = 0.6) 
        )
    ) %>% 
    purrr::modify_at(
      .at = 1,\(p) p + 
        labs(title = "Sample characteristics") +
        theme(
          plot.title.position = "plot",
          plot.title = element_text(hjust = 0.4,face = "bold")
        )
    ) %>% 
    purrr::modify_at(
      .at = 4,\(p) p + 
        labs(title = "Design characteristics") +
        theme(
          plot.title.position = "plot",
          plot.title = element_text(hjust = 0.4, face = "bold")
        )
    ) %>% 
    wrap_plots(ncol = 3,heights = c(9,14)) / 
    guide_area() +
    plot_layout(
      heights = c(15,1),
      guides = "collect",
    ) #+
    # plot_annotation(
    #   caption = paste(
    #     paste0("<strong>Figure ",fignum,". ",cap.title,"</strong>"),
    #     paste("<em>Note.</em>",cap.note),
    #     data$abbr_key %>% unlist() %>% unique() %>% na_rm() %>% 
    #       str_subset("^no = ",negate = TRUE) %>% 
    #       paste(collapse= "; ") %>% 
    #       {paste0("<em>Abbreviaions:</em> ",.,".")}
    #   ),
    #   theme = theme(
    #     plot.caption.position = "plot",
    #     plot.caption = ggtext::element_textbox(
    #       width = grid::unit(0.9, "npc"), 
    #       halign = 0,lineheight = 1.2
    #     )
    #   )
    # )
  

}
