plot_all_metaregs <- function(
    data = tar_read(ALLreg_sum_l)[feature=="design",],
    fignum = 2,
    cap.title = "Metaregression estimates.<br>",
    cap.note = "Undepicted estimates were not calculated due to low cell frequencies or overall missingness."
) {
  
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
        str_c(outcome," (%)"),
        str_c(outcome,"<br>(arcsine scale)")
      ) %>% 
        str_to_sentence() %>% 
        factor(
          levels = purrr::map2_chr(
            unique(.),c("ompl","etent","ccept"),\(x,y) str_subset(x,y)
          )
        ),
      sig = nif(
        # pval < 0.001, "p < .001",
        pval < 0.005, "p < .005",
        pval < 0.05, "p < .05",
        default = "p ≥ .05"
      ) %>% factor(levels = c("p < .005","p < .05","p ≥ .05"))#,
      # abbr_key = abbr_key %>% 
      #   rrapply(f = \(x) str_subset(x,"^no = ",negate = TRUE))
    ) %>% 
    split(by="outcome") %>% 
    
    lapply(plot_metaregs) %>%  
    
    purrr::map2(names(.),\(p,t) p+labs(x=t)) %>% 
    lapply(
      \(p) p + theme(
        # plot.margin = unit(c(1,0.1,0.1,0.1),units = "lines"),
        plot.caption = element_blank(),
        axis.title.x.bottom = ggtext::element_markdown(),
        # axis.title.x.bottom = ggtext::element_markdown(size = 7),
        # axis.text = element_text(size = 7),
        # legend.text = element_text(size = 6)
      )
    ) %>% 
    {.[[1]] + 
        theme(strip.text.y.right = element_blank()) +
        # coord_cartesian(xlim = c(-7,20)) +
        # scale_x_continuous(breaks = seq(-5,15,by=5)) +
        .[[2]] + guides(y = "none") + theme(strip.text.y.right = element_blank()) +
        .[[3]] + guides(y = "none")
    } / 
    guide_area() +
    plot_layout(
      heights = c(15,1),
      guides = "collect",
    ) +
    plot_annotation(
      caption = paste(
        paste0("<strong>Figure ",fignum,". ",cap.title,"</strong>"),
        paste("<em>Note.</em>",cap.note),
        data$abbr_key %>% unlist() %>% unique() %>% na_rm() %>% 
          str_subset("^no = ",negate = TRUE) %>% 
          paste(collapse= "; ") %>% 
          {paste0("<em>Abbreviaions:</em> ",.,".")}
      ),
      theme = theme(
        plot.caption.position = "plot",
        plot.caption = ggtext::element_textbox(
          width = grid::unit(0.9, "npc"), 
          halign = 0,lineheight = 1.2
        ),
        plot.margin = grid::unit(rep(0.5,4), "lines")
      )
    )
  

}
