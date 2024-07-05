get_params_long <- function(
  # # Continuous single predictor
  # metareg = tar_read(reg_retent__resp_dur_m_sec_log)
  # # Categorical single predictor
  # metareg = tar_read(reg_compl__parent_involvement)
  # # Interaction (categorical, continuous)
  # metareg = tar_read(reg_compl__clin_sample.strict_bin_x_prompt_dfreq.log)
  # # Interaction (both continuous)
  metareg = tar_read(reg_accept__age_x_inc_val_pot_max.usd.log)
) {
  
  mod_var <- paste(metareg$formula.mods,collapse="") %>% 
    str_remove("~") %>% 
    str_split(" \\* ",simplify = TRUE) %>% 
    str_trim()
  
  mod_class <- sapply(mod_var,\(x) class(metareg$data[[x]]))
  
  mod_type <- nif(
    length(mod_class)>1, "interaction",
    mod_class %in% c("integer","numeric"), "continuous",
    mod_class %in% c("factor","logical"), "categorical"
  )
  
  get_categorical_params <- function(metareg, mod_var){
    metareg %$% 
      data.table::data.table(
        k = qtab(metareg$data[,.SD,.SDcols = mod_var]) %>% as.numeric(),
        params = c("Intercept",rep("Beta",nrow(b)-1)),
        levels = levels(data[[mod_var]]),
        b = b[,1], se, pval = pval, ci.lb, ci.ub
      ) %>% 
      mtt(
        intrcpt = first(levels),
        terms = paste0(params, " (",levels(metareg$data[[mod_var]]),")"),
        tick_title = metareg$data[[mod_var]] %>% 
          vlabels(attrn = "tick_title"),
        
        level_abbr = metareg$data[[mod_var]] %>% 
          vlabels(attrn = "level_abbr") %>% .[levels],
        levels = fcoalesce(level_abbr,levels),
        
        intrcpt_abbr = metareg$data[[mod_var]] %>% 
          vlabels(attrn = "level_abbr") %>% .[intrcpt],
        intrcpt = fcoalesce(intrcpt_abbr,intrcpt),
        
        effect = ifelse(
          levels!=intrcpt,
          ifelse(
            !is.na(tick_title),
            paste(tick_title,levels,"vs.",intrcpt),
            paste(levels,"vs.",intrcpt)
          ),
          NA
        )
      )
  }
  
  get_continuous_params <- function(metareg){
    metareg %$% 
      data.table::data.table(
        params = c("Intercept","Beta"),
        terms = c("Intercept","Beta"),
        b=b[,1], se, pval=pval, ci.lb, ci.ub,
        effect = NA_character_
      )
  }
  
  get_interaction_params <- function(metareg, mod_var){
    metareg %$% data.table(
        k = data %>% 
          gv(mod_var) %>% 
          fsubset(!panyNA(gv(.,is.numeric))) %>% 
          {if(any(sapply(.,\(v) !is.numeric(v)))){
            gv(., \(x) !is.numeric(x)) %>% qtab() %>% as.numeric() %>% rep(length(mod_var))
          # }else if(all(sapply(.,\(v) is.numeric(v)))){
          #   dapply(.,\(x) !is.na(x)) %>% qtab() %>% as.numeric() %>% append(rep(NA,3))
          }else{rep(NA_character_,nrow(b))
          }},
        params = nif(
          str_detect(rownames(b),"intrcpt"),"Intercept",
          str_detect(rownames(b),":"),"Interaction",
          default = "Beta"
        ),
        levels = data %>% 
          gv(mod_var) %>% gv(is.factor) %>%
          {if(nrow(.)==0) rep(NA_character_,nrow(b))
              else sapply(.,levels) %>% rep(length(mod_var))
          },
        b = b[,1], se, pval = pval, ci.lb, ci.ub,
        terms.0 = rownames(b) %>% 
          {if(any(sapply(gv(data,mod_var),\(v) !is.numeric(v)))){
            str_remove(.,data %>% gv(mod_var) %>% gv(is.factor,"names"))
          }else{.}} %>% 
          str_replace_all(
            data %>% gv(mod_var) %>% gv(is.numeric) %>% vlabels()
          ) %>% 
          str_remove(" \\(.+\\)")
    ) %>% 
      mtt(
        terms = nif(
          params == "Intercept" & is.na(levels), params, 
          params == "Intercept", paste0(params, " (",levels,")"),
          params == "Interaction",  params,
          # params == "Beta", paste0(params," (",terms,")"),
          default = paste0(params," (",terms.0,")")
        )
      )
  }
  
  regsum_l <- list(
      performance = if(all(mod_type=="categorical") | mod_type == "interaction"){
       metareg %$% data.table::data.table( k,QM,QMdf=QMdf[1],QMp,R2 = R2 %||% NA_real_ )
      }else if(all(mod_type=="continuous")){
        metareg %$% data.table::data.table(k,R2)
      },
      parameters = if(all(mod_type=="categorical")){
        get_categorical_params(metareg,mod_var)
      }else if(all(mod_type=="continuous")){
        get_continuous_params(metareg)
      }else if(all(mod_type=="interaction")){
        get_interaction_params(metareg,mod_var)
      }
    ) %>% 
    data.table::rbindlist(idcol = "model_part",fill=TRUE) %>% 
    collapse::mtt(
      moderator = if(mod_type=="interaction") 
        paste(mod_var,collapse = " * ") 
      else 
        mod_var,
      label_long = if(mod_type!="interaction") 
        gv(metareg$data,mod_var) %>% vlabels() 
      else 
        metareg$data %>% gv(mod_var) %>% vlabels() %>% 
        str_remove(" \\(.+\\)") %>% 
        paste(collapse = " * "),
      label_short = if(mod_type!="interaction") 
        gv(metareg$data,mod_var) %>% vlabels(attrn = "label_short")
      else
        NA_character_,
      abbr_key = if(mod_type=="categorical")
        gv(metareg$data,mod_var) %>% 
        gv(is.factor) %>% 
        lapply(vlabels,attrn = "abbr_key")
      else
        NA_character_,
      label = fcoalesce(label_short,label_long),
      terms = fcoalesce(terms,label_short,label),
      class = if(mod_type=="interaction") list(mod_class) else mod_class,
      type = mod_type,
      effect = if(all(mod_type=="continuous")){
        iif(str_detect(terms,"^Beta$"),label,NA_character_)
      } else if (mod_type=="interaction"){
        iif(str_detect(terms,"intrcpt"),paste0("Intercept (",levels,")"),terms)
      } else {effect}
    ) %>% 
    collapse::colorder(
      moderator,label,label_short,class,type,model_part,k,
      terms,b,se,pval,ci.lb,ci.ub
    ) %>% 
    collapse::colorder(R2,pos = "end")
  
return(regsum_l)
}
