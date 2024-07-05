count_available_data <- function(
    data = tar_read(meta_dat),
    outcomes = c("compliance","retention","acceptance"),
    moderators = c(
      "design","emi_used","prompt_dfreq","n_ema_days","prompt_tot_n","age"
    )
) {
  
  filtered_availability <- function(data,cond,.cols){
    data[
      cond==TRUE,
      .(outcome = cond_name,reported=.N,list(sapply(.SD,\(x) sum(!is.na(x))))),
      .SDcols = .cols,
      env = list(cond = cond,cond_name = I(deparse(cond)),.cols = I(.cols))
    ][,(.cols) := transpose(V3)
    ][,V3 := NULL][] %>% 
      transpose(keep.names = "Variable",make.names = "outcome")
  }
  
  outcomes %>% 
    paste0("_reported") %>% 
    append(paste(.,collapse = " | ")) %>% 
    lapply(str2lang) %>%
    lapply(\(x) filtered_availability(data,x,moderators))  %>% 
    Reduce(\(x,y) join(x,y,on="Variable",verbose = 0),x=.) %>% 
    setNames(c(names(.)[1],outcomes %>% str_to_title(),"Any outcome"))

  }
