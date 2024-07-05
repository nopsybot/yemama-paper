convert_compl_units <- function(
    data, vars, units,
    p_tot = "prompt_tot_n", p_day = "prompt_dfreq", n_day = "n_ema_days"
){ 
  data[
    , paste0(vars,".prc") := lapply(
      .SD,\(x) nif(
        units == "n° prompts/person", x*100/p_tot,
        units == "n° prompts/day", x*100/p_day,
        units == "n° days/person", x*100/n_day,
        default = x
      )
    ),
    .SDcols = vars,
    env = list(vars = I(vars),units=units,p_tot=p_tot,p_day=p_day,n_day=n_day)
  ][] 
}