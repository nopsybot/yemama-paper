#' Title
#'
#' @param variables
#'
#' @return
#' @export
#'
#' @examples
preproc_dict <- function(dict_raw) {

  drop_any_brackets <- \(x) {
    x %>% str_remove_all("(<.*?>)|(\\{.*?\\})|( ?\\(.*?\\) ?)")
  }

  dict_raw[
    f_type!="descriptive" & (!str_detect(f_annot,"HIDDEN") | is.na(f_annot)),
  ][] %>%
    mtt(
      item = f_type != "notes",
      
      var_labs = f_label %>% drop_any_brackets(),
      
      choice_split = iif(f_type=="calc",NA_character_,choices) %>% 
        str_split("(\\|)") |>
        lapply(str_trim) %>% 
        lapply(\(x) str_replace(x,",\\s*", "HeRrRrE")) %>% 
        lapply(\(x) str_split(x,"HeRrRrE",simplify = TRUE)),
      
      choice_vals = choice_split %>% 
        lapply(\(x) x[,1] %>% type.convert(as.is=TRUE)),
      
      choice_labs = lapply(choice_split,\(x) x[,ncol(x)]),
      
      choice_shortlabs = lapply(choice_labs,drop_any_brackets),
      
      choice_defs = Map(setNames,choice_vals,choice_shortlabs) %>% 
        rrapply(is.na,unname) # NOTE: don't set NULL because fwrite won't handle
    )
}
