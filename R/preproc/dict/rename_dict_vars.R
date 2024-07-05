#' Rename dict vars for redcapuche package
#'
#' @param dic raw imported data dictionary
#'
#' @return renamed data dictionary
#' @export
#'
rename_dict_vars <- function(dic){
  names(dic) <- c("var","form","s_header","f_type","f_label","choices","f_note",
                  "txt_val","txt_val_min","txt_val_max","id","branch","f_req",
                  "align","qu_num","mat_grp","mat_rnk","f_annot")
  return(dic)
}
