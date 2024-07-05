split_rgx_comparison <- function(
    data, rgx, pfx_x = "diff2", pfx_y="diff3" 
){
  
  esc_char_vars <- c("sig","grp1","grp2","measure","info")
  esc_num_vars <- c("esc","se","var","ci.lo","ci.hi","w","totaln")
  esc_vars = c(esc_char_vars,esc_num_vars)
  
  data[
    ,paste0("diff",rgx,"_",esc_vars) := Map(
      \(x,y) nif( 
        str_detect(str_to_lower(diff_x_grp1),rgx),as.list(x),  
        str_detect(str_to_lower(diff_y_grp1),rgx),as.list(y) 
      ),
      diff_x_vars, diff_y_vars
    ),
    env = list(
      rgx = I(rgx),
      diff_x_grp1 = paste0(pfx_x,"_grp1"),
      diff_y_grp1 = paste0(pfx_y,"_grp1"),
      diff_x_vars = as.list(paste0(pfx_x,"_",esc_vars)), 
      diff_y_vars = as.list(paste0(pfx_y,"_",esc_vars))
    )
  ][
    , paste0("diff",rgx,"_",esc_char_vars) := lapply(
      .SD, \(x) sapply(x,\(y) if(!is.null(y)) y else NA_character_)
    ),.SDcols = paste0("diff",rgx,"_",esc_char_vars)
  ][
    , paste0("diff",rgx,"_",esc_num_vars) := lapply(
      .SD, \(x) sapply(x,\(y) if(!is.null(y)) y else NA_real_)
    ),.SDcols = paste0("diff",rgx,"_",esc_num_vars)
  ][]
}