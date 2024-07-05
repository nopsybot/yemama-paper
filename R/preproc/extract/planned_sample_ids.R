#' Title: Expand planned sample IDs
#' Description: Expand planned record IDs by eventual sample suffixes
#' 
#' @param manifest data.table: The manifest of the preprocessed data
#' @param extr_name character: The names of the extractors to consider
#' 
#' @return character: The planned sample IDs
#' @export 
#'
#' @examples 
#' planned_sample_ids(manifest, "Vanisha")
#' 
planned_sample_ids <- function(
    manifest,
    extr_name = c("Konstantin","Assel","Sophie","Vanisha","Fabienne","Talia")
) {
  abbr <- paste0("abbr_group",2:3)
  cond <- paste0("txt_group",2:3)
  
  c(
    manifest[extractor %in% extr_name & final_identification==1, record_id],
    manifest[
      extractor %in% extr_name & final_identification==1,
      lapply(
        Map( # consider nonmissing and unflagged suppl abbreviations
          \(x,y) ifelse(is.na(get(x))|stringr::str_detect(get(y),"not"),NA_character_,get(x)),
          (abbr),(cond)
        ),
        \(x) stringr::str_remove(record_id,"_\\d?[:lower:].+$") %>% 
          stringr::str_c("_",x)
      )
    ]|> collapse::qM() |> collapse::na_rm()
  ) |> sort()
}
