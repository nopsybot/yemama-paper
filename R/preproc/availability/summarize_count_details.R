summarize_count_details <- function(data,outcome,modvars) {
  
  data %>%
    fsubset(.[[outcome]]==1) %>% 
    gv(modvars) %>%
    lapply(
      \(x) {if(is.factor(x) | is.character(x)){
        c(Total = fnobs(x), 
          fnobs(x[!is.na(x)],g=x[!is.na(x)]), # %>% rev(), # to control presented order
          missing=fsum(is.na(x))) %>% 
          qDT(row.names.col = "count_cat") %>% 
          mtt(cat = c(NA,rep("cat",length(count_cat)-2),NA))
      } else if(is.numeric(x)|is.integer(x)){
        c(Total = fnobs(x),missing = fsum(is.na(x))) %>% 
          qDT(row.names.col = "count_cat")
      }}
    ) %>% 
    rowbind(idcol = "var",fill=TRUE) %>% 
    frename(.x=.,value = `.`) %>% 
    mtt(
      class = sapply(var,\(x) vclasses(data)[names(vclasses(data))==x]),
      label = sapply(var,\(x) fcoalesce(namlab(data[,x,env=list(x=x)])[2:1])),
      footnote = sapply(var,\(x) namlab(data[,x,env=list(x=I(x))],attrn="note")$Label)
    ) %T>% 
    (\(x) tar_assert_true(
      x[is.na(cat),fsum(value,g=var)] %>% all_identical()
    ))()
}
