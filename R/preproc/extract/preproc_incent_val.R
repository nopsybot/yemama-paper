preproc_incent_val <- function(extracts) {
  
  extracts %>% 
    mtt(
      incent_currency.conv = nif(
        incent_currency=="other" &
          record_id %in% c("Turri_2023_7947_vr","Michel_2023_8125_vr"),
        "swiss francs CHF",
        incent_currency=="other" & record_id %in% c("Domoff_2021_3195_us_aa"),
        "pound_sterling £",
        default = as.character(incent_currency)
      ) %>% 
        setLabels(attrn="feature","design"),
      
      # Exchange 
      inc_val_pot_max.usd = nif(#use priceR package for conversion and inflation?
        incent_currency.conv=="dollar $",inc_val_pot_max,
        incent_currency.conv=="euro €",inc_val_pot_max*(1/0.9),
        incent_currency.conv=="swiss francs CHF",inc_val_pot_max*(1/1),
        incent_currency.conv=="pound_sterling £",inc_val_pot_max*(1/0.8),
        default = NA_real_
      ) %>% 
        setLabels("Monetary incentive ($)") %>% 
        setLabels(attrn="feature","design")
    )
  
}
