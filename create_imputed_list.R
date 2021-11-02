# prepares data of the International Civic and Citizenship Education Study 
# with plausible values for use in Mplusautomation package
# please merge student questionnaire files with PVs and import as sav file first
# https://www.iea.nl/data-tools/repository/iccs
library(dplyr)

create_imputed_list <- function(country = NA, dat = iccs_data) {
  # create unique IDs for stundets and classes
  dat$uniqstud <- (dat$IDCNTRY * 100000000) + dat$IDSTUD
  dat$uniqclas <- (dat$IDCNTRY * 1000000) + dat$IDCLASS
  # split plausible values and rest of data
  # by country ID or for all countries
  if(is.na(country)) {
    df_pv <- dat %>%
      select(., "uniqstud", "PV1CIV", "PV2CIV", "PV3CIV", "PV4CIV", "PV5CIV") %>%
      round(., 1)
    df_no_pv <- dat %>%
      select(., -"PV1CIV", -"PV2CIV", -"PV3CIV", -"PV4CIV", -"PV5CIV") 
  } else {
    df_pv <- dat %>%
      filter(., IDCNTRY == country) %>% 
      select(., "uniqstud", "PV1CIV", "PV2CIV", "PV3CIV", "PV4CIV", "PV5CIV") %>%
      round(., 1)
    df_no_pv <- dat %>%
      filter(., IDCNTRY == country) %>% 
      select(., -"PV1CIV", -"PV2CIV", -"PV3CIV", -"PV4CIV", -"PV5CIV")
  }
  # create empty list
  imputed_pv_func <- list()
  
  # attach one data frame per PV to list
  for(i in 1:5) {
    print(i)
    pvciv <- paste0("PV", i, "CIV")
    print(pvciv)
    pv_temp <- select(df_pv, "uniqstud", pvciv) %>%
      rename(., "PVCIV" = pvciv)
    imp_temp <- left_join(df_no_pv, pv_temp)
    head(imp_temp)
    imputed_pv_func[[i]] <- data.frame(imp_temp)
    rm(imp_temp)
  }
  return(imputed_pv_func)
}

# test_list <- create_imputed_list(100)
