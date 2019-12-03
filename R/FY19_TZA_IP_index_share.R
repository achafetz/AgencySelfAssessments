## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  TZA IP shares of index testing
## DATE:     2019-12-03
## UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)


# IMPORT DATA -------------------------------------------------------------

df_mer <- list.files("~/ICPI/Data", "OU_IM", full.names = TRUE) %>% 
  read_rds()

df_mer %>% 
  filter(operatingunit == "Tanzania",
         fundingagency == "USAID",
         indicator %in% c("HTS_TST_POS"),
         standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result",
                                         "Modality/Age/Sex/Result")) %>% 
  left_join(df_mech_map, by = "mech_code") %>% 
  mutate(modality = case_when(str_detect(modality, "Index") ~ "Index",
                              !indicator %in% c("HTS_TST", "HTS_TST_POS") ~ as.character(NA),
                              TRUE ~ "Non-Index")) %>% 
  group_by(fiscal_year, partner, indicator, modality) %>% #snu1
  summarise_if(is.double, sum, na.rm = TRUE) %>% 
  ungroup() %>%
  reshape_msd("long") %>% 
  mutate(period = str_remove(period, "20") %>% toupper) %>% 
  filter(partner %in% c("EGPAF", "Deloitte"),
         period == "FY19Q4") %>% 
  group_by(period, partner, indicator) %>% 
  mutate(share = val/sum(val)) %>% 
  ungroup() %>% 
  filter(modality == "Index")
