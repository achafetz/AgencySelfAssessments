## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  loses across age bands
## DATE:     2020-12-02
## UPDATED:  


# DEPEDENCIES -------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(glue)
library(gt)
library(RColorBrewer)
library(extrafont)
library(glitr)
library(scales)
library(ggtext)


# IMPORT DATA -------------------------------------------------------------

df <-  list.files("~/Data", "OU_IM", full.names = TRUE) %>% 
  read_rds() %>% 
  filter(operatingunit == "Tanzania")  

df <-  list.files("~/Data", "OU_IM", full.names = TRUE) %>% 
  read_rds() %>% 
  filter(operatingunit == "Tanzania")  

df_tx_cat <- df %>% 
  filter(fundingagency == "USAID",
         indicator %in% c("TX_CURR", "TX_NEW", "TX_RTT", "TX_NET_NEW"),
         standardizeddisaggregate == "Age/Sex/HIVStatus",
         fiscal_year == 2020,
         ageasentered != "Unknown Age") %>% 
  group_by(fundingagency, indicator, fiscal_year, ageasentered) %>% 
  summarize(cumulative = sum(cumulative, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(indicator = factor(indicator, c("TX_CURR", "TX_NEW", "TX_RTT","TX_NET_NEW")),
         age_color = ifelse(ageasentered %in% c("25-29", "30-34", "35-39"), scooter, "#7f7f7f"))


df_tx_cat %>% 
  ggplot(aes(cumulative, fct_rev(ageasentered), fill = age_color)) +
  geom_blank(aes(x = 15000)) +
  geom_col() +
  # facet_grid(~indicator) +
  facet_grid(~indicator, scales = "free_x") +
  scale_x_continuous(label = comma) +
  scale_fill_identity() +
  labs(x = NULL, y = NULL,
       title = "HIGH PROPORTION NEW AND RETURNED TO TX AGED 25-39 NOT REFLECTED IN NET NEW",
       caption = "USAID Only
       Source: FY20Q4i MSD") +
  si_style_xgrid()

si_save("out/TZA_TX_Categories.png", #height = 9.63, 
        width = 10, height = 6.44)
