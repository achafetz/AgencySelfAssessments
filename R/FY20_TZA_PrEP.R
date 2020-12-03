## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  prep trend
## DATE:     2020-12-01
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



# MUNGE -------------------------------------------------------------------

df_prep <- df %>% 
  filter(fundingagency == "USAID",
         indicator %in% c("PrEP_NEW", "PrEP_CURR"),
         standardizeddisaggregate == "Total Numerator") %>% 
  group_by(indicator, fiscal_year) %>% 
  summarise(across(c(cumulative, targets), sum, na.rm = TRUE)) %>% 
  ungroup()

df_prep <- df_prep %>% 
  mutate(period = as.character(fiscal_year)  %>% str_replace("20", "FY"))
df_prep %>% 
  ggplot(aes(period, cumulative)) +
  geom_col(fill = scooter) +
  geom_errorbar(aes(y = targets, ymin = targets, ymax= targets), 
                size = 1.1, color = old_rose) +
  facet_wrap(~fct_rev(indicator)) +
  scale_y_continuous(label = comma) +
  labs(x = NULL, y = NULL,
       title = "MASSIVE INCREASE IN USAID FY21 PrEP <span style='color:#c43d4d'>TARGETS</span> <br>REQUIRE ADVOCACY",
       caption = "Source: FY20Q4i MSD") +
  si_style_ygrid() +
  theme(plot.title = element_markdown())


si_save("out/TZA_PrEP.png", height= 5.5, width = 5.5)
