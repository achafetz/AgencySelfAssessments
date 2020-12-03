## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  MMD trend
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


# IMPORT DATA -------------------------------------------------------------

df <-  list.files("~/Data", "OU_IM", full.names = TRUE) %>% 
  read_rds() %>% 
  filter(operatingunit == "Tanzania")  


# MUNGING -----------------------------------------------------------------


df_mmd_share <- df %>% 
  filter(fundingagency == "USAID",
         indicator == "TX_CURR",
         standardizeddisaggregate %in% c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus"),
         fiscal_year == 2020) %>% 
  mutate(otherdisaggregate = case_when(is.na(otherdisaggregate) ~ "total",
                                       otherdisaggregate == "ARV Dispensing Quantity - Less than 3 months" ~ "mmd_u3",
                                       TRUE ~ "mmd_o3"))

df_mmd_share <- df_mmd_share %>% 
  reshape_msd(clean = TRUE) %>% 
  filter(period_type == "results")

df_mmd_share <- df_mmd_share %>% 
  group_by(fundingagency, indicator, otherdisaggregate, period) %>% 
  summarise(across(c(val), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(otherdisaggregate, val) %>% 
  filter(total > 0) %>% 
  mutate(mmd_unk = total - mmd_o3 - mmd_u3,
         share_o3 = mmd_o3 / total) %>% 
  mutate(indicator = "MMD +3mo") 

df_mmd_share %>% 
  ggplot(aes(period, share_o3, label = percent(share_o3, 1))) +
  geom_blank(aes(y = share_o3 * 1.1))+
  geom_col(fill = genoa) +
  geom_text(family = "Source Sans Pro", color = "gray30",
            size = 6, vjust = -1) +
  labs(x = NULL, y = NULL, 
       title = "3+ MONTH MMD INCREASED STEADILY CONTRIBUTING TO CLIENT RETENTION",
       caption = "Source: FY20Q4i MSD") +
  si_style_nolines() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14))

si_save("out/TX_MMD_share.png", height= 3.6, width = 6.6)
