## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  recreate OVC linked to ART
## DATE:     2019-12-03
## UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)


# IMPORT DATA -------------------------------------------------------------

df_mer <- list.files("~/ICPI/Data", "OU_IM", full.names = TRUE) %>% 
  read_rds()


# MUNGE -------------------------------------------------------------------

df_mer %>% 
  filter(indicator == "OVC_HIVSTAT",
         fiscal_year == 2018) %>% 
  count(standardizeddisaggregate, wt = cumulative)

df_ovc <- df_mer %>% 
  filter(indicator == "OVC_HIVSTAT",
         operatingunit == "Tanzania",
         fundingagency == "USAID",
         standardizeddisaggregate %in% c("StatusPosART", "ReportedStatus"),
         otherdisaggregate %in% c("Receiving ART")) %>% 
  group_by(indicator, fiscal_year) %>% 
  summarise_if(is.double, sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  reshape_msd("long") %>% 
  filter(str_detect(period, "q")) %>% 
  arrange(period) %>% 
  mutate(period = str_remove(period, "20") %>% toupper)



df_ovc_denom <- df_mer %>% 
  filter(indicator == "OVC_HIVSTAT",
         operatingunit == "Tanzania",
         fundingagency == "USAID",
         standardizeddisaggregate == "ReportedStatus",
         statushiv == "Positive") %>% 
  group_by(indicator, fiscal_year) %>% 
  summarise_if(is.double, sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  reshape_msd("long") %>% 
  filter(str_detect(period, "q")) %>% 
  arrange(period) %>% 
  mutate(period = str_remove(period, "20") %>% toupper) %>% 
  rename(denom = val)

df_ovc_viz <- df_ovc %>% 
  left_join(df_ovc_denom) %>% 
  mutate(share = percent(val/denom, 1),
         share_y = max(val) + 6000)

df_ovc_viz %>% 
  ggplot(aes(period, val)) +
  geom_col(fill = "#26456a") +
  geom_blank(aes(y = val * 1.4)) +
  geom_point(aes(y = share_y), color = "#739bcc", size = 13) +
  geom_text(aes(label = comma(val)), vjust = -.9, color = "gray30",
            family = "Gill Sans MT") +
  geom_text(aes(y = share_y, label = share), size = 4, 
            color = "white", family = "Gill Sans MT") +
  labs(x = NULL, y = NULL,
       title = "HIV positives OVCs", 
       subtitle = "linked to treatment and total receiving ART",
       caption = "Source: FY19Q4i MSD") +
  theme_minimal() +
  theme(text = element_text(family = "Gill Sans MT", size = 14),
        plot.caption = element_text(color = "gray30", size = 10),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

ggsave("out/TZA_OVC_HIVStat.png",dpi = 300,
       height = 3.1, width = 4.6)
