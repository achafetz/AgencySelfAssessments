## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  Visualize DREAMs package completition
## DATE:     2019-11-26
## UPDATED:  2019-11-30


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(scales)
library(extrafont)
library(gridExtra)


# IMPORT DATA -------------------------------------------------------------

df_mer <- list.files("~/ICPI/Data", "OU_IM", full.names = TRUE) %>% 
  read_rds()


# MUNGE -------------------------------------------------------------------

df_agyw <- df_mer %>% 
  filter(operatingunit == "Tanzania",
         #fundingagency == "USAID",
         str_detect(indicator, "AGYW"),
         fiscal_year == 2019,
         ageasentered %in% c("10-14", "15-19", "20-24"),
         str_detect(standardizeddisaggregate, "Age/Sex/Time")) %>% 
  mutate(type = str_extract(standardizeddisaggregate, "Incomplete|Complete")) %>% 
  group_by(fundingagency, ageasentered, type) %>% 
  summarise_at(vars(cumulative), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  spread(type, cumulative) %>% 
  mutate(Total = Complete + Incomplete,
         share = Complete/Total)
 

df_agyw %>% 
  ggplot(aes(x = ageasentered, fill = ageasentered, color = ageasentered)) +
  geom_col(aes(y = Total), fill = "white", size = 1) + 
  geom_col(aes(y = Complete)) +
  geom_blank(aes(y = 1.1 * Total)) +
  geom_text(aes(y = Total, label = percent(share, 1)),
            family = "Gill Sans MT", vjust = -1) +
  geom_hline(aes(yintercept = 0), size = 1, color = "gray30") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#26456a", "#335B8E", "#739bcc")) +
  scale_color_manual(values = c("#26456a", "#335B8E", "#739bcc")) +
  labs(x = NULL, y = NULL, 
       title = "DREAMS Primary Package Completition",
       subtitle = "completition of primary/primary+",
       caption = "FY19Q4i MSD") +
  theme_minimal() +
  theme(text = element_text(family = "Gill Sans MT"),
        plot.caption = element_text(color = "gray30", size = 10),
        legend.position = "none")

ggsave("out/TZA_SelfAssessment_DREAMS.png",
       dpi = 300, width = 4.1, height = 4.6)
