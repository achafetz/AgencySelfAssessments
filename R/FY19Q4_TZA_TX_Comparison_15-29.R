## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  Visualize trends for TX for 15-29
## DATE:     2019-12-03


#dependencies

library(tidyverse)
library(ICPIutilities)
library(scales)
library(extrafont)
library(gridExtra)

path_msd <- list.files("~/ICPI/Data", "OU_IM", full.names = TRUE)

df_msd <- read_rds(path_msd)

df_tx <- df_msd %>% 
  filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus"),
         fundingagency == "USAID",
         operatingunit == "Tanzania",
         fiscal_year > 2017,
         ageasentered %in% c("15-19", "20-24", "25-29"),
         #sex == "Female"
         ) %>% 
  mutate(age = "15-29") %>% 
  unite(sex, c("sex", "age"), sep = " ")

df_tx <- df_tx %>% 
  group_by(indicator, fundingagency, fiscal_year, sex) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  reshape_msd("long") %>% 
  filter(str_detect(period, "q")) %>% 
  arrange(indicator, period) 


df_viz <- df_tx %>% 
  mutate(period = str_remove(period, "20") %>% toupper,
         lab = case_when(str_detect(period, "Q(2|4)") ~ comma(val)),
         pd_fill = case_when(period == "FY19Q4" ~ "z_now",
                             str_detect(period, "19") ~ "y_rest of fy19",
                             TRUE ~ "x_earlier"))


df_viz %>% 
  ggplot(aes(period, val, fill = pd_fill)) +
  geom_col() +
  geom_blank(aes(y = 1.2 * val)) +
  geom_text(aes(label = lab, #round(val/1000, 1) %>% paste0("k"),
                vjust = ifelse(val <0, 1, -.8)), size = 3,
            family = "Gill Sans MT", color = "gray30") +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL, caption = "Source: FY19Q4i MSD") +
  scale_fill_manual(values =  c("#739bcc", "#335B8E", "#26456a")) +
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels = c("FY18Q1", "", "FY18Q3", "",
                              "FY19Q1", "", "FY19Q3", "")) +
  facet_grid(indicator ~ sex) +
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        text = element_text(family = "Gill Sans MT"),
        axis.text.y = element_blank(),
        #panel.grid.major.x = element_blank(),
        strip.text = element_text(size = 16),
        plot.caption = element_text(size = 9, color = "gray30"),
        legend.position = "none")

ggsave("out/TX_NET_NEW_TZA_USAID_Trends_Sex.png",
       height = 5.3, width = 9, units = "in", dpi = 300)
