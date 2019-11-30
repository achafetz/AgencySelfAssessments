## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  Visualize trends for the 90s indicators
## DATE:     2019-11-19
## UPDATED:  

#dependencies

library(tidyverse)
library(ICPIutilities)
library(scales)
library(extrafont)
library(gridExtra)

path_msd <- list.files("ICPI/Data", "OU_IM", full.names = TRUE)

path_genie <- "../Downloads/PEPFAR-Data-Genie-OUByIMs-2019-11-19/Genie_Daily_c2a76a83ee734074819d3863be59a08d.txt"

df_msd <- read_rds(path_msd)

df_genie <- read_tsv(path_genie, col_types = c(.default = "c")) %>% 
  select(-c("dataelementuid", "approvallevel", "approvalleveldescription", 
            "categoryoptioncombouid")) %>% 
  mutate(fiscal_year = as.integer(fiscal_year)) %>% 
  mutate_at(vars(targets:cumulative), as.double)

df_tx <- df_msd %>% 
  filter(fiscal_year != 2019,
         indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
         standardizeddisaggregate == "Total Numerator")

df_tx_fy19q4 <- df_genie %>% 
  filter(fiscal_year == 2019,
         indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
         standardizeddisaggregate == "Total Numerator")

df_tx_combo <- bind_rows(df_tx, df_tx_fy19q4) 

df_tx_combo <- df_tx_combo %>% 
  filter(fundingagency == "USAID",
         operatingunit == "Tanzania")

df_tx_combo <- df_tx_combo %>% 
  group_by(indicator, fundingagency, fiscal_year) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  reshape_msd("long") %>% 
  filter(str_detect(period, "q")) %>% 
  arrange(indicator, period) 

# df_tx_combo <- df_tx_combo %>% 
#   spread(indicator, val) %>% 
#   mutate(`Proxy Retention` = 1 + (TX_NET_NEW - TX_NEW)/(TX_CURR - TX_NET_NEW + TX_NEW)) %>% 
#   gather(indicator, val,-period)


df_viz <- df_tx_combo %>% 
  mutate(period = str_remove(period, "20") %>% toupper,
         pd_fill = case_when(period == "FY19Q4" ~ "z_now",
                             str_detect(period, "19") ~ "y_rest of fy19",
                             TRUE ~ "x_earlier"))


v_curr <- df_viz %>% 
  filter(indicator == "TX_CURR") %>% 
  ggplot(aes(period, val, fill = pd_fill)) +
  geom_col() +
  #geom_text(aes(label = round(val/1000000, 2) %>% paste0("m")),
  geom_text(aes(label = round(val/1000, 0) %>% paste0("k")),
            family = "Gill Sans MT", color = "gray30", size = 3,
            vjust = -1) +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL, caption = "") +
  scale_fill_manual(values = c("#739bcc", "#335B8E", "#26456a")) +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = c("FY17Q1", "", "FY17Q3", "",
                              "FY18Q1", "", "FY18Q3", "",
                              "FY19Q1", "", "FY19Q3", "")) +
  facet_wrap(indicator ~ ., scales = "free_y") +
  theme_light() +
  theme(axis.ticks = element_blank(),
        text = element_text(family = "Gill Sans MT"),
        axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(size = 16),
        legend.position = "none")

v_new <- df_viz %>% 
  filter(indicator %in% c("TX_NEW", "TX_NET_NEW")) %>% 
  ggplot(aes(period, val, fill = pd_fill)) +
  geom_col() +
  geom_blank(aes(y = 1.2 * val)) +
  geom_text(aes(label = round(val/1000, 0) %>% paste0("k"),
                vjust = ifelse(val <0, 1, -.8)), size = 3,
            family = "Gill Sans MT", color = "gray30") +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL, caption = "Source: FY19Q4 Genie Pull [2019-11-19]") +
  scale_fill_manual(values =  c("#739bcc", "#335B8E", "#26456a")) +
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels = c("FY17Q1", "", "FY17Q3", "",
                              "FY18Q1", "", "FY18Q3", "",
                              "FY19Q1", "", "FY19Q3", "")) +
  facet_wrap(. ~ indicator, ncol = 1) +
  theme_light() +
  theme(axis.ticks = element_blank(),
        text = element_text(family = "Gill Sans MT"),
        axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(size = 16),
        plot.caption = element_text(size = 9, color = "gray30"),
        legend.position = "none")

(v_out <- grid.arrange(v_curr, v_new, nrow = 1))

# ggsave("TX_NET_NEW_USAID_Trends.png", plot = v_out, path = "../Downloads",
#        height = 5.63, width = 10, units = "in", dpi = 300)

ggsave("out/TX_NET_NEW_TZA_USAID_Trends.png", plot = v_out,
       height = 5.63, width = 10, units = "in", dpi = 300)

#Ou breakdown

df_ou <- df_tx_combo %>% 
  filter(fundingagency == "USAID") %>% 
  group_by(indicator, operatingunit, fiscal_year) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  reshape_msd("long") %>% 
  filter(str_detect(period, "q")) %>% 
  arrange(operatingunit, indicator, period)

df_ou_gr <- df_ou %>% 
  filter(indicator == "TX_CURR") %>% 
  group_by(operatingunit) %>% 
  mutate(delta = val - lag(val),
         growth = (val - lag(val))/lag(val)) %>% 
  filter(period == "fy2019q4")

#New v Net NEW

df_ratio <- df_tx_combo %>% 
  filter(fundingagency == "USAID") %>% 
  spread(indicator, val) %>% 
  mutate(ratio = TX_NET_NEW/TX_NEW,
         period = str_remove(period, "20") %>% toupper,
         period = as_factor(period)) 

df_ratio %>% 
  ggplot(aes(TX_NEW, TX_NET_NEW)) +
  geom_hline(yintercept = 0) +
  geom_abline(slope = .5, intercept = 0) +
  geom_path(size = 1) +
  geom_point(size = 5) +
  expand_limits(y = 0, x = 0) +
  geom_text(aes(label = period),
            family = "Gill Sans MT", color = "gray30",
            vjust = -.8) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  theme_light() +
  theme(text = element_text(family = "Gill Sans MT"))

#funding agency
df_tx_combo %>% 
  filter(fundingagency %in% c("USAID", "HHS/CDC"),
         indicator %in% c("TX_NET_NEW", "TX_CURR")) %>% 
  mutate(period = str_remove(period, "20") %>% toupper,
         fundingagency = str_remove(fundingagency, "HHS/"),
         fundingagency = factor(fundingagency, c("USAID", "CDC"))) %>% 
  ggplot(aes(period, val, fill = fundingagency)) +
  geom_col() +
  geom_blank(aes(y = 1.2 * val)) +
  geom_text(aes(label = ifelse(val > 1000000, 
                               round(val/1000000, 2) %>% paste0("m"),
                               round(val/1000, 0) %>% paste0("k")),
                vjust = ifelse(val <0, 1, -.8)), size = 3,
            family = "Gill Sans MT", color = "gray30") +
  geom_hline(yintercept = 0) +
  labs(x = NULL, y = NULL, caption = "Source: FY19Q4 Genie Pull [2019-11-19]") +
  scale_fill_manual(values =  c("#335B8E", "#739bcc")) +
  scale_y_continuous(labels = comma)+
  scale_x_discrete(labels = c("FY17Q1", "", "FY17Q3", "",
                              "FY18Q1", "", "FY18Q3", "",
                              "FY19Q1", "", "FY19Q3", "")) +
  facet_grid(indicator ~ fundingagency, scales = "free_y") +
  theme_light() +
  theme(axis.ticks = element_blank(),
        text = element_text(family = "Gill Sans MT"),
        axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(size = 16),
        plot.caption = element_text(size = 9, color = "gray30"),
        legend.position = "none")

ggsave("out/TX_Agency_Comaparison.png",
       height = 5.63, width = 10, units = "in", dpi = 300)
