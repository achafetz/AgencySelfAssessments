## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  Local partners
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
library(patchwork)


# IMPORT DATA -------------------------------------------------------------

df_lp_targets <- read_csv("data/local_target_share.csv")
df_lp_budget <- read_csv("data/local_funding_share.csv")

# MUNGE -------------------------------------------------------------------


df_lp_targets <- df_lp_targets %>% 
  mutate(indicator = ifelse(indicator == "HTS_TST_POS", "HTS_POS", indicator),
         indicator = fct_inorder(indicator))
  
  
v_lp_t <- df_lp_targets %>% 
  ggplot(aes(period, share_loca, group = indicator, label = percent(share_loca, 1))) +
  geom_path(size = 1.5, color = genoa) +
  geom_point(size = 4, color = genoa) +
  geom_text(data = filter(df_lp_targets, period == "FY20"),
            vjust = 1.4, 
            family = "Source Sans Pro", color = "gray40") +
  geom_text(data = filter(df_lp_targets, period == "FY21"),
            vjust = -.8, 
            family = "Source Sans Pro", color = "gray40") +
  facet_grid(~ indicator) +
  scale_x_discrete(expand = c(.1, .1)) +
  labs(x = NULL, y = NULL,
       title = toupper("USAID has seen a rapid movement toward local partners between fy20-21"),
       subtitle = "Share of targets allocated to local partners") +
  si_style_ygrid() +
  theme(panel.spacing = unit(.1, "lines"),
        axis.text.y = element_blank())

 # si_save("out/TZA_LP_targets.png", width = 10, heigh = 5)
 
 
 df_lp_budget <- df_lp_budget %>% 
   mutate(actual = case_when(period %in% c("COP18", "COP19", "COP20") ~ share),
          planned = case_when(period %in% c("COP20", "COP21", "COP22") ~ share))

 v_lp_b <- df_lp_budget %>% 
   ggplot(aes(period, group = "x")) +
   geom_path(aes(y = planned), size = 1, linetype = "dashed", color = scooter, na.rm = TRUE) +
   geom_path(aes(y = actual), size = 1, color = scooter, na.rm = TRUE) +
   geom_point(aes(y = planned),
              size = 5, shape = 21, stroke = 1.5,
              color = scooter, fill = "white",
              na.rm = TRUE) +
   geom_point(aes(y = actual),
              size = 5, shape = 21, stroke = 1.5,
              color = "white", fill = scooter,
              na.rm = TRUE) +
   geom_text(aes(y = share, label = percent(share, 1)),
             vjust = -.9,
             family = "Source Sans Pro", color = "gray30") +
   expand_limits(y = 1) +
   scale_x_discrete(expand = c(.05, .05)) +
   labs(x = NULL, y = NULL,
        title = toupper("Anticipated growth in USAID funding toward local partners"),
        subtitle = "Share of total COP funding allocated to local partners",
        caption = "Note: Total funding without GHSC") +
   si_style_nolines() +
   theme(axis.text.y = element_blank())
 
 
 # si_save("out/TZA_LP_budget.png", width = 10, heigh = 2)

 
 (v_lp_b / v_lp_t) +
   plot_layout(heights = c(1, 3))
 
 si_save("out/TZA_LP.png", width = 7.5, height = 6)
 