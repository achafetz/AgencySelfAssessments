## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  clean up TLD graphic
## DATE:     2019-12-03
## UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(scales)
library(extrafont)
library(gridExtra)


# IMPORT DATA -------------------------------------------------------------

#MMD data from A.Amoah (2019-12-02)
df_tld <- read_excel("data/TLD_transition.xlsx")

# VIZ ---------------------------------------------------------------------

df_tld %>% 
  ggplot(aes(x = sex, fill = sex, color = sex)) +
  geom_col(aes(y = TX_CURR), fill = "white", size = 1) + 
  geom_col(aes(y = TLD)) +
  geom_blank(aes(y = 1.1 * TX_CURR)) +
  geom_text(aes(y = TX_CURR, label = percent(TLD_share, 1)),
            family = "Gill Sans MT", vjust = -1) +
  geom_hline(aes(yintercept = 0), size = 1, color = "gray30") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#26456a", "#335B8E", "#739bcc")) +
  scale_color_manual(values = c("#26456a", "#335B8E", "#739bcc")) +
  labs(x = NULL, y = NULL, 
       title = "TLD patients as a share of TX_CURR",
       caption = "EGPAF Program Data") +
  theme_minimal() +
  theme(text = element_text(family = "Gill Sans MT"),
        plot.caption = element_text(color = "gray30", size = 10),
        legend.position = "none")

ggsave("out/TZA_SelfAssessment_EGPAF_TLD.png",
       dpi = 300, width = 4.1, height = 4.6)
