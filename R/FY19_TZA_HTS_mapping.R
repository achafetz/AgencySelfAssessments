## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  Maps testing trends
## DATE:     2019-11-30
## UPDATED:  


# Dependencies ------------------------------------------------------------

library(tidyverse)
library(sf)
library(RColorBrewer)


# Import Shapefile and Data -----------------------------------------------

shp_tza <- st_read("data/Tanzania_PROD_4_Region_RegionLsib_2019_May.shp") %>% 
  mutate(snu1 = as.character(orgunit_na)) %>% 
  select(snu1, geometry)

#pull data from FY19_TZA_SelfAssessment_95s_regional.R

hts_pos <- df_viz %>% 
  group_by(period, snu1, agesex, indicator) %>% #snu1
  summarise_at(vars(val), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  filter(indicator == "HTS_TST_POS",
         str_detect(agesex, "Unknown Age", negate = TRUE),
         snu1 != "_Military Tanzania") %>%
  arrange(snu1, agesex, period) 

positivity <- df_viz %>% 
  filter(indicator == "Positivity",
         str_detect(agesex, "Unknown Age", negate = TRUE),
         snu1 != "_Military Tanzania") %>%
  arrange(snu1, agesex, period)

# Create map theme --------------------------------------------------------


map_theme <- function() {
  theme(text = element_text(family = "Gill Sans MT", size = 8),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        # panel.border = element_blank(),
        panel.grid.major = element_line(colour="white"),
        panel.grid.minor = element_line(colour="white"),
        panel.background = element_blank(),
        legend.position = "bottom", 
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold")
        
  )
}

palette <- brewer.pal("GnBu", n = 5)

# Munge -------------------------------------------------------------------

#join shapefile to data & create quantiles for mappping
  df_tza_hts <- left_join(hts_pos, shp_tza, by = "snu1") %>% 
    mutate(quantile = ntile(.$val, 5))

  df_tza_positivity <- left_join(positivity, shp_tza, by = "snu1") %>% 
    mutate(quantile = ntile(.$val, 5))


# Function for creating the legend ----------------------------------------

ntile_legend <- function(col, n = 5){
  col %>% 
    quantile(., seq(0, 1, 1/n), na.rm = TRUE) %>% 
    round(digits = 0) %>% 
    tibble(breaks = .) %>% 
    mutate(breaks_formatted = scales::comma(breaks),
           quantile = 1:n(),
           legend_lab = paste(breaks_formatted, "-", lead(breaks_formatted))) %>%
    filter(breaks != max(breaks)) %>% 
    pull(legend_lab)
}

ntile_legend_pct <- function(col, n = 5){
  col %>% 
    quantile(., seq(0, 1, 1/n), na.rm = TRUE) %>% 
    round(digits = 3) %>% 
    tibble(breaks = .) %>% 
    mutate(breaks_formatted = scales::percent(breaks, .1),
           quantile = 1:n(),
           legend_lab = paste(breaks_formatted, "-", lead(breaks_formatted))) %>%
    filter(breaks != max(breaks)) %>% 
    pull(legend_lab)
}




# Map ---------------------------------------------------------------------

lgnd_pos <- ntile_legend(df_tza_hts$val)

map_pos <- df_tza_hts %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = factor(quantile)), color = "white", na.rm = TRUE) +
  scale_fill_manual(values = palette, 
                    labels = lgnd_pos,
                    name = "Positive Tests") +
  facet_grid(agesex ~ period, switch = "y") +
  map_theme() +
  labs(title = "USAID Trends in Positive Tests",
       caption = "Source: FY19Q4i MSD")


lgnd_positivity <- ntile_legend_pct(df_tza_positivity$val)

map_positivity <- df_tza_positivity %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = factor(quantile)), color = "white", na.rm = TRUE) +
  scale_fill_manual(values = palette, 
                    labels = lgnd_positivity,
                    name = "Positivity") +
  facet_grid(agesex ~ period, switch = "y") +
  map_theme() +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = "USAID Trends in Test Positivity",
       caption = "Source: FY19Q4i MSD")

lo <- "#d0deee"
hi <- "#26456a"
v_combo <- grid.arrange(map_pos, map_positivity, nrow = 2) 
ggsave("out/TZA_SelfAssessment_MAP_Pos+Positivity.png", v_combo, dpi = 300,
       height = 5.6, width = 10, units = "in")
