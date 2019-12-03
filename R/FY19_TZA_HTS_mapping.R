## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  Maps testing trends
## DATE:     2019-11-30
## UPDATED:  


# Dependencies ------------------------------------------------------------

library(tidyverse)
library(sf)
library(RColorBrewer)
library(ggrepel)


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

regions_usaid <- c("Morogoro", "Arusha",  "Tabora", 
                   "Kilimanjaro", "Mtwara", "Dodoma", 
                   "Singida", "Iringa", "Lindi", 
                   "Njombe", "Manyara")

regions <- df_mer %>% 
  filter(operatingunit == "Tanzania") %>% 
  distinct(snu1) %>% 
  filter(!snu1 %in% c("_Military Tanzania", NA))

df_index_positivity <- df_mer %>% 
  filter(operatingunit == "Tanzania",
         fundingagency == "USAID",
         snu1 %in% regions_usaid,
         indicator %in% c("HTS_TST_POS", "HTS_TST"),
         standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result",
                                         "Modality/Age/Sex/Result"),
         str_detect(modality, "Index"),
         fiscal_year == 2019) %>% 
  group_by(snu1, indicator, fiscal_year) %>% 
  summarise_at(vars(cumulative), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  spread(indicator, cumulative) %>% 
  mutate(Positivity = HTS_TST_POS/HTS_TST) %>% 
  ungroup() %>% 
  gather(indicator, val, -snu1, -fiscal_year) %>% 
  mutate(indicator = paste("Index", indicator)) %>% 
  filter(indicator == "Index Positivity") %>% 
  left_join(regions, .)

df_index_share <- df_mer %>% 
  filter(operatingunit == "Tanzania",
         fundingagency == "USAID",
         snu1 %in% regions_usaid,
         indicator == "HTS_TST_POS",
         standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result",
                                         "Modality/Age/Sex/Result"),
         fiscal_year == 2019) %>% 
  mutate(modality = case_when(str_detect(modality, "Index") ~ "Index",
                              !indicator %in% c("HTS_TST", "HTS_TST_POS") ~ as.character(NA),
                              TRUE ~ "Non-Index")) %>% 
  group_by(snu1, indicator, fiscal_year, modality) %>% 
  summarise_at(vars(cumulative), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  group_by(snu1) %>% 
  mutate(share = cumulative/sum(cumulative, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(modality == "Index") %>% 
  left_join(regions, .)


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
  
  df_tza_index_share <- left_join(df_index_share, shp_tza, by = "snu1") %>% 
    mutate(quantile = ntile(.$share, 5),
           lab = case_when(!is.na(indicator) ~ snu1))
  
  df_tza_index_positivity <- left_join(df_index_positivity, shp_tza, by = "snu1") %>% 
    mutate(quantile = ntile(.$val, 5),
           lab = case_when(!is.na(indicator) ~ snu1))


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

#hts
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

#positivity
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

#combine output
  v_combo <- grid.arrange(map_pos, map_positivity, nrow = 2)
  
  ggsave("out/TZA_SelfAssessment_MAP_Pos+Positivity.png", v_combo, dpi = 300,
         height = 5.6, width = 10, units = "in")


#index
  lgnd_index <- ntile_legend_pct(df_tza_index_share$share)
  
  v_index1 <- df_tza_index_share %>% 
    ggplot() +
    geom_sf(aes(geometry = geometry, fill = factor(quantile)), color = "gray", na.rm = TRUE) +
    geom_sf_text(aes(geometry = geometry, label = lab,
                     color = ifelse(quantile == 5, "white","gray30")),
                 inherit.aes = FALSE, size = 3, family = "Gill Sans MT",
                 na.rm = TRUE) +
    scale_color_manual(values = c("gray30", "white"), guide = "none") +
    scale_fill_manual(na.translate = TRUE, na.value = "#f0f0f0",
                      values = palette, 
                      labels = lgnd_index,
                      name = "Index Share of Positives") +
    map_theme() +
    guides(fill = guide_legend(override.aes = list(size = 0.5))) +
    labs(title = "FY19 USAID Regional Shares of Index Positives",
         #caption = "Source: FY19Q4i MSD",
         x = NULL, y = NULL) +
    theme(text = element_text(family = "Gill Sans MT", size = 10),
          legend.text = element_text(size = 8),
          legend.position = "right", 
          plot.caption = element_text(color = "gray30"))
  
  ggsave("out/TZA_SelfAssessment_MAP_Index_Share.png", dpi = 300,
         height = 5.6, width = 6, units = "in")
  
  
#index positivity
  lgnd_index_positivity <- ntile_legend_pct(df_tza_index_positivity$val)
  
  v_index2 <- df_tza_index_positivity %>% 
    ggplot() +
    geom_sf(aes(geometry = geometry, fill = factor(quantile)), color = "gray", na.rm = TRUE) +
    geom_sf_text(aes(geometry = geometry, label = lab,
                     color = ifelse(quantile == 5, "white","gray30")),
                 inherit.aes = FALSE, size = 3, family = "Gill Sans MT",
                 na.rm = TRUE) +
    scale_color_manual(values = c("gray30", "white"), guide = "none") +
    scale_fill_manual(na.translate = TRUE, na.value = "#f0f0f0",
                      values = palette, 
                      labels = lgnd_index_positivity,
                      name = "Index Positivity") +
    map_theme() +
    guides(fill = guide_legend(override.aes = list(size = 0.5))) +
    labs(title = "FY19 USAID Regional Index Positivity",
         caption = "Source: FY19Q4i MSD",
         x = NULL, y = NULL) +
    theme(text = element_text(family = "Gill Sans MT", size = 10),
          legend.text = element_text(size = 8),
          legend.position = "right", 
          plot.caption = element_text(color = "gray30"))


  v_combo <- grid.arrange(v_index1, v_index2, nrow = 1)
  
  ggsave("out/TZA_SelfAssessment_MAP_Index_Share+Positivity.png", v_combo, dpi = 300,
         height = 5.6, width = 10, units = "in")
  