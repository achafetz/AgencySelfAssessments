## PROJECT:  AGENCY SELF ASSESSMENT
## AUTHOR:   A.CHAFETZ | USAID
## PURPOSE:  Map EID coverage, PEPFAR
## DATE:     2019-12-03
## UPDATED:  


# Dependencies ------------------------------------------------------------

library(tidyverse)
library(sf)
library(RColorBrewer)


# Import Shapefile and Data -----------------------------------------------

shp_tza <- st_read("data/Tanzania_PROD_4_Region_RegionLsib_2019_May.shp") %>% 
  mutate(snu1 = as.character(orgunit_na)) %>% 
  select(snu1, geometry)

# IMPORT DATA -------------------------------------------------------------

df_mer <- list.files("~/ICPI/Data", "PSNU_IM", full.names = TRUE) %>% 
  read_rds()


# MUNGE -------------------------------------------------------------------


df_eid <- df_mer %>% 
  filter(indicator == "PMTCT_EID",
         operatingunit == "Tanzania",
         standardizeddisaggregate %in% c("Total Denominator", "Total Numerator"),
         fiscal_year == 2019) %>% 
  group_by(snu1, indicator, numeratordenom, fiscal_year) %>% 
  summarise_at(vars(cumulative), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  spread(numeratordenom, cumulative) %>% 
  mutate(coverage = N/D)
 

regions <- df_mer %>% 
  filter(operatingunit == "Tanzania") %>% 
  distinct(snu1) %>% 
  filter(!snu1 %in% c("_Military Tanzania", NA))

df_eid <- left_join(regions, df_eid)

#join shapefile to data & create quantiles for mappping
  df_tza_eid <- left_join(df_eid, shp_tza, by = "snu1") %>% 
    mutate(quantile = ntile(.$coverage, 5),
           lab = case_when(!is.na(indicator) ~ snu1))


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


# Function for creating the legend ----------------------------------------

  ntile_legend_pct <- function(col, n = 5){
    col %>% 
      quantile(., seq(0, 1, 1/n), na.rm = TRUE) %>% 
      round(digits = 3) %>% 
      tibble(breaks = .) %>% 
      mutate(breaks_formatted = scales::percent(breaks, 1),
             quantile = 1:n(),
             legend_lab = paste(breaks_formatted, "-", lead(breaks_formatted))) %>%
      filter(breaks != max(breaks)) %>% 
      pull(legend_lab)
  }




# Map ---------------------------------------------------------------------

#eid
lgnd <- ntile_legend_pct(df_tza_eid$coverage)

df_tza_eid %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = factor(quantile)), color = "gray", na.rm = TRUE) +
  geom_sf_text(aes(geometry = geometry, label = lab,
                   color = ifelse(quantile == 5, "white","gray30")),
               inherit.aes = FALSE, size = 2, family = "Gill Sans MT",
               na.rm = TRUE) +
  scale_color_manual(values = c("gray30", "white"), guide = "none") +
  scale_fill_manual(na.translate = TRUE, na.value = "#f0f0f0",
                    values = palette, 
                    labels = lgnd,
                    name = "EID Coverage") +
  map_theme() +
  guides(fill = guide_legend(override.aes = list(size = 0.5))) +
  labs(title = "FY19 PEPFAR EID Coverage",
       caption = "Source: FY19Q4i MSD",
       x = NULL, y = NULL) +
  theme(text = element_text(family = "Gill Sans MT", size = 10),
        legend.text = element_text(size = 8),
        legend.position = "right", 
        plot.caption = element_text(color = "gray30"))

ggsave("out/TZA_SelfAssessment_MAP_EID_coverage.png",
       dpi = 300, width = 4.1, height = 4.6)

