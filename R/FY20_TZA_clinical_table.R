library(tidyverse)
library(ICPIutilities)
library(glue)
library(gt)
library(RColorBrewer)
library(extrafont)


pal <- brewer.pal(5, "Spectral")[2:5]


df <-  list.files("~/Data", "OU_IM", full.names = TRUE) %>% 
  read_rds()


df_tza <- df %>% 
  filter(operatingunit == "Tanzania",
         indicator %in% c("TX_CURR", "HTS_TST", "HTS_TST_POS", "TX_NET_NEW", "TX_NEW"),
         fundingagency != "Dedup",
         standardizeddisaggregate == "Total Numerator"
         ) %>% 
  mutate(fundingagency = str_remove(fundingagency, "HHS/"))

df_agency <- df_tza %>% 
  group_by(fundingagency, indicator, fiscal_year) %>% 
  summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd(clean = TRUE) %>% 
  filter(period != "FY21Q1")

df_nn <- df_agency %>% 
  filter(indicator == "TX_CURR" & 
         (str_detect(period, "4") | period_type == "targets")) %>% 
  arrange(fundingagency, period) %>% 
  group_by(fundingagency) %>% 
  mutate(indicator = "TX_NET_NEW",
         val = case_when(period_type == "targets" ~ val - lag(val))) %>% 
  ungroup() %>% 
  filter(period_type == "targets",
         !is.na(val))
  


df_agency <- df_agency %>%
  bind_rows(df_nn) %>% 
  filter(str_detect(period, "FY18", negate = TRUE)) %>% 
  mutate(pd = case_when(period_type == "results" ~ str_replace(period, "Q", "\nQ"),
                        period_type == "targets" ~ paste(period, "Targets", sep = "\n"),
                        period_type == "cumulative" ~  paste(period, "Total", sep = "\n")),
         fy = str_sub(period, 3,4),
         q = case_when(period_type == "results" ~ str_sub(period, -1),
                       period_type == "cumulative" ~ "5",
                       period_type == "targets" ~ "6"),
         order = as.numeric(fy) + (as.numeric(q)/100),
         pd = fct_reorder(pd, order),
         fundingagency = factor(fundingagency, c("USAID", "CDC", "DOD")),
         indicator = factor(indicator, c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_NET_NEW"))) %>% 
  select(-c(period, period_type, fy, q, order))

tbl <- df_agency %>% 
  arrange(fundingagency, indicator, pd) %>% 
  pivot_wider(names_from = pd, values_from = val) %>% 
  mutate(`FY19\nAchieved` = `FY19\nTotal` / `FY19\nTargets`,
         `FY20\nAchieved` = `FY20\nTotal` / `FY20\nTargets`) %>% 
  relocate(`FY19\nAchieved`, .after = `FY19\nTargets`) %>% 
  relocate(`FY20\nAchieved`, .after = `FY20\nTargets`) %>% 
  gt(rowname_col = "indicator",
     groupname_col = "fundingagency")

tbl <- tbl %>% 
  tab_header(title = "Agency Performance") %>% 
  tab_options(table.font.names = "Source Sans Pro") %>% 
  cols_label(fundingagency = "",
             indicator = "") 

tbl <- tbl %>% 
  fmt_percent(vars(`FY19\nAchieved`, `FY20\nAchieved`), decimals = 0) %>% 
  fmt_number(vars(`FY19\nQ1`, `FY19\nQ2`, `FY19\nQ3`, `FY19\nQ4`, `FY19\nTotal`, `FY19\nTargets`,
                  `FY20\nQ1`, `FY20\nQ2`, `FY20\nQ3`, `FY20\nQ4`, `FY20\nTotal`, `FY20\nTargets`,
                  `FY21\nTargets`), decimals = 0) 

tbl <- tbl %>% 
  tab_style(
    style = cell_borders(
      sides = "right",
      weight = px(1.5),
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )) %>% 
  tab_style(style = cell_fill(color = pal[4]),
            locations = cells_body(
              columns = vars(`FY19\nAchieved`),
              rows = `FY19\nAchieved` >= 1.1)) %>% 
  tab_style(style = cell_fill(color = pal[3]),
            locations = cells_body(
              columns = vars(`FY19\nAchieved`),
              rows = `FY19\nAchieved` < 1.1)) %>% 
  tab_style(style = cell_fill(color = pal[2]),
            locations = cells_body(
              columns = vars(`FY19\nAchieved`),
              rows = `FY19\nAchieved` < .9)) %>% 
  tab_style(style = cell_fill(color = pal[1]),
            locations = cells_body(
              columns = vars(`FY19\nAchieved`),
              rows = `FY19\nAchieved` < .75)) %>% 
  tab_style(style = cell_fill(color = pal[4]),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` >= 1.1)) %>% 
  tab_style(style = cell_fill(color = pal[3]),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` < 1.1)) %>% 
  tab_style(style = cell_fill(color = pal[2]),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` < .9)) %>% 
  tab_style(style = cell_fill(color = pal[1]),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` < .75)) 

tbl %>% 
cols_width(
  vars(indicator) ~ px(120),
  everything() ~ px(80)
)

  


df_ptnr <- df_tza %>% 
  filter(fundingagency == "USAID") %>% 
  rename_official() %>%
  mutate(primepartner = recode(primepartner,
                          "BAYLOR COLLEGE OF MEDICINE CH ILDREN FOUNDATION TANZANIA" = "Baylor",
                          "DELOITTE CONSULTING LIMITED" = "Deloitte",
                          "Elizabeth Glaser Pediatric Aids Foundation" = "EGPAF",
                          "Elizabeth Glaser Pediatric AIDS Foundation" = "EGPAF",
                          "JHPIEGO CORPORATION" = "Jhpiego",
                          "Family Health International" = "EpiC",
                          "Koninklijke Nederlandse Centrale Vereniging tot Bestrijding der Tuberculose (KNCV)" = "KNVC",
                          "TANZANIA HEALTH PROMOTION SUPP ORT (THPS)" = "THPS",
                          "JSI Research And Training Institute, INC." = "JSI",
                          "ASSOCIATION OF PRIVATE HEALTH FACILITIES IN TANZANIA" = "Tohara Salama")) %>% 
  group_by(primepartner, indicator, fiscal_year) %>% 
  summarise(across(where(is.double), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  reshape_msd(clean = TRUE) %>% 
  filter(period != "FY21Q1")


df_nn_ptnr <- df_ptnr %>% 
  filter(indicator == "TX_CURR" & 
           (str_detect(period, "4") | period_type == "targets")) %>% 
  arrange(primepartner, period) %>% 
  group_by(primepartner) %>% 
  mutate(indicator = "TX_NET_NEW",
         val = case_when(period_type == "targets" ~ val - lag(val))) %>% 
  ungroup() %>% 
  filter(period_type == "targets",
         !is.na(val))



df_ptnr <- df_ptnr %>%
  bind_rows(df_nn_ptnr) %>% 
  filter(str_detect(period, "18", negate = TRUE)) %>% 
  mutate(pd = case_when(period_type == "results" ~ str_replace(period, "Q", "\nQ"),
                        period_type == "targets" ~ paste(period, "Targets", sep = "\n"),
                        period_type == "cumulative" ~  paste(period, "Total", sep = "\n")),
         fy = str_sub(period, 3,4),
         q = case_when(period_type == "results" ~ str_sub(period, -1),
                       period_type == "cumulative" ~ "5",
                       period_type == "targets" ~ "6"),
         order = as.numeric(fy) + (as.numeric(q)/100),
         pd = fct_reorder(pd, order),
         # fundingagency = factor(fundingagency, c("USAID", "CDC", "DOD")),
         indicator = factor(indicator, c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_NET_NEW"))) %>% 
  select(-c(period, period_type, fy, q, order))


df_ptnr_grp <- df_ptnr %>% 
  # filter(!primepartner %in% c("Baylor College of Medicine", "KNVC", "NACOPHA", "THPS", "Tohara Salama"))
  # filter(primepartner %in% c("Baylor", "Deloitte", "EGPAF"))
  filter(primepartner %in% c("EpiC", "JSI"))

tbl_ptnr <- df_ptnr_grp %>% 
  complete(pd, nesting(primepartner, indicator)) %>% 
  arrange(primepartner, indicator, pd) %>%
  pivot_wider(names_from = pd, values_from = val) %>% 
  mutate(`FY19\nAchieved` = `FY19\nTotal` / `FY19\nTargets`,
         `FY20\nAchieved` = `FY20\nTotal` / `FY20\nTargets`) %>% 
  relocate(`FY19\nAchieved`, .after = `FY19\nTargets`) %>% 
  relocate(`FY20\nAchieved`, .after = `FY20\nTargets`) %>% 
  gt(rowname_col = "indicator",
     groupname_col = "primepartner")

tbl_ptnr <- tbl_ptnr %>% 
  tab_header(title = "USAID Partner Performance") %>% 
  tab_options(table.font.names = "Source Sans Pro") %>% 
  cols_label(primepartner = "",
             indicator = "") 

tbl_ptnr <- tbl_ptnr %>% 
  fmt_percent(vars(`FY19\nAchieved`, `FY20\nAchieved`), decimals = 0) %>% 
  fmt_number(vars(`FY19\nQ1`, `FY19\nQ2`, `FY19\nQ3`, `FY19\nQ4`, `FY19\nTotal`, `FY19\nTargets`,
                  `FY20\nQ1`, `FY20\nQ2`, `FY20\nQ3`, `FY20\nQ4`, `FY20\nTotal`, `FY20\nTargets`,
                  `FY21\nTargets`), decimals = 0) 

tbl_ptnr <- tbl_ptnr %>% 
  fmt_missing(columns = everything(),
              missing_text = "")

tbl_ptnr <- tbl_ptnr %>% 
  tab_style(
    style = cell_borders(
      sides = "right",
      weight = px(1.5),
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )) %>% 
  tab_style(style = cell_fill(color = "white"),
            locations = cells_body(
              columns = vars(`FY19\nAchieved`),
              rows = `FY19\nAchieved` == "")) %>%
  tab_style(style = cell_fill(color = pal[4]),
            locations = cells_body(
              columns = vars(`FY19\nAchieved`),
              rows = `FY19\nAchieved` >= 1.1)) %>% 
  tab_style(style = cell_fill(color = pal[3]),
            locations = cells_body(
              columns = vars(`FY19\nAchieved`),
              rows = `FY19\nAchieved` < 1.1)) %>% 
  tab_style(style = cell_fill(color = pal[2]),
            locations = cells_body(
              columns = vars(`FY19\nAchieved`),
              rows = `FY19\nAchieved` < .9)) %>% 
  tab_style(style = cell_fill(color = pal[1]),
            locations = cells_body(
              columns = vars(`FY19\nAchieved`),
              rows = `FY19\nAchieved` < .75)) %>%
  tab_style(style = cell_fill(color = "white"),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` == "")) %>%
  tab_style(style = cell_fill(color = pal[4]),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` >= 1.1)) %>% 
  tab_style(style = cell_fill(color = pal[3]),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` < 1.1)) %>% 
  tab_style(style = cell_fill(color = pal[2]),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` < .9)) %>% 
  tab_style(style = cell_fill(color = pal[1]),
            locations = cells_body(
              columns = vars(`FY20\nAchieved`),
              rows = `FY20\nAchieved` < .75)) 




tbl_ptnr %>% 
  cols_width(
    vars(indicator) ~ px(120),
    everything() ~ px(80)
  )

