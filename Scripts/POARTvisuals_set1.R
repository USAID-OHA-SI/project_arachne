# PROJECT:  project_arachne
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  initial basic figures for POARTs
# REF ID:   4c86a407 
# LICENSE:  MIT
# DATE:     2022-11-17
# UPDATED: 
# tags: achievement, positivity trend, trends in treatment, iit trend

# DEPENDENCIES -----------------------------------------------------------------
  
  library(tidyverse)
  library(extrafont)
  library(gagglr)
  library(glue)
  library(scales)

# GLOBAL VARIABLES -------------------------------------------------------------
  
  ref_id <- "4c86a407"

# IMPORT -----------------------------------------------------------------------
  
  path <- "MER_Structured_Datasets_OU_IM_FY20-23"
  
  df <- si_path() %>%
    return_latest(path) %>%
    read_msd()
    
  df_filt <- df %>%
    filter(country %in% c("South Sudan", "Democratic Republic of the Congo"), 
           fiscal_year %in% c("2021", "2022"))

# MUNGE ------------------------------------------------------------------------
  
  # IIT
  
  df_iit <- df_filt %>% 
    filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW"),
           funding_agency %in% c("USAID")) %>%
    pluck_totals() %>%
    group_by(country, fiscal_year, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}") %>%
    # tx_curr_lag1 = # of patients currently on treatment - quarterly net 
    #                gain/loss in # of patients on treatment
    mutate(tx_curr_lag1 = tx_curr - tx_net_new) %>% 
    rowwise() %>% 
    # % of newly enrolled ART patients who experienced 
    #     interruptions in treatment during reporting period
    # iit = Number of ART patients who were on or initiated treatment during 
    #      the reporting period and then had no clinical contact since their 
    #     last expected contact /
    #      sum of (tx_curr_lag1 + # of patients newly enrolled into treatment)
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE), 
           tx_curr_label = comma(round(tx_curr)), 
           iit_label = percent(iit)) %>% 
    ungroup()
  
  df_iit %>%
    mutate(country_label = glue("{country} - {tx_curr_label}")) %>%
    ggplot(aes(x = period, y = iit, fill = country)) +
    geom_area()
    
    
    
    
    geom_smooth(aes(weight = tx_curr_lag1, 
                    group = country, color = country),
                method = "loess",
                formula = "y ~ x", se = FALSE, na.rm = TRUE,
                linewidth = 1.5) +
    geom_area(aes(group = country), 
              stat = "bin") +
    facet_wrap(~country) +
    scale_y_continuous(limits = c(0, 0.15)) +
    si_style_yline()
    
    # scale_y_continuous(limits = c(-4000, 4000))
  
  
  
  
  
  
  
  
  
  
  
  
  
  