# PROJECT:  project_arachne
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  functions for the figures used for POART EDA
# REF ID:   cc2c736a 
# LICENSE:  MIT
# DATE:     2022-12-1
# UPDATED: 
# tags: arachne, achievement + adjusted achievement, 
#       positivity trends, trends in treatment, iit trend

# DEPENDENCIES ----------------------------------------------------------------

  library(tidyverse)
  library(extrafont)
  library(gagglr)
  library(glue)
  library(scales)
  library(stringr)
  library(janitor)

# Functions --------------------------------------------------------------------

# Calculate PLHIV in a given OU, returns a data frame containing OU level 
# TX_CURR_SUBNAT and an associated label, PLHIV and an associated label, 
# gap between TX_CURR_SUBNAT and PLHIV both as a number (gap) and a percent (gap_pct)

calculate_plhiv <- function(.nat_subnat_df) {
  
  df_nat <- .nat_subnat_df %>%
    filter(indicator %in% c("TX_CURR_SUBNAT", "PLHIV"),
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
    group_by(fiscal_year, indicator) %>% 
    summarise(across(targets, sum, na.rm = TRUE)) %>% 
    pivot_wider(names_from = indicator, values_from = targets) %>% 
    mutate(gap = TX_CURR_SUBNAT/PLHIV, 
           gap_pct = percent(gap),
           need_label = comma(PLHIV - TX_CURR_SUBNAT), 
           PLHIV_label = comma(PLHIV), 
           TX_CURR_SUBNAT_label = comma(TX_CURR_SUBNAT))
  
  return(df_nat)
  
}

# Summarizes cumulative quarterly progress on selected indicators

ou_trend_cum <- function(.path, .df, .indicator, .ou, .fiscal_year, 
                         .funding_agency = NULL, ...){
  
  qtrs_to_keep <- metadata$curr_pd %>%
                  convert_qtr_to_date() %>%
                  seq.Date(by = "-3 months", length = 6) %>%
                  convert_date_to_qtr()
                
  if(!is.null(.funding_agency)){
    
    .df <- .df %>%
      filter(funding_agency == .funding_agency)
    
    # how can we dynamically title the agency specific graphs?
    
  }
    
    df_new <- .df %>%
      filter(
        indicator %in% .indicator,
        operatingunit == .ou,
        fiscal_year %in% .fiscal_year) %>%
      pluck_totals() %>%
      group_by(operatingunit, indicator, fiscal_year) %>%
      summarise(across(c(starts_with("qtr"), cumulative, targets),
                       sum,na.rm = TRUE), .groups = "keep") %>%
      reshape_msd(direction = "quarters") %>%
      adorn_achievement() %>%
      arrange(period) %>%
      mutate(period_num = as.numeric(str_sub(period, -1)),
             qtr_target = targets/((4-period_num) + 1), 
             fiscal_year2 = str_extract(period, "FY[1-2][0-9]"),
             results_lab = case_when(period == metadata$curr_pd | 
                                       fiscal_year2 == metadata$curr_fy_lab ~ 
                                       glue("{comma(results_cumulative)}")), 
             achv_pct_label = case_when(period == metadata$curr_pd | 
                                       fiscal_year2 == metadata$curr_fy_lab ~ 
                                       glue("{percent(achievement_qtrly)}")), 
             ind_period = str_c(indicator, period, sep = "_")) %>%
      filter(period %in% qtrs_to_keep)
    
    df_new %>%
      ggplot(aes(x = period)) +
      geom_col(aes(y = qtr_target), alpha = .7, fill = usaid_lightgrey,
               position = position_dodge(width = .65)) +
      geom_col(aes(y = results_cumulative), alpha = .7, fill = usaid_medgrey,
               position = position_dodge(width = .65)) +
      geom_text(aes(label = achv_pct_label, y = 0), 
                position = position_dodge(width = 0.75), color = "#FFFFFF",
                family = "Source Sans Pro", size = 12 / .pt, 
                vjust = -.5, na.rm = TRUE) +
      geom_text(aes(label = results_lab, y = results_cumulative), 
                position = position_dodge(width = 0.75), color = usaid_medgrey,
                family = "Source Sans Pro", size = 12 / .pt, 
                vjust = -.5, na.rm = TRUE) +
      scale_x_discrete(breaks = unique(df_new$period)[grep("Q(2|4)", unique(df_new$period))]) +
      # how to dynamically add just a little excess to the 
      # max. value to accommodate the geom_text?
      scale_y_continuous(limits = c(0, max(df_new$qtr_target)+ 5000), 
                         label = label_number(scale_cut = cut_short_scale())) +
      labs(x = NULL, y = NULL, fill = NULL,
           subtitle = glue("{.indicator} cumulative achievement against quarterly targets ({df_new$operatingunit[1]})"),
           caption = glue("{metadata$caption} | US Agency for International Development")) +
      si_style_yline() +
      theme(panel.spacing = unit(.5, "line"),
            legend.position = "none",
            plot.title = element_markdown(),
            strip.text = element_markdown())
    
  }

# Summarizes quarterly progress on selected indicators

ou_trend <- function(.path, .df, .indicator,
                     .ou, .funding_agency = NULL, ...){
  
  # metadata
  si_path() %>% 
    return_latest(.path) %>%
    get_metadata()
  
  peds <- c("<01", "01-04", "05-09", "10-14")
  adults <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
              "50-54", "55-59", "60-64", "65+")
  
  if(!is.null(.funding_agency)){
    
    .df <- .df %>%
      filter(
        funding_agency == .funding_agency)
    
  }
    
    df_new <-  .df %>%
      filter(
        operatingunit == .ou,
        indicator == .indicator,
        (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% peds) |
          (standardizeddisaggregate == "Total Numerator")) %>%
      mutate(type = ifelse(standardizeddisaggregate == "Total Numerator",
                           "Total", "Peds")) %>%
      group_by(fiscal_year, operatingunit, indicator, type) %>%
      summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE),
                .groups = "drop") %>%
      reshape_msd("quarters") %>%
      select(-results_cumulative) %>%
      arrange(type, operatingunit, period)
    
    df_new <- df_new %>%
      mutate(
        growth_rate_req =
          case_when(period == metadata$curr_pd ~
                      ((targets / results)^(1 / (4 - metadata$curr_qtr))) - 1)) %>%
      group_by(type) %>%
      fill(growth_rate_req, .direction = "updown") %>%
      mutate(
        growth_rate = (results / lag(results, order_by = period)) - 1,
        growth_rate = na_if(growth_rate, Inf)) %>%
      ungroup() %>%
      mutate(
        geo_gr_lab = case_when(
          is.infinite(growth_rate_req) ~ glue("{toupper(operatingunit)}"),
          growth_rate_req < 0 ~ glue("{toupper(operatingunit)}\nTarget achieved"),
          growth_rate_req < .1 ~ glue("{toupper(operatingunit)}\n{percent(growth_rate_req, 1)}"),
          TRUE ~ glue("{toupper(operatingunit)}\n{percent(growth_rate_req, 1)}")),
        gr_lab = case_when(fiscal_year == metadata$curr_fy ~ 
                             glue("{percent(growth_rate, 1)}")),
        gr_lab = stringr::str_replace(gr_lab, "NA", "0"),
        gr_label_position = 0,
        results_lab =    case_when(fiscal_year == metadata$curr_fy ~ 
                                     glue("{comma(results)}")),
        disp_targets = case_when(fiscal_year == metadata$curr_fy ~ targets),
        unit_label = glue("(Operating Unit)"),
        amount_diff = targets - results, 
        pct_change = round_half_up((results - targets)/abs(targets) * 100),0)
    
    # percentage change from q1 to q4
    pct_change_new <- df_new %>%
      filter(type == "Total") %>%
      select(fiscal_year, pct_change) %>%
      filter(pct_change == max(as.numeric(pct_change))) %>%
      pull()
    
    df_new %>%
      filter(type == "Total") %>%
      ggplot(aes(period, results, fill = as.character(period))) +
      geom_col(na.rm = TRUE, alpha = .7, width = 1) +
      geom_text(aes(label = results_lab, y = results), 
                family = "Source Sans Pro", color = usaid_darkgrey, size = 9 / .pt,
                vjust = -.5, na.rm = TRUE) +
      geom_text(aes(label = gr_lab, y = gr_label_position),
                family = "Source Sans Pro", color = "white", size = 9 / .pt,
                vjust = -.5, na.rm = TRUE) +
      scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
      scale_x_discrete(breaks = unique(df_new$period)[grep("Q(4)", unique(df_new$period))]) +
      scale_fill_manual(values = c(usaid_lightgrey, usaid_lightgrey, usaid_lightgrey, usaid_lightgrey,
                                   usaid_lightgrey, usaid_lightgrey, usaid_lightgrey, usaid_lightgrey,
                                   usaid_darkgrey, usaid_darkgrey, usaid_darkgrey, usaid_darkgrey)) +
      labs(
        x = NULL, y = NULL,
        title = NULL, 
        subtitle = glue("{.indicator} quarterly trend and growth rate ({df_new$operatingunit[1]})"),
        caption = glue("{metadata$caption} | US Agency for International Development")) +
      si_style_ygrid() +
      theme(
        legend.position = "none",
        panel.spacing = unit(.5, "picas"),
        axis.text.x = element_text(size = 8))

}

# Summarizes patient gain/loss quarterly trend

ou_patient_change_trend <- function(.path, .df, .ou, .funding_agency = NULL, ...) {
  
  # metadata
  si_path() %>% 
    return_latest(.path) %>%
    get_metadata()
  
  # add a unit test to check that tx_new and tx_net_new exist in .df					 
  
  if(!is.null(.funding_agency)){
    
    .df <- .df %>%
      filter(funding_agency == .funding_agency)
    
    # how can we dynamically title the agency specific graphs?
    
  }
  
  df_iit <- .df %>%
    filter(
      indicator %in% c("TX_NEW", "TX_NET_NEW"), 
      operatingunit == .ou) %>%
    pluck_totals() %>%
    clean_indicator() %>%
    group_by(operatingunit, fiscal_year, indicator) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
    reshape_msd(include_type = FALSE) %>%
    pivot_wider(
      names_from = indicator,
      names_glue = "{tolower(indicator)}") %>%
    mutate(
      fiscal_year = str_extract(period, "FY[0-2]{2}"),
      patient_loss_gain = tx_net_new - tx_new) %>%
    ungroup() %>%
    pivot_longer(c(patient_loss_gain), 
                 names_to = "indicator") %>%
    mutate(
      delta_lab = if_else(indicator == "patient_loss_gain", 
                          comma(value), ""))
  
  df_iit %>%
    ggplot(aes(x = period)) +
    geom_col(aes(y = value, fill = fiscal_year), alpha = .7) +
    # note, ncol can be changed usingthe function if > 2 fiscal years are used
    facet_wrap(~fiscal_year, scales = "free_x", ncol = 1) +
    geom_text(aes(label = delta_lab, y = value, color = fiscal_year), 
              position = position_dodge(width = 0.75),
              family = "Source Sans Pro", size = 12 / .pt, 
              vjust = -.5, na.rm = TRUE) +
    scale_fill_manual(values = c(usaid_lightgrey, usaid_medgrey)) +
    scale_color_manual(values = c(usaid_medgrey, usaid_darkgrey)) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = unique(df_iit$period)[grep("Q(4)", unique(df_iit$period))]) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          legend.position = "none",
          plot.title = element_markdown(),
          strip.text = element_markdown()) +
    labs(x = NULL, y = NULL, fill = NULL,
         subtitle = glue("Are we gaining or losing patients each quarter? ({df_iit$operatingunit[1]})"),
         caption = glue(" Note: patient gain/loss = TX_NET_NEW - TX_NEW
                  {metadata$caption} | US Agency for International Development"))
  
  # Summarizes the quarterly trend in IIT, RTT, and unexplained gain/loss in patients
  ou_iit_rtt_trend <- function(.path, .df, .ou, .funding_agency = NULL, ...) {
    
    # metadata
    si_path() %>% 
      return_latest(.path) %>%
      get_metadata()
    
    # add a unit test to check that required indicators exist in .df					 
    
    if(!is.null(.funding_agency)){
      
      .df <- .df %>%
        filter(funding_agency == .funding_agency)
      
      # how can we dynamically title the agency specific graphs?
      
    }
    
    df_iit <- .df %>%
      filter(
        indicator %in% c("TX_ML", "TX_ML_IIT", "TX_CURR", 
                         "TX_NEW", "TX_NET_NEW","TX_RTT"),
        standardizeddisaggregate %in% 
          c("Age/Sex/ARTNoContactReason/HIVStatus", "Total Numerator"), 
        is.na(otherdisaggregate) |
          str_detect(otherdisaggregate, 
                     "No Contact Outcome - Interruption in Treatment"), 
        !(standardizeddisaggregate == "Total Numerator" & 
            indicator == "TX_ML")) %>%
      clean_indicator() %>%
      group_by(operatingunit, fiscal_year, indicator) %>%
      mutate(
        indicator = if_else(
          indicator == "TX_ML", "TX_ML_IIT", indicator)) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
      reshape_msd(include_type = FALSE) %>%
      pivot_wider(
        names_from = indicator,
        names_glue = "{tolower(indicator)}") %>%
      rowwise() %>%
      mutate(
        tx_curr_lag1 = as.numeric(tx_curr - tx_net_new),
        share_rtt_curr = as.numeric(tx_rtt / tx_curr),
        share_rtt_label = percent(share_rtt_curr),
        tx_curr_label = comma(round(tx_curr)),
        iit_label = comma(tx_ml_iit),
        fiscal_year = str_extract(period, "FY[0-2]{2}"),
        period_num = str_extract(period, "Q[1-4]"),
        period_num = as.numeric(str_extract(period_num, "[1-4]")), 
        delta_patients = -tx_ml_iit + as.numeric(tx_rtt),
        tx_rtt_gain = (tx_curr_lag1 + tx_new + tx_rtt),
        unexplained_loss_gain =  (tx_rtt_gain - tx_ml_iit  - tx_curr),
        gain_loss_colors = if_else(unexplained_loss_gain > 0, 
                                   usaid_lightgrey, "#FFFFFF")) %>%
      ungroup() %>%
      pivot_longer(c(tx_ml_iit, unexplained_loss_gain, tx_rtt), 
                   names_to = "indicator") %>%
      mutate(
        unexplained_lab = if_else(indicator == "unexplained_loss_gain", 
                                  comma(value), ""),
        value = if_else(indicator == "tx_ml_iit", -value, value), 
        value_filt = if_else(indicator == "unexplained_loss_gain", 
                             0, value))
    
    df_iit %>%
      ggplot(aes(x = period, fill = fct_rev(indicator))) +
      geom_col(aes(y = value_filt), alpha = .7) +
      facet_wrap(~fiscal_year, scales = "free_x", ncol = 1) +
      geom_text(aes(label = unexplained_lab, y = 0, color = gain_loss_colors), 
                position = position_dodge(width = 0.75),
                family = "Source Sans Pro", size = 12 / .pt, 
                vjust = -.5, na.rm = TRUE) +
      scale_color_manual(values = c(usaid_medgrey, "#FFFFFF")) +
      scale_fill_manual(values = c("tx_ml_iit" = usaid_darkgrey,
                                   "tx_rtt" = usaid_lightgrey, 
                                   "unexplained_loss_gain" = usaid_medgrey)) +
      scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
      scale_x_discrete(breaks = unique(df_iit$period)[grep("Q(4)", unique(df_iit$period))]) +
      si_style_ygrid() +
      theme(panel.spacing = unit(.5, "line"),
            # temporary since this is in grayscale
            # legend.position = "none",
            plot.title = element_markdown(),
            strip.text = element_markdown()) +
      labs(x = NULL, y = NULL, fill = NULL,
           subtitle = glue("IIT, RTT, and Unexplained Loss/Gain {df_iit$operatingunit[1]}"),
           caption = glue(" Note: tx_ml_iit = TX_ML where patient experienced IIT;
                         tx_rtt_gain = tx_curr_lag1 + tx_new + tx_rtt;
                         unexplained_loss_gain =  tx_rtt_gain - tx_ml_iit  - tx_curr
                  {metadata$caption} | US Agency for International Development"))
    
  }	
}