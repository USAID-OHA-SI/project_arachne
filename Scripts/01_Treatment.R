# PROJECT:  project_arachne
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  initial basic figures for POARTs
# REF ID:   4c86a407 
# LICENSE:  MIT
# DATE:     2022-12-1
# UPDATED: 
# tags: achievement + adjusted achievement, positivity trends, trends in treatment, iit trend

# DEPENDENCIES -----------------------------------------------------------------
  
  library(tidyverse)
  library(extrafont)
  library(gagglr)
  library(glue)
  library(scales)
  library(stringr)
  library(patchwork)
  library(ggtext)
  library(tidytext)
  library(cowplot)
  library(janitor)

# GLOBAL VARIABLES -------------------------------------------------------------
  
  ref_id <- "4c86a407"
  
  nat_subnat <- "NAT_SUBNAT_FY15-23"
  msd_path_ou <- "MER_Structured_Datasets_OU_IM_FY20-23"
  genie_path_psnu <- "Genie_PSNU_IM_South_Sudan"
  
  loom_script <- "Scripts/00_loom.R"
  
  selected_ou <- "South Sudan"
  selected_years <- c("2021", "2022")
  selected_agency <- "USAID"
  
  output_path <- "Images"
  
# FUNCTIONS --------------------------------------------------------------------
  
  source(loom_script)

# IMPORT -----------------------------------------------------------------------
  
  nat_subnat_df <- si_path() %>%
    return_latest(nat_subnat) %>%
    read_msd() %>%
    filter(operatingunit == selected_ou, 
          fiscal_year %in% selected_years)
  
  df_ou <- si_path() %>%
    return_latest(msd_path_ou) %>%
    read_msd()
    
  df_ou_filt <- df_ou %>%
    filter(operatingunit == selected_ou, 
           fiscal_year %in% selected_years)
  
  df_psnu <- si_path() %>%
    return_latest(genie_path_psnu) %>%
    read_msd()
  
  df_filt_psnu <- df_psnu %>%
    filter(
      # operatingunit == selected_ou,
      fiscal_year %in% selected_years)
  
  # metadata
  si_path() %>%
    return_latest(msd_path_ou) %>%
    get_metadata()
  
# MUNGE ------------------------------------------------------------------------
  
  df_nat <- calculate_plhiv(nat_subnat_df) %>%
    filter(fiscal_year == metadata$curr_fy)
  
  # PLHIV
  ssd_plhiv <- df_nat$PLHIV
  
  # TX_CURR_SUBNAT
  ssd_tx_curr_subnat <- df_nat$TX_CURR_SUBNAT
  
# VIZ --------------------------------------------------------------------------
  
  # Summary of trends ----------------------------------------------------------
  
    # OU level
      # All agencies, one indicator ---------------------------
      
      ou_progress_summary(.path = path, 
                          .df = df_ou, 
                          .indicator = "TX_CURR", 
                          .ou = selected_ou, 
                          # choose up to 2
                          .fiscal_year = selected_years)
      
      si_save(paste0(metadata$curr_pd, "TX_CURR_summary_SSD.png"),
              path = output_path,
              scale = 0.8)
      
      # USAID, one indicator ----------------------------------
      
      ou_progress_summary(.path = path, 
                          .df = df_ou, 
                          .indicator = "TX_CURR", 
                          .ou = selected_ou, 
                          # choose up to 2
                          .fiscal_year = selected_years, 
                          .funding_agency = selected_agency)
      
      si_save(paste0(metadata$curr_pd, "TX_CURR_summary_SSD_USAID.png"),
              path = output_path,
              scale = 0.8)
      
      # All agencies two indicators --------------------------
      
      
      ou_progress_summary(.path = path, 
                          .df = df_ou, 
                          .indicator = c("TX_NEW", "TX_NET_NEW"), 
                          .ou = selected_ou, 
                          # choose up to 2
                          .fiscal_year = selected_years)
      
      si_save(paste0(metadata$curr_pd, "TX_NEW_NET_NEW_summary_SSD.png"),
              path = output_path,
              scale = 0.8)
      
      # USAID two indicators --------------------------------
      
      ou_progress_summary(.path = path, 
                          .df = df_ou, 
                          .indicator = c("TX_NEW", "TX_NET_NEW"), 
                          .ou = selected_ou, 
                          # choose up to 2
                          .fiscal_year = selected_years, 
                          .funding_agency = selected_agency)
      
      si_save(paste0(metadata$curr_pd, "TX_NEW_NET_NEW_summary_SSD_USAID.png"),
              path = output_path,
              scale = 0.8)
      
    
  # Quarterly growth trend -----------------------------------------------------

  
    # OU level
     
      # All Agencies ---------------------------------------------
    
      ou_progress_qtr(.path = path, 
                    .df = df_ou, 
                    .indicator = "TX_CURR", 
                    .ou = selected_ou)
    
      si_save(paste0(metadata$curr_pd, "TX_CURR_quarterly_SSD.png"),
              path = output_path,
              scale = 0.8)
      
      # USAID ----------------------------------------------------
    
      ou_progress_qtr(.path = path, 
                      .df = df_ou, 
                      .indicator = "TX_CURR", 
                      .ou = selected_ou, 
                      .funding_agency = selected_agency)
      
      si_save(paste0(metadata$curr_pd, "TX_CURR_quarterly_SSD_USAID.png"),
              path = output_path,
              scale = 0.8)
  
    
  # Quarterly Trend in IIT, IIT, and Change Over Time --------------------------
      
      # OU level
      # All Agencies --------------------------------
  
       df_iit <- df_ou_filt %>%
        filter(
          indicator %in% c("TX_ML", "TX_ML_IIT", "TX_CURR", 
                           "TX_NEW", "TX_NET_NEW","TX_RTT"),
          funding_agency != "Dedup", 
          standardizeddisaggregate %in% 
            c("Age/Sex/ARTNoContactReason/HIVStatus", "Total Numerator"), 
          is.na(otherdisaggregate) |
          str_detect(otherdisaggregate, 
                     "No Contact Outcome - Interruption in Treatment"), 
          !(standardizeddisaggregate == "Total Numerator" & 
              indicator == "TX_ML")) %>%
        clean_indicator() %>%
        group_by(country, fiscal_year, indicator) %>%
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
          period_num = as.numeric(
            str_extract(period_num, "[1-4]")), 
          # ask if this makes sense or would be meaningful
          delta_patients = -tx_ml_iit + as.numeric(tx_rtt)) %>%
        ungroup() %>%
        pivot_longer(c(tx_ml_iit, delta_patients, tx_rtt), 
                     names_to = "indicator") %>%
        mutate(
          delta_patients_lab = if_else(indicator == "delta_patients", 
            comma(value), ""),
          value = if_else(indicator == "tx_ml_iit", -value, value))

  df_iit %>%
     ggplot(aes(x = period, fill = fct_rev(indicator))) +
    # include delta patients as geom_col at all?
     geom_col(aes(y = value), alpha = .7,
              position = position_dodge(width = .5)) +
     facet_wrap(~fiscal_year, scales = "free_x", ncol = 1) +
     geom_text(aes(label = delta_patients_lab, y = 0), 
               position = position_dodge(width = 0.75),
               family = "Source Sans Pro", size = 12 / .pt, 
               vjust = -.5, na.rm = TRUE) +
     scale_fill_manual(values = c("tx_ml_iit" = usaid_darkgrey,
                                  "tx_rtt" = usaid_lightgrey, 
                                  "delta_patients" = usaid_medgrey)) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = unique(df_iit$period)[grep("Q(4)", unique(df_iit$period))]) +
    si_style_ygrid() +
     theme(panel.spacing = unit(.5, "line"),
           # temporary since this is in grayscale
           # legend.position = "none",
           plot.title = element_markdown(),
           strip.text = element_markdown()) +
   labs(x = NULL, y = NULL, fill = NULL,
   subtitle = glue("IIT, RTT, and change between them per quarter"),
  caption = glue(" Note: tx_ml_iit = TX_ML where patient experienced IIT; 
                         delta_patients = TX_RTT - TX_ML_ITT
                  {metadata$caption} | US Agency for International Development"))
   
 #  si_save(paste0(metadata$curr_pd, "IIT_RTT_quarterly.png"),
 #          path = output_path,
 #          scale = 0.8)

 
 
 
 #  
 #  # df_iit_rtt <- ou_df %>% 
 #  #   filter(funding_agency == selected_agency, 
 #  #          indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW", "TX_RTT"), 
 #  #          fiscal_year == "2022") %>%
 #  #   pluck_totals() %>%
 #  #   group_by(fiscal_year, country, indicator) %>% 
 #  #   summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
 #  #   reshape_msd(include_type = FALSE) %>% 
 #  #   pivot_wider(names_from = "indicator",
 #  #               names_glue = "{tolower(indicator)}")
 #  # 
 #  # df_iit_rtt <- df_iit_rtt %>%
 #  #   mutate(tx_curr_lag1 = tx_curr - tx_net_new) %>% 
 #  #   rowwise() %>% 
 #  #   mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE), 
 #  #          iit_label = percent(iit), 
 #  #          qtr_label = glue("{period} ({iit_label})")) %>% 
 #  #   ungroup()
 #  # 
 #  # vct_itt_cntry <- ou_df %>% 
 #  #   filter(funding_agency == selected_agency, 
 #  #          indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW", "TX_RTT"), 
 #  #          fiscal_year == "2022") %>%
 #  #   pluck_totals() %>%
 #  #   group_by(fiscal_year, country, indicator) %>% 
 #  #   summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
 #  #   reshape_msd(include_type = FALSE) %>% 
 #  #   pivot_wider(names_from = "indicator",
 #  #               names_glue = "{tolower(indicator)}") %>% 
 #  #   filter(period == metadata$curr_pd) %>% 
 #  #   mutate(tx_curr_lag1 = tx_curr - tx_net_new) %>% 
 #  #   rowwise() %>% 
 #  #   mutate(
 #  #     iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
 #  #   ungroup() %>% 
 #  #   pull() %>% 
 #  #   percent()
 #  # 
 #  # df_iit_rtt %>%
 #  #   mutate(fiscal_year = str_sub(period, end = 4)) %>% 
 #  #   filter(tx_curr_lag1 != 0) %>%
 #  #   ggplot(aes(qtr_label, iit)) +
 #  #   geom_smooth(aes(weight = tx_curr_lag1, group = country),
 #  #               method = "loess",
 #  #               formula = "y ~ x", se = FALSE, na.rm = TRUE,
 #  #               linewidth = 1.5, color = old_rose_light) +
 #  #   scale_y_continuous(limits = c(0,.15),
 #  #                      label = NULL,
 #  #                      oob = oob_squish) +
 #  #   coord_cartesian(clip = "off") +
 #  #   labs(x = NULL, y = NULL,
 #  #        size = "Site TX_CURR (1 period prior)",
 #  #        title = glue("USAID MAINTAINED A RELATIVELY CONSTANT {vct_itt_cntry} IIT OVER FY22"),
 #  #        subtitle = glue("A 1% increase Occurred in Q3"),
 #  #        caption = glue("Note: IIT = TX_ML / TX_CURR - TX_NET_NEW + TX_NEW; ITT capped to 15%
 #  #                       {metadata$caption} | US Agency for International Development")) +
 #  #   si_style_ygrid() +
 #  #   theme(panel.spacing = unit(.5, "line"),
 #  #         axis.text = element_text(size = 8),
 #  #         plot.subtitle = element_markdown())
 #  # 
 #  # si_save(glue("Images/{metadata$curr_pd}_SSD_OU_iit.png"),
 #  #         scale = 0.8)  
 #  
 #  si_save(paste0(metadata$curr_pd, "TX_NEW_quarterly.png"),
 #          path = output_path,
 #          scale = 0.8)
 
 #  # USAID only --------------------------------------
 #  
 #    # TX-CURR Performance quarterly growth trend 
 #  ou_progress_qtr(.path = path, 
 #                  .df = df, 
 #                  .indicator = "TX_CURR", 
 #                  .ou = selected_ou, 
 #                  .funding_agency = selected_agency)
 #  
 #  si_save(paste0(metadata$curr_pd, "TX_CURR_quarterly_USAID.png"),
 #          path = output_path,
 #          scale = 0.8)
 #  
 #    # TX_NEW Performance quarterly growth trend
 #  ou_progress_qtr(.path = path, 
 #                  .df = df, 
 #                  .indicator = "TX_NEW", 
 #                  .ou = selected_ou, 
 #                  .funding_agency = selected_agency)
 #  
 #  si_save(paste0(metadata$curr_pd, "TX_NEW_quarterly_USAID.png"),
 #          path = output_path,
 #          scale = 0.8)
 #                  
    # Quarterly Trend in IIT
  
    # Peds ---------------------------------------------------------------------
  
      # TX-CURR Performance quarterly growth trend 
      # TX_NEW Performance quarterly growth trend
      # Quarterly Trend in IIT
  
    # AGYW (15-24) -------------------------------------------------------------
    
      # TX-CURR Performance quarterly growth trend 
      # TX_NEW Performance quarterly growth trend
      # Quarterly Trend in IIT
      
  
  