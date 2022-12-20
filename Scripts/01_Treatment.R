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
  filter(
    operatingunit == selected_ou,
    fiscal_year %in% selected_years)

df_ou <- si_path() %>%
  return_latest(msd_path_ou) %>%
  read_msd()

df_ou_filt <- df_ou %>%
  filter(
    operatingunit == selected_ou,
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

# Total ------------------------------------------------------------------------

# Cumulative trends ----------------------------------------------------------

  # OU level
    # All agencies ------------------------------------
    
    # TX_CURR example
    
    ou_achv_cumul(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou,
      .subtitle = "What is the cumulative achievement of TX_CURR (All Ages)
      against quarterly targets in South Sudan?",
      # choose up to 2
      .fiscal_year = selected_years, 
      .type = "Total")
    
    si_save(paste0(metadata$curr_pd, "TX_CURR_summary_SSD_Total.png"),
      path = output_path,
      scale = 0.8)
    
    # TX_NEW example
    
    ou_achv_cumul(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_NEW",
      .subtitle = "What is the cumulative achievement of TX_NEW (All Ages)
      against quarterly targets in South Sudan?",
      .ou = selected_ou,
      # choose up to 2
      .fiscal_year = selected_years,
      .type = "Total")
    
    si_save(paste0(metadata$curr_pd, "TX_NEW_summary_SSD_Total.png"),
      path = output_path,
      scale = 0.8)
    
    # USAID ---------------------------------------------
    
    # TX_CURR example
    
    ou_achv_cumul(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou,
      .subtitle = "What is the cumulative achievement of TX_CURR (All Ages)
      against quarterly targets in USAID supported facilities in South Sudan?",
      # choose up to 2
      .fiscal_year = selected_years,
      .funding_agency = selected_agency,
      .type = "Total")
    
    si_save(paste0(metadata$curr_pd, "TX_CURR_summary_SSD_USAID_Total.png"),
      path = output_path,
      scale = 0.8)
    
    # TX_NEW example
    
    ou_achv_cumul(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_NEW",
      .ou = selected_ou,
      .subtitle = "What is the cumulative achievement of TX_NEW (All Ages)
      against quarterly targets in USAID supported facilities in South Sudan?",
      # choose up to 2
      .fiscal_year = selected_years,
      .funding_agency = selected_agency,
      .type = "Total")
    
    si_save(paste0(metadata$curr_pd, "TX_NEW_summary_SSD_USAID_Total.png"),
      path = output_path,
      scale = 0.8)
     
# Quarterly growth trend -----------------------------------------------------

  # OU level

    # All Agencies ---------------------------------------------
    
    ou_achv_qtr(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou, 
      .subtitle = "What is the TX_CURR trend and growth rate across all ages in South Sudan?",
      # can be one of 3 options: "Total", "Adults", or "Pediatric"
      .type = "Total")
    
    si_save(paste0(metadata$curr_pd, "TX_CURR_quarterly_SSD_Total.png"),
      path = output_path,
      scale = 0.8)
    
    # USAID ----------------------------------------------------
    
    ou_achv_qtr(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou,
      .subtitle = "What is the TX_CURR trend and growth rate across all ages at
      USAID supported facilities in South Sudan?",
      .funding_agency = selected_agency, 
      # can be one of 3 options: "Total", "Adults", or "Pediatric"
      .type = "Total")
    
    si_save(paste0(metadata$curr_pd, "TX_CURR_quarterly_SSD_USAID_Total.png"),
      path = output_path,
      scale = 0.8)
    
# Quarterly Trend in the Number of New Patients over time ---------------------
    
    
    # OU level
    # All Agencies --------------------------------
    
    ou_patient_delta(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .fiscal_year = selected_years,
      .ou = selected_ou,
      .subtitle = "Is South Sudan gaining or losing patients of all ages each quarter?",
      # can be one of 3 options: "Total", "Adults", or "Pediatric"
      .type = "Total")
    
    si_save(paste0(metadata$curr_pd, "Patient_Delta_quarterly_SSD_Total.png"),
            path = output_path,
            scale = 0.8)
    
    # USAID ---------------------------------------
    
    ou_patient_delta(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou,
      .fiscal_year = selected_years,
      .funding_agency = selected_agency,
      .subtitle = "Are USAID supported facilities in South Sudan gaining or losing patients of all ages?",
      # can be one of 3 options: "Total", "Adults", or "Pediatric"
      .type = "Total")
    
    si_save(paste0(metadata$curr_pd, "Patient_Delta_quarterly_SSD_USAID_Total.png"),
            path = output_path,
            scale = 0.8)
    
# Quarterly Trend in IIT, IIT, and Change Over Time ----------------------------
    
  # OU level
    # All Agencies --------------------------------
    
    ou_iit_rtt_trend(
      .path = msd_path_ou,
      .df = df_ou,
      .fiscal_year = selected_years,
      .ou = selected_ou,
      .subtitle = "What is the quarterly trend in IIT, RTT, and Unexplained
      Loss/Gain across all ages in South Sudan?",
      # can be one of 3 options: "Total", "Adults", or "Pediatric"
      .type = "Total")
        
    si_save(paste0(metadata$curr_pd, "IIT_RTT_quarterly_SSD_Total.png"),
      path = output_path,
      scale = 0.8)
    
    # USAID ---------------------------------------
    
    ou_iit_rtt_trend(
      .path = msd_path_ou,
      .df = df_ou,
      .fiscal_year = selected_years,
      .ou = selected_ou,
      .funding_agency = selected_agency,
      .subtitle = 
        "What is the quarterly trend in IIT, RTT, and Unexplained Loss/Gain 
        across all ages treated at USAID supported facilities in South Sudan?",
      # can be one of 3 options: "Total", "Adults", or "Pediatric"
      .type = "Total")
    
    si_save(paste0(metadata$curr_pd, "IIT_RTT_quarterly_SSD_Total.png"),
            path = output_path,
            scale = 0.8)
    
# Adults -----------------------------------------------------------------------
    
    # Cumulative trends ----------------------------------------------------------
    
    # OU level
    # All agencies ------------------------------------
    
    # TX_CURR example
    
    ou_achv_cumul(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou,
      .subtitle = "What is the cumulative achievement of TX_CURR
      against quarterly targets among adults in South Sudan?",
      # choose up to 2
      .fiscal_year = selected_years, 
      .type = "Adults")
    
    si_save(paste0(metadata$curr_pd, "TX_CURR_summary_SSD_Adults.png"),
            path = output_path,
            scale = 0.8)
    
    # TX_NEW example
    
    ou_achv_cumul(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_NEW",
      .subtitle = "What is the cumulative achievement of TX_NEW
      against quarterly targets in South Sudan?",
      .ou = selected_ou,
      # choose up to 2
      .fiscal_year = selected_years, 
      .type = "Adults")
    
    si_save(paste0(metadata$curr_pd, "TX_NEW_summary_SSD_Adults.png"),
            path = output_path,
            scale = 0.8)
    
    # USAID ---------------------------------------------
    
    # TX_CURR example
    
    ou_achv_cumul(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou,
      .subtitle = "What is the cumulative achievement of TX_CURR against quarterly 
      targets among adult patients at USAID supported facilities in South Sudan?",
      # choose up to 2
      .fiscal_year = selected_years,
      .funding_agency = selected_agency, 
      .type = "Adults")
    
    si_save(paste0(metadata$curr_pd, "TX_CURR_summary_SSD_USAID_Adults.png"),
            path = output_path,
            scale = 0.8)
    
    # TX_NEW example
    
    ou_achv_cumul(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_NEW",
      .ou = selected_ou,
      .subtitle = "What is the cumulative achievement of TX_NEW against quarterly 
      targets among adult patients in USAID supported facilities in South Sudan?",
      # choose up to 2
      .fiscal_year = selected_years,
      .funding_agency = selected_agency, 
      .type = "Adults")
    
    si_save(paste0(metadata$curr_pd, "TX_NEW_summary_SSD_USAID_Adults.png"),
            path = output_path,
            scale = 0.8)
    
    # Quarterly growth trend ---------------------------------------------------
    
    # OU level
    
    # All Agencies ---------------------------------------------
    
    ou_achv_qtr(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou, 
      .subtitle = "What is the TX_CURR trend and growth rate among adult patients in South Sudan?",
      # can be one of 3 options: "Total", "Adults", or "Pediatric"
      .type = "Adults")
    
    si_save(paste0(metadata$curr_pd, "TX_CURR_quarterly_SSD_Adults.png"),
            path = output_path,
            scale = 0.8)
    
    # USAID ----------------------------------------------------
    
    ou_achv_qtr(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou,
      .subtitle = "What is the TX_CURR trend and growth rate among adult patients at
      USAID supported facilities in South Sudan?",
      .funding_agency = selected_agency, 
      # can be one of 3 options: "Total", "Adults", or "Pediatric"
      .type = "Adults")
    
    si_save(paste0(metadata$curr_pd, "TX_CURR_quarterly_SSD_USAID_Adults.png"),
            path = output_path,
            scale = 0.8)
    
    # Quarterly Trend in the Number of New Patients over time ------------------
    
    
    # OU level
    # All Agencies --------------------------------
    
    ou_patient_delta(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .fiscal_year = selected_years,
      .ou = selected_ou,
      .subtitle = "Is South Sudan gaining or losing adult patients each quarter?",
      # can be one of 3 options: "Total", "Adults", or "Pediatric"
      .type = "Adults")
    
    si_save(paste0(metadata$curr_pd, "Patient_Delta_quarterly_SSD_Adults.png"),
            path = output_path,
            scale = 0.8)
    
    # USAID ---------------------------------------
    
    ou_patient_delta(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou,
      .fiscal_year = selected_years,
      .funding_agency = selected_agency,
      .subtitle = "Are USAID supported facilities in South Sudan gaining or losing adult patients?",
      # can be one of 3 options: "Total", "Adults", or "Pediatric"
      .type = "Adults")
    
    si_save(paste0(metadata$curr_pd, "Patient_Delta_quarterly_SSD_USAID_Adults.png"),
            path = output_path,
            scale = 0.8)
    
    # Quarterly Trend in IIT, IIT, and Change Over Time ----------------------------
    
    # OU level
    # All Agencies --------------------------------
    
    ou_iit_rtt_trend(
      .path = msd_path_ou,
      .df = df_ou,
      .fiscal_year = selected_years,
      .ou = selected_ou,
      .subtitle = "What is the quarterly trend in IIT, RTT, and 
          Unexplained Loss/Gain among adult patients in South Sudan?",
      # can be one of 3 options: "Total", "Adults", or "Pediatric"
      .type = "Adults")
    
    si_save(paste0(metadata$curr_pd, "IIT_RTT_quarterly_SSD_Adults.png"),
            path = output_path,
            scale = 0.8)
    
    # USAID ---------------------------------------
    
    ou_iit_rtt_trend(
      .path = msd_path_ou,
      .df = df_ou,
      .fiscal_year = selected_years,
      .ou = selected_ou,
      .funding_agency = selected_agency,
      .subtitle = 
        "What is the quarterly trend in IIT, RTT, and Unexplained Loss/Gain 
        among adult patients at USAID supported facilities in South Sudan?",
      # can be one of 3 options: "Total", "Adults", or "Pediatric"
      .type = "Adults")
    
    si_save(paste0(metadata$curr_pd, "IIT_RTT_quarterly_SSD_Adults.png"),
            path = output_path,
            scale = 0.8)
    
    
# Peds -------------------------------------------------------------------------

    # Cumulative trends --------------------------------------------------------
    
    # OU level
    # All agencies ------------------------------------
    
    # TX_CURR example
    
    ou_achv_cumul(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou,
      .subtitle = "What is the cumulative achievement of TX_CURR 
      against quarterly targets among pediatric patients in South Sudan?",
      # choose up to 2
      .fiscal_year = selected_years, 
      .type = "Pediatric")
    
    si_save(paste0(metadata$curr_pd, "TX_CURR_summary_SSD_peds.png"),
            path = output_path,
            scale = 0.8)
    
    # TX_NEW example
    
    ou_achv_cumul(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_NEW",
      .subtitle = "What is the cumulative achievement of TX_NEW
      against quarterly targets among pediatric patients in South Sudan?",
      .ou = selected_ou,
      # choose up to 2
      .fiscal_year = selected_years, 
      .type = "Pediatric")
    
    si_save(paste0(metadata$curr_pd, "TX_NEW_summary_SSD_peds.png"),
            path = output_path,
            scale = 0.8)
    
    # USAID ---------------------------------------------
    
    # TX_CURR example
    
    ou_achv_cumul(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou,
      .subtitle = "What is the cumulative achievement of TX_CURR 
      against quarterly targets among pediatric patients at 
      USAID supported facilities in South Sudan?",
      # choose up to 2
      .fiscal_year = selected_years,
      .funding_agency = selected_agency, 
      .type = "Pediatric")
    
    si_save(paste0(metadata$curr_pd, "TX_CURR_summary_SSD_USAID_peds.png"),
            path = output_path,
            scale = 0.8)
    
    # TX_NEW example
    
    ou_achv_cumul(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_NEW",
      .ou = selected_ou,
      .subtitle = "What is the cumulative achievement of TX_NEW 
      against quarterly targets among pediatric patients at 
      USAID supported facilities in South Sudan?",
      # choose up to 2
      .fiscal_year = selected_years,
      .funding_agency = selected_agency, 
      .type = "Pediatric")
    
    si_save(paste0(metadata$curr_pd, "TX_NEW_summary_SSD_USAID_peds.png"),
            path = output_path,
            scale = 0.8)
    
    # Quarterly growth trend ---------------------------------------------------
    
    # OU level
    
    # All Agencies ---------------------------------------------
    
    ou_achv_qtr(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou, 
      .subtitle = "What is the TX_CURR trend and growth rate among pediatric patients in South Sudan?",
      # can be one of 3 options: "Total", "Adults", or "Pediatric"
      .type = "Pediatric")
    
    si_save(paste0(metadata$curr_pd, "TX_CURR_quarterly_SSD_peds.png"),
            path = output_path,
            scale = 0.8)
    
    # USAID ----------------------------------------------------
    
    ou_achv_qtr(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou,
      .subtitle = "What is the TX_CURR trend and growth rate among pediatric patients at
      USAID supported facilities in South Sudan?",
      .funding_agency = selected_agency, 
      # can be one of 3 options: "Total", "Adults", or "Pediatric"
      .type = "Pediatric")
    
    si_save(paste0(metadata$curr_pd, "TX_CURR_quarterly_SSD_USAID_peds.png"),
            path = output_path,
            scale = 0.8)
    
    # Quarterly Trend in the Number of New Patients over time ---------------------
    
    
    # OU level
    # All Agencies --------------------------------
    
    ou_patient_delta(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .fiscal_year = selected_years,
      .ou = selected_ou,
      .subtitle = "Is South Sudan gaining or losing pediatric patients each quarter?",
      # can be one of 3 options: "Total", "Adults", or "Pediatric"
      .type = "Pediatric")
    
    si_save(paste0(metadata$curr_pd, "Patient_Delta_quarterly_SSD_peds.png"),
            path = output_path,
            scale = 0.8)
    
    # USAID ---------------------------------------
    
    ou_patient_delta(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou,
      .fiscal_year = selected_years,
      .funding_agency = selected_agency,
      .subtitle = "Are USAID supported facilities in South Sudan gainin or losing pediatric patients?",
      # can be one of 3 options: "Total", "Adults", or "Pediatric"
      .type = "Pediatric")
    
    si_save(paste0(metadata$curr_pd, "Patient_Delta_quarterly_SSD_USAID_peds.png"),
            path = output_path,
            scale = 0.8)
    
    # Quarterly Trend in IIT, IIT, and Change Over Time ----------------------------
    
    # OU level
    # All Agencies --------------------------------
    
    ou_iit_rtt_trend(
      .path = msd_path_ou,
      .df = df_ou,
      .fiscal_year = selected_years,
      .ou = selected_ou,
      .subtitle = "What is the quarterly trend in IIT, RTT, and Unexplained Loss/Gain
      among pediatric patients in South Sudan?",
      # can be one of 3 options: "Total", "Adults", or "Pediatric"
      .type = "Pediatric")
    
    si_save(paste0(metadata$curr_pd, "IIT_RTT_quarterly_SSD.png"),
            path = output_path,
            scale = 0.8)
    
    # USAID ---------------------------------------
    
    ou_iit_rtt_trend(
      .path = msd_path_ou,
      .df = df_ou,
      .fiscal_year = selected_years,
      .ou = selected_ou,
      .funding_agency = selected_agency,
      .subtitle = 
        "What is the quarterly trend in IIT, RTT, and Unexplained Loss/Gain 
        among pediatric patients treated at USAID supported facilities in South Sudan?",
      # can be one of 3 options: "Total", "Adults", or "Pediatric"
      .type = "Pediatric")
    
    si_save(paste0(metadata$curr_pd, "IIT_RTT_quarterly_SSD_peds.png"),
            path = output_path,
            scale = 0.8)

# AGYW (15-24) -----------------------------------------------------------------
