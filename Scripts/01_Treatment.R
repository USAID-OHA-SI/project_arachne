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

# Cumulative trends ----------------------------------------------------------

  # OU level
    # All agencies ------------------------------------
    
    # TX_CURR example
    
    ou_trend_cum(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou,
      # choose up to 2
      .fiscal_year = selected_years)
    
    si_save(paste0(metadata$curr_pd, "TX_CURR_summary_SSD.png"),
      path = output_path,
      scale = 0.8)
    
    # TX_NEW example
    
    ou_trend_cum(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_NEW",
      .ou = selected_ou,
      # choose up to 2
      .fiscal_year = selected_years)
    
    si_save(paste0(metadata$curr_pd, "TX_NEW_summary_SSD.png"),
      path = output_path,
      scale = 0.8)
    
    # USAID ---------------------------------------------
    
    # TX_CURR example
    
    ou_trend_cum(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou,
      # choose up to 2
      .fiscal_year = selected_years,
      .funding_agency = selected_agency)
    
    si_save(paste0(metadata$curr_pd, "TX_CURR_summary_SSD_USAID.png"),
      path = output_path,
      scale = 0.8)
    
    # TX_NEW example
    
    ou_trend_cum(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_NEW",
      .ou = selected_ou,
      # choose up to 2
      .fiscal_year = selected_years,
      .funding_agency = selected_agency)
    
    si_save(paste0(metadata$curr_pd, "TX_NEW_summary_SSD_USAID.png"),
      path = output_path,
      scale = 0.8)
     
# Quarterly growth trend -----------------------------------------------------


  # OU level

    # All Agencies ---------------------------------------------
    
    ou_trend(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou, 
      .type = "Total")
    
    si_save(paste0(metadata$curr_pd, "TX_CURR_quarterly_SSD.png"),
      path = output_path,
      scale = 0.8)
    
    # USAID ----------------------------------------------------
    
    ou_trend(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou,
      .funding_agency = selected_agency, 
      .type = "Total")
    
    si_save(paste0(metadata$curr_pd, "TX_CURR_quarterly_SSD_USAID.png"),
      path = output_path,
      scale = 0.8)
    
# Quarterly Trend in IIT, IIT, and Change Over Time --------------------------

  # OU level
    # All Agencies --------------------------------
    
    ou_iit_rtt_trend(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou)
        
    si_save(paste0(metadata$curr_pd, "IIT_RTT_quarterly_SSD.png"),
      path = output_path,
      scale = 0.8)
    
    # USAID ---------------------------------------
    
    ou_iit_rtt_trend(
      .path = msd_path_ou,
      .df = df_ou,
      .indicator = "TX_CURR",
      .ou = selected_ou,
      .funding_agency = selected_agency)
    
    si_save(paste0(metadata$curr_pd, "IIT_RTT_quarterly_SSD_USAID.png"),
      path = output_path,
      scale = 0.8)
    
    # Are we gaining or losing patients each quarter?
    
    # All agencies
    
    
    
    
    ou_patient_change_trend(
      .path = msd_path_ou,
      .df = df_ou_filt,
      .ou = selected_ou
    )
    
    # USAID only
    
    ou_patient_change_trend(
      .path = msd_path_ou,
      .df = df_ou_filt,
      .ou = selected_ou,
      .funding_agency = selected_agency
    )
    
    
    
    
    

# Peds ---------------------------------------------------------------------

# TX-CURR Performance quarterly growth trend
    
# TX_NEW Performance quarterly growth trend
    
# Quarterly Trend in IIT
    

# AGYW (15-24) -------------------------------------------------------------

# TX-CURR Performance quarterly growth trend
    
# TX_NEW Performance quarterly growth trend
    
# Quarterly Trend in IIT
