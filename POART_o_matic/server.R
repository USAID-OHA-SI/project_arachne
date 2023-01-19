# PROJECT:  project_arachne
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  Server settings for POART-l
# REF ID:   2ecb28be
# LICENSE:  MIT
# DATE:     2023-01-18
# UPDATED:  2023-01-19

function(input, output, session) {

  # If the user selects to preview a figure...
  
  observeEvent(input$figure_preview, {
    
    # get their inputs...
    selected_ou <- as.character(input$select_ou)
    selected_indicator <- as.character(input$select_indicator)
    # selected_years = as.list(input$select_year)
    selected_agency <- as.character(input$select_agency)
    selected_type <- as.character(input$select_type)
    
    # set them as the parameters for the RMD...
    param_list <- list(
      selected_ou = selected_ou,
      selected_indicator = selected_indicator,
      # selected_years = selected_years,
      selected_agency = selected_agency,
      selected_type = selected_type)
    
    # TODO: add back functionality to select and use selected template

    # establish the input and output files...
    dict <- list(
      in_file = here::here("POART_o_matic/Scripts/preview_01.Rmd"),
      out_file = "preview_01.html")

    # and output the plots requested in the app.
    output$plot <- renderUI({
      rmarkdown::render(
        input = dict$in_file,
        params = param_list,
        output_dir = "./www/",
        output_format = "flexdashboard::flex_dashboard",
        quiet = FALSE)
      tags$html(
        tags$iframe(
          seamless = "seamless",
          src = dict$out_file,
          width = "100%",
          height = "500px",
          id = "reportIframe")
      )})}) # end preview loop

  # If the user selects to generate a slide...
  observeEvent(input$generate_slide, {

    # get their inputs...
    selected_ou <- as.character(input$select_ou)
    selected_indicator <- as.character(input$select_indicator)
    # selected_years = as.list(input$select_year)
    selected_agency <- as.character(input$select_agency)
    selected_type <- as.character(input$select_type)
    
    # set them as the parameters for the RMD...
    param_list <- list(
      selected_ou = selected_ou,
      selected_indicator = selected_indicator,
      # selected_years = selected_years,
      selected_agency = selected_agency,
      selected_type = selected_type)

    # and generate the requested slide
    rmarkdown::render(
      input = here::here("POART_o_matic/Scripts/generate_01.Rmd"),
      params = param_list,
      quiet = FALSE)})} # end generate loop
