pacman::p_load(
  "shiny", "officer", "tidyverse","assertr", "forcats",
  "Cairo", "grid", "gridExtra", "gagglr", "scales",
  "stringr", "janitor", "rvg", "glue", "janitor", 
  "withr","writexl", "shinyWidgets", "shinydashboard",
  "shinydashboardPlus", "plotly", "shinybusy", "flexdashboard",
  "readxl"
)

# List of inputs used in the app -----------------------------------------------

inputs <- list(
  
  # folder where data files will be read in from
  input_dir = "Data/",
  
  # folder where templates are stored
  template_source = "POART-o-matic/templates/",
  
  # list of templates
  templates = list(
    template_one = "POART-o-matic/templates/Template_Cumulative_Achievement.pptx",
    template_two = "POART-o-matic/templates/Template_Quarterly_Achievement.pptx",
    template_three = "POART-o-matic/templates/Template_Quarterly_IIT_RTT.pptx", 
    template_four = "POART-o-matic/templates/Template_Quarterly_Patient_Delta.pptx" 
  ),
  
  # list of functions used by app
  functions = list(
    clean = "POART-o-matic/functions/00_loom.R",
    preview_rmd = "POART-o-matic/preview_.rmd",
    generate_rmd = "POART-o-matic/generate_.rmd")
)

# Lists of data files available to use in app ----------------------------------

# Create lists of templates available for data ---------------------------------
template_list <- list.files(inputs$template_source,
                            full.names = FALSE,
                            pattern = "Template"
)


# Setup user interface for app -------------------------------------------------

ui <- function() {
  
  ui <- dashboardPage(
    skin = "blue",
    # Title
    header = dashboardHeader(
      title = as.character("POART-al: 
                           For exploratory data analysis for POART presentations"),
      titleWidth = 800
    ),
    # Disable ugly sidebar
    sidebar = dashboardSidebar(disable = TRUE),
    # Create body of the page
    body = dashboardBody(
      fluidRow(
        # # progress spinner progress icon
        add_busy_spinner(
          spin = "fading-circle",
          timeout = 100,
          position = "top-left"
        ),
        # Initialize tabs
        # First tab
        tabBox(
          id = "panels",
          tabPanel(
            as.character("Treatment"),
            # select template
            shinyWidgets::pickerInput(
              inputId = "select_template",
              label = "Select a template file:",
              choices = template_list,
              options = list(size = 8)
            ),
            width = 4,
            tags$br(),
            # Figure Preview button
            actionButton(
              inputId = "figure_preview",
              label = "Preview Figure"
            ),
            # Generate slide button
            actionButton(
              inputId = "generate_slide",
              label = "Generate Slide"
            ),
            width = 4,
            tags$br()
          ) # close first tab
        ), # tab box
        box(
          title = "Figure Preview",
          htmlOutput("plot"), width = 12
        )
      ) # fluid row
    ) # dashboard body
  ) # dashboard page
  return(ui)
} # ui function

# Initialize connection to the R Shiny server ----------------------------------
server <- function(input, output, session) {
  
  # Update on figure preview ---------------------------------------------------
  
  # if the command is submitted
  # If the user selects to preview a figure
  observeEvent(input$figure_preview, {
    temp <- isolate(input$select_template)
    
    template_dict <- list(
      # replace with user indicated name of template
      "Template_Cumulative_Achievement.pptx" =
        list(
          in_file_p = inputs$functions$preview_rmd,
          out_file = "preview_cumul_achv.html"
        )
    )
    
    # Output the plots requested
    output$plot <- renderUI({
      rmarkdown::render(
        input = template_dict[[temp]]$in_file_p,
        params = param_list,
        output_dir = "./www/",
        output_format = "flexdashboard::flex_dashboard",
        quiet = FALSE
      )
      tags$html(
        tags$iframe(
          seamless = "seamless",
          src = template_dict[[temp]]$out_file,
          width = "100%",
          height = "800px",
          id = "reportIframe"
        )
      )
    })
  }) # end slide preview loop
  
  # If the "Generate Slide button is pushed,
  observeEvent(input$generate_slide, {
    temp <- isolate(input$select_template)
    
    template_dict <- list(
      # replace with user indicated name of template
      "Template_Cumulative_Achievement.pptx" =
        list(in_file_g = inputs$functions$generate_rmd)
    )
    
    # generate the slides with user-provided parameters
    rmarkdown::render(
      input = template_dict[[temp]]$in_file_g,
      params = param_list,
      quiet = FALSE
    )
  })
} # server
# run complete App -------------------------------------------------------------
shinyApp(ui, server) # Shiny App