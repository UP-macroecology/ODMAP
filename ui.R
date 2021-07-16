library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(DT)


ui <-  tagList(
  tags$head(tags$style(HTML(readChar("www/odmap.css", file.info("www/odmap.css")$size)))),
  useShinyjs(),

  navbarPage(
    id = "navbar",
    windowTitle = "ODMAP v1.0",
    title = div(
      div(
        id = "github_logo", 
        a(img(src="github_logo_40px.png"), href = "https://github.com/UP-macroecology/ODMAP", target="_blank")
      ),
      "ODMAP v1.0"
    ),
    position = "fixed-top",
    theme = shinytheme("cosmo"),
    selected = "about",
  
    # HOME TAB
    tabPanel("What is ODMAP?", value = "about", fluidPage(
      fluidRow(
        column(width = 2),
        column(width = 8, 
               p("What is ODMAP?", style = "padding-top: 10px; font-size: 30px; font-weight:bold;"),
               p("Species distribution models (SDMs) constitute the most common class of biodiversity models. The advent of ready-to-use software packages and
             increasing availability of digital geo-information have considerably assisted the application of SDMs in recent years  enabling their use in
             informing conservation and management, and quantifying impacts from global change.", style= "font-size: 18px;"),
               p("However, models must be fit for purpose, with all important aspects of their development and applications properly considered. 
             Despite the widespread use of SDMs, the standardisation and documentation of model protocols remains limited. To address these issues, 
             we propose a standard protocol for reporting SDMs. We call this the ODMAP (Overview, Data, Model, Assessment and Prediction) protocol
             as each of its components reflects the main steps involved in building SDMs and other empirically-based biodiversity models (",a(href = 'https://doi.org/10.1111/ecog.04960', 'Zurell et al. 2020', target = "_blank", .noWS = "outside"),").", style= "font-size: 18px;"), 
               img(src = "workflow.jpg", width = "60%", style="display: block; margin-left: auto; margin-right: auto; min-width: 500px;"), br(),
               p("The ODMAP protocol serves two main purposes. First, it provides a checklist for authors detailing key steps for model building and analyses. 
             Second, it introduces a standard approach to documentation that ensures transparency and reproducibility, facilitating peer review and 
             expert evaluation of model quality as well as meta-analyses.", style= "font-size: 18px;"),
               p("This Shiny web application helps to implement the ODMAP approach and produces well formatted protocols that can be exported for further usage. 
                 For further explanation please refer to the original publication (Zurell et al. 2020).", style= "font-size: 18px;"),
               em(p("Please cite as follows:", style = "font-size: 18px;")),
               p("Zurell D, Franklin J, König C, Bouchet PJ, Serra-Diaz JM, Dormann CF, Elith J, Fandos Guzman G, Feng X, Guillera-Arroita G, Guisan A, Leitão PJ, 
                 Lahoz-Monfort JJ, Park DS, Peterson AT,  Rapacciuolo G, Schmatz DR, Schröder B, Thuiller W, Yates KL, Zimmermann NE, Merow C (2020) 
                 A standard protocol for describing species distribution models. Ecography 43: 1261-1277. DOI: ",
                 a(href = 'https://doi.org/10.1111/ecog.04960', '10.1111/ecog.04960', target = "_blank", .noWS = "outside"), style= "font-size: 18px;")
        )),
      column(width = 2)
    )),
    
    
    tabPanel("How to use this app", value = "howto", fluidPage(
      fluidRow(
        column(width = 2),
        column(width = 8, 
               strong(p("How to create an ODMAP protocol", style = "padding-top: 10px; font-size: 30px; font-weight:bold;")),
               p("Enter all relevant information into the fields provided under 'Create a protocol'. Your progress in individual ODMAP sections is displayed in the 
             side bar at the left. The 'Hide optional fields' switch on the left allows you to only display mandatory fields, which depend on the model objective chosen.
             A preview of your current protocol is available in the 'Protocol Viewer'.", style = "font-size: 18px;"),
               p("You can always save your progress by clicking the download button on the left. We recommend always downloading the csv file as this will allow you to resume your work later. After downloading your protocol as csv, it is safe to close the Shiny app. For a better layout, you can also download the ODMAP protocol as word document. Please note that word documents cannot be uploaded again to the Shiny app.", style = "font-size: 18px;"),
               p("You will be able to resume working on your protocol by choosing the Upload tab above and uploading your previously saved ODMAP csv file. 
             In addition, you can import objects generated with the ", 
                 a(href = 'https://cran.r-project.org/web/packages/rangeModelMetadata/index.html', 'rangeModelsMetaData', target = "_blank", .noWS = "outside"),
                 " R-package to autofill your ODMAP protocol.", style = "font-size: 18px;"),
               p("Should you have any conceptual questions or feedback regarding the ODMAP protocol, or any technical questions and feedback regarding the Shiny app, please do not hesitate to visit our ", 
                 a(href = 'https://github.com/UP-macroecology/ODMAP', 'Github repository', target = "_blank", .noWS = "outside"), 
                 " or contact us directly at ", 
                 a(href = 'mailto:odmap@wsl.ch', 'odmap@wsl.ch', .noWS = "outside"), ".", style = "font-size: 18px;")
        )),
      column(width = 2)
    )),
    
    tabPanel("Create a protocol", value = "create", sidebarLayout(
      sidebarPanel(
        style = "position:fixed; width: 16%;",
        width = 2,
        
        h5("Progress", style = "font-weight: bold"),
        uiOutput("progress_bars"),
        
        h5("Hide optional fields", style = "font-weight: bold"),
        materialSwitch("hide_optional", label = NULL, status = "danger"),
        
        h5("Download protocol", style = "font-weight: bold"),
        radioButtons("document_format", label = NULL, choices = c("csv", "docx")),
        downloadButton("protocol_download")
      ),
      
      mainPanel(
        tabsetPanel(
          id = "tabset",
          tabPanel("1. Overview", value = "Overview",  fluidPage(
            em(p("Give a brief overview of all important parts of your study.", style = "padding-top: 10px; font-weight: 300;")),
            uiOutput("Overview_UI")
          )),
          
          tabPanel("2. Data", value = "Data", fluidPage(
            em(p("Describe your your data in detail.", style = "padding-top: 10px; font-weight: 300")),
            uiOutput("Data_UI")
          )),
          
          tabPanel("3. Model", value = "Model", fluidPage(
            em(p("Describe your modeling approach in detail.", style = "padding-top: 10px; font-weight: 300")),
            uiOutput("Model_UI")
          )),
          
          tabPanel("4. Assessment", value = "Assessment", fluidPage(
            em(p("Describe how you assessed your model results.", style = "padding-top: 10px; font-weight: 300")),
            uiOutput("Assessment_UI")
          )),
          
          tabPanel("5. Prediction", value = "Prediction", fluidPage(
            em(p("Describe your model predictions in detail.", style = "padding-top: 10px; font-weight: 300")),
            uiOutput("Prediction_UI")
          )) 
        ))
    )),
    
    # PREVIEW PROTOCOL
    tabPanel("Protocol viewer", value = "viewer", fluidPage(
      fluidRow(
        column(width = 2),
        column(width = 8, htmlOutput("markdown")),
        column(width = 2)
      )
    )),
    
    tabPanel("Upload / Import", value = "import", fluidPage(
      fluidRow(
        column(width = 2),
        column(width = 8, 
               p("There are two options for importing data into your ODMAP protocol", style = "font-size: 18px;"),
               p(tags$b("(1) Upload an ODMAP protocol (.csv)"), br(), "This option is convenient if you want to edit or resume working on a previously saved ODMAP protocol.", style = "font-size: 18px;"),
               p(tags$b("(2) Upload an RMM file (.RDS or .csv)"), br(), "The rangeModelMetaData package of Merow et al. (2019) allows exporting standardized metadata 
                         for SDMs directly from R. Note that the objectives of ODMAP and RMM differ and not all fields can be mapped between both approaches. 
                         This option is therefore not a replacement for filling out ODMAP, but may be helpful for e.g. documenting model settings or references. 
                         If RMM values have been imported, the corresponding field and entity is indicated in parentheses as Field1($Field2)($Field3)-Entity)", style = "font-size: 18px;"),
               p("Choose file", style = "font-size: 18px; font-weight: bold"),
               fileInput("upload", label = NULL, accept = c(".csv")),
               uiOutput("Upload_UI")),
        column(width = 2)
      )
    ))
  )
)