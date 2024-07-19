library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(DT)
library(tidyverse)
library(rangeModelMetadata)

odmap_dict = read.csv("www/odmap_dict.csv", header = T, stringsAsFactors = F)
rmm_dict = rmmDataDictionary()

server <- function(input, output, session) {
  # ------------------------------------------------------------------------------------------#
  #                           Rendering functions for UI elements                             # 
  # ------------------------------------------------------------------------------------------#
  render_text = function(element_id, element_placeholder){
    textAreaInput(inputId = element_id, label = element_placeholder, height = "45px", resize = "vertical")
  }
  
  render_authors = function(){
    div(
      dataTableOutput("authors_table", width = "100%"),
      actionButton("add_author", label = NULL, icon = icon("plus")),
      br(), br()
    )
  }
  
  render_objective = function(element_id, element_placeholder){
    selectizeInput(inputId = element_id, label = NULL, multiple = F, options = list(create = T, placeholder = "Choose from list"),
                   choices = list("", "Inference and explanation", "Mapping and interpolation", "Forecast and transfer"))
  }
  
  render_suggestion = function(element_id, element_placeholder, suggestions){
    suggestions = sort(trimws(unlist(strsplit(suggestions, ","))))
    selectizeInput(inputId = element_id, label = element_placeholder, choices = suggestions, multiple = TRUE, options = list(create = T,  placeholder = "Choose from list or insert new values"))
  }
  
  render_extent = function(element_id){
    if(element_id == "o_scale_1"){
      splitLayout(
        cellWidths = c("170px",  "150px", "150px", "150px", "150px"),
        p("Spatial extent (long/lat)", style = "padding: 9px 12px"),
        textAreaInput(inputId = paste0(element_id, "_xmin"), label = "xmin", height = "45px", resize = "none"),
        textAreaInput(inputId = paste0(element_id, "_xmax"), label = "xmax", height = "45px", resize = "none"),
        textAreaInput(inputId = paste0(element_id, "_ymin"), label = "ymin", height = "45px", resize = "none"),
        textAreaInput(inputId = paste0(element_id, "_ymax"), label = "ymax", height = "45px", resize = "none")
      )
    } else {
      splitLayout(
        cellWidths = c("120px",  "150px", "150px", "150px", "150px"),
        p("Spatial extent", style = "padding: 9px 12px"),
        textAreaInput(inputId = paste0(element_id, "_xmin"), label = "xmin", height = "45px", resize = "none"),
        textAreaInput(inputId = paste0(element_id, "_xmax"), label = "xmax", height = "45px", resize = "none"),
        textAreaInput(inputId = paste0(element_id, "_ymin"), label = "ymin", height = "45px", resize = "none"),
        textAreaInput(inputId = paste0(element_id, "_ymax"), label = "ymax", height = "45px", resize = "none")
      )
    }
  }
  
  render_model_algorithm = function(element_id, element_placeholder){
    selectizeInput(inputId = element_id, label = element_placeholder, choices = model_settings$suggestions, multiple = TRUE, options = list(create = T,  placeholder = "Choose from list or insert new values"))
  }
  
  render_model_settings = function(){
    div(
      em(p("Edit fields by double clicking in the table. Add new settings with the plus sign.", style = "font-weight: 300;")),
      tabsetPanel(id = "settings_tabset"),
      actionButton("add_setting", label = NULL, icon = icon("plus")),
      br(),br()
    )
  }
  
  render_section = function(section, odmap_dict){
    section_dict = filter(odmap_dict, section == !!section) 
    section_rendered = renderUI({
      section_UI_list = vector("list", nrow(section_dict)) # holds UI elements for all ODMAP elements belonging to 'section'
      subsection = ""
      for(i in 1:nrow(section_dict)){
        element_UI_list = vector("list", 3) # holds UI elements for current element 
        
        # First element: Header 
        if(subsection != section_dict$subsection_id[i]){
          subsection = section_dict$subsection_id[i]
          subsection_label = section_dict$subsection[i]
          element_UI_list[[1]] = div(id = section_dict$subsection_id[i], h5(subsection_label, style = "font-weight: bold"))
        }
        
        # Second element: Input field(s) 
        element_UI_list[[2]] = switch(section_dict$element_type[i],
                                      text = render_text(section_dict$element_id[i], section_dict$element_placeholder[i]),
                                      author = render_authors(),
                                      objective = render_objective(section_dict$element_id[i], section_dict$element_placeholder[i]),
                                      suggestion = render_suggestion(section_dict$element_id[i], section_dict$element_placeholder[i], section_dict$suggestions[i]),
                                      extent = render_extent(section_dict$element_id[i]),
                                      model_algorithm = render_model_algorithm(section_dict$element_id[i], section_dict$element_placeholder[i]),
                                      model_setting = render_model_settings())
        
        # Third element: Next/previous button
        if(i == nrow(section_dict)){
          # TODO add next/previous buttons
        }
        
        # Reduce list to non-empty elements
        element_UI_list = Filter(Negate(is.null), element_UI_list)
        section_UI_list[[i]] = element_UI_list
      }
      return(section_UI_list)
    })
    return(section_rendered)
  }
  
  # ------------------------------------------------------------------------------------------#
  #                            Rendering functions for Markdown Output                        # 
  # ------------------------------------------------------------------------------------------#
  # Functions for dynamically knitting text elements
  knit_section = function(section_id){
    section = unique(odmap_dict$section[which(odmap_dict$section_id == section_id)])
    cat("\n\n##", section, "\n")
  }
  
  knit_subsection= function(subsection_id){
    # Get all elements
    element_ids = odmap_dict$element_id[which(odmap_dict$subsection_id == subsection_id)]
    subsection = unique(odmap_dict$subsection[which(odmap_dict$subsection_id == subsection_id)])
    
    # Find out whether subsection needs to be rendered at all
    # Are all elements optional?
    all_optional = all((element_ids %in% elem_hide[[input$o_objective_1]] | element_ids %in% elem_optional))
    
    # If not, render header
    if(!all_optional){
      cat("\n\n####", subsection, "\n")
    } else { # if not, render header only when user provided optional inputs
      all_empty = T
      for(id in element_ids){
        if(input[[id]] != ""){
          all_empty = F
          break
        }
      }
      if(!all_empty){
        cat("\n\n####", subsection, "\n")
      }
    }
  }
  
  knit_text = function(element_id){
    if(input[[element_id]] == ""){
      knit_missing(element_id)
    } else {
      element_name = odmap_dict$element[which(odmap_dict$element_id == element_id)]
      cat("\n", element_name, ": ", input[[element_id]], "\n", sep="")
    }
  }
  
  knit_authors = function(element_id){
    paste(authors$df$first_name, authors$df$last_name, collapse = ", ")
  }
  
  knit_extent = function(element_id){
    if(any(c(input[[paste0(element_id, "_xmin")]], input[[paste0(element_id, "_xmax")]], input[[paste0(element_id, "_ymin")]], input[[paste0(element_id, "_ymax")]]) == "")){
      knit_missing(element_id)
    } else {
      element_value = paste(c(input[[paste0(element_id, "_xmin")]], input[[paste0(element_id, "_xmax")]], 
                              input[[paste0(element_id, "_ymin")]], input[[paste0(element_id, "_ymax")]]), collapse = ", ")
      cat("\nSpatial extent: ", element_value, " (xmin, xmax, ymin, ymax)\n", sep="")
    }
  }
  
  knit_suggestion = function(element_id){
    if(is.null(input[[element_id]])){
      knit_missing(element_id)
    } else {
      element_name = odmap_dict$element[which(odmap_dict$element_id == element_id)]
      cat("\n", element_name, ": ", paste(input[[element_id]], collapse = ", "), "\n", sep="")
    }
  }
  
  knit_model_settings = function(element_id){
    if(is.null(input[["o_algorithms_1"]])){
      knit_missing(element_id)
    } else {
      for(alg in input[["o_algorithms_1"]]){
        settings_tab = model_settings[[alg]] %>% filter(value != "")
        if(nrow(settings_tab) == 0) {
          cat("\n\n <span style='color:#DC3522'>\\<", alg, "\\> </span>\n", sep = "")
        } else {
          settings_char = paste0(settings_tab$setting, " (", settings_tab$value, ")", collapse = ", ")
          cat("\n", alg, ": ", settings_char, "\n", sep="")
        }
      }
    }
  }
  
  knit_missing = function(element_id){
    if(!(element_id %in% elem_hide[[input$o_objective_1]] | element_id %in% elem_optional)){
      placeholder = odmap_dict$element[which(odmap_dict$element_id == element_id)]
      cat("\n\n <span style='color:#DC3522'>\\<", placeholder, "\\> </span>\n", sep = "")
    }
  }
  
  # ------------------------------------------------------------------------------------------#
  #                                   Import functions                                        # 
  # ------------------------------------------------------------------------------------------#
  # ODMAP import functions
  import_odmap_to_text = function(element_id, values){
    if(input[[element_id]] == "" | input[["replace_values"]] == "Yes"){
      updateTextAreaInput(session = session, inputId = element_id, value = values)  
    }
  }
  
  import_odmap_to_authors = function(element_id, values){
    if(nrow(authors$df) == 0 | input[["replace_values"]] == "Yes"){
      names_split = unlist(strsplit(values, split = "; "))
      names_split = regmatches(names_split, regexpr(" ", names_split), invert = TRUE)
      authors$df = authors$df[0,] # Delete previous entries
      for(i in 1:length(names_split)){
        author_tmp = names_split[[i]]
        authors$df = rbind(authors$df, data.frame("first_name" = author_tmp[1],  "last_name" = author_tmp[2]))
      }
    }
  }
  
  import_odmap_to_model_objective = function(element_id, values){
    if(input[[element_id]] == "" | input[["replace_values"]] == "Yes"){
      updateSelectizeInput(session = session, inputId = "o_objective_1", selected = values)
    }
  }
  
  import_odmap_to_extent = function(element_id, values){
    values = gsub("(^.*)( \\(xmin, xmax, ymin, ymax\\)$)", "\\1", values)
    values_split = unlist(strsplit(values, ", "))
    names(values_split) = c("xmin", "xmax", "ymin", "ymax")
    for(i in 1:length(values_split)){
      if(input[[paste0(element_id, "_", names(values_split[i]))]] == "" | input[["replace_values"]] == "Yes"){
        updateTextAreaInput(session = session, inputId = paste0(element_id, "_", names(values_split[i])), value = as.numeric(values_split[i]))
      }
    }
  }
  
  import_odmap_to_model_algorithm = function(element_id, values){
    if(length(input[[element_id]]) == 0 | input[["replace_values"]] == "Yes"){
      values = unlist(strsplit(values, split = "; "))
      suggestions_new =  sort(trimws(c(model_settings$suggestions, as.character(values))))
      updateSelectizeInput(session = session, inputId = element_id, choices = suggestions_new, selected = values)  
    }
  }
  
  # RMMS import functions
  import_rmm_to_text = function(element_id, values){
    if(input[[element_id]] == "" | input[["replace_values"]] == "Yes"){
      updateTextAreaInput(session = session, inputId = element_id, 
                          value = paste(paste0(values, " (", names(values), ")"), collapse = ",\n"))
    }
  }
  
  import_rmm_to_authors = function(element_id, values){
    if(nrow(authors$df) == 0 | input[["replace_values"]] == "Yes"){
      names_split = unlist(strsplit(values[[1]], split = " and "))
      names_split = strsplit(names_split, split = ", ")
      authors$df = authors$df[0,] # Delete previous entries
      for(i in 1:length(names_split)){
        author_tmp = names_split[[i]]
        authors$df = rbind(authors$df, data.frame("first_name" = author_tmp[2],  "last_name" = author_tmp[1]))
      }
    }
  }
  
  import_rmm_to_extent = function(element_id, values){
    values = trimws(unlist(strsplit(values[[1]], ";")))
    for(i in 1:length(values)){
      values_split = unlist(strsplit(values[i], ": "))
      if(input[[paste0(element_id, "_", values_split[1])]] == "" | input[["replace_values"]] == "Yes"){
        updateTextAreaInput(session = session, inputId = paste0(element_id, "_", values_split[1]), value = as.numeric(values_split[2]))
      }
    }
  }
  
  # Generic import functions
  import_model_settings = function(element_id, values){
    settings_all = unlist(strsplit(values, split = "; ",))
    algorithms = c()
    for(settings_tmp in settings_all){
      if(grepl("no settings provided", settings_tmp)){next}
      settings_split = unlist(regmatches(settings_tmp, regexpr(": ", settings_tmp), invert = TRUE)) # split at first instance of ":"
      alg = settings_split[1]
      algorithms = c(algorithms, alg)
      values_indices = gregexpr("\\((?>[^()]|(?R))*\\)", settings_split[2], perl = T) # indices of model settings in parentheses and string length per setting
      values_start = unlist(values_indices) + 1
      values_end = values_start + attr(values_indices[[1]], "match.length") - 3
      settings_start = c(1, values_end[-length(values_end)] + 4)
      settings_end = c(values_start - 3)
      values_extr = substring(settings_split[2], values_start, values_end)
      settings_extr = substring(settings_split[2], settings_start, settings_end)
      settings_df = data.frame(setting = settings_extr, value = values_extr, stringsAsFactors = F)
      model_settings_import[["algorithms"]] = c(model_settings_import[["algorithms"]], alg)
      model_settings_import[[alg]] = settings_df
    }
    updateSelectizeInput(session = session, inputId = "o_algorithms_1", selected = algorithms)
  }
  
  import_suggestion = function(element_id, values){
    if(length(input[[element_id]]) == 0 | input[["replace_values"]] == "Yes"){
      values = trimws(unlist(strsplit(values, split = ";")))
      suggestions = unlist(strsplit(odmap_dict$suggestions[odmap_dict$element_id == element_id], ","))
      suggestions_new =  sort(trimws(c(suggestions, as.character(values))))
      updateSelectizeInput(session = session, inputId = paste0(element_id), choices = suggestions_new, selected = as.character(values))
    }
  }
  
  # ------------------------------------------------------------------------------------------#
  #                                     Export functions                                      # 
  # ------------------------------------------------------------------------------------------#
  export_standard = function(element_id){
    val = input[[element_id]]
    return(ifelse(!is.null(val), val, NA))
  }
  
  export_authors = function(element_id){
    return(ifelse(nrow(authors$df) > 0, paste(authors$df$first_name, authors$df$last_name, collapse = "; "), NA))
  }
  
  export_suggestion = function(element_id){
    val = input[[element_id]]
    return(ifelse(!is.null(val), paste(input[[element_id]], collapse = "; "), NA))
  }
  
  export_extent = function(element_id){
    if(any(c(input[[paste0(element_id, "_xmin")]], input[[paste0(element_id, "_xmax")]], input[[paste0(element_id, "_ymin")]], input[[paste0(element_id, "_ymax")]]) == "")){
      return(NA)
    } else {
      values = paste(c(input[[paste0(element_id, "_xmin")]], input[[paste0(element_id, "_xmax")]], 
                       input[[paste0(element_id, "_ymin")]], input[[paste0(element_id, "_ymax")]]), collapse = ", ")
      return(paste0(values, " (xmin, xmax, ymin, ymax)"))
    }
  }
  
  export_model_setting = function(element_id){
    if(is.null(input[["o_algorithms_1"]])){
      return(NA)
    } else {
      settings = c()
      for(alg in input[["o_algorithms_1"]]){
        settings_tab = model_settings[[alg]] %>% filter(value != "")
        if(nrow(settings_tab) == 0) {
          settings[alg] = paste0(alg, ": no settings provided")
        } else {
          settings_char = paste0(settings_tab$setting, " (", settings_tab$value, ")", collapse = ", ")
          settings[alg] = paste0(alg, ": ", settings_char)
        }
      }
      return(paste0(settings, collapse = "; "))
    }
  }
  
  # ------------------------------------------------------------------------------------------#
  #                                   UI Elements                                             # 
  # ------------------------------------------------------------------------------------------#
  # "Create a protocol" - mainPanel elements
  output$Overview_UI = render_section("Overview", odmap_dict)
  output$Data_UI = render_section("Data", odmap_dict)
  output$Model_UI = render_section("Model", odmap_dict)
  output$Assessment_UI = render_section("Assessment", odmap_dict)
  output$Prediction_UI = render_section("Prediction", odmap_dict)
  
  for(tab in c("Overview_UI", "Data_UI", "Model_UI", "Assessment_UI", "Prediction_UI")){
    outputOptions(output, tab, suspendWhenHidden = FALSE) # Add tab contents to output object before rendering
  } 
  
  # -------------------------------------------
  # "Create a protocol" - sidebarPanel elements
  get_progress = reactive({
    progress = c()
    for(sect in unique(odmap_dict$section)){
      all_elements = odmap_dict %>% 
        filter(section == sect & !element_id %in% unlist(elem_hidden) & !element_type %in% c("model_setting", "author")) %>% 
        filter(if(input$hide_optional) !element_id %in% elem_optional else T) %>% 
        mutate(element_id = ifelse(element_type == "extent", paste0(element_id, "_xmin"), element_id)) %>%  
        pull(element_id)
      if(length(all_elements) == 0){
        next 
      } else {
        completed_elements = sum(sapply(all_elements, function(x){
          !(identical(input[[x]], "") | identical(input[[x]], 0) | identical(input[[x]], NULL))
        }, USE.NAMES = T, simplify = T))
        progress[sect] = (sum(completed_elements) / length(all_elements)) * 100
      }
    }
    return(progress)
  }) 
  
  output$progress_bars = renderUI({
    progress = get_progress()
    progress_UI_list = lapply(names(progress), function(sect){
      progressBar(paste("progress", sect, sep = "_"), value = progress[sect], title = sect)
    })
    return(progress_UI_list)
  })
  
  output$protocol_download = downloadHandler(
    filename = function(){
      author_list = authors$df$last_name
      if(length(author_list) > 2){
        name_string = paste0(author_list[1],"_EtAl")
      } else if(length(author_list) == 2){
        name_string = paste0(author_list[1], author_list[2])
      } else if(length(author_list) == 1){
        name_string = author_list[1]
      } else {
        name_string = "Anonymous"
      }
      paste0("ODMAP_", name_string, "_", Sys.Date(), ".", input$document_format)
    },
    content = function(file){
      odmap_download = odmap_dict %>% 
        filter(!element_id %in% elem_hide[[input$o_objective_1]]) %>% # use only relevent rows
        dplyr::select(section, subsection, element, element_id, element_type) %>% 
        mutate(Value = NA)
      
      # Create .csv-files
      if(input$document_format == "csv"){
        # Create table
        for(i in 1:nrow(odmap_download)){
          odmap_download$Value[i] = switch(odmap_download$element_type[i],
                                           text = export_standard(odmap_download$element_id[i]),
                                           author = export_authors(odmap_download$element_id[i]),
                                           objective = export_standard(odmap_download$element_id[i]),
                                           suggestion = export_suggestion(odmap_download$element_id[i]),
                                           extent = export_extent(odmap_download$element_id[i]),
                                           model_algorithm = export_suggestion(odmap_download$element_id[i]),
                                           model_setting = export_model_setting(odmap_download$element_id[i]),
                                           "") 
        }
        
        odmap_download$element_id = NULL
        odmap_download$element_type = NULL
        
        # Write output
        file_conn = file(file, open = "w")
        write.csv(odmap_download, file = file_conn, na = "", row.names = F)
        close(file_conn)
        
        # CREATE WORD FILES
      } else {
        src <- normalizePath("protocol_output.Rmd")
        
        # temporarily switch to the temp dir, in case of missing write permission in the current working directory
        wd_orig <- setwd(tempdir())
        on.exit(setwd(wd_orig))
        file.copy(src, "protocol_output.Rmd", overwrite = TRUE)
        odmap_download = rmarkdown::render("protocol_output.Rmd", rmarkdown::word_document(),
                                           params = list(study_title = paste(input$o_authorship_1), authors = paste(authors$df$first_name, authors$df$last_name, collapse = ", ")))
        file.rename(odmap_download, file)
      }
    }
  )
  
  # -------------------------------------------
  # "Protocol Viewer"
  output$markdown = renderUI({includeMarkdown(knitr::knit("protocol_preview.Rmd", quiet = T))})
  
  # -------------------------------------------
  # "Upload / Import"
  output$Upload_UI = renderUI({
    UI_list = list()
    if(!is.null(input$upload)){
      # Obtain file extension
      file_type = gsub( "(^.*)(\\.[A-z]*$)", replacement = "\\2", input$upload$datapath)
      if(!file_type %in% c(".csv", ".RDS")){
        showNotification("Please select and provide a .csv (ODMAP, RMMS) or .RDS file (RMMS).", duration = 3, type = "error")
        reset("upload")
        return()
      }
      
      # Read in file
      if(file_type == ".RDS"){
        tryCatch({
          protocol_upload = rmmToCSV(protocol_upload, input$upload$datapath)
        }, error = function(e){
          showNotification("Could not read file.", duration = 3, type = "error")
          reset("upload")
          return()
        })
      } else {
        protocol_upload = read.csv(file = input$upload$datapath, header = T, sep = ",", stringsAsFactors = F, na.strings = c("NA", "", "NULL"))
      }
      
      # Identify protocol type
      if(all(c("section", "subsection", "element", "Value") %in% colnames(protocol_upload))){
        protocol_type = "ODMAP"
      } else if(all(c("Field.1", "Field.2", "Field.3", "Entity", "Value") %in% colnames(protocol_upload))){
        protocol_type = "RMMS"
      } else {
        showNotification("Please select a valid ODMAP or RMMS file", duration = 3, type = "error")
        reset("upload")
        return()
      }
      
      if(sum(!is.na(protocol_upload$Value))>0){
        UI_list[[1]] = p(paste0("File: ", input$upload$name, " (", protocol_type, " protocol, ", sum(!is.na(protocol_upload$Value)), " non-empty fields)"))
        UI_list[[2]] = radioButtons("replace_values", "Overwrite non-empty fields with uploaded values?", choices = c("Yes", "No"), selected = "No")
        UI_list[[3]] = actionButton(paste0(protocol_type, "_to_input"), "Copy to input form")
      } else{
        showNotification("Please select a ODMAP or RMMS file with at least one non-empty field", duration = 5, type = "error")
      }
      
    }
    return(UI_list)
  })
  
  # Monitor current progress
  elem_hide = list("Inference and explanation" = c(pull(odmap_dict %>% filter(inference == 0), element_id), # unused elements
                                                   unique(pull(odmap_dict %>% group_by(subsection_id) %>% filter(all(inference  == 0)), subsection_id)), # unused subsections
                                                   "p"),
                   "Prediction and mapping" =  c(pull(odmap_dict %>% filter(prediction == 0), element_id), 
                                                 unique(pull(odmap_dict %>% group_by(subsection_id) %>% filter(all(prediction == 0)), subsection_id))),
                   "Projection and transfer" =  c(pull(odmap_dict %>% filter(projection == 0), element_id),
                                                  unique(pull(odmap_dict %>% group_by(subsection_id) %>% filter(all(projection  == 0)), subsection_id))))
  
  elem_optional = c(pull(odmap_dict %>% filter(optional == 1), element_id), # optional elements
                    unique(pull(odmap_dict %>% group_by(subsection_id) %>% filter(all(optional == 1)), subsection_id))) # optional subsections)
  
  elem_hidden = "" # keep track of hidden elements
  
  # ------------------------------------------------------------------------------------------#
  #                                      Event handlers                                       # 
  # ------------------------------------------------------------------------------------------#
  # Study objective
  observeEvent(input$o_objective_1, {
    # Dynamically show/hide corresponding input fields
    shinyjs::show(selector = paste0("#", setdiff(elem_hidden, elem_hide[[input$o_objective_1]])))
    shinyjs::hide(selector = paste0("#", elem_hide[[input$o_objective_1]]))
    elem_hidden <<- elem_hide[[input$o_objective_1]]
    
    # Show/hide Prediction tab when study objective is inference
    if(input$o_objective_1 == "Inference and explanation"){
      hideTab("tabset", "Prediction")
    } else {
      showTab("tabset", "Prediction")
    }
  })
  
  # Extent Long/Lat
  observe({
    input_names = c("o_scale_1_xmin", "o_scale_1_xmax", "o_scale_1_ymax", "o_scale_1_ymin")
    print_message = F
    for(input_name in input_names){
      if(grepl("xmin|xmax", input_name)){
        value_range = c(-180, 180)
      } else { 
        value_range = c(-90, 90)
      }
      
      if(is.null(input[[input_name]])){
        next
      } else if(input[[input_name]] == "") {
        next
      } else {
        input_value = ifelse(is.na(as.numeric(input[[input_name]])), -999, as.numeric(input[[input_name]]))  # Check if input is valid number
        if(input_value < value_range[1] | input_value > value_range[2]){
          updateTextAreaInput(session = session, inputId = input_name, value = "")
          print_message = T
        }
      }
    }
    if(print_message){
      showNotification("Please enter valid latitude/longitude coordinates", duration = 3, type = "error")  
    }
  })
  
  # -------------------------------------------
  # Optional fields
  observeEvent(input$hide_optional,{
    if(is.null(input$o_objective_1)){
      return(NULL)
    } else if(input$hide_optional == T & input$o_objective_1 == ""){
      showNotification("Please select a model objective under '1. Overview'", duration = 3, type = "message")
      Sys.sleep(0.3)
      updateMaterialSwitch(session, "hide_optional", value = F)
      updateTabsetPanel(session, "tabset", "Overview")
      # TODO jump to Input field
    } else {
      shinyjs::toggle(selector = paste0("#", setdiff(elem_optional, elem_hide[[input$o_objective_1]])), condition = !input$hide_optional) 
    }
  })
  
  # -------------------------------------------
  # Model algorithms and settings
  model_settings = reactiveValues(suggestions = rmm_dict %>% filter(field1 == "model" & field2 == "algorithm") %>% pull(field3) %>% unique() %>% trimws(),
                                  settings_tabset = NULL)
  model_settings_import = reactiveValues(algorithms = character(0))
  
  observeEvent(input$o_algorithms_1, {
    if(length(input$o_algorithms_1) > length(model_settings$settings_tabset)) { # New algorithm selected
      new_algs = setdiff(input$o_algorithms_1, model_settings$settings_tabset)
      for(new_alg in new_algs){
        # Create dataframe for new algorithm
        if(new_alg %in% model_settings_import[["algorithms"]]){
          model_settings[[new_alg]] = model_settings_import[[new_alg]]
        } else if(new_alg %in% filter(rmm_dict, field2 == "algorithm")$field3){
          model_settings[[new_alg]] = rmm_dict %>%
            filter(field1 == "model" & field2 == "algorithm" & field3 == new_alg) %>%
            mutate(setting = entity, value = as.character(NA)) %>%
            dplyr::select(setting, value)
        } else {
          model_settings[[new_alg]] = data.frame(setting = character(0), value = character(0))
        }
        
        # Add new dataframe to output and settings_tabset
        local({ # Needs local evaluation because of asynchronous execution of renderDataTable
          .new_alg = new_alg
          output[[.new_alg]] = renderDataTable(model_settings[[.new_alg]], editable = T, rownames = F,
                                        options = list(dom = "t", pageLength = 50, autoWidth = T, columnDefs = list(list(width = '50%', targets = "_all"))))
          observeEvent(input[[paste0(.new_alg, '_cell_edit')]], {
            model_settings[[.new_alg]][input[[paste0(.new_alg, '_cell_edit')]]$row, input[[paste0(.new_alg, '_cell_edit')]]$col + 1] = input[[paste0(.new_alg, '_cell_edit')]]$value
          })
        })
        appendTab(inputId = "settings_tabset", select = T, tab = tabPanel(title = new_alg, value = new_alg, dataTableOutput(outputId = new_alg)))
      }
      model_settings$settings_tabset = input$o_algorithms_1 # update name list of displayed tabs
    } else {
      hide_alg = setdiff(model_settings$settings_tabset, input$o_algorithms_1)
      removeTab(inputId = "settings_tabset", target = hide_alg)
      model_settings$settings_tabset = input$o_algorithms_1
    }
    
    if(length(model_settings$settings_tabset) > 0){
      updateTabsetPanel(session, "settings_tabset", selected = model_settings$settings_tabset[1])
    }
  }, ignoreNULL = F, ignoreInit = F, priority = 1)
  
  observeEvent(input$add_setting, {
    if(!is.null(input$settings_tabset)){
      empty_row = data.frame(setting = NA, value = NA)
      model_settings[[input$settings_tabset]] = rbind(model_settings[[input$settings_tabset]], empty_row)
    } else {
      showNotification("Please select or add a model algorithm under '1. Overview'", duration = 3, type = "message")
      updateTabsetPanel(session, "tabset", selected = "Overview")
      # TODO jump to Input field
    }
  })
  
  # -------------------------------------------
  # Authors
  authors = reactiveValues(df = data.frame("first_name" = character(0),  "last_name" = character(0))) 
  
  output$authors_table = DT::renderDataTable({
    if(nrow(authors$df) == 0){
      authors_dt = datatable(authors$df, escape = F, rownames = F, colnames = NULL, 
                             options = list(dom = "t", ordering = F, language = list(emptyTable = "Author list is empty"), columnDefs = list(list(className = 'dt-left', targets = "_all"))))
    } else {
      authors_tmp = authors$df %>% 
        rownames_to_column("row_id") %>% 
        mutate(row_id = as.numeric(row_id),
               delete = sapply(1:nrow(.), function(row_id){as.character(actionButton(inputId = paste("remove_author", row_id, sep = "_"), label = NULL, 
                                                                                     icon = icon("trash"),
                                                                                     onclick = 'Shiny.setInputValue(\"remove_author\", this.id, {priority: "event"})'))})) %>% 
        dplyr::select(-row_id)
      
      authors_dt = datatable(authors_tmp, escape = F, rownames = F, editable = T, colnames = c("First name", "Last name", ""), 
                             options = list(dom = "t", autoWidth = F, 
                                            columnDefs = list(list(width = '10%', targets = c(2)), 
                                                              list(width = '45%', targets = c(0:1)),
                                                              list(orderable = F, targets = c(0:2)))))
    }
    return(authors_dt)
  })
  
  observeEvent(input$add_author, {
    showModal(
      modalDialog(title = "Add new author", footer = NULL, easyClose = T,
                  textInput("first_name", "First name"),
                  textInput("last_name", "Last name"),
                  actionButton("save_new_author", "Save")
      )
    )
  })
  
  observeEvent(input$save_new_author, {
    if(input$first_name == "" | input$last_name == ""){
      showNotification("Please provide first and last name", duration = 3, type = "message")
    } else {
      new_author = data.frame("first_name" = input$first_name, "last_name" = input$last_name, stringsAsFactors = F)
      authors$df = rbind(authors$df, new_author)
      removeModal()
    }
  })
  
  observeEvent(input$remove_author, {
    item_remove = as.integer(parse_number(input$remove_author))
    authors$df = authors$df[-item_remove,]
    output$authors_df = renderDataTable(authors$df)
  })
  
  observeEvent(input$authors_table_cell_edit, {
    authors$df[input$authors_table_cell_edit$row, input$authors_table_cell_edit$col + 1] = input$authors_table_cell_edit$value
    output$authors_df = renderDataTable(authors$df)
  })
  
  # -------------------------------------------
  # Import
  observeEvent(input$ODMAP_to_input, {
    protocol_upload = read.csv(input$upload$datapath, sep = ",", stringsAsFactors = F, na.strings = c("NA", "", "NULL")) %>% 
      right_join(odmap_dict, by = c("section", "subsection", "element"))  %>% 
      mutate(Value = trimws(Value)) %>%
      filter(!is.na(Value))
    
    # Update ODMAP input fields with imported values
    for(i in 1:nrow(protocol_upload)){
      switch(protocol_upload$element_type[i],
             text = import_odmap_to_text(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             author = import_odmap_to_authors(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             objective = import_odmap_to_model_objective(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             suggestion = import_suggestion(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             extent = import_odmap_to_extent(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             model_algorithm = import_odmap_to_model_algorithm(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             model_setting = import_model_settings(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]))
    }
    
    # Switch to "Create a protocol" 
    reset("upload")
    updateNavbarPage(session, "navbar", selected = "create")
    updateTabsetPanel(session, "Tabset", selected = "Overview")
  })
  
  observeEvent(input$RMMS_to_input, {
    protocol_upload = read.csv(input$upload$datapath, sep = ",", stringsAsFactors = F, na.strings = c("NA", "", "NULL")) %>% 
      mutate(Value = trimws(Value)) %>% 
      filter(!is.na(Value))
    
    # 1. Prepare imported values
    if(nrow(protocol_upload>0)){
        imported_values = list()
        for(i in 1:nrow(protocol_upload)){ ## ignore first line?
          rmm_fields = c(protocol_upload$Field.1[i], protocol_upload$Field.2[i], protocol_upload$Field.3[i])
          rmm_fields = rmm_fields[!is.na(rmm_fields)]
          rmm_fields = paste0(rmm_fields, collapse = "$")
          rmm_entity = protocol_upload$Entity[i]
          
          odmap_subset = odmap_dict[grepl(rmm_fields, str_split(odmap_dict$rmm_fields, pattern = ","), fixed = T) & grepl(rmm_entity, str_split(odmap_dict$rmm_entities, pattern = ","), fixed = T),]
          if(nrow(odmap_subset) == 1){
            imported_values[[odmap_subset$element_id]][[paste(rmm_fields, rmm_entity, sep = "-")]] = protocol_upload$Value[i]
          }
        }
        
        model_settings = protocol_upload %>% 
          dplyr::filter(Field.1 == "model" & Field.2 == "algorithm") %>% 
          group_by(Field.3) %>% 
          summarize(settings_string = paste(paste0(Entity, " (", Value, ")"), collapse = ", "))
        
        if(nrow(model_settings) > 0){
          imported_values[["m_settings_1"]] = model_settings %>% 
            ungroup() %>% 
            summarize(x = paste(paste0(Field.3, ": ", settings_string), collapse = "; ")) %>% 
            pull(x)
        }
        
        # 2. Update ODMAP input fields with imported values
        for(i in 1:length(imported_values)){
          switch(odmap_dict$element_type[which(odmap_dict$element_id == names(imported_values)[i])],
                 text = import_rmm_to_text(element_id = names(imported_values)[i], values = imported_values[[i]]),
                 author = import_rmm_to_authors(element_id = names(imported_values)[i], values = imported_values[[i]]),
                 suggestion = import_suggestion(element_id = names(imported_values)[i], values = unlist(imported_values[[i]])),
                 extent = import_rmm_to_extent(element_id = names(imported_values)[i], values = imported_values[[i]]),
                 model_setting = import_model_settings(element_id = names(imported_values)[i], values = imported_values[[i]]))
        }
        
        # Switch to "Create a protocol" 
        reset("upload")
        updateNavbarPage(session, "navbar", selected = "create")
        updateTabsetPanel(session, "Tabset", selected = "Overview")
    }

  })
}
