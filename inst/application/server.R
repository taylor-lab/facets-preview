
#' helper function for app
#'
#' @param input list of facets run directories
#' @param output progress bar from shiny
#' @return runs serverend
#' @export server
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom DT datatable
function(input, output, session) {
  
  values <- reactiveValues()

  observeEvent(input$fileInputButton, {
    
    if ( is.null(input$fileInputTxt) || is.null(input$fileInputTxt$datapath)) {
      return(NULL)
    }
    updateNavbarPage(session, "navbarPage1", selected = "samplesPanel")
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Reading Samples:", value = 0)

    con <- file(input$fileInputTxt$datapath)
    manifest = readLines(con)
    close(con)
    manifest_metadata <- load_samples(manifest, progress)
    
    values$df_data <- manifest_metadata %>% mutate(reviewed = FALSE, note = "") 
  })
  
  observeEvent(input$sampleInputButton, {
    updateNavbarPage(session, "navbarPage1", selected = "samplesPanel")

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Reading Samples:", value = 0)
    
    manifest = unlist(str_split(input$samplesInputTxt, "\n"))
    manifest_metadata <- load_samples(manifest, progress)
    values$df_data <- manifest_metadata %>% mutate(reviewed = FALSE, note = "") 
  })
  
  output$dtSamples <- DT::renderDataTable({
    DT::datatable(values$df_data, selection=list(mode='single', selected=values$dt_sel), 
              options = list(columnDefs = list(list(className = 'dt-center'), list(visible=FALSE, targets = c(2)))))  
    # hide path column
  })

  observeEvent(input$dtSamples_rows_selected, {
    updateNavbarPage(session, "navbarPage1", selected = "reviewPanel")
    values$selected_sample = paste(unlist(values$df_data[input$dtSamples_rows_selected,1]), collapse="")
    values$selected_sample_path = paste(unlist(values$df_data[input$dtSamples_rows_selected,2]), collapse="")
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Loading FACETS runs for the selected sample:", value = 0)
    values$sample_runs <- metadata_init(values$selected_sample, values$selected_sample_path, progress)
    
    output$run_params <- renderText({})
    output$alt_bal_logR <- renderText({})
    output$pngImage1 <- renderImage({ list(src="", width=0, height=0)})
    
    if ( dim(values$sample_runs)[1] == 0) {
      next  # print some kind of error and exit;
    }
    
    ## bind to drop-down
    updateSelectInput(session, "inSelect",
                      choices = as.list(c("Not selected", unlist(values$sample_runs$fit_name))),
                      selected = "Not selected"
    )
    shinyWidgets::updateRadioGroupButtons(session, "radioGroupButton_fitType", selected="Hisens")
  })
  
  observeEvent(input$inSelect, {
    output$run_params <- renderText({})
    output$alt_bal_logR <- renderText({})
    output$pngImage1 <- renderImage({ list(src="", width=0, height=0)})
    if ( input$inSelect == "Not selected") {
      return (NULL)
    }
  
    # update other text options
    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$inSelect)),]
    
    if (0) {    ## TODO
      output$pngImage_lr <- renderPlot({
        load("")
        cnlr = copy.number.log.ratio(out, fit)
        valor = var.allele.log.odds.ratio(out, fit)
        icnem = integer.copy.number(out, fit, method='em')
        icncncf = integer.copy.number(out, fit, method='cncf')  
        plot(list(cnlr, valor, icnem, icncncf))
      })
    }
    shinyWidgets::updateRadioGroupButtons(session, "radioGroupButton_fitType", selected="Purity")
  })
  
  observeEvent(input$radioGroupButton_fitType, {
    if (input$inSelect == "Not selected" || 
        (grepl("facets_refit", input$inSelect) && input$radioGroupButton_fitType == "Purity")){
      output$run_params <- renderText({})
      output$alt_bal_logR <- renderText({})
      output$pngImage1 <- renderImage({ list(src="", width=0, height=0)})
      return(NULL)
    }
    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$inSelect)),]

    output$run_params <- renderText({
      if (input$radioGroupButton_fitType == "Hisens") {
        paste0("Seed: ", selected_run$hisens_run_Seed[1], ", ", 
               "cval: ", selected_run$hisens_run_cval[1], ", ",
               "purity: ", selected_run$hisens_run_Purity[1], ", ",
               "ploidy: ", selected_run$hisens_run_Ploidy[1], ", ",
               "diplogR: ", selected_run$hisens_run_dipLogR[1]
        )
      } else {
        paste0("Seed: ", selected_run$purity_run_Seed[1], ", ", 
               "cval: ", selected_run$purity_run_cval[1], ", ",
               "purity: ", selected_run$purity_run_Purity[1], ", ",
               "ploidy: ", selected_run$purity_run_Ploidy[1], ", ",
               "diplogR: ", selected_run$purity_run_dipLogR[1]
        )
      }
    })
    output$pngImage1 <- renderImage({
      if (input$radioGroupButton_fitType == "Hisens") {
        png_filename = paste0(selected_run$hisens_run_prefix[1], ".CNCF.png")
      } else {
        png_filename = paste0(selected_run$purity_run_prefix[1], ".CNCF.png")
      }
      list(src = png_filename, contentType = 'image/png', width = 650, height = 800)
    }, 
    deleteFile = FALSE)
    
    output$alt_bal_logR <- renderText({
      if (input$radioGroupButton_fitType == "Hisens" ) {
        paste("NA")
      } else {
        paste0(selected_run$purity_run_alBalLogR[1])
      }
    })
  })
  
  
  observeEvent(input$buttonRefit, {
    if (input$inSelect == "Not selected" || 
        is.na(suppressWarnings(as.integer(input$newDipLogR))) || 
        is.na(suppressWarnings(as.integer(input$newCval))))
    {
      return(NULL)
    }
    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$inSelect)),]
    refit_dir <- paste0(selected_run$path[1], "/facets_refit_c", 
                        input$newCval, "_diplogR_", input$newDipLogR)
    
    refit_cmd <-
      paste0("mkdir -p ", refit_dir, 
             "; cmo_facets --lib-version 0.5.6 doFacets -c ", input$newCval, " -d ", input$newDipLogR," --seed 100 -f ", 
             selected_run$path[1], "/countsMerged____", selected_run$tumor_sample_id[1], ".dat.gz ",
             "-t ", selected_run$tumor_sample_id[1], " -D ", refit_dir)
    
    refit_cmd_file <- 
      paste0("/ifs/res/taylorlab/bandlamc/facets_review_tool/facets_refit_watcher/facets_refit_cmd_",
            selected_run$tumor_sample_id[1], "_c", input$newCval, "_diplogR_", input$newDipLogR, ".sh")
    write(refit_cmd, refit_cmd_file)
    showModal(modalDialog(
      title = "Job submitted!", paste0("Check back in a few minutes. Logs: ", refit_cmd_file, ".*")
    ))
  })
}
