
#' helper function for app
#'
#' @param input list of facets run directories
#' @param output progress bar from shiny
#' @return runs serverend
#' @export server
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom DT datatable
#' @import dplyr
#' @import stringr
#' @import shinyjs

function(input, output, session) {
  values <- reactiveValues()

  #' helper function for app
  #'
  #' @return checks for mount
  #' @export verify_ifs_mount
  verify_ifs_mount <- function() {
    if (grepl(":/ifs ", paste(system("mount 2>&1", intern=TRUE), collapse=" "))) {
      shinyjs::hideElement(id= "wellPanel_mountFail")
      return(TRUE)
    } else { 
      shinyjs::showElement(id= "wellPanel_mountFail") 
      showModal(modalDialog( title = "/ifs mount not detected", "Re-mount and try again" ))
      return (FALSE)
    }
  }
  
  #' helper function for app
  #'
  #' @param selected_sample sampleid
  #' @param selected_sample_path facets run directory containing 'facets_review.manifest'
  #' @return nothing
  #' @export refresh_review_status
  refresh_review_status <- function(selected_sample, selected_sample_path) {
    review_df <- facetsPreview:::get_review_status(selected_sample, selected_sample_path)
    if ( dim(review_df)[1] > 0)
    output$datatable_reviewHistory <- DT::renderDataTable({
      DT::datatable(review_df %>%
                      select(-sample, -path) %>% 
                      arrange(desc(date_reviewed)), 
                    selection=list(mode='single'),
                    options = list(columnDefs = list(list(className = 'dt-center')),
                                   pageLength = 5),
                    rownames=FALSE)  
    })
  }
  
  observeEvent(input$button_mountFailRefresh, { 
    if (grepl("mskcc.org:/ifs ", paste(system("mount 2>&1", intern=TRUE), collapse=" "))) {
      shinyjs::hideElement(id= "wellPanel_mountFail")
    }
    return(NULL)
  })
   
  # check if /ifs is mounted
  if (!verify_ifs_mount()) {
    return(NULL)
  }
  
  observeEvent(input$button_fileInput, {
    if (!verify_ifs_mount()) { return (NULL) }
    
    if ( is.null(input$textInput_filename) || 
         !file.exists(input$textInput_filename) || 
         file.info(input$textInput_filename)$isdir) 
    {
      showModal(modalDialog( title = "File not found", paste0( input$textInput_filename) ))
      return(NULL)
    }
    updateNavbarPage(session, "navbarPage1", selected = "tabPanel_samplesManifest")
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Reading Samples:", value = 0)

    con <- file(input$textInput_filename)
    manifest = readLines(con)
    close(con)
    manifest_metadata <- load_samples(manifest, progress)
    
    values$df_data <- manifest_metadata 
    
    values$submitted_refits <- c()
  })
  
  observeEvent(input$button_samplesInput, {
    if (!verify_ifs_mount()) { return (NULL) }
  
    updateNavbarPage(session, "navbarPage1", selected = "tabPanel_samplesManifest")

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Reading Samples:", value = 0)
    
    manifest = unlist(stringr::str_split(stringr::str_replace_all(input$textAreaInput_samplesInput, " ", ""), "\n"))
    manifest_metadata <- load_samples(manifest, progress)
    values$df_data <- manifest_metadata 
    
    values$submitted_refits <- c()
  })
  
  output$datatable_samples <- DT::renderDataTable({
    DT::datatable(values$df_data %>%
                    select(-path), 
                  selection=list(mode='single', selected=values$dt_sel),
                  #options = list(columnDefs = list(list(className = 'dt-center'), list(visible=FALSE, targets = c(1))),
                  options = list(pageLength = 20),
                  rownames=FALSE)  
    # hide path column
  })

  observeEvent(input$datatable_samples_rows_selected, {
    if (!verify_ifs_mount()) { return (NULL) }
    selected_sample = paste(unlist(values$df_data[input$datatable_samples_rows_selected,1]), collapse="")
    selected_sample_path = paste(unlist(values$df_data[input$datatable_samples_rows_selected,2]), collapse="")
    selected_sample_num_fits = values$df_data[input$datatable_samples_rows_selected,4]
    
    if (selected_sample_num_fits == 0) {
      showModal(modalDialog( title = "No fits found for this sample", 
                             "Path to this sample may be incorrect. " ))
      return(NULL)  # print some kind of error and exit;
    }
    
    updateNavbarPage(session, "navbarPage1", selected = "tabPanel_reviewFits")

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Loading FACETS runs for the selected sample:", value = 0)
    values$sample_runs <- metadata_init(selected_sample, selected_sample_path, progress)
    
    output$verbatimTextOutput_runParams <- renderText({})
    output$verbatimTextOutput_altBalLogR <- renderText({})
    output$imageOutput_pngImage1 <- renderImage({ list(src="", width=0, height=0)})
    
    if ( dim(values$sample_runs)[1] == 0) {
      showModal(modalDialog( title = "Unable to read sample", "likey cause: /ifs mount failed.  Re-mount and try." ))
      return(NULL)  # print some kind of error and exit;
    }
    
    # update with review status
    refresh_review_status(selected_sample, selected_sample_path)
    
    ## bind to drop-down
    updateSelectInput(session, "selectInput_selectFit",
                      choices = as.list(c("Not selected", unlist(values$sample_runs$fit_name))),
                      selected = "Not selected"
    )
    updateSelectInput(session, "selectInput_selectBestFit",
                      choices = as.list(c("Not selected", unlist(values$sample_runs$fit_name))),
                      selected = "Not selected"
    )
    shinyWidgets::updateRadioGroupButtons(session, "radioGroupButton_fitType", selected="Hisens")
  })
  
  observeEvent(input$button_copyClipPath, {
    if (input$selectInput_selectFit == "Not selected"){
      return(NULL)
    }    
    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]
    clip <- pipe("pbcopy", "w")
    write.table(paste0(selected_run$path[1], "/", selected_run$fit_name[1], "/"),
                file=clip, 
                quote=F, 
                col.names=F, 
                row.names=F)
    close(clip)
  })
  
  observeEvent(input$selectInput_selectFit, {
    if (!verify_ifs_mount()) { return (NULL) }
    output$verbatimTextOutput_runParams <- renderText({})
    output$verbatimTextOutput_altBalLogR <- renderText({})
    output$imageOutput_pngImage1 <- renderImage({ list(src="", width=0, height=0)})
    if ( input$selectInput_selectFit == "Not selected") {
      return (NULL)
    }
  
    # update other text options
    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]

    if (grepl("facets_refit", input$selectInput_selectFit) ) {
      shinyWidgets::updateRadioGroupButtons(session, "radioGroupButton_fitType", selected="Hisens")
    } else {
      shinyWidgets::updateRadioGroupButtons(session, "radioGroupButton_fitType", selected="Purity")
    }
  })
  
  observeEvent(input$button_addReview, {
    selected_run <- values$sample_runs[1,] # select any fit 
    sample = selected_run$tumor_sample_id[1]
    path = selected_run$path[1]
    review_status = input$radioButtons_reviewStatus
    fit_name = input$selectInput_selectBestFit[1]
    signed_as = input$textInput_signAs[1]  
    note = input$textAreaInput_reviewNote[1]
    
    df <- get_review_status(sample, path)
    if (dim(df)[1] > 0){
      ## check if the sample has been recently reviewed (in the past 1hr)
      cur_time = Sys.time()
      if (any(which(as.numeric(difftime(cur_time, df$date_reviewed, units="hours")) < 1))) {
        showModal(modalDialog(
          title = "ALERT", paste0("This sample has been reviewed within the past hour. 
                                  Your review is added but make sure all is tight.")
        ))
      }
    }
    df <- data.frame(
      sample = c(sample),
      path = c(path),
      review_status = c(review_status),
      best_fit = c(fit_name),
      review_notes = c(note),
      reviewed_by = c(signed_as),
      date_reviewed = as.character(Sys.time()),
      stringsAsFactors=FALSE
    )
    update_review_status_file(path, df)
    
    refresh_review_status(sample, path)
  })
  
  observeEvent(input$radioGroupButton_fitType, {
    if (!verify_ifs_mount()) { return (NULL) }
    if (input$selectInput_selectFit == "Not selected" || 
        (grepl("facets_refit", input$selectInput_selectFit) && input$radioGroupButton_fitType == "Purity")){
      output$verbatimTextOutput_runParams <- renderText({})
      output$verbatimTextOutput_altBalLogR <- renderText({})
      output$imageOutput_pngImage1 <- renderImage({ list(src="", width=0, height=0)})
      return(NULL)
    }
    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]

    output$verbatimTextOutput_runParams <- renderText({
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

    output$datatable_cncf <- DT::renderDataTable({
      if (input$radioGroupButton_fitType == "Hisens") {
        run_prefix = selected_run$hisens_run_prefix[1]
      } else {
        run_prefix = selected_run$purity_run_prefix[1]
      }
      DT::datatable(data.table::fread(paste0(run_prefix, ".cncf.txt")) %>%
                      rowwise() %>%
                      mutate(cnlr.median = round_down(cnlr.median),
                             mafR = round_down(mafR),
                             cf = round_down(cf),
                             cf.em = round_down(cf.em)) %>%
                      select(-ID, -cnlr.median.clust, -mafR.clust, -segclust), 
                    selection=list(mode='single'),
                    options = list(columnDefs = list(list(className = 'dt-center')),
                                   pageLength = 50),
                    rownames=FALSE)  
    })
    
    output$imageOutput_pngImage1 <- renderImage({
      if (input$radioGroupButton_fitType == "Hisens") {
        png_filename = paste0(selected_run$hisens_run_prefix[1], ".CNCF.png")
      } else {
        png_filename = paste0(selected_run$purity_run_prefix[1], ".CNCF.png")
      }
      list(src = png_filename, contentType = 'image/png', width = 650, height = 800)
    }, 
    deleteFile = FALSE)
    
    output$verbatimTextOutput_altBalLogR <- renderText({
      if (input$radioGroupButton_fitType == "Hisens" ) {
        paste("NA")
      } else {
        paste0(selected_run$purity_run_alBalLogR[1])
      }
    })
  })
  
  observeEvent(input$button_refit, {
    if (!verify_ifs_mount()) { return (NULL) }
    if (input$selectInput_selectFit == "Not selected" || 
        is.na(suppressWarnings(as.integer(input$textInput_newDipLogR))) || 
        is.na(suppressWarnings(as.integer(input$textInput_newCval))))
    {
      return(NULL)
    }
    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]
    
    ## check if the refit has been recently submitted
    refit_name <- paste0("/facets_refit_c", input$textInput_newCval, "_diplogR_", input$textInput_newDipLogR)
    refit_cmd_file <- 
      paste0("/ifs/res/taylorlab/bandlamc/facets_review_tool/facets_refit_watcher/facets_refit_cmd_",
             selected_run$tumor_sample_id[1], "_c", input$textInput_newCval, "_diplogR_", input$textInput_newDipLogR, ".sh")
    
    if (any(values$submitted_refit == refit_name)) {
      showModal(modalDialog(
        title = "Not submitted", paste0("Job already queued. Check logs: ", refit_cmd_file, ".*")
      ))
      return(NULL)
    }
    values$submitted_refit <- c(values$submitted_refit, refit_name)
    
    refit_dir <- paste0(selected_run$path[1], refit_name)
    
    refit_cmd <-
      paste0("mkdir -p ", refit_dir, "; ",
             "cmo_facets --lib-version 0.5.6 doFacets -c ", input$textInput_newCval, 
             " -d ", input$textInput_newDipLogR," --seed 100 -f ", 
             selected_run$path[1], "/countsMerged____", selected_run$tumor_sample_id[1], ".dat.gz ",
             "-t ", selected_run$tumor_sample_id[1], " -D ", refit_dir)
   
    write(refit_cmd, refit_cmd_file)
    showModal(modalDialog(
      title = "Job submitted!", paste0("Check back in a few minutes. Logs: ", refit_cmd_file, ".*")
    ))
  })
  
  ## check if watcher is running
  {
    cur_time = as.numeric(system(" date +%s", intern=TRUE))
    last_mod = as.numeric(system("stat -f%c /ifs/res/taylorlab/bandlamc/facets_review_app/facets_refit_watcher/watcher.log", intern=TRUE))
    if ( cur_time - last_mod < 900) {
      shinyjs::showElement(id="div_watcherSuccess")  
    } else {
      shinyjs::showElement(id="div_watcherFail")
    }
  }
}
