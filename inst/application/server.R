
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
#' @import rhandsontable
#' @import gridExtra

server <-
function(input, output, session) {
  values <- reactiveValues()

  #' helper function for app
  #'
  #' @return checks for mount
  #' @export verifiy_sshfs_mount
  verifiy_sshfs_mount <- function() {
    if (!grepl(":/juno ", paste(system("mount 2>&1", intern=TRUE), collapse=" ")) |
        grepl("No such file", paste(system("ls /juno/work/ccs/bandlamc/facets_review_app/facets_refit_watcher/ 2>&1", intern=TRUE), collapse=" "))) {
      shinyjs::showElement(id= "wellPanel_mountFail")
      showModal(modalDialog( title = "/juno mount not detected", "Re-mount and try again" ))
      return (FALSE)
    }
    shinyjs::hideElement(id= "wellPanel_mountFail")
    return(TRUE)
  }

  #' helper function for app
  #'
  #' @param selected_sample sampleid
  #' @param selected_sample_path facets run directory containing 'facets_review.manifest'
  #' @return nothing
  #' @export refresh_review_status
  refresh_review_status <- function(selected_sample, selected_sample_path, facets_runs) {
    review_df <- get_review_status(selected_sample, selected_sample_path)
    if ( dim(review_df)[1] > 0) {
      gicon <- function(x) as.character(icon(x, lib = "glyphicon"))
      
      output$datatable_fitReviews <- DT::renderDataTable({
        DT::datatable(facets_runs %>% 
                        mutate(facets_suite_qc = ifelse(facets_suite_qc, gicon('ok'), gicon('remove'))) %>%
                        mutate(is_best_fit = ifelse(is_best_fit, gicon('thumbs-up'), '')) %>%
                        select(fit_name, facets_suite_qc, manual_review_best_fit = is_best_fit),
                      selection=list(mode='single'),
                      colnames = c('Fit', 'facets-suite QC', 'Reviewed as best fit?'),
                      options = list(columnDefs = list(list(className = 'dt-center', targets = 0:2)),
                                     pageLength = 100, dom='t'),
                      rownames=FALSE, escape = F)
      })
      
      output$datatable_reviewHistory <- DT::renderDataTable({
        DT::datatable(review_df %>%
                        filter(review_status != 'not_reviewed') %>%
                        mutate(use_only_purity_run = ifelse(use_only_purity_run, gicon('ok-sign'), '')) %>%
                        mutate(use_edited_cncf = ifelse(use_edited_cncf, gicon('ok-sign'), '')) %>%
                        dplyr::select(-sample, -path, -facets_suite_qc) %>%
                        dplyr::arrange(desc(date_reviewed)),
                      selection=list(mode='single'),
                      colnames = c('Review Status', 'Fit', 'Notes', 'Reviewer', 'Date Reviewed', 'Use purity run only?', 
                                   'Use edited.cncf.txt?', 'Reviewer set purity:'),
                      options = list(columnDefs = list(list(className = 'dt-center', targets = 0:6)),
                                     pageLength = 100, dom = 't'),
                      rownames=FALSE, escape = F)
      })
    }
  }
  shinyjs::hideElement("button_saveChanges")
  observeEvent(input$button_mountFailRefresh, {
    verifiy_sshfs_mount()
    return(NULL)
  })

  # check if /juno is mounted
  if (!verifiy_sshfs_mount()) {
    return(NULL)
  }

  observeEvent(input$button_fileInput, {
    if (!verifiy_sshfs_mount()) { return (NULL) }

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
    
    values$manifest_metadata <- load_samples(manifest, progress)

    values$submitted_refits <- c()
  })

  observeEvent(input$button_dmpSamplesInput, {
    if (!verifiy_sshfs_mount()) { return (NULL) }
    
    # make sure the sample input string is the right format
    dmp_ids <- gsub(' |\\s*,\\s*$', '', input$textAreaInput_dmpSamplesInput)

    if (!grepl("^P-\\d{7}-T0\\d-IM\\d(,P-\\d{7}-T0\\d-IM\\d)*$", dmp_ids)) {
      showModal(modalDialog(title = "Incorrect format!", 
                            paste0("IMPACT Tumor Sample IDs are in incorrect format. ",
                                   "Expecting one or more (comma-separated) DMP IDs")
                            ))
      return(NULL)
    }
    dmp_ids <- unlist(strsplit(dmp_ids, ","))
    
    updateNavbarPage(session, "navbarPage1", selected = "tabPanel_samplesManifest")
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Reading Samples:", value = 0)
    
    values$manifest_metadata <- load_impact_samples(dmp_ids, progress)
    
    num_samples_queried = length(dmp_ids)
    num_samples_found = nrow(values$manifest_metadata)
    if (num_samples_queried != num_samples_found) {
      showModal(modalDialog(title = "Warning!", 
                            paste0("Note: Only ", num_samples_found, " of the ", num_samples_queried, 
                                   " DMP IDs queried are found in the IMPACT FACETS folder.")
      ))
      if (num_samples_found == 0) {
        return(NULL)
      }
    }
    values$submitted_refits <- c()
  })

  observeEvent(input$button_samplesInput, {
    if (!verifiy_sshfs_mount()) { return (NULL) }

    updateNavbarPage(session, "navbarPage1", selected = "tabPanel_samplesManifest")

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Reading Samples:", value = 0)

    manifest = unlist(stringr::str_split(stringr::str_replace_all(input$textAreaInput_samplesInput, " ", ""), "\n"))
    manifest_metadata <- load_samples(manifest, progress)
    values$manifest_metadata <- manifest_metadata

    values$submitted_refits <- c()
  })

  output$datatable_samples <- DT::renderDataTable({
    if (is.null(values$manifest_metadata) || nrow(values$manifest_metadata) == 0) {
      return(NULL)
    }
    
    gicon <- function(x) as.character(icon(x, lib = "glyphicon"))
    DT::datatable(values$manifest_metadata %>%
                    dplyr::select(-path) %>%
                    mutate(default_fit_qc = ifelse(default_fit_qc, gicon('ok'), gicon('remove'))) %>%
                    mutate(reviewed_fit_facets_suite_qc = 
                             ifelse(review_status == 'Not reviewed', '',
                                    ifelse(reviewed_fit_facets_suite_qc, gicon('ok'), gicon('remove')))) %>%
                    mutate(reviewed_fit_use_purity = ifelse(reviewed_fit_use_purity, gicon('ok-sign'), '')) %>%
                    mutate(reviewed_fit_use_edited_cncf = ifelse(reviewed_fit_use_edited_cncf, gicon('ok-sign'), '')),
                  selection=list(mode='single', selected=values$dt_sel),
                  colnames = c('Sample ID (tag)', '# fits', 'Default Fit', 'Default Fit QC', 
                               'Review Status', 'Reviewed Fit', 'Reviewed Fit QC', 'purity run only?', 
                               'edited.cncf.txt?', 'Reviewer purity', 'Date Reviewed'),
                  options = list(pageLength = 20, columnDefs = list(list(className = 'dt-center', targets = 0:9))),
                  rownames=FALSE, escape = F)
  })

  # Downloadable csv of selected dataset ----
  output$download_mapping_file <- downloadHandler(
    filename = function() {
      paste0('facets_mapping_file_', gsub(' |-|:', '_', Sys.time()), '.txt')
    },
    content = function(file) {
      
      write.table(values$manifest_metadata %>%
                  rowwise %>% 
                  dplyr::mutate(has_reviewed_fit = 
                           ifelse(review_status %in% c('reviewed_acceptable_fit','reviewed_best_fit'), 
                                  T, F)) %>%
                  dplyr::mutate(run_type = ifelse(has_reviewed_fit & as.logical(reviewed_fit_use_purity), 
                                                  'purity', 'hisens')) %>%
                  dplyr::mutate(fit_to_use = ifelse(has_reviewed_fit, 
                                                    reviewed_fit_name, default_fit_name)) %>%
                  dplyr::mutate(cncf_file = paste0(path, '/', fit_to_use, '/', sample_id, '_', run_type, '.cncf',
                                                   ifelse(reviewed_fit_use_edited_cncf, 
                                                          '.edited.txt', '.txt'))) %>%
                  select(-fit_to_use, -run_type, -has_reviewed_fit), 
                file, row.names = F, quote=F, sep='\t')
    }
  )
  
  observeEvent(input$datatable_samples_rows_selected, {
    if (!verifiy_sshfs_mount()) { return (NULL) }
    selected_sample = paste(unlist(values$manifest_metadata[input$datatable_samples_rows_selected,1]), collapse="")
    selected_sample_path = paste(unlist(values$manifest_metadata[input$datatable_samples_rows_selected,2]), collapse="")
    selected_sample_num_fits = values$manifest_metadata[input$datatable_samples_rows_selected,4]

    if (selected_sample_num_fits == 0) {
      showModal(modalDialog( title = "No fits found for this sample",
                             "Path to this sample may be incorrect. " ))
      return(NULL)  # print some kind of error and exit;
    }
  
    updateNavbarPage(session, "navbarPage1", selected = "tabPanel_reviewFits")
    updateTabsetPanel(session, "reviewTabsetPanel", selected = "png_image_tabset")
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Loading FACETS runs for the selected sample:", value = 0)
    values$sample_runs <- metadata_init(selected_sample, selected_sample_path, progress)

    output$verbatimTextOutput_runParams <- renderText({})
    output$verbatimTextOutput_altBalLogR <- renderText({})
    output$imageOutput_pngImage1 <- renderImage({ list(src="", width=0, height=0)})

    if ( is.null(values$sample_runs) || dim(values$sample_runs)[1] == 0) {
      showModal(modalDialog( title = "Unable to read sample", "Either no runs exist for this sample, or, /juno mount failed." ))
      return(NULL)  # print some kind of error and exit;
    }

    # update with review status
    refresh_review_status(selected_sample, selected_sample_path, values$sample_runs)
    
    ## get best fit if exists; other-wise default
    selected_run = values$sample_runs %>% filter(fit_name=='default') %>% head(n=1)
    
    if (nrow(values$sample_runs %>% filter(is_best_fit)) == 1) {
      selected_run = values$sample_runs %>% filter(is_best_fit) %>% head(n=1)
    }
    
    ## hack around reactive to toggle to selected_run$fit_name 
    values$show_fit = ifelse(nrow(selected_run) == 0, 'Not selected', selected_run$fit_name)
    
    ## bind to drop-down
    updateSelectInput(session, "selectInput_selectFit",
                      choices = as.list(c("Not selected", unlist(values$sample_runs$fit_name))),
                      selected = ifelse (input$selectInput_selectFit == 'Not selected' & values$show_fit != 'Not selected', 
                                         values$show_fit, 'Not selected')
    )
    updateSelectInput(session, "selectInput_selectBestFit",
                      choices = as.list(c("Not selected", unlist(values$sample_runs$fit_name))),
                      selected = "Not selected"
    )
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
    if (!verifiy_sshfs_mount()) { return (NULL) }
    output$verbatimTextOutput_runParams <- renderText({})
    output$verbatimTextOutput_altBalLogR <- renderText({})
    output$imageOutput_pngImage1 <- renderImage({ list(src="", width=0, height=0)})
    
    if ( input$selectInput_selectFit == 'Not selected') {
      if (!is.null(values$show_fit) && values$show_fit != '') {
        updateSelectInput(session, "selectInput_selectFit",
                          choices = as.list(c("Not selected", unlist(values$sample_runs$fit_name))),
                          selected = values$show_fit
        )
        values$show_fit = ''
      }
      return (NULL)
    }
    
    # update other text options
    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]
    if ( selected_run$is_best_fit[1]) {
      shinyjs::showElement(id="div_bestFitTrophy")
    } else {
      shinyjs::hideElement(id="div_bestFitTrophy")
    }
    
    output$verbatimTextOutput_name_of_qc_fit <- renderText({ 
      paste0(selected_run$fit_name)
    })
    
    output$datatable_QC_flags <- DT::renderDataTable({
      filter_columns = c("homdel_filter", "diploid_seg_filter", "waterfall_filter", 
                         "hyper_seg_filter", "high_ploidy_filter", "valid_purity_filter")
      filter_names = c("Homozygous deletions", "Diploid segments (in dipLogR)", "No Waterfall pattern", 
                       "No hyper segmentation", "Not high ploidy", "Has valid purity")
      
      df <- data.frame(filter_name = filter_names, 
                       passed = unlist(selected_run[, paste0(filter_columns, '_pass')], use.names = F),
                       note = unlist(selected_run[, paste0(filter_columns, '_note')], use.names = F))
      gicon <- function(x) as.character(icon(x, lib = "glyphicon"))
      DT::datatable(df %>%
                      mutate(passed = ifelse(passed, gicon('ok'), gicon('remove'))), 
                    selection=list(mode='single'),
                    options = list(columnDefs = list(list(className = 'dt-center', targets=0:2)),
                                   pageLength = 50, dom = 't', rownames= FALSE),
                    colnames = c("Filter" , "Passed?", "Note"), 
                    escape=F)
    })
    
    output$datatable_QC_metrics <- DT::renderDataTable({
      DT::datatable(selected_run %>% t,
                    options = list(columnDefs = list(list(className = 'dt-center')),
                                   pageLength = 200, dom = 't', rownames= FALSE),
                    colnames = c(""))
    })
    
    ## if 'purity' run exists, show it by default; otherwise show the hisens run. 
    ## The following piece of code is just a hack to fool the reactive environment to trigger showing 
    ## selected run on the first selection 
    values$show_fit_type = ifelse(!is.na(selected_run$purity_run_version[1]), 'Purity', 'Hisens')

    if (is.null(input$radioGroupButton_fitType) || input$radioGroupButton_fitType == 'Hisens') {
      shinyWidgets::updateRadioGroupButtons(session, "radioGroupButton_fitType", selected="Purity")
    } else {
      shinyWidgets::updateRadioGroupButtons(session, "radioGroupButton_fitType", selected="Hisens")
    }
  })

  observeEvent(input$button_addReview, {
    selected_run <- values$sample_runs[1,] 
    sample = selected_run$tumor_sample_id[1]
    path = selected_run$path[1]
    facets_suite_qc = selected_run$facets_suite_qc[1]
    
    review_status = input$radioButtons_reviewStatus
    fit_name = input$selectInput_selectBestFit[1]
    signed_as = input$textInput_signAs[1]
    note = input$textAreaInput_reviewNote[1]
    use_only_purity_run = input$checkbox_purity_only[1]
    use_edited_cncf = input$checkbox_use_edited_cncf[1]
    reviewer_set_purity = input$textInput_purity[1]
    
    df <- get_review_status(sample, path)
    if (nrow(df) > 0){
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
      fit_name = c(fit_name),
      review_notes = c(note),
      reviewed_by = c(signed_as),
      date_reviewed = as.character(Sys.time()),
      facets_suite_qc = c(facets_suite_qc),
      use_only_purity_run = c(use_only_purity_run),
      use_edited_cncf = c(use_edited_cncf),
      reviewer_set_purity = c(reviewer_set_purity),
      stringsAsFactors=FALSE
    )
    update_review_status_file(path, df)

    refresh_review_status(sample, path, values$sample_runs)
  })

  observeEvent(input$radioGroupButton_fitType, {
    if (!verifiy_sshfs_mount()) { return (NULL) }

    if (input$selectInput_selectFit == "Not selected") {
      return(NULL)
    }
    
    if (values$show_fit_type != "" & input$radioGroupButton_fitType != values$show_fit_type) {
      shinyWidgets::updateRadioGroupButtons(session, "radioGroupButton_fitType", selected=values$show_fit_type)
      return(NULL)
    }
    values$show_fit_type = ""
    
    output$verbatimTextOutput_runParams <- renderText({})
    output$verbatimTextOutput_altBalLogR <- renderText({})
    output$imageOutput_pngImage1 <- renderImage({ list(src="", width=0, height=0)})
    
    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]
    
    shinyjs::hideElement("button_saveChanges")
    output$verbatimTextOutput_runParams <- renderText({
      if (input$radioGroupButton_fitType == "Hisens") {
        paste0("Seed: ", selected_run$hisens_run_Seed[1], ", ",
               "cval: ", selected_run$hisens_run_cval[1], ", ",
               "min.nhet: ", selected_run$hisens_run_nhet[1], ", ",
               "Purity: ", selected_run$hisens_run_Purity[1], ", ",
               "Ploidy: ", selected_run$hisens_run_Ploidy[1], ", ",
               "dipLogR: ", selected_run$hisens_run_dipLogR[1]
        )
      } else {
        paste0("Seed: ", selected_run$purity_run_Seed[1], ", ",
               "cval: ", selected_run$purity_run_cval[1], ", ",
               "min.nhet: ", selected_run$purity_run_nhet[1], ", ",
               "Purity: ", selected_run$purity_run_Purity[1], ", ",
               "Ploidy: ", selected_run$purity_run_Ploidy[1], ", ",
               "dipLogR: ", selected_run$purity_run_dipLogR[1]
        )
      }
    })
    
    output$verbatimTextOutput_altBalLogR <- renderText({
      if (input$radioGroupButton_fitType == "Purity") {
        paste0(selected_run$purity_run_alBalLogR[1])
      } else {
        paste0("")
      }
    })

    output$datatable_cncf <- DT::renderDataTable({
      cncf_data <-
        get_cncf_table(input$radioGroupButton_fitType, selected_run)
      if ( dim(cncf_data)[1] == 0 ){
        showModal(modalDialog( title = "CNCF file missing", "Invalid CNCF file" ))
        return()
      }
      DT::datatable(cncf_data,
                    selection=list(mode='single'),
                    options = list(columnDefs = list(list(className = 'dt-center')),
                                   pageLength = 50),
                    rownames=FALSE)
    })

    output$editableSegmentsTable <- rhandsontable::renderRHandsontable({
      cncf_data <-
        get_cncf_table(input$radioGroupButton_fitType, selected_run) %>%
        data.frame()
      if ( dim(cncf_data)[1] == 0 ){
        showModal(modalDialog( title = "CNCF file missing", "Invalid CNCF file" ))
        return()
      }
      if (!is.null(cncf_data)) {
        rhandsontable::rhandsontable(cncf_data,
                                     useTypes=FALSE, stretchH = "all") %>%
          rhandsontable::hot_table(columnSorting = TRUE,
                    highlightRow = TRUE,
                    highlightCol = TRUE)
      }
    })

    observe({
      if(!is.null(input$editableSegmentsTable$changes$changes)){
        shinyjs::show("button_saveChanges")
      }
    })

    output$imageOutput_pngImage1 <- renderImage({
      if (input$radioGroupButton_fitType == "Hisens") {
        png_filename = paste0(selected_run$hisens_run_prefix[1], ".CNCF.png")
      } else {
        png_filename = paste0(selected_run$purity_run_prefix[1], ".CNCF.png")
      }
      if (!file.exists(png_filename)) {
        png_filename = gsub("\\.CNCF", "", png_filename)
      }
      list(src = png_filename, contentType = 'image/png', width = 650, height = 800)
    },
    deleteFile = FALSE)
    
    output$plotOutput_closeup <- renderPlot ({
      list(src="", width=0, height=0)
    })
  })

  observeEvent(input$button_saveChanges, {
    if(is.null(input$editableSegmentsTable$changes$changes)){
      return(NULL)
    }
    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]
    df <- rhandsontable::hot_to_r(input$editableSegmentsTable)

    if (input$radioGroupButton_fitType == "Hisens") {
      run_prefix = selected_run$hisens_run_prefix[1]
    } else {
      run_prefix = selected_run$purity_run_prefix[1]
    }
    if ( file.exists(paste0(run_prefix, ".cncf.edited.txt"))) {
      cncf_filename = paste0(run_prefix, ".cncf.edited.txt")
    } else {
      cncf_filename = paste0(run_prefix, ".cncf.txt")
    }
    df <-
      fread(cncf_filename) %>%
      dplyr::select(-(tcn:lcn.em)) %>%
      dplyr::left_join(df %>% data.table %>% dplyr::select(chrom, loc.start, loc.end, seg, tcn:lcn.em),
                by=c( "chrom", "loc.start", "loc.end", "seg"))
    write.table(df, paste0(run_prefix, ".cncf.edited.txt"), quote=F, row.names=F, sep="\t")

    shinyjs::hideElement("button_saveChanges")
  })

  observeEvent(input$button_closeUpView, {
    if (!verifiy_sshfs_mount()) { return (NULL) }
    if (input$selectInput_selectFit == "Not selected") {
      output$verbatimTextOutput_runParams <- renderText({})
      output$verbatimTextOutput_altBalLogR <- renderText({})
      output$imageOutput_pngImage1 <- renderImage({ list(src="", width=0, height=0)})
      return(NULL)
    }
    
    selected_run <-
      values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]

    selected_gene = input$textInput_geneForCloseup
    if (selected_gene == "") {
      showModal(modalDialog( title = "No action", "Enter a gene name to get the closeup" ))
      return(NULL)
    }
    output$plotOutput_closeup <- renderPlot ({
      if (input$radioGroupButton_fitType == "Hisens") {
        rdata_file = paste0(selected_run$hisens_run_prefix[1], ".Rdata")
      } else {
        rdata_file = paste0(selected_run$purity_run_prefix[1], ".Rdata")
      }
      load(rdata_file)
      closeup_output <- close.up(out, fit, gene.name=selected_gene,
                                 cached.gene.path =
                                   system.file("data/Homo_sapiens.GRCh37.75.gene_positions.txt",
                                               package="facetsPreview"))
      gridExtra::grid.arrange(closeup_output$cnlr,
                   closeup_output$valor,
                   closeup_output$icnem,
                   closeup_output$cfem,
                   ncol=1, nrow=4, top = paste0(selected_gene,
                                                " ", closeup_output$chrom,
                                                ":", closeup_output$start,
                                                "-", closeup_output$end))
    })
  })


  observeEvent(input$button_refit, {
    if (!verifiy_sshfs_mount()) { return (NULL) }
    if (input$selectInput_selectFit == "Not selected" ||
        is.na(suppressWarnings(as.integer(input$textInput_newDipLogR))) ||
        is.na(suppressWarnings(as.integer(input$textInput_newPurityCval))) ||
        is.na(suppressWarnings(as.integer(input$textInput_newHisensCval))) ||
        is.na(suppressWarnings(as.integer(input$textInput_newMinNHet))))
    {
      showModal(modalDialog(
        title = "Not submitted", paste0("Need all four parameters for refit.")
      ))
      return(NULL)
    }

    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]
    sample_id = selected_run$tumor_sample_id[1]
    facets_version = selected_run$hisens_run_version[1]
    if(is.na(facets_version)) {
      facets_version = selected_run$purity_run_version[1]
    }
    run_path = selected_run$path[1]
    new_purity_c = input$textInput_newPurityCval
    new_hisens_c = input$textInput_newHisensCval
    new_m = input$textInput_newMinNHet
    new_diplogR = input$textInput_newDipLogR
    
    refit_name <- glue("/refit_c{new_hisens_c}_pc{new_purity_c}_m{new_m}_diplogR_{new_diplogR}")
    cmd_script_pfx = "/juno/work/ccs/bandlamc/facets_review_app/facets_refit_watcher/facets_refit_cmd_"
    refit_cmd_file <- glue("{cmd_script_pfx}{sample_id}_c{new_hisens_c}_pc{new_purity_c}_m{new_m}_diplogR_{new_diplogR}.sh")

    if (any(values$submitted_refit == refit_name)) {
      showModal(modalDialog(
        title = "Not submitted", paste0("Job already queued. Check logs: ", refit_cmd_file, ".*")
      ))
      return(NULL)
    }
    
    refit_dir <- paste0(run_path, refit_name)
    
    facets_lib_path = fread('/juno/work/ccs/bandlamc/facets_review_app/facets_versions.dat')[version==facets_version]$r_lib_path
    
    ### cur_version of facets;
    refit_cmd = glue(paste('/opt/common/CentOS_7-dev/bin/Rscript /juno/work/ccs/bandlamc/software/R_libs/facetsSuite/2.0.1-beta/run-facets-wrapper.R ',
                           '--facets-lib-path {facets_lib_path} ', 
                           '--counts-file {run_path}/countsMerged____{sample_id}.dat.gz ',
                           '--sample-id {sample_id} ',
                           '--snp-window-size 250 --normal-depth 35 ',
                           '--min-nhet {new_m} --purity-min-nhet {new_m} --seed 100 ',
                           '--cval {new_hisens_c} --purity-cval {new_purity_c} --legacy-output T ',
                           '--genome hg19 --directory {refit_dir} '))

    write(refit_cmd, refit_cmd_file)
    showModal(modalDialog(
      title = "Job submitted!", paste0("Check back in a few minutes. Logs: ", refit_cmd_file, ".*")
    ))
    values$submitted_refit <- c(values$submitted_refit, refit_name)
  })

  ## check if watcher is running
  {
    cur_time = as.numeric(system(" date +%s ", intern=TRUE))
    last_mod = as.numeric(system("stat -f%c /juno/work/ccs/bandlamc/facets_review_app/facets_refit_watcher/watcher.log", intern=TRUE))
    if ( cur_time - last_mod < 900) {
      shinyjs::showElement(id="div_watcherSuccess")
    } else {
      shinyjs::showElement(id="div_watcherFail")
    }
  }
}
