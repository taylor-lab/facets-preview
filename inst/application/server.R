
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
  values <- reactiveValues(config_file = ifelse( exists("facets_preview_config_file"), facets_preview_config_file, "<not set>"))
  output$verbatimTextOutput_sessionInfo <- renderPrint({print(sessionInfo())})
  output$verbatimTextOutput_signAs <- renderText({paste0(system('whoami', intern = T))})
  
  observe({  
    values$config_file = ifelse( exists("facets_preview_config_file"), facets_preview_config_file, "<not set>")
    if (!suppressWarnings(file.exists(values$config_file))) {
      showModal(modalDialog( title = "config file not found",  
                             'config file not found. Expects \"facets_preview_config_file\" variable in .Rprofile',
                             easyClose = TRUE))
      return(NULL)
    }

    ### NOTE: Removing json validation because installing 'jsonvalidate' on juno is a nightmare. 
    ### Need to figure out an alternative or somehow install it.
    # json_validation_status = jsonvalidate::json_validate(values$config_file, 
    #                                                      system.file("data/config_schema.json", package="facetsPreview"), 
    #                                                      verbose=T)
    # if (!json_validation_status) {
    #   showModal(modalDialog( title = "config file parsing error",  
    #                          'likely missing or incorrectly set parameters in config file. check console for error information',
    #                          easyClose = TRUE))
    #   print(json_validation_status)
    #   stop('Error parsing config file')
    #   stopApp(1)
    # }
    
    values$config = configr::read.config(values$config_file)
    
    updateSelectInput(session, "selectInput_repo",
                      choices = as.list(c("none", values$config$repo$name)),
                      selected = "none")
    
    source(values$config$facets_qc_script)
    
    library(facetsSuite, lib.loc = values$config$facets_suite_lib)
    
    shinyjs::html("element_facets_qc_version1", paste0('facets qc version: ', facets_qc_version()))
    shinyjs::html("element_facets_qc_version2", paste0('facets qc version: ', facets_qc_version()))
    
    # check if sshfs is mounted
    if (!verify_sshfs_mount(values$config$watcher_dir)) {
      return(NULL)
    }

    ## check if watcher is running
    {
      if (!file.exists(paste0(values$config$watcher_dir, '/watcher.log'))) {
        showModal(modalDialog( title = "Error",  
                               'refit watcher is not setup. check the config file. aborting!',
                               easyClose = TRUE))
        stopApp(1)
      }
      
      cur_time = as.numeric(system(" date +%s ", intern=TRUE))
      if (Sys.info()['sysname'] == "Linux" ) { 
	last_mod = as.numeric(system(paste0("stat -c %Y ", values$config$watcher_dir, "/watcher.log"), intern=TRUE)) 
      } else { 
	last_mod = as.numeric(system(paste0("stat -f%c ", values$config$watcher_dir, "/watcher.log"), intern=TRUE)) 
      }
      
      if ( cur_time - last_mod < 900) {
        values$watcher_status = T
        shinyjs::showElement(id="div_watcherSuccess")
      } else {
        values$watcher_status = F
        shinyjs::showElement(id="div_watcherFail")
      }
    }
  })
  
  observeEvent(input$link_choose_repo, {
    # Change the following line for more examples
    showModal(
      modalDialog(
        selectInput("selectInput_repo", "choose respository:", values$config$repo$name),
        footer = tagList(
          actionButton("actionButton_selectRepo", "Submit"),
          modalButton('Dismiss'))
      )
    )
  })
  
  observeEvent(input$actionButton_selectRepo, {
    values$selected_repo = as.list(values$config$repo %>% filter(name == input$selectInput_repo) %>% head(n=1))
    shinyjs::html("element_repo_name", paste0('Selected repository: ', values$selected_repo$name))
    shinyjs::html("element_repo_manifest", paste0('manifest file: ', values$selected_repo$manifest_file))
    removeModal()
  })
  
  #' helper function for app
  #'
  #' @return checks for mount
  #' @export verify_sshfs_mount
  verify_sshfs_mount <- function(watcher_dir) {
    if (values$config$verify_sshfs_mount == "") {
      return(TRUE)
    }
    fs = paste0("/", values$config$verify_sshfs_mount)
    
    if (!grepl(paste0(":", fs, " "), 
               paste(system("mount 2>&1", intern=TRUE), collapse=" ")) |
        grepl("No such file", 
              paste(system(paste0("ls ", watcher_dir, " 2>&1"), intern=TRUE), collapse=" "))) {
      shinyjs::showElement(id= "wellPanel_mountFail")
      showModal(modalDialog( title = paste0(fs, " mount not detected"), "Re-mount and try again" ))
      stopApp(1)
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
                        mutate(facets_qc = ifelse(facets_qc, gicon('ok'), gicon('remove'))) %>%
                        mutate(is_best_fit = ifelse(is_best_fit, gicon('thumbs-up'), '')) %>%
                        select(fit_name, facets_qc, facets_qc_version, manual_review_best_fit = is_best_fit) %>%
                        unique,
                      selection=list(mode='single'),
                      colnames = c('Fit', 'facets QC', 'facets QC ver.','Reviewed as best fit?'),
                      options = list(columnDefs = list(list(className = 'dt-center', targets = 0:2)),
                                     pageLength = 100, dom='t'),
                      rownames=FALSE, escape = F)
      })
      
      output$datatable_reviewHistory <- DT::renderDataTable({
        DT::datatable(review_df %>%
                        filter(review_status != 'not_reviewed') %>%
                        mutate(use_only_purity_run = ifelse(use_only_purity_run, gicon('ok-sign'), '')) %>%
                        mutate(use_edited_cncf = ifelse(use_edited_cncf, gicon('ok-sign'), '')) %>%
                        mutate(facets_qc = ifelse(facets_qc, gicon('ok'), gicon('remove'))) %>%
                        dplyr::select(-sample, -path, -facets_suite_version) %>%
                        dplyr::arrange(desc(date_reviewed)) %>%
                        select(fit_name, review_status, facets_qc, facets_qc_version, review_notes,
                               reviewed_by, date_reviewed, use_only_purity_run, use_edited_cncf,
                               reviewer_set_purity),
                      selection=list(mode='single'),
                      colnames = c('Fit', 'Review Status', 'facets QC', 'facets QC ver.', 'Notes', 
                                   'Reviewer', 'Date Reviewed', 'Use purity run only?',  
                                   'Use edited.cncf.txt?', 'Reviewer set purity:'),
                      options = list(columnDefs = list(list(className = 'dt-center', targets = 0:6)),
                                     pageLength = 100, dom = 't'),
                      rownames=FALSE, escape = F)
      })
    }
  }
  
  shinyjs::hideElement("button_saveChanges")
  
  observeEvent(input$button_mountFailRefresh, {
    verify_sshfs_mount(values$config$watcher_dir)
    return(NULL)
  })

  observeEvent(input$reviewTabsetPanel, {
   if (input$reviewTabsetPanel == "cBioPortal") {
     if (!verify_sshfs_mount(values$config$watcher_dir)) { return (NULL) }
     selected_sample = paste(unlist(values$manifest_metadata[input$datatable_samples_rows_selected,1]), collapse="")
     dmp_id = (values$manifest_metadata %>% filter(sample_id == selected_sample))$dmp_id[1]

     if (!is.null(dmp_id) && !is.na(dmp_id)) {
       browseURL(paste0('https://cbioportal.mskcc.org/patient?studyId=mskimpact&caseId=', dmp_id))
       updateTabsetPanel(session, "reviewTabsetPanel", selected = "png_image_tabset")
     } else if (grepl('P\\-\\d{7}.*', selected_sample)) {
       patient_id = gsub("\\-T.*", "", selected_sample)
       browseURL(paste0('https://cbioportal.mskcc.org/patient?studyId=mskimpact&caseId=', patient_id))
       updateTabsetPanel(session, "reviewTabsetPanel", selected = "png_image_tabset")
     } else{
       showModal(modalDialog( title = "Not a valid DMP ID", "Cannot open this sample in cBioPortal"))
     }
   }
  })

  observeEvent(input$button_repoSamplesInput, {
    if (!verify_sshfs_mount(values$config$watcher_dir)) { return (NULL) }
    
    if (is.null(values$selected_repo)) {
      showModal(modalDialog(title = "Failed", 
                            paste0("No facets repository selected. Please choose one.")
      ))
      return(NULL)
    }
    
    # make sure the sample input string is the right format
    tumor_ids <- gsub(' |\\s|\\t', '', input$textAreaInput_repoSamplesInput)

    if (!grepl(values$selected_repo$tumor_id_format, tumor_ids)) {
      showModal(modalDialog(title = "Incorrect format!", 
                            paste0("Tumor Sample IDs are in incorrect format. ",
                                   "Expecting one or more (comma-separated) IDs")
                            ))
      return(NULL)
    }
    values$loaded_time = Sys.time()
    
    tumor_ids <- unlist(strsplit(tumor_ids, ","))
    
    updateNavbarPage(session, "navbarPage1", selected = "tabPanel_samplesManifest")
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Reading Samples:", value = 0)
    
    values$manifest_metadata <- load_repo_samples(tumor_ids, values$selected_repo$manifest_file, progress)
    
    num_samples_queried = length(tumor_ids)
    num_samples_found = nrow(values$manifest_metadata)
    if (num_samples_queried != num_samples_found) {
      showModal(modalDialog(title = "Warning!", 
                            paste0("Note: Only ", num_samples_found, " of the ", num_samples_queried, 
                                   " Tumor IDs queried are found in the respository.")
      ))
      if (num_samples_found == 0) {
        return(NULL)
      }
    }
    values$submitted_refits <- c()
  })

  observeEvent(input$button_samplesInput, {
    if (!verify_sshfs_mount(values$config$watcher_dir)) { return (NULL) }

    values$selected_repo = NULL
    
    values$loaded_time = Sys.time()
    
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
                    dplyr::select(-path, -facets_suite_version, -facets_qc_version) %>%
                    mutate(default_fit_qc = ifelse(default_fit_qc, gicon('ok'), gicon('remove'))) %>%
                    mutate(reviewed_fit_facets_qc = 
                             ifelse(review_status == 'Not reviewed', '',
                                    ifelse(reviewed_fit_facets_qc, gicon('ok'), gicon('remove')))) %>%
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
      
      elapsed_time = as.integer(difftime(Sys.time(), values$loaded_time, units = 'secs'))
      showModal(modalDialog( title = "Warning!",
                             paste0(elapsed_time, 
                             " seconds have elapsed since reviews were loaded. To ensure capturing most recent reviews, ",
                             " load samples again from 'Load Samples' page")))
      
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
    if (!verify_sshfs_mount(values$config$watcher_dir)) { return (NULL) }
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
    output$imageOutput_pngImage1 <- renderImage({ list(src="", width=0, height=0)})

    if ( is.null(values$sample_runs) || dim(values$sample_runs)[1] == 0) {
      showModal(modalDialog( title = "Unable to read sample", "Either no runs exist for this sample, or, 'sshfs' mount failed." ))
      return(NULL)  # print some kind of error and exit;
    }

    # update with review status
    refresh_review_status(selected_sample, selected_sample_path, values$sample_runs)
    
    ## get best fit if exists; other-wise default
    selected_run = values$sample_runs %>% filter(fit_name=='default') %>% head(n=1)
    
    if (nrow(values$sample_runs %>% filter(is_best_fit)) == 1) {
      selected_run = values$sample_runs %>% filter(is_best_fit) %>% head(n=1)
    } else {
      default_fit = (values$manifest_metadata %>% filter(sample_id == selected_sample))$default_fit_name
      selected_run = values$sample_runs %>% filter(fit_name==default_fit) %>% head(n=1)
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
    
    if (nrow(selected_run) > 0) {
      updateTextInput(session, "textInput_newDipLogR", label = NULL, value = "")
      updateTextInput(session, "textInput_newPurityCval", label = NULL, value = selected_run$purity_run_cval)
      updateTextInput(session, "textInput_newHisensCval", label = NULL, value = selected_run$hisens_run_cval)
      updateTextInput(session, "textInput_newPurityMinNHet", label = NULL, value = selected_run$purity_run_nhet)
      updateTextInput(session, "textInput_newHisensMinNHet", label = NULL, value = selected_run$hisens_run_nhet)
      updateTextInput(session, "textInput_newSnpWindowSize", label = NULL, value = selected_run$purity_run_snp_nbhd)
      updateTextInput(session, "textInput_newNormalDepth", label = NULL, value = selected_run$purity_run_ndepth)
      updateSelectInput(session, "selectInput_newFacetsLib",
                        choices = as.list(values$config$facets_lib$version),
                        selected = selected_run$purity_run_version)
    }
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
                row.names=F,
                eol = '')
    close(clip)
  })
  
  observeEvent(input$link_advancedOptions, {
    shinyjs::toggleElement(id='wellPanel_advancedOptions')
  })

  observeEvent(input$selectInput_selectFit, {
    if (!verify_sshfs_mount(values$config$watcher_dir)) { return (NULL) }
    output$verbatimTextOutput_runParams <- renderText({})
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
                         "hyper_seg_filter", "high_ploidy_filter", "valid_purity_filter", 
                         "em_cncf_icn_discord_filter", "dipLogR_too_low_filter", 
                         "icn_allelic_state_concordance_filter", "subclonal_genome_filter", "contamination_filter")
      filter_names = c("Homozygous deletions", "Diploid segments (in dipLogR)", "No Waterfall pattern", 
                       "No hyper segmentation", "Not high ploidy", "Has valid purity", 
                       "em vs. cncf TCN/LCN discordance", "dipLogR not too low", 
                       "ICN is discordant with allelic state ", "High % subclonal","contamination check")
      
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
      DT::datatable(selected_run %>% 
                      select(-ends_with("note"),
                             -ends_with("pass")) %>%
                      t,
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
    
    if (!verify_access_to_write(path)) {
      showModal(modalDialog(
        title = "Failed to add review", 
        paste0("You do not have permissions to create/edit: ", path, "/facets_review.manifest")
      ))
      return(NULL)
    }
    
    facets_qc = as.character(selected_run$facets_qc[1])
    facets_qc_version = as.character(selected_run$facets_qc_version[1])
    facets_suite_version = as.character(selected_run$facets_suite_version[1])

    review_status = input$radioButtons_reviewStatus
    fit_name = input$selectInput_selectBestFit[1]
    signed_as = system('whoami', intern=T)
    note = input$textAreaInput_reviewNote[1]
    use_only_purity_run = input$checkbox_purity_only[1]
    use_edited_cncf = input$checkbox_use_edited_cncf[1]
    reviewer_set_purity = input$textInput_purity[1]
    
    ### reset review stauts fields
    #updateSelectInput(session, 'selectInput_selectBestFit', choices=c("Not selected"))
    updateCheckboxInput(session, 'checkbox_purity_only', value = F)
    updateCheckboxInput(session, 'checkbox_use_edited_cncf', value = F)
    updateRadioButtons(session, 'radioButtons_reviewStatus', selected='not_reviewed')
    updateTextInput(session, 'textInput_purity', value='')
    updateTextAreaInput(session, 'textAreaInput_reviewNote', value='')
    
    ### make sure the edited cncf file exists
    if (use_edited_cncf) {
      cncf_filename = paste0(path, fit_name, '/', sample, '_', 
                             ifelse(use_only_purity_run, 'purity', 'hisens'),
                             '.cncf.edited.txt')
      if (!file.exists(cncf_filename)) {
        showModal(modalDialog(
          title = "Failed", paste0("Review not added. CNCF file assigned to this review does not exist. ",
                                             cncf_filename)
        ))
        return(NULL)
      }
    }

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
      facets_qc = c(facets_qc),
      use_only_purity_run = c(use_only_purity_run),
      use_edited_cncf = c(use_edited_cncf),
      reviewer_set_purity = c(reviewer_set_purity),
      facets_qc_version = c(facets_qc_version),
      facets_suite_version = c(facets_suite_version),
      stringsAsFactors=FALSE
    )
    update_review_status_file(path, df)
    
    update_best_fit_status(sample, path)

    refresh_review_status(sample, path, values$sample_runs)
  })

  observeEvent(input$radioGroupButton_fitType, {
    if (!verify_sshfs_mount(values$config$watcher_dir)) { return (NULL) }

    if (input$selectInput_selectFit == "Not selected") {
      return(NULL)
    }
    
    if (values$show_fit_type != "" & input$radioGroupButton_fitType != values$show_fit_type) {
      shinyWidgets::updateRadioGroupButtons(session, "radioGroupButton_fitType", selected=values$show_fit_type)
      return(NULL)
    }
    values$show_fit_type = ""
    
    output$verbatimTextOutput_runParams <- renderText({})
    output$imageOutput_pngImage1 <- renderImage({ list(src="", width=0, height=0)})
    
    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]
    
    shinyjs::hideElement("button_saveChanges")
    output$verbatimTextOutput_runParams <- renderText({
      if (input$radioGroupButton_fitType == "Hisens") {
        paste0("purity: ", selected_run$hisens_run_Purity[1], ", ",
               "ploidy: ", selected_run$hisens_run_Ploidy[1], ", ",
               "dipLogR: ", selected_run$hisens_run_dipLogR[1], "\n",
               "facets_lib: ", selected_run$hisens_run_version[1])
      } else {
        paste0("purity: ", selected_run$purity_run_Purity[1], ", ",
               "ploidy: ", selected_run$purity_run_Ploidy[1], ", ",
               "dipLogR: ", selected_run$purity_run_dipLogR[1], "\n",
               "facets_lib: ", selected_run$purity_run_version[1], "\n",
               "alt dipLogR: ", selected_run$purity_run_alBalLogR[1])
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
    if (!verify_sshfs_mount(values$config$watcher_dir)) { return (NULL) }
    if (input$selectInput_selectFit == "Not selected") {
      output$verbatimTextOutput_runParams <- renderText({})
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
    if (!verify_sshfs_mount(values$config$watcher_dir)) { return (NULL) }
    if (input$selectInput_selectFit == "Not selected") {
      showModal(modalDialog(
        title = "Cannot submit refit", paste0("select 'any' fit first and then click 'Run'")
      ))
      return(NULL)
    }
    
    # Enforce required values for all parameters.
    if (input$textInput_newPurityCval == "" || input$textInput_newHisensCval == "" || 
        input$textInput_newPurityMinNHet == "" || input$textInput_newHisensMinNHet == "" || 
        input$textInput_newNormalDepth == "" || input$textInput_newSnpWindowSize == "" ||
        input$selectInput_newFacetsLib == "") {
      showModal(modalDialog(
        title = "Cannot submit refit", "All refit parameters are required."
      ))
      return(NULL)
    }

    # make sure all the parameters are numeric
    if (!suppressWarnings(all(!is.na(as.numeric(c(input$textInput_newPurityCval, 
                                                 input$textInput_newHisensCval, 
                                                 input$textInput_newPurityMinNHet, 
                                                 input$textInput_newHisensMinNHet, 
                                                 input$textInput_newNormalDepth, 
                                                 input$textInput_newSnpWindowSize)))))) {
      showModal(modalDialog(
        title = "Cannot submit refit", paste0("Non-numeric characters are found in re-fit parameters")
      ))
      return(NULL)
    }
    
    with_dipLogR = T
    refit_note = ""
    if (input$textInput_newDipLogR == "") {
      with_dipLogR = F
      refit_note = "Refit job is submitted without a dipLogR and therefore will be determined by purity run."
    }
    
    sample_id = values$sample_runs$tumor_sample_id[1]
    
    ## get best fit if exists; other-wise default
    selected_run = values$sample_runs %>% filter(fit_name=='default') %>% head(n=1)
    
    if (nrow(selected_run) == 0) {
      selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]
    } 
    
    run_path = selected_run$path[1]
    new_purity_c = input$textInput_newPurityCval
    new_hisens_c = input$textInput_newHisensCval
    new_purity_m = input$textInput_newPurityMinNHet
    new_hisens_m = input$textInput_newHisensMinNHet
    new_normal_depth = input$textInput_newNormalDepth
    new_snp_window_size = input$textInput_newSnpWindowSize
    new_facets_lib = input$selectInput_newFacetsLib
    new_diplogR = input$textInput_newDipLogR
    
    default_run_facets_version = selected_run$hisens_run_version[1]
    if(is.na(default_run_facets_version)) {
      default_run_facets_version = selected_run$purity_run_version[1]
    }
    
    facets_version_to_use = new_facets_lib
    if (grepl('use current', new_facets_lib)) {
      facets_version_to_use = default_run_facets_version
    }
    
    supported_facets_versions = values$config$facets_lib %>% data.table
    if (!(facets_version_to_use %in% supported_facets_versions$version)) {
      showModal(modalDialog(
        title="Not submitted", 
        paste0("Current version of facets-preview does not support refits using facets version: ", facets_version_to_use)
      ))
      return(NULL)
    }
    
    name_tag = (paste0("c{new_hisens_c}_pc{new_purity_c}",
                       ifelse(with_dipLogR, '_diplogR_{new_diplogR}', ''),
                       ifelse(new_purity_m != selected_run$purity_run_nhet, '_pm{new_purity_m}', ''),
                       ifelse(new_hisens_m != selected_run$hisens_run_nhet, '_m{new_hisens_m}', ''),
                       ifelse(new_normal_depth != selected_run$purity_run_ndepth, '_nd{new_normal_depth}', ''),
                       ifelse(new_snp_window_size != selected_run$purity_run_snp_nbhd, '_n{new_snp_window_size}', ''),
                       ifelse(new_facets_lib != selected_run$purity_run_version, '_v{facets_version_to_use}', '')
    ))

    name_tag = glue(name_tag)
    refit_name <- glue('/refit_{name_tag}')
    
    cmd_script_pfx = paste0(values$config$watcher_dir, "/refit_jobs/facets_refit_cmd_")
    
    refit_dir <- paste0(run_path, refit_name)
    facets_lib_path = supported_facets_versions[version==facets_version_to_use]$lib_path
  
    counts_file_name = glue("{run_path}/countsMerged____{sample_id}.dat.gz")
    if (!is.null(values$selected_repo)) {
      counts_file_name = glue(paste0("{run_path}/",values$selected_repo$counts_file_format))
    }
    
    if (!file.exists(counts_file_name)) {
      # try alternate counts file; tempo format; eg: SU2LC_MSK_1365_T__SU2LC_MSK_1365_N.snp_pileup.gz
      if (file.exists(glue("{run_path}/{sample_id}.snp_pileup.gz"))) {
        counts_file_name = glue("{run_path}/{sample_id}.snp_pileup.gz")
      } else {
        showModal(modalDialog( title = "Not submitted", paste0("Counts file does not exist: ", counts_file_name) ))
        return(NULL)
      }
    }
    
    refit_cmd_file <- glue("{cmd_script_pfx}{sample_id}_{name_tag}.sh")
    if (file.size(counts_file_name) > 5e7) {
      refit_cmd_file <- glue("{cmd_script_pfx}{sample_id}_{name_tag}.bsub.sh")  
    }
    
    if (any(values$submitted_refit == refit_dir)) {
      showModal(modalDialog(
        title = "Not submitted", paste0("Job already queued. Check logs: ", refit_cmd_file, ".*")
      ))
      return(NULL)
    }
    
    ## check if the user has permissions to write to that directory
    if (!has_permissions_to_write(refit_dir)) {
      showModal(modalDialog(
        title = "Not submitted",
        paste0("Unable to create refit directory. Check if you have permissions to write to: ", refit_dir)
      ))
      return(NULL)
    }
    
    refit_cmd = glue(paste0('{values$config$r_script_path}  ',
                           '{values$config$facets_suite_run_wrapper} ',
                           '--facets-lib-path {facets_lib_path} ', 
                           '--counts-file {counts_file_name} ',
                           '--sample-id {sample_id} ',
                           '--snp-window-size {new_snp_window_size} ',
                           '--normal-depth {new_normal_depth} ',
                           ifelse(with_dipLogR, '--dipLogR {new_diplogR} ', ''),
                           '--min-nhet {new_hisens_m} ',
                           '--purity-min-nhet {new_purity_m} ',
                           '--seed 100 ',
                           '--cval {new_hisens_c} --purity-cval {new_purity_c} --legacy-output T -e ',
                           '--genome hg19 --directory {refit_dir} '))

    write(refit_cmd, refit_cmd_file)
    
    showModal(modalDialog(
      title = "Job submitted!", 
      paste0(ifelse(refit_note != '', paste('Warning: ', refit_note, '\n\n'), ''),
             "Check back in a few minutes. Logs: ", refit_cmd_file, ".*")
    ))
    values$submitted_refit <- c(values$submitted_refit, refit_dir)
  })
}
