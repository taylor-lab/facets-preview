
#' helper function for app
#'
#' @param manifest list of facets run directories
#' @param progress progress bar from shiny
#' @return simple metadata data.frame
#' @export load_samples
load_samples <- function(manifest, progress) {
  metadata <- data.frame(sample=c(), path = c(), run_dir_exists = c(), num_fits_found = c(),stringsAsFactors=FALSE)
  for(i in 1:length(manifest)[1]) {
    sample_path = manifest[i]
    sample = tail(unlist(strsplit(sample_path, "/")), 1)
    facets_review_metadata_file = paste(sample_path, "/facets_review.metadata", sep="")
    m <- metadata_init_quick(sample, sample_path)
    metadata <- rbind(metadata, 
                      data.frame(sample=sample, path=sample_path, run_dir_exists = m[1], num_fits_found = m[2], 
                                 stringsAsFactors=FALSE ))
    progress$inc(1/length(manifest), detail = paste(" ", i, "/", length(manifest)))
  }
  metadata
}

#' helper function for app
#'
#' @param sample sample name
#' @param sample_path path to facets run dir
#' @return minimal description of the facets run
#' @export metadata_init_quick
metadata_init_quick <- function(sample, sample_path) {
  run_dir_exists = FALSE
  if ( dir.exists(sample_path)) {
    run_dir_exists = TRUE
  }
  facets_run_dirs = list.dirs(sample_path, full.names=FALSE)
  facets_run_dirs <- facets_run_dirs[grep("^facets", facets_run_dirs)]
  return (c(run_dir_exists, length(facets_run_dirs)))
}

#' helper function for app
#'
#' @param sample sample name
#' @param sample_path path to facets run dir
#' @param progress progress bar from shiny
#' @return description of the facets run
#' @export metadata_init
metadata_init <- function(sample, sample_path, progress) {
  facets_runs <- get_new_facets_runs_df()
  facets_run_dirs = list.dirs(sample_path, full.names=FALSE)
  facets_run_dirs <- facets_run_dirs[grep("^facets", facets_run_dirs)]
  
  for(fi in 1:length(facets_run_dirs)) {
    progress$inc(1/length(facets_run_dirs), detail = paste(" ", fi, "/", length(facets_run_dirs)))
    fit_name = facets_run_dirs[fi]
    facets_run = paste(sample_path, "/", fit_name, sep="")
    facets_run_files = list.files(facets_run, pattern=".out$")
    if ( length(facets_run_files) == 0 ) {
      next
    }
    
    for( fif in 1:length(facets_run_files) ) {
      paste(facets_run, facets_run_files[fif], sep="/")
      facets_out_params = readLines(paste0(facets_run, "/", facets_run_files[fif]))
      run_type = "hisens_"
      if (grepl("_purity", facets_out_params[2])) {
        run_type = "purity_"
      }
      
      rm(list = ls()[grep(run_type, ls())]) # remove all previous facets_params
      run_prefix = ""
      for ( p_idx in 1:length(facets_out_params)) {
        line = gsub(" |#", "", facets_out_params[p_idx])
        sp = unlist(strsplit(line, "="))
        if ( length(sp) == 2) {
          if (sp[1] == "TAG"){
            run_prefix = paste0(facets_run, "/", sp[2])
            assign(paste0(run_type, "prefix"), run_prefix )      
          }
          assign(paste0(run_type, sp[1]), sp[2])
        }
      }
      rdata_file = paste0(run_prefix, ".Rdata")
      if ( file.exists(rdata_file)) {
        load(rdata_file)
        if (!is.null(out$alBalLogR)) {
          assign(paste0(run_type, "alBalLogR"), paste(round(out$alBalLogR[,1],digits = 2), collapse=", "))
        }
      }
    }
    
    facets_runs <- rbind(facets_runs, 
                         data.frame(tumor_sample_id = sample, path = sample_path, fit_name = fit_name,
                                    purity_run_prefix = get0("purity_prefix", ifnotfound = NA),
                                    purity_run_Seed = get0("purity_Seed", ifnotfound = NA),
                                    purity_run_cval = get0("purity_cval", ifnotfound = NA),
                                    purity_run_Purity = round_down(get0("purity_Purity", ifnotfound = NA)),
                                    purity_run_Ploidy = round_down(get0("purity_Ploidy", ifnotfound = NA)),
                                    purity_run_dipLogR = round_down(get0("purity_dipLogR", ifnotfound = NA)),
                                    purity_run_alBalLogR = get0("purity_alBalLogR", ifnotfound = NA),
                                    
                                    hisens_run_prefix = get0("hisens_prefix", ifnotfound = NA),
                                    hisens_run_Seed = get0("hisens_Seed", ifnotfound = NA),
                                    hisens_run_cval = get0("hisens_cval", ifnotfound = NA),
                                    hisens_run_hisens = round_down(get0("hisens_hisens", ifnotfound = NA)),
                                    hisens_run_Ploidy = round_down(get0("hisens_Ploidy", ifnotfound = NA)),
                                    hisens_run_dipLogR = round_down(get0("hisens_dipLogR", ifnotfound = NA)),
                                    
                                    manual_note = NA,
                                    is_best_fit = NA,
                                    stringsAsFactors=FALSE
                         )
    )
  }
  facets_runs
}

#' helper function for app
#'
#' @param val_str input string
#' @return converts string to numeric and rounds to 2-digits
#' @export round_down
round_down <- function(val_str){
  if ( is.na(val_str)) {
    return (NA)
  }

  if ( grepl("^\\-?\\d*\\.?\\d*$", as.character(val_str)) ) {
    return (round(as.numeric(val_str), digits=2))
  } else {
    val_str
  }
}

#' helper function for app
#'
#' @return returns an empty dataframe
#' @export get_new_facets_runs_df
get_new_facets_runs_df <- function() {
  return (data.frame(
    tumor_sample_id = c(), path = c(), fit_name = c(),
    
    purity_run_prefix = c(), purity_run_Seed = c(), purity_run_cval = c(),
    purity_run_Purity = c(), purity_run_Ploidy = c(), purity_run_dipLogR = c(), purity_run_alBalLogR = c(),
    
    hisens_run_prefix = c(), hisens_run_Seed = c(), hisens_run_cval = c(),
    hisens_run_hisens = c(), hisens_run_Ploidy = c(), hisens_run_dipLogR = c(),
    
    manual_note = c(),
    is_best_fit = c(),
    stringsAsFactors=FALSE
  )
  )
}

#' helper function for app
#'
#' @return launches app
#' @export launch_application
launch_application <- function() {
  shiny::runApp(appDir = system.file("application", package = "facetsPreview"))
  
}

#' helper function for app
#'
#' @return launches app
#' @export launch_application_browser
launch_application_browser <- function() {
  shiny::runApp(appDir = system.file("application", package = "facetsPreview"), launch.browser = TRUE)
  
}