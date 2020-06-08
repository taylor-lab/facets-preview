
#' helper function for app
#'
#' @param manifest list of facets run directories
#' @param progress progress bar from shiny
#' @return simple metadata data.frame
#' @import dplyr
#' @export load_samples
load_samples <- function(manifest, progress=NA) {
  metadata <- data.frame()
  ## could do this using sapply but want to display the progress bar;
  for(i in 1:length(manifest)[1]) {
    sample_path = manifest[i]
    sample_id = tail(unlist(strsplit(sample_path, "/")), 1)
    
    metadata <- rbind(metadata, as.data.frame.list(metadata_init_quick(sample_id, sample_path), stringsAsFactors = F))
    if (!is.null(progress)) {
      progress$inc(1/length(manifest), detail = paste(" ", i, "/", length(manifest)))
    }
  }
  metadata
}

#' helper function for app
#'
#' @param manifest list of facets run directories
#' @param progress progress bar from shiny
#' @return simple metadata data.frame
#' @import dplyr
#' @export load_repo_samples
load_repo_samples <- function(tumor_ids, manifest_file, progress) {
  metadata <- data.frame()
  ##
  ## Load facets manifest files
  ##
  if (is.na(manifest_file) | !file.exists(manifest_file) | countLines(manifest_file) == 0) {
    stop(paste0('Aborting! manifest file does not exist. ', manifest_file))
  }

  repo <- fread(cmd=paste0('gzip -dc ', manifest_file)) 

  ### if manifest file has old column names for sample_id (which was 'tag') and sample_path ('run_prefix'), just rename them
  if (all(c("tag", "run_prefix") %in% names(repo)) & !any(c("sample_id", "sample_path") %in% names(repo))) {
    repo <- repo %>% mutate(sample_id = tag, sample_path = run_prefix)
  }

  if (!(all(c("sample_id", "sample_path", "tumor_sample") %in% names(repo)))) {
    stop("Aborting. Manifest file does not have the required columns: sample_id, sample_path, tumor_sample")
  }
  
  repo <- repo %>% dplyr::filter(tumor_sample %in% tumor_ids) %>% unique
  
  if (nrow(repo) == 0) {
    return(metadata)
  }
  
  for(i in 1:dim(repo)[1]) {
    sample_meta <- metadata_init_quick(repo$sample_id[i], repo$sample_path[i])
    sample_meta$dmp_id = ifelse('dmp_id' %in% names(repo), repo$dmp_id[i] , NA)
    metadata <- rbind(metadata, as.data.frame.list(sample_meta, stringsAsFactors = F))  
    progress$inc(1/length(repo), detail = paste(" ", i, "/", length(repo)))
  }
  metadata
}

#' helper function for app
#'
#' @param sample_id sample_id name
#' @param sample_path path to facets run dir
#' @return minimal description of the facets run
#' @export metadata_init_quick
metadata_init_quick <- function(sample_id, sample_path) {
  run_dir_exists = "No"
  if ( dir.exists(sample_path)) {
    run_dir_exists = "Yes"
  }
  facets_run_dirs = list.dirs(sample_path, full.names=FALSE)
  facets_run_dirs <- facets_run_dirs[grep("^facets|^default$|^refit_|^alt_diplogR", facets_run_dirs, ignore.case = T)]

  review_file = paste0(sample_path, "/facets_review.manifest")

  num_fits = ''
  default_fit_name = ''
  default_fit_qc = ''
  review_status = 'Not reviewed'
  reviewed_fit_name = ''
  reviewed_fit_facets_qc = F
  reviewed_fit_use_purity = F
  reviewed_fit_use_edited_cncf = F
  reviewer_set_purity = NA
  reviewed_date = NA
  facets_qc_version = 'unknown'
  facets_suite_version = 'unknown'
  
  reviews <- load_reviews(sample_id, sample_path)
  if ( nrow(reviews) > 0 ){
    num_fits = nrow(reviews %>% filter(!grepl('Not selected', fit_name)) %>% select(fit_name) %>% unique)

    default_fit_name = 'default'
    if (!(any(default_fit_name %in% reviews$fit_name))) {
      default_fit_name =
        ((reviews %>% 
        filter(!grepl('^facets_refit|^refit_|^alt_diplogR|Not sel', fit_name, ignore.case = T)))$fit_name %>% 
        unique)[1]
      
      ## if default_fit_name is still not find, just pick any
      if (is.na(default_fit_name)) {
        default_fit_name = reviews$fit_name[1]
      }
    }
    
    if (any(default_fit_name %in% reviews$fit_name)) {
      default_fit_qc = (reviews %>% filter(fit_name == default_fit_name) %>% arrange(desc(date_reviewed)))$facets_qc[1]
    }
    
    reviews = (reviews %>% filter(review_status != 'not_reviewed') %>% arrange(desc(date_reviewed)))
    
    if (nrow(reviews) > 0) {
      review_status = reviews$review_status[1]
      reviewed_fit_name = reviews$fit_name[1]
      reviewed_fit_facets_qc = as.logical(reviews$facets_qc[1])
      reviewed_fit_use_purity = as.logical(reviews$use_only_purity[1])
      reviewed_fit_use_edited_cncf = as.logical(reviews$use_edited_cncf[1])
      reviewer_set_purity = reviews$reviewer_set_purity[1]
      reviewed_date = reviews$date_reviewed[1]
      facets_qc_version = reviews$facets_qc_version[1]
      facets_suite_version = reviews$facets_suite_version[1]
    }
  }

  return (list('sample_id' = sample_id,
            'path' = sample_path,
            'num_fits' = num_fits, 
            'default_fit_name' = default_fit_name,
            'default_fit_qc' = default_fit_qc,
            'review_status' = review_status, 
            'reviewed_fit_name' = reviewed_fit_name, 
            'reviewed_fit_facets_qc' = reviewed_fit_facets_qc,
            'reviewed_fit_use_purity' = reviewed_fit_use_purity, 
            'reviewed_fit_use_edited_cncf' = reviewed_fit_use_edited_cncf, 
            'reviewer_set_purity' = reviewer_set_purity,
            'reviewed_date' = reviewed_date,
            #'reviewed_date' = as.POSIXlt(reviewed_date, tz = Sys.timezone()),
            'facets_qc_version' = facets_qc_version,
            'facets_suite_version' = facets_suite_version))
}

#' helper function for app
#'
#' @param sample_id sample_id name
#' @param sample_path path to facets run dir
#' @param progress progress bar from shiny
#' @return description of the facets run
#' @export metadata_init
metadata_init <- function(sample_id, sample_path, progress = NULL, update_qc_file = TRUE) {
  facets_runs <- get_new_facets_runs_df()
  facets_run_dirs = list.dirs(sample_path, full.names=FALSE)
  
  ## identify different fits generated for this sample.
  facets_run_dirs <- facets_run_dirs[grep("^facets|^default$|^refit_|^alt_diplogR", facets_run_dirs, ignore.case = T)]

  ### for each run directory, load metadata.
  for(fi in 1:length(facets_run_dirs)) {
    if (!is.null(progress)) {
      progress$inc(1/length(facets_run_dirs), detail = paste(" ", fi, "/", length(facets_run_dirs)))
    }
    fit_name = facets_run_dirs[fi]
    facets_run = paste(sample_path, "/", fit_name, sep="")
    facets_run_files = list.files(facets_run, pattern=".out$")
    
    if ( length(facets_run_files) == 0 ) { next }
    
    rm(list = ls()[grep("purity_", ls())]) # remove all previous facets_params
    rm(list = ls()[grep("hisens_", ls())])
    for( fif in 1:length(facets_run_files) ) {
      
      facets_out_params = readLines(paste0(facets_run, "/", facets_run_files[fif]))
      run_type = "hisens_"
      if (grepl("_purity", facets_out_params[2])) {
        run_type = "purity_"
      }

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
      
      ### for purity runs, run QC. (here we are checking for "not hisens" because some 
      ### runs may not have _purity or _hisens suffix)
      rdata_file = paste0(run_prefix, ".Rdata")
      if ( (length(facets_run_files) == 1) || (!grepl('hisens', run_type) & file.exists(rdata_file))) {
        facets_output = facetsSuite::load_facets_output(rdata_file)

        if (!is.null(facets_output$alBalLogR)) {
          assign(paste0(run_type, "alBalLogR"), 
                 paste(round(facets_output$alBalLogR[,1],digits = 2), 
                       collapse=", "))
        }
        fit_qc = facets_fit_qc(facets_output)
      }
    }

    facets_runs <- rbind(facets_runs,
                         cbind(
                           data.frame(tumor_sample_id = sample_id, path = sample_path, fit_name = fit_name,
                                      purity_run_version = get0("purity_Facetsversion", ifnotfound = NA),
                                      purity_run_prefix = get0("purity_prefix", ifnotfound = NA),
                                      purity_run_Seed = get0("purity_Seed", ifnotfound = NA),
                                      purity_run_cval = get0("hisens_purity_cval", ifnotfound = NA),
                                      purity_run_nhet = get0("purity_min.nhet", ifnotfound = NA),
                                      purity_run_snp_nbhd = get0("purity_snp.nbhd", ifnotfound = NA),
                                      purity_run_ndepth = get0("purity_ndepth", ifnotfound = NA),
                                      purity_run_Purity = round_down(get0("purity_Purity", ifnotfound = NA)),
                                      purity_run_Ploidy = round_down(get0("purity_Ploidy", ifnotfound = NA)),
                                      purity_run_dipLogR = round_down(get0("purity_dipLogR", ifnotfound = NA)),
                                      purity_run_alBalLogR = get0("purity_alBalLogR", ifnotfound = NA),
    
                                      hisens_run_version = get0("hisens_Facetsversion", ifnotfound = NA),
                                      hisens_run_prefix = get0("hisens_prefix", ifnotfound = NA),
                                      hisens_run_Seed = get0("hisens_Seed", ifnotfound = NA),
                                      hisens_run_cval = get0("hisens_cval", ifnotfound = NA),
                                      hisens_run_nhet = get0("hisens_min.nhet", ifnotfound = NA),
                                      hisens_run_snp_nbhd = get0("hisens_snp.nbhd", ifnotfound = NA),
                                      hisens_run_ndepth = get0("hisens_ndepth", ifnotfound = NA),
                                      hisens_run_hisens = round_down(get0("hisens_hisens", ifnotfound = NA)),
                                      hisens_run_Purity = round_down(get0("hisens_Purity", ifnotfound = NA)),
                                      hisens_run_Ploidy = round_down(get0("hisens_Ploidy", ifnotfound = NA)),
                                      hisens_run_dipLogR = round_down(get0("hisens_dipLogR", ifnotfound = NA)),
    
                                      manual_note = NA,
                                      is_best_fit = NA,
                                      stringsAsFactors=FALSE),
                           fit_qc))
  }
  
  if (nrow(facets_runs) == 0) {
    return(NULL)
  }
  
  # load reviews from the manifest file and annotate each review with the facets QC status.
  # Here is where we make sure the facets_review.manifest is forward-compatible - transformed with 
  # new columns 
  existing_reviews <- get_review_status(sample_id, sample_path)
  
  fit_qc <- 
    facets_runs %>% 
    mutate(facets_qc_version = as.character(facets_qc_version),
           facets_suite_version = as.character(facets_suite_version)) %>%
    select(sample = tumor_sample_id, fit_name, 
           facets_suite_version, facets_qc_version, facets_qc)
  
  reviews <-
    rbind(fit_qc, fit_qc %>% mutate(fit_name = "Not selected", facets_qc = F) %>% unique) %>%
    left_join(existing_reviews %>% 
                filter(!is.na(date_reviewed)) %>%
                select(-facets_qc, -facets_suite_version)) %>% 
    mutate(path = sample_path,
           review_status = ifelse(is.na(review_status), 'not_reviewed', review_status),
           ### Note: do no do this. because it will re-order the best_reviewed dates
           ### date_reviewed = ifelse(is.na(date_reviewed), as.character(Sys.time()), date_reviewed)
           ) %>%
    select(sample, path, review_status, fit_name, review_notes, reviewed_by, 
           date_reviewed, facets_qc, use_only_purity_run, use_edited_cncf, reviewer_set_purity,
           facets_qc_version, facets_suite_version) 

  reviews <-
    rbind(existing_reviews %>% 
            filter(!(fit_name == 'Not selected' | facets_qc_version == facets_qc_version())),
          reviews)
  
  ### determine if the sample has at least an acceptable_fit; get the most recent review 
  ### and determine if the status is 'reviewed_best_fit' or 'reviewed_acceptable_fit'
  best_fit = (reviews %>% 
                arrange(desc(date_reviewed)) %>%
                filter(review_status %in% c('reviewed_acceptable_fit', 
                                            'reviewed_best_fit'))
              )$fit_name[1]
  
  facets_runs$is_best_fit = F
  facets_runs$is_best_fit[which(facets_runs$fit_name == best_fit)] = T
  
  if (update_qc_file) {
    if (verify_access_to_write(sample_path)) {
      write.table(facets_runs %>% select(-ends_with("_filter_note")), 
                  file=paste0(sample_path, '/facets_qc.txt'), quote=F, row.names=F, sep='\t')
    } else {
      warning(paste0('You do not have write permissions to update ', sample_path, '/facets_qc.txt file'))
    }
  }
  
  if (update_qc_file) {
    update_review_status_file(sample_path, reviews, T)
    
    update_best_fit_status(sample_id, sample_path)
  }
  facets_runs
}

#' Loads reviews from facets_review.manifest file.
#' - If .manifest does not exist, then create generate facets QC calls and generate a new one.
#' - If .manifest exists and is old-format, then, generate facets QC calls and merge reviews
#' - If .manifest exists and has all the necessary columns, then just read it in.
#'
#' @param manifest list of facets run directories
#' @param progress progress bar from shiny
#' @return simple metadata data.frame
#' @import dplyr
#' @export load_reviews
load_reviews <- function(sample_id, sample_path) {
  review_file = paste0(sample_path, "/facets_review.manifest")
  
  reviews = get_review_status(sample_id, sample_path)

  if (nrow(reviews) == 0 || !('facets_qc' %in% names(reviews)) || length(which(is.na(reviews$facets_qc))) > 0) {
    metadata_init(sample_id, sample_path)
    return(get_review_status(sample_id, sample_path))
  } 
  return(reviews)
}

#' helper function for app
#'
#' @param sample sampleid
#' @param sample_path facets run directory containing 'facets_review.manifest'
#' @return converts string to numeric and rounds to 2-digits
#' @export get_review_status
get_review_status <- function(sample_id, sample_path) {
  review_file = paste0(sample_path, "/facets_review.manifest") 
  if ( !file.exists( review_file ) || file.size(review_file) == 0 || countLines(review_file) < 2 ) {
    df <- data.frame(
      sample = character(),
      path = character(),
      review_status = character(),
      fit_name = character(),
      review_notes = character(),
      reviewed_by = character(),
      date_reviewed = as.POSIXlt(character()),
      use_only_purity_run = character(),
      use_edited_cncf = character(),
      facets_qc = character(),
      facets_qc_version = character(),
      facets_suite_version = character(),
      reviewer_set_purity = character(),
      stringsAsFactors=FALSE
    )
    return(df)
  }
  reviews <-
    suppressWarnings(fread(review_file, colClasses=list(character="facets_qc_version", 
                                       character="facets_suite_version"), verbose = F, skip = 1)) %>%
    rename_all(recode, 'best_fit' = 'fit_name') 
  
  ### backwards compatibility;
  {
    if (!('use_only_purity_run' %in% names(reviews))) { reviews$use_only_purity_run = FALSE }
    
    if (!('use_edited_cncf' %in% names(reviews))) { reviews$use_edited_cncf = FALSE }
    
    if (!('reviewer_set_purity' %in% names(reviews))) { reviews$reviewer_set_purity = NA }
    
    if ('facets_suite_qc' %in% names(reviews)) { reviews <- reviews %>% rename(facets_qc = facets_suite_qc)}
    
    if(!('facets_qc' %in% names(reviews))) { reviews <- reviews %>% mutate(facets_qc = NA)}
    
    if (!('facets_qc_version' %in% names(reviews))) { reviews <- reviews %>% mutate(facets_qc_version = 'unknown') }
    
    if (!('facets_suite_version' %in% names(reviews))) { reviews <- reviews %>% mutate(facets_suite_version = 'unknown') }
  }
  
  return (reviews %>% arrange(desc(date_reviewed)))
}

#' @param sample sampleid
#' @param sample_path facets run directory containing 'facets_review.manifest'
#' @return converts string to numeric and rounds to 2-digits
#' @export update_best_fit_status
update_best_fit_status <- function(sample_id, sample_path) {

  reviews <-
    get_review_status(sample_id, sample_path) %>% 
    filter(!(fit_name == 'Not selected'))
  
  ### determine if the sample has at least an acceptable_fit; get the most recent review 
  ### and determine if the status is 'reviewed_best_fit' or 'reviewed_acceptable_fit'
  best_fit = (reviews %>% 
                arrange(desc(date_reviewed)) %>%
                filter(review_status %in% c('reviewed_acceptable_fit', 
                                            'reviewed_best_fit')))$fit_name[1]
  
  facets_runs <- fread(paste0(sample_path, '/facets_qc.txt'))
  facets_runs$is_best_fit = F
  facets_runs$is_best_fit[which(facets_runs$fit_name == best_fit)] = T
  
  if (verify_access_to_write(sample_path)) {
    write.table(facets_runs %>% select(-ends_with("_filter_note")), 
                file=paste0(sample_path, '/facets_qc.txt'), quote=F, row.names=F, sep='\t')
  } else {
    warning(paste0('You do not have write permissions to update ', sample_path, '/facets_qc.txt file'))
  }
}

#' helper function for app
#'
#' @param path  path to verify permissions
#' @return True/False
#' @export has_permissions_to_write
has_permissions_to_write <- function(path) {
  random_file = paste0(path, as.character(as.numeric(now()) * (1e10 * runif(1,0,1))))
  has_permission = !system(paste0('touch ', random_file), ignore.stderr = T)
  if (has_permission) {
    system(paste0('rm -f ', random_file))
  }
  return(has_permission)
}

#' helper function for app
#'
#' @param sample_path  facets run directory containing 'facets_review.manifest'
#' @return True/False
#' @export verify_access_to_write
verify_access_to_write <- function(sample_path) {
  review_file = paste0(sample_path, '/facets_review.manifest')
  qc_file = paste0(sample_path, '/facets_qc.txt')
  
  can_edit_review = F
  if (file.exists(review_file)) {
    #can_edit_review = !system(paste0('touch -a -r ', review_file, ' ', review_file), ignore.stderr = T)
    can_edit_review = !system(paste0('(mv ', review_file, ' ', review_file, '. ; ', 
                                     ' mv ', review_file, '. ', review_file, ') 2> /dev/null'), ignore.stderr = T)
  } else {
    can_edit_review = has_permissions_to_write(sample_path)
  }
  
  can_edit_qc = F
  if (file.exists(qc_file)) {
    #can_edit_qc = !system(paste0('touch -c -a -r ', qc_file, ' ', qc_file), ignore.stderr = T)
    can_edit_qc = !system(paste0('(mv ', qc_file, ' ', qc_file, '. ; ', 
                                 ' mv ', qc_file, '. ', qc_file, ') 2> /dev/null'), ignore.stderr = T)
  } else {
    can_edit_qc = has_permissions_to_write(sample_path)
  }
  
  return (can_edit_review & can_edit_qc)
}

#' helper function for app
#'
#' @param sample_path  facets run directory containing 'facets_review.manifest'
#' @param df dataframe
#' @return converts string to numeric and rounds to 2-digits
#' @export update_review_status_file
update_review_status_file <- function(sample_path, df, overwrite=F) {
  if (is.null(df) || dim(df)[1] == 0) {
    return (FALSE)
  }
  
  if (!verify_access_to_write(sample_path)) {
    warning('You do not have write permissions to update review status file')
    return(FALSE)
  }
  
  review_file = paste0(sample_path, "/facets_review.manifest")
  if ( !file.exists(review_file) | overwrite) {
    con = file(review_file)
    writeLines("# generated by facets-preview app. DO NOT EDIT.", con)
    close(con)
    suppressWarnings(write.table(df %>% unique, review_file, append=TRUE, sep="\t", row.names=F, quote=F))
  }
  else {
    write.table(df %>% unique, review_file, append=TRUE, sep="\t", row.names=F, quote=F, col.names = F)
  }
  return (TRUE)
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

#'
#'
#' @return
#' @export get_cncf_table
get_cncf_table <- function(fit_type, selected_run) {
  if (fit_type == "Hisens") {
    run_prefix = selected_run$hisens_run_prefix[1]
  } else {
    run_prefix = selected_run$purity_run_prefix[1]
  }
  if ( file.exists(paste0(run_prefix, ".cncf.edited.txt"))) {
    cncf_filename = paste0(run_prefix, ".cncf.edited.txt")
  } else {
    cncf_filename = paste0(run_prefix, ".cncf.txt")
  }

  cncf_data <- data.table::fread(cncf_filename)

  if ( !("cf" %in% names(cncf_data)) & !("cf.em" %in% names(cncf_data)) ) {
    return(data.table())
  }
  if (!("cf" %in% names(cncf_data))) {
    cncf_data[, cf := cf.em]
    cncf_data[, tcn := tcn.em]
    cncf_data[, lcn := lcn.em]
  }

  cncf_data <-
    cncf_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(cnlr.median = round_down(cnlr.median),
           mafR = round_down(mafR),
           cf = round_down(cf),
           cf.em = round_down(cf.em)) %>%
    dplyr::select(-ID, -cnlr.median.clust, -mafR.clust, -segclust)

  return(cncf_data)
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

require(bit64)
require(Cairo)
require(ggplot2)
require(grid)
require(gridExtra)
require(plyr)
require(data.table)

#' helper function for app
#'
#' @param out
#' @param fit
#' @return simple metadata data.frame
#' @import bit64
#' @import Cairo
#' @import ggplot2
#' @import grid
#' @import gridExtra
#' @import plyr
#' @import data.table
#' @import magrittr
#' @export copy.number.log.ratio
copy.number.log.ratio = function(out, fit, load.genome=FALSE, gene.pos=NULL, col.1="#0080FF", col.2="#4CC4FF", sample.num=NULL, lend='butt', theme='bw', subset.indices=NULL){

  mat = out$jointseg
  mat = subset(mat, chrom < 23)
  mat = get.cumulative.chr.maploc(mat, load.genome)
  mid = mat$mid
  mat = mat$mat

  cncf = fit$cncf
  cncf = subset(cncf, chrom < 23)
  dipLogR = out$dipLogR

  cnlr.median = rep(cncf$cnlr.median, cncf$num.mark)
  mat = cbind(mat, cnlr.median)

  starts = cumsum(c(1,cncf$num.mark))[1:length(cncf$num.mark)]
  ends = cumsum(c(cncf$num.mark))
  my.starts = mat[starts,c('chr.maploc','cnlr.median')]
  my.ends = mat[ends,c('chr.maploc','cnlr.median')]

  if(is.null(sample.num)){subset_ = 1:nrow(mat)}

  if(is.null(sample.num) == FALSE){
    if(sample.num >= nrow(mat)){subset_ = 1:nrow(mat)}
    if(sample.num < nrow(mat)){subset_ = sort(sample(1:nrow(mat), sample.num, replace=FALSE))}
  }

  if (!is.null(subset.indices)) {
    mat = mat[subset.indices,]
  } else { mat = mat[subset_,] }

  col.rep = 1 + rep(mat$chrom - 2 * floor(mat$chrom/2))
  pt.cols = c(col.1, col.2)[col.rep]

  ymin = floor(min(range(cncf$cnlr.median), na.rm = T))
  if (ymin > -3) ymin = -3

  cnlr = ggplot(mat, environment = environment())
  if(!is.null(gene.pos)){
    cnlr = cnlr + geom_vline(xintercept=gene.pos$mid, color='palevioletred1')
    mat$gene = FALSE
    mat$gene[which(mat$chrom == gene.pos$chrom & mat$maploc >= gene.pos$start & mat$maploc <= gene.pos$end)] = TRUE
  } else { mat$gene = FALSE }

  cnlr = cnlr +
    #geom_point(aes(y=cnlr,x=chr.maploc), colour=pt.cols, size=.4) +
    geom_point(aes(y=cnlr,x=chr.maploc), pch = 19, col=pt.cols, size=.4) +
    geom_point(data = subset(mat, gene==T), aes(y=cnlr,x=chr.maploc), color='#525252', size=.4) +
    scale_x_continuous(breaks=mid, labels=names(mid)) +
    xlab('') +
    scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(ymin ,3)) +
    ylab('Log Ratio') +
    geom_hline(yintercept = dipLogR, color = 'sandybrown', size = .8) +
    geom_segment(data=cncf,aes(x=my.starts$chr.maploc, xend=my.ends$chr.maploc, y=my.starts$cnlr.median, yend=my.ends$cnlr.median), col='red3', size=1, lineend=lend)

  panel.grid.col='white'; grid.width = .5
  if(theme=='bw'){panel.grid.col='grey'; grid.width = .2; cnlr = cnlr + theme_bw()}

  cnlr = cnlr + theme(axis.text.x  = element_text(angle=0, size=8),
                      axis.text.y = element_text(angle=0, size=8),
                      text = element_text(size=10),
                      panel.grid.minor.x=element_line(colour=panel.grid.col, size=grid.width),
                      panel.grid.major.x=element_line(colour=panel.grid.col, size=0),
                      plot.margin = unit(c(0,1,0,0), 'lines'))
  cnlr
}

#' helper function for app
#'
#' @param out
#' @return simple metadata data.frame
#' @import dplyr
#' @export var.allele.log.odds.ratio
var.allele.log.odds.ratio = function(out, fit, load.genome=FALSE, gene.pos=NULL, col.1="#0080FF", col.2="#4CC4FF", sample.num=NULL, lend='butt', theme='bw', subset.indices=NULL){

  mat = out$jointseg
  mat = subset(mat, chrom < 23)
  mat = get.cumulative.chr.maploc(mat, load.genome)
  mid = mat$mid
  mat = mat$mat

  cncf = fit$cncf
  cncf = subset(cncf, chrom < 23)

  mafR = rep(sqrt(abs(cncf$mafR)), cncf$num.mark)
  mat = cbind(mat, mafR)

  starts = cumsum(c(1,cncf$num.mark))[1:length(cncf$num.mark)]
  ends = cumsum(c(cncf$num.mark))
  my.starts = mat[starts,c('chr.maploc','mafR')]
  my.ends = mat[ends,c('chr.maploc','mafR')]

  if(is.null(sample.num)){subset_ = 1:nrow(mat)}

  if(is.null(sample.num) == FALSE){
    if(sample.num >= nrow(mat)){subset_ = 1:nrow(mat)}
    if(sample.num < nrow(mat)){subset_ = sort(sample(1:nrow(mat), sample.num, replace=FALSE))}
  }

  if (!is.null(subset.indices)) {
    mat = mat[subset.indices,]
  } else { mat = mat[subset_,] }

  col.rep = 1 + rep(mat$chrom - 2 * floor(mat$chrom/2))
  pt.cols = c(col.1, col.2)[col.rep]

  valor = ggplot(mat, environment = environment())
  if(!is.null(gene.pos)){
    valor = valor + geom_vline(xintercept=gene.pos$mid, color='palevioletred1')
    mat$gene = FALSE
    mat$gene[which(mat$chrom == gene.pos$chrom & mat$maploc >= gene.pos$start & mat$maploc <= gene.pos$end)] = TRUE
  } else { mat$gene = FALSE }

  valor = valor +
    geom_point(aes(y=valor,x=chr.maploc), colour=pt.cols, size=.4) +
    geom_point(data = subset(mat, gene==T), aes(y=valor,x=chr.maploc), color='#525252', size=.4) +
    scale_x_continuous(breaks=mid, labels=names(mid)) +
    xlab('') +
    ylim(-4,4) +
    ylab('Log Odds Ratio') +
    geom_segment(data=cncf, aes(x=my.starts$chr.maploc, xend=my.ends$chr.maploc, yend=my.ends$mafR, y=my.starts$mafR), col='red3', size=1, lineend=lend) +
    geom_segment(data=cncf, aes(x=my.starts$chr.maploc, xend=my.ends$chr.maploc, yend=-my.ends$mafR, y=-my.starts$mafR), col='red3', size=1, lineend=lend)

  panel.grid.col='white'; grid.width = .5
  if(theme=='bw'){panel.grid.col='grey'; grid.width = .2; valor = valor + theme_bw()}

  valor = valor + theme(axis.text.x = element_text(angle=0, size=8),
                        axis.text.y = element_text(angle=0, size=8),
                        text = element_text(size=10),
                        panel.grid.minor.x=element_line(colour=panel.grid.col, size=grid.width),
                        panel.grid.major.x=element_line(colour=panel.grid.col, size=0),
                        plot.margin = unit(c(0,1,0,0), 'lines'))
  valor
}

#' helper function for app
#'
#' @param out
#' @return simple metadata data.frame
#' @import dplyr
#' @export cellular.fraction
cellular.fraction = function(out, fit, method=c('cncf', 'em'), load.genome=FALSE, gene.pos=NULL, main='', lend='butt', theme='bw', ...){

  mat = out$jointseg
  mat = subset(mat, chrom < 23)
  mat = get.cumulative.chr.maploc(mat, load.genome)
  mid = mat$mid
  mat = mat$mat

  cncf = fit$cncf
  cncf = subset(cncf, chrom < 23)

  if(method == 'em'){cncf$cf.em[is.na(cncf$cf.em)] = -1; cf = rep(cncf$cf.em, cncf$num.mark); my.ylab='EM - CF'}
  if(method == 'cncf'){cncf$cf[is.na(cncf$cf)] = -1; cf = rep(cncf$cf, cncf$num.mark); my.ylab='CNCF - CF'}

  mat = cbind(mat, cf)
  starts = cumsum(c(1,cncf$num.mark))[1:length(cncf$num.mark)]
  ends = cumsum(c(cncf$num.mark))
  my.starts = mat[starts,c('chr.maploc','cf')]
  my.ends = mat[ends,c('chr.maploc','cf')]

  cf = ggplot(mat, environment = environment())
  if(!is.null(gene.pos)){
    cf = cf + geom_vline(xintercept=gene.pos$mid, color='palevioletred1')
  }

  cf = cf +
    geom_segment(data=cncf, aes(x=my.starts$chr.maploc, xend=my.ends$chr.maploc, yend=my.ends$cf, y=my.starts$cf), col='black', size=1, lineend=lend) +
    scale_x_continuous(breaks=mid, labels=names(mid)) +
    xlab('') +
    ylim(0,1) +
    ylab(my.ylab)

  panel.grid.col='white'; grid.width = .5
  if(theme=='bw'){panel.grid.col='grey'; grid.width = .2; cf = cf + theme_bw()}

  cf = cf + theme(axis.text.x  = element_text(angle=90, vjust=0, size=8),
                  axis.text.y = element_text(angle=90, vjust=0, size=8),
                  text = element_text(size=10),
                  panel.grid.minor.x=element_line(colour=panel.grid.col, size=grid.width),
                  panel.grid.major.x=element_line(colour=panel.grid.col, size=0),
                  plot.margin = unit(c(0,1,0,0), 'lines'))
  cf
}

#' helper function for app
#'
#' @param out
#' @return simple metadata data.frame
#' @import dplyr
#' @export integer.copy.number
integer.copy.number = function(out, fit, method=c('cncf', 'em'), load.genome=FALSE, gene.pos=NULL, main='', lend='butt', theme='bw', ...){

  mat = out$jointseg
  mat = subset(mat, chrom < 23)
  mat = get.cumulative.chr.maploc(mat, load.genome)
  mid = mat$mid
  mat = mat$mat

  cncf = fit$cncf
  cncf = subset(cncf, chrom < 23)

  if(method == 'em'){tcnscaled = cncf$tcn.em; tcnscaled[cncf$tcn.em > 5 & !is.na(cncf$tcn.em)] = (5 + (tcnscaled[cncf$tcn.em > 5 & !is.na(cncf$tcn.em)] - 5)/3)}
  if(method == 'cncf'){tcnscaled = cncf$tcn; tcnscaled[cncf$tcn > 5 & !is.na(cncf$tcn)] = (5 + (tcnscaled[cncf$tcn > 5 & !is.na(cncf$tcn)] - 5)/3)}
  tcn_ = rep(tcnscaled, cncf$num.mark)

  if(method == 'em'){lcn_ = rep(cncf$lcn.em, cncf$num.mark); my.ylab='Integer CN (EM)'}
  if(method == 'cncf'){lcn_ = rep(cncf$lcn, cncf$num.mark); my.ylab='Integer CN (CNCF)'}

  mat = cbind(mat, cbind(tcn_, lcn_))
  starts = cumsum(c(1,cncf$num.mark))[1:length(cncf$num.mark)]
  ends = cumsum(c(cncf$num.mark))
  my.tcn.starts = mat[starts,c('chr.maploc','tcn_')]
  my.tcn.ends = mat[ends,c('chr.maploc','tcn_')]
  my.lcn.starts = mat[starts,c('chr.maploc','lcn_')]
  my.lcn.ends = mat[ends,c('chr.maploc','lcn_')]

  icn = ggplot(mat, environment = environment())
  if(!is.null(gene.pos)){
    icn = icn + geom_vline(xintercept=gene.pos$mid, color='palevioletred1')
  }

  icn = icn +
    geom_segment(data=cncf, aes(x=my.lcn.starts$chr.maploc, xend=my.lcn.ends$chr.maploc, y=my.lcn.starts$lcn_, yend=my.lcn.ends$lcn_), col='red', size=1, lineend=lend) +
    geom_segment(data=cncf, aes(x=my.tcn.starts$chr.maploc, xend=my.tcn.ends$chr.maploc, y=my.tcn.starts$tcn_, yend=my.tcn.ends$tcn_), col='black', size=1,lineend=lend) +
    scale_y_continuous(breaks=c(0:5, 5 + (1:35)/3), labels=0:40,limits = c(0, NA)) +
    scale_x_continuous(breaks=mid, labels=names(mid)) +
    ylab(my.ylab) +
    xlab('')

  panel.grid.col='white'; grid.width = .5
  if(theme=='bw'){panel.grid.col='grey'; grid.width = .2; icn = icn + theme_bw()}

  icn = icn + theme(axis.text.x  = element_text(angle=0, size=8),
                    axis.text.y = element_text(angle=0, size=8),
                    text = element_text(size=10),
                    panel.grid.minor.x=element_line(colour=panel.grid.col, size=grid.width),
                    panel.grid.major.x=element_line(colour=panel.grid.col, size=0),
                    plot.margin = unit(c(0,1,0,0), 'lines'))

  icn
}

#' helper function for app
#'
#' @param out
#' @return simple metadata data.frame
#' @import dplyr
#' @export get.cumulative.chr.maploc
get.cumulative.chr.maploc = function(mat, load.genome=FALSE){

  chrom.lengths = c(249250621, 243199373, 198022430, 191154276, 180915260, 171115067, 159138663, 146364022, 141213431, 135534747,135006516, 133851895, 115169878, 107349540, 102531392, 90354753,  81195210,  78077248,  59128983,  63025520, 48129895,  51304566) #hg19

  cum.chrom.lengths = cumsum(as.numeric(chrom.lengths))
  mid = cum.chrom.lengths - (chrom.lengths/2)
  names(mid) = 1:22

  chr.maploc.to.gen.maploc = function(x){mat[mat$chrom==x,]$maploc + cum.chrom.lengths[x-1]}
  chr.maploc = sapply(2:22,chr.maploc.to.gen.maploc)
  chr.maploc = unlist(chr.maploc)
  chr.maploc = c(mat[mat$chrom==1,]$maploc,chr.maploc)
  mat = cbind(mat,chr.maploc)

  list(mat=mat, mid=mid)
}

#' helper function for app
#'
#' @return simple metadata data.frame
#' @import dplyr
#' @export get.gene.pos.cached
get.gene.pos.cached <- function(hugo.symbol, 
                                my.path=NULL) {
  data.table::fread(my.path) %>%
    data.table %>%
    dplyr::filter(gene == hugo.symbol)
}

#' helper function for app
#'
#' @param out
#' @return simple metadata data.frame
#' @import dplyr
#' @export close.up
close.up = function(out, fit, chrom.range=NULL, method=NA, gene.name=NULL, lend='butt', bed.path=NULL, cached.gene.path = NULL, subset.snps=FALSE, ...){
  if (!is.null(cached.gene.path)) {gene.info = get.gene.pos.cached(gene.name, my.path = cached.gene.path)
  } else if (!is.null(bed.path)) { gene.info = get.gene.pos(gene.name, my.path = bed.path)
  } else { gene.info = get.gene.pos(gene.name) }

  if (!is.null(gene.name)) gene.pos = gene.info
  if (is.null(gene.name)) gene.pos = NULL
  if (is.null(chrom.range)) chrom.range = min(gene.info$chrom):max(gene.info$chrom)

  out$out = out$out[out$out$chrom %in% chrom.range,]
  out$jointseg = out$jointseg[out$jointseg$chrom %in% chrom.range,]
  out$IGV = out$IGV[out$IGV$chrom %in% chrom.range,]
  fit$cncf = fit$cncf[fit$cncf$chrom %in% chrom.range,]

  subset.indices = NULL

  cnlr = copy.number.log.ratio(out, fit, gene.pos=gene.pos, lend=lend, subset.indices=subset.indices, ...)
  valor = var.allele.log.odds.ratio(out, fit, gene.pos=gene.pos, lend=lend, subset.indices=subset.indices, ...)

  output_list <- list(cnlr=cnlr,valor=valor)
  if(method == 'em' | is.na(method)){
    cfem = cellular.fraction(out, fit, method='em', gene.pos=gene.pos, lend=lend, ...)
    icnem = integer.copy.number(out, fit, method='em', gene.pos=gene.pos, lend=lend, ...)
    output_list <- c(output_list, list(cfem=cfem, icnem=icnem))
  }
  if(method == 'cncf' | is.na(method)){
    cfcncf = cellular.fraction(out, fit, method='cncf', gene.pos=gene.pos, lend=lend, ...)
    icncncf = integer.copy.number(out, fit, method='cncf', gene.pos=gene.pos, lend=lend, ...)
    output_list <- c(output_list, list(cfcncf=cfcncf, icncncf=icncncf))
  }
  c(output_list, list(chrom=gene.info$chrom, start=gene.info$start, end=gene.info$end))
}
