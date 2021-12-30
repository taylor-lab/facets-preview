#' Compile cohort annotations for given set of facets runs
#'
#' Compiles cohort-wide cohort annotations.
#'
#' @param samples_to_annotate - data.table with two required columns: sample_id (eg: P-0012345-T01-IM5_P-0012345-N01-IM5), sample_path (eg: /<path_to_facets_run_for_P-0012345-T01-IM5_P-0012345-N01-IM5) and one optional column: fit_to_use (eg: refit_c50_pc100_diplogR_-0.13 or simply NA)
#' @param output_prefix - prefix to which .gene_level.txt, .arm_level.txt and .ccf.maf are writted to
#'
#' @return samples_annotated table containing the facets_suite.qc.txt output for the selected fits used to compile the calls
#' \itemize{
#'   \item{\code{arm_level_file}:} {arm_level file used}
#'   \item{\code{gene_level_file}:} {gene_level file used}
#'   \item{\code{ccf_file}:} {ccf file used}
#' }
#'  
#' @export compile_cohort_annotations
compile_cohort_annotations <- function(samples_to_annotate, output_prefix, ncores=1) {
  
  parallelize = F
  if (ncores > 1) { 
    library(doParallel) 
    doParallel::registerDoParallel(cores = ncores)
    parallelize = T
  }
  
  if (!("fit_to_use" %in% colnames(samples_to_annotate))) {
    samples_to_annotate$fit_to_use = NA
  }
  
  samples_annotated <-
    samples_to_annotate %>%
    select(sample_id, sample_path, fit_to_use) %>%
    left_join(
      adply(samples_to_annotate, 1,
          function(x) {
            sample_id = x$sample_id
            sample_path = x$sample_path
            fit_to_use = x$fit_to_use
            
            if (!is.na(fit_to_use)) {
              return(fit_to_use)
            }
            
            ## read from facets_qc.txt
            qc_file = paste0(sample_path, '/facets_qc.txt')
            review_file = paste0(sample_path, '/facets_review.manifest')
            
            if (!file.exists(qc_file)| !file.exists(review_file)) { return() }
            
            qc_runs = fread(qc_file) %>% filter(fit_name != 'Not selected')
            
            if (nrow(qc_runs) == 0) { return() }
            
            fit = ''          
            if(any(qc_runs$is_best_fit)) {
              fit = qc_runs %>% filter(is_best_fit) %>% head(n=1)
            } else if ("default" %in% qc_runs$fit_name) {
              fit = qc_runs %>% filter(fit_name == 'default') %>% head(n=1)
            } else {
              fit = qc_runs %>% head(n=1)
            }
            
            reviews <-
              fread(review_file) %>%
              filter(fit_name == fit$fit_name) %>%
              arrange(desc(date_reviewed)) %>%
              select(review_status, fit_name, review_notes, reviewed_by, date_reviewed, use_only_purity_run, use_edited_cncf, reviewer_set_purity) %>%
              head(n=1)
            
            fit <-
              fit %>% 
              left_join(reviews) 
            
            ####
            #### update purity and ploidy based on reviews;
            ####
            fit <- 
              fit %>% 
              rowwise %>%
              mutate(purity = ifelse(!is.na(reviewer_set_purity) & reviewer_set_purity != '', 
                                     reviewer_set_purity,
                                     ifelse(!is.na(use_only_purity_run) & use_only_purity_run, 
                                            purity_run_Purity, 
                                            hisens_run_Purity))) %>%
              mutate(ploidy = ifelse(!is.na(use_only_purity_run) & use_only_purity_run, 
                                     purity_run_Ploidy, 
                                     hisens_run_Ploidy))
            
            return (fit)
          }, .parallel = parallelize)
    )

  samples_annotated <-
    samples_annotated %>%
    rowwise %>%
    mutate(pfx = paste0(sample_path, '/', fit_name, '/', sample_id)) %>% 
    mutate(arm_level_file = paste0(pfx, '.arm_level.txt'),
           gene_level_file = paste0(pfx, '.gene_level.txt'),
           ccf_file = paste0(pfx, '.ccf.maf'),
           ccf_nonsignedout_file = paste0(pfx, '.nonsignedout.ccf.maf'),
           cncf_file = ifelse(!is.na(use_only_purity_run) & use_only_purity_run,
                              paste0(pfx, '_purity.cncf.txt'), paste0(pfx, '_hisens.cncf.txt')),
           seg_file = ifelse(!is.na(use_only_purity_run) & use_only_purity_run,
                              paste0(pfx, '_purity.seg'), paste0(pfx, '_hisens.seg'))) %>%
    mutate(arm_level_file_exists = file.exists(arm_level_file),
           gene_level_file_exists = file.exists(gene_level_file),
           ccf_file_exists = file.exists(ccf_file),
           ccf_nonsignedout_file_exists = file.exists(ccf_nonsignedout_file),
           cncf_file_exists = file.exists(cncf_file),
           seg_file_exists = file.exists(seg_file))
  
  write.table(samples_annotated, file=paste0(output_prefix, '.cohort.txt'), quote=F, row.names=F, sep='\t')
  cl <- makeCluster(ncores)
  
  ccf_calls = rbindlist(parSapply(cl,
                                  (samples_annotated %>% filter(ccf_file_exists))$ccf_file,
                                  fread, 
                                  simplify = F, 
                                  USE.NAMES=F), 
                        fill = T)
  write.table(ccf_calls, file=paste0(output_prefix, '.ccf.maf'), quote=F, row.names=F, sep='\t')
  
  ccf_nonsignedout_calls = rbindlist(parSapply(cl,
                                  (samples_annotated %>% filter(ccf_nonsignedout_file_exists))$ccf_nonsignedout_file,
                                  fread, 
                                  simplify = F, 
                                  USE.NAMES=F),  
                        fill = T)
  write.table(ccf_nonsignedout_calls, file=paste0(output_prefix, '.nonsignedout.ccf.maf'), quote=F, row.names=F, sep='\t')
  
  arm_level_calls = rbindlist(parSapply(cl,
                                        (samples_annotated %>% filter(arm_level_file_exists))$arm_level_file,
                                        fread, 
                                        simplify = F, 
                                        USE.NAMES=F), 
                              fill = T)
  write.table(arm_level_calls, file=paste0(output_prefix, '.arm_level.txt'), quote=F, row.names=F, sep='\t')
  
  gene_level_calls = rbindlist(parSapply(cl,
                                        (samples_annotated %>% filter(gene_level_file_exists))$gene_level_file,
                                        fread, 
                                        simplify = F, 
                                        USE.NAMES=F), 
                              fill = T)
  write.table(gene_level_calls, file=paste0(output_prefix, '.gene_level.txt'), quote=F, row.names=F, sep='\t')
  
  seg_calls = rbindlist(parSapply(cl,
                                  (samples_annotated %>% filter(seg_file_exists))$seg_file,
                                  fread, 
                                  simplify = F, 
                                  USE.NAMES=F), 
                        fill = T)
  write.table(seg_calls, file=paste0(output_prefix, '.seg'), quote=F, row.names=F, sep='\t')
  
  cncf_calls = rbindlist(parSapply(cl,
                                  (samples_annotated %>% filter(cncf_file_exists))$cncf_file,
                                  fread, 
                                  simplify = F, 
                                  USE.NAMES=F), 
                        fill = T)
  write.table(cncf_calls, file=paste0(output_prefix, '.cncf.txt'), quote=F, row.names=F, sep='\t')
  
  
  return (samples_annotated)
}








