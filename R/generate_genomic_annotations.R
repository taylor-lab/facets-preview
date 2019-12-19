library(purrr)
library(tibble)
library(tidyr)
library(facetsSuite, lib.loc = '/juno/work/ccs/bandlamc/software/R_libs/facetsSuite/2.0.1-beta/')
source("/juno/work/ccs/bandlamc/facets_review_app/facets-preview/R/global.R")
source("/juno/work/ccs/bandlamc/facets_review_app/facets-preview/R/get_impact_qc_for_fit.R")

#' helper function for app
#'
#' @param sample_id sample_path
#' @return simple qc
#' @import dplyr
#' @export get_impact_qc_for_fit
generate_genomic_annotations = function(sample_id, sample_path) {
  
  facets_runs_qc <- metadata_init(sample_id, sample_path) %>% data.table

  reviews <-
    get_review_status(sample_id, sample_path) %>% 
    select(sample_id = sample, path, review_status, fit_name, 
           date_reviewed, use_only_purity_run, use_edited_cncf, reviewer_set_purity) %>%
    filter(fit_name != 'Not selected')
  
  reviews <-
    reviews %>% 
    left_join(reviews %>% 
                group_by(fit_name) %>% 
                summarise(date_r = sort(as.POSIXct(date_reviewed), decreasing = F)[1]) %>%
                mutate(use = 1), 
              by=c("fit_name", "date_reviewed" = "date_r")) %>%
    filter(!is.na(use)) %>%
    select(-use) %>%
    replace_na(list(use_only_purity_run = F, use_edited_cncf = F))
  
  facets_runs_qc <- 
    facets_runs_qc %>% 
    left_join(reviews)

  plyr::adply(facets_runs_qc, 1, function(r) {
    print(paste0('processing ', r$tumor_sample_id, ' fit:', r$fit_name))
    prefix = paste0(r$path, '/', r$fit_name, '/', r$tumor_sample_id)
    purity_rdata_file = paste0(prefix, '_purity.Rdata')
    hisens_rdata_file = paste0(prefix, '_hisens.Rdata')
    
    if (!file.exists(purity_rdata_file) || !file.exists(hisens_rdata_file)) {
      print('_purity or _hisens runs not found. Skipping....')
      return()
    }
    
    purity_output = facetsSuite::load_facets_output(purity_rdata_file)
    hisens_output = facetsSuite::load_facets_output(hisens_rdata_file)
    
    metadata = c(
      map_dfr(list(purity_output, hisens_output), 
              function(x) { facetsSuite::arm_level_changes(x$segs, x$ploidy, 'hg19')[-5] }),
      map_dfr(list(purity_output, hisens_output), 
              function(x) facetsSuite::calculate_lst(x$segs, x$ploidy, 'hg19')),
      map_dfr(list(purity_output, hisens_output), 
              function(x) facetsSuite::calculate_ntai(x$segs, x$ploidy, 'hg19')),
      map_dfr(list(purity_output, hisens_output), 
              function(x) facetsSuite::calculate_hrdloh(x$segs, x$ploidy)),
      map_dfr(list(purity_output, hisens_output), 
              function(x) facetsSuite::calculate_loh(x$segs, x$snps, 'hg19'))
    )
    
    out_file = paste0(prefix, '_hisens.out')
    out_params = readLines(out_file)

    for ( p_idx in 1:length(out_params)) {
      line = gsub(" |#", "", out_params[p_idx])
      sp = unlist(strsplit(line, "="))
      if (length(sp) == 2) {
        if (sp[1] == "purity_cval"){ out_purity_cval = sp[2] }
        if (sp[1] == "cval"){ out_cval = sp[2] }
      }
    }
    
    qc = map_dfr(list(purity_output, hisens_output), function(x) check_fit(x, genome = 'hg19')) %>% 
      add_column(sample = sample_id,
                 cval = c(out_purity_cval, out_cval), .before = 1)
    
    # Write QC
    write.table(qc, paste0(prefix, '.qc.txt'), quote=F, row.names=F, sep='\t')
    
    # Write gene level // use hisensitivity run
    gene_level = facetsSuite::gene_level_changes(hisens_output, 'hg19') %>% 
      add_column(sample = sample_id, .before = 1)
    write.table(gene_level, paste0(prefix, '.gene_level.txt'), quote=F, row.names=F, sep='\t')
    
    # Write arm level // use purity run
    arm_level = facetsSuite::arm_level_changes(purity_output$segs, purity_output$ploidy, 'hg19') %>% 
      pluck('full_output') %>% 
      add_column(sample = sample_id, .before = 1)
    write.table(arm_level, paste0(prefix, '.arm_level.txt'), quote=F, row.names=F, sep='\t')
    
    # ccf annotation
    cncf_txt_file = paste0(prefix, '_hisens.cncf.txt')
    purity = hisens_output$purity
    if (r$use_only_purity_run) {
      cncf_txt_file = paste0(prefix, '_purity.cncf.txt')
      purity = purity_output$purity
    }
    
    if (r$use_edited_cncf) {
      cncf_txt_file = gsub("cncf.txt", "cncf.edited.txt", cncf_txt_file)
    }
    
    if (!is.na(r$reviewer_set_purity) & is.numeric(r$reviewer_set_purity)) {
      purity = r$reviewer_set_purity
    }
    
    sample_maf_file = paste0(sample_path, '/', sample_id, '.maf')
    print(cncf_txt_file)
    print(sample_maf_file)
    
    if (!file.exists(cncf_txt_file)) {
      warning(paste0('cncf file does not exist: ', cncf_txt_file, '! Skipping ccf-annotation for: ', r$sample_id))
      return()
    } 
    if (!file.exists(sample_maf_file)) {
      warning(paste0('MAF file does not exist: ', sample_maf_file, '! Skipping ccf-annotation for: ', r$sample_id))
      return()
    } 
    
    ccf_maf = facetsSuite::ccf_annotate_maf_legacy(fread(sample_maf_file) %>% data.table, 
                                                   cncf_txt_file, 
                                                   purity, 
                                                   algorithm='em')
    ccf_maf$facets_fit = paste0(r$path, '/', r$fit_name)
    ccf_maf$facets_suite_qc = r$facets_suite_qc
    ccf_maf$reviewer_set_purity = r$reviewer_set_purity
    ccf_maf$use_only_purity_run = r$use_only_purity_run
    ccf_maf$use_edited_cncf = r$use_edited_cncf
    ccf_maf$cncf_file_used = cncf_txt_file

    write.table(ccf_maf, file=paste0(sample_path, '/', r$fit_name, '/', sample_id, '.ccf.maf'), quote=F, row.names=F, sep='\t')
  }, .parallel = F)
}




### 4. 