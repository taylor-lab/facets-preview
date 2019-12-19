

#' helper function for app
#'
#' @param facets_output facets-suite 
#' @return simple qc
#' @import dplyr
#' @export get_impact_qc_for_fit
get_impact_qc_for_fit = function(facets_output) {
  
  em = append( list( purity = facets_output$purity, ploidy = facets_output$ploidy, dipLogR = facets_output$dipLogR),
               facetsSuite::check_fit(facets_output, genome = 'hg19', algorithm = 'em') )

  ###
  ### Filter 1: clonal homdels should be < 2% of the autosomal genome
  ###
  homdel_filter_pass = ifelse(em$frac_homdels_clonal < 0.02, T, F)
  homdel_filter_note = paste0('% genome clonal-homdel: ', round(em$frac_homdels_clonal * 100, 2), 
                              '% (expected <2%), and, % genome (any)-homdel: ', round(em$frac_homdels * 100, 2), 
                              '% (expected <5%)') 
    
  ###
  ### Filter 2: 'number of' and fraction of genome within balanced and imbalanced diploid regions.
  ###
  diploid_bal_seg_filter_pass = ifelse(em$frac_dip_bal_segs > 0.01 & em$n_dip_bal_segs > 0, T, F)
  diploid_imbal_seg_filter_pass = ifelse(em$frac_dip_imbal_segs > 0.05 & em$n_dip_imbal_segs > 1, T, F)
  diploid_seg_filter_note = 
    paste0('One of these should be true:',
           '\tfrac. of diploid genome that is balanced: ', round(em$frac_dip_bal_segs * 100, 2), '% (expected: atleast 1%)\n',
           '\t# of segments that are diploid and balanced: ', em$n_dip_bal_segs, ' (expected: at least 1)\n',
           '\nor\n',
           '\tfrac. of diploid genome that is imbalanced: ', round(em$frac_dip_imbal_segs * 100, 2), '% (expected: atleast 5%)\n',
           '\t# of segments that are diploid and imbalanced: ', em$n_dip_imbal_segs, ' (expected: at least 2)\n')

  ###
  ### Filter 3: Waterfall Flag: pattern where the variance of logR ratio is very high; 
  ### typically attributed to assay artifact
  ###
  waterfall_filter_pass = ifelse((is.na(facets_output$purity) | facets_output$purity < 0.5) && em$sd_cnlr_residual > 1, F, T)
  waterfall_filter_note = paste0('SD of residuals from cnlr: ', round(em$sd_cnlr_residual, 3), ' (expected atleast 50% purity or sd_cnlr_residual < 1)')
  
  ###
  ### Filter 4: Hypersegmentation Flag: Heuristic filter to flag hypersegmented fits that do not 
  ### have sufficient fraction of the genome that is balanced (note kind of arbitrary criteria w.r.t 
  ### total # of diploid segs or fraction diploid. Maybe too stringent)
  hyper_seg_filter_pass = ifelse(em$n_segs > 65 &&
                                   (em$n_dip_bal_segs + em$n_dip_imbal_segs) < 5 && 
                                   max(em$frac_dip_bal_segs, em$frac_dip_imbal_segs) < 0.1, F, T)
  hyper_seg_filter_note = paste0('# segments: ', em$n_segs, ' (fail if n_segs > 65 and insufficient fraction of the genome that is diploid)')
  
  ###
  ### Filter 5: ploidy-too-high. Flag if: purity is >>7, or purity is high (>5) and sample is low purity or has 
  ### too small of a fraction of genome that is balanced and diploidy
  ###
  high_ploidy_filter_pass = ifelse( facets_output$ploidy > 5 && (facets_output$ploidy > 7 || facets_output$purity < 0.1 || em$frac_dip_bal_segs < 0.05), F, T)
  high_ploidy_filter_note = paste0('ploidy: ', round(facets_output$ploidy, 2), 
                                   ' (Fail if ploidy > 7, or, if ploidy > 5 and is low purity or ',
                                   'insufficient fraction of genome that is diploid and balanced ')
  
  ###
  ### Filter 6: valid purity filter.
  ###
  valid_purity_filter_pass = ifelse(is.na(facets_output$purity) || facets_output$purity == 0.3, F, T)
  valid_purity_filter_note = paste0('purity: ', round(facets_output$purity, 3), ' (expected: purity is not 0.3 or NA)')

  diploid_seg_filter_pass = ifelse((diploid_bal_seg_filter_pass || diploid_imbal_seg_filter_pass), T, F)
  
  facets_suite_qc = ifelse(homdel_filter_pass & diploid_seg_filter_pass & 
                             waterfall_filter_pass & hyper_seg_filter_pass &
                             high_ploidy_filter_pass & valid_purity_filter_pass, T, F)
  append(
    em, 
    list(
      homdel_filter_pass = homdel_filter_pass,
      homdel_filter_note = homdel_filter_note,
      diploid_bal_seg_filter_pass = diploid_bal_seg_filter_pass,
      diploid_imbal_seg_filter_pass = diploid_imbal_seg_filter_pass,
      diploid_seg_filter_note = diploid_seg_filter_note,
      waterfall_filter_pass = waterfall_filter_pass,
      waterfall_filter_note = waterfall_filter_note,
      hyper_seg_filter_pass = hyper_seg_filter_pass,
      hyper_seg_filter_note = hyper_seg_filter_note,
      high_ploidy_filter_pass = high_ploidy_filter_pass,
      high_ploidy_filter_note = high_ploidy_filter_note,
      valid_purity_filter_pass = valid_purity_filter_pass,
      valid_purity_filter_note = valid_purity_filter_note,
      diploid_seg_filter_pass = diploid_seg_filter_pass,
      facets_suite_qc = facets_suite_qc
      )
   )
}


