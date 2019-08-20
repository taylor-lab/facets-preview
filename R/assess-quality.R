#' @importFrom dplyr distinct

`%!=na%` <- function(e1, e2) (e1 != e2 | (is.na(e1) & !is.na(e2)) | (is.na(e2) & !is.na(e1))) & !(is.na(e1) & is.na(e2))

# 
# calculates important QC metrics for the fit. Where applicable, criteria are independently calculated for 
# 'em' and 'cncf' algorithms. variable names are suffixed with '.em' or '.cncf" to reflect that.
# 'facets_output' can be a list or a string containing path to the Rdata file.
# 
assess_quality_for_fit = function(facets_output,
                                  maf = NULL,
                                  genome = 'hg19') {
    
    # if the 'facets_output' is not a list, then load from Rdata
    if (!is.list(facets_output)) {
      if ( is.character(facets_output) & file.exists(facets_output) ) {
        print(paste0("loading: ", facets_output))
        facets_output = get_facets_output_from_rdata(facets_output)
      } else {
        stop(paste0('failed to load ', facets_output, ' file'))
      }
    }
    
    purity = facets_output$purity
    
    # Check diplogr
    diplogr_flag = abs(facets_output$diplogr) > 1
    
    # SNP counts
    n_snps = nrow(facets_output$snps)
    het_snps = facets_output$snps[het == 1 & rCountN > 50, ]
    n_het_snps = nrow(het_snps)
    
    # metric to flag dropped out snps
    n_het_snps_vafT_homzyg_1pt = nrow(het_snps[ vafT < 0.01 | vafT > 0.99, ])
    n_het_snps_vafT_homzyg_5pt = nrow(het_snps[ vafT < 0.05 | vafT > 0.95, ])
    
    em <- get_fit_metrics(facets_output, maf, genome, 'em')
    names(em) <- paste0('em_', names(em))
    
    cncf <- get_fit_metrics(facets_output, maf, genome, 'cncf')
    names(cncf) <- paste0('cncf_', names(cncf))
    
    # check concordance between em and cncf.
    em_vs_cncf_discord <-
      facets_output$segs %>% 
      dplyr::filter(chrom <= 22) %>% # autosomes only    
      dplyr::mutate(seglen = end - start, 
             tcn_d = ifelse(tcn.em %!=na% tcn, seglen, 0),
             lcn_d = ifelse(lcn.em %!=na% lcn, seglen, 0)) %>%
        filter(tcn_d > 0 | lcn_d > 0) %>% 
      dplyr::mutate(either_d = ifelse(tcn_d > 0 | lcn_d > 0, seglen, 0),
             both_d = ifelse(tcn_d > 0 & lcn_d > 0, seglen, 0)) %>%
      dplyr::summarise(n_tcn_segs = length(which(tcn_d > 0)),
                n_lcn_segs = length(which(lcn_d > 0)),
                total_tcn = sum(tcn_d),
                total_lcn = sum(lcn_d),
                total_either = sum(either_d))
    has_alt_dipLogR = F
    if( !is.null(facets_output$alballogr) && dim(facets_output$alballogr)[1] > 0) {
      has_alt_dipLogR = T
    }
  
    append(
      list(
        purity = facets_output$purity,
        ploidy = facets_output$ploidy,
        diplogr = facets_output$diplogr,
        diplogr_alt = has_alt_dipLogR,
        diplogr_flag = diplogr_flag,
        snps = n_snps,
        n_het_snps = n_het_snps,
        n_het_snps_vafT_homzyg_1pt = n_het_snps_vafT_homzyg_1pt,
        f_het_snps_vafT_homzyg_1pt = n_het_snps_vafT_homzyg_1pt/n_het_snps,
        n_het_snps_vafT_homzyg_5pt = n_het_snps_vafT_homzyg_5pt,
        f_het_snps_vafT_homzyg_5pt = n_het_snps_vafT_homzyg_5pt/n_het_snps,
        n_segs = nrow(facets_output$segs),
        em_cncf_discord_n_tcn_segs = em_vs_cncf_discord$n_tcn_segs[1],
        em_cncf_discord_n_lcn_segs = em_vs_cncf_discord$n_lcn_segs[1],
        em_cncf_discord_frac_tcn = round(em_vs_cncf_discord$total_tcn[1]/em[["em_interrogated_genome"]], 3),
        em_cncf_discord_frac_lcn = round(em_vs_cncf_discord$total_lcn[1]/em[["em_interrogated_genome"]], 3),
        em_cncf_discord_frac_either = round(em_vs_cncf_discord$total_either[1]/em[["em_interrogated_genome"]], 3)
        ),
      append(em, cncf)
    )
}

calculate_vaf_cn_concordance_rate = function(facets_output, algorithm = c('em', 'cncf')) {
  
  inp_maf <-
    facets_output$snps %>%
    dplyr::filter(het == 1, vafN > 0.4 & vafN < 0.6, rCountN > 50, vafT != 0, vafN != 0) %>% 
    rowwise %>%
    dplyr::mutate(Chromosome = chrom,
           t_alt_count = rCountT,
           t_depth = rCountT/vafT,
           t_alt_freq = vafT,
           n_alt_count = rCountN,
           n_depth = rCountN/vafN,
           n_alt_freq = vafN,
           Purity = facets_output$purity) %>%
    left_join(
      facets_output$segs %>% 
        rowwise %>%
        ## annotate_with_zygosity currently calculates this for only 'em', so just a hack for now.
        dplyr::mutate(cf.em = ifelse(algorithm == 'cncf', cf, cf.em),
               tcn.em = ifelse(algorithm == 'cncf', tcn, tcn.em),
               lcn.em = ifelse(algorithm == 'cncf', lcn, lcn.em)) %>%
        dplyr::select(seg, cf.em, tcn.em, lcn.em, cf, tcn, lcn), by=c("seg")
    ) %>%
    dplyr::mutate(has_FACETS=1)
  
  zyg_annotated_maf <- annotate_with_zygosity(inp_maf)
  
  summary <-
    zyg_annotated_maf %>% 
    ungroup %>%
    dplyr::filter(!is.na(Purity)) %>% 
    dplyr::filter(!(Chromosome %in% c( "X", "Y", "MT"))) %>%
    dplyr::filter(!is.na(Purity), !is.na(tcn.em), !is.na(lcn.em)) %>% 
    dplyr::filter(tcn.em <= 3, n_alt_freq > 0.3 , n_alt_freq < 0.7) %>% 
    dplyr::filter(is.na(tumor_vaf_cn_concordance)) %>% 
    dplyr::summarise(total=n(),
              concordant = length(which(tumor_vaf_cn_concordance)),
              rate = length(which(tumor_vaf_cn_concordance))/n(),
              concordant_99 = length(which(tumor_vaf_cn_concordance_99)),
              rate_99 = length(which(tumor_vaf_cn_concordance_99))/n(),
              num_ai_loh_alt = length(which(zygosity_flag == "AI_LOH_ALT")),
              num_ai_loh_ref = length(which(zygosity_flag == "AI_LOH_REF"))) 
  summary
}

get_fit_metrics = function(facets_output,
                          maf = NULL,
                          genome = 'hg19',
                          algorithm = c('em', 'cncf')) {
    
    fcna_output = calculate_fraction_cna(facets_output$segs, facets_output$ploidy, genome, algorithm)
    
    # Create chrom_info for sample
    segs = parse_segs(facets_output$segs, algorithm, facets_output$purity) %>% data.table
    auto_segs <- segs %>% filter(chrom < 23) %>% data.table
    
    # Balanced diploid regions 
    # Note: changing mafR.clust --> mafR to check if the separation of logOR segments exceeds
    # mafr_threshold.
    
    # 
    # For 'hisens' runs, where the run is generated with a specified dipLogR, the cnrl.median.clust 
    # corresponding to the diploid regions will not be identical to the cnlr.median.clust of the 'purity' run. 
    # So, find the cnlr.median.clust in the 'hisens' run that is closest to the dipLogR. For purity run, 
    # the cnlr_clust_value will be the dipLogR. However, note that this may not be too relevant as there
    # could be many different cnlr.median.clust segs just around the diplogr from purity run (especially in
    # hypersegmented cases)
    # 
    cnlr_clusts = unique(segs$cnlr.median.clust)
    cnlr_clust_value = cnlr_clusts[ which.min(abs(cnlr_clusts - facets_output$diplogr)) ]
    
    dip_bal_segs = auto_segs[cnlr.median.clust == cnlr_clust_value & mafR < 0.025, ]
    n_dip_bal_segs = nrow(dip_bal_segs)
    f_dip_bal_segs = dip_bal_segs[, sum(end - start)]/fcna_output$autosomal_genome
    
    dip_imbal_segs = auto_segs[cnlr.median.clust == cnlr_clust_value & mafR >= 0.025, ]
    n_dip_imbal_segs = nrow(dip_imbal_segs)
    f_dip_imbal_segs = dip_imbal_segs[, sum(end - start)]/fcna_output$autosomal_genome
    
    # Number of amplifications and deletions
    n_amps = nrow(segs[tcn >= 10, ])
    n_dels = nrow(segs[tcn == 1, ])  ### <- what would this mean in a GD case?
    n_homdels = nrow(auto_segs[tcn == 0, ])
    f_homdels <- sum(auto_segs[tcn == 0, length])/fcna_output$autosomal_genome
    
    n_homdels_clonal = nrow(auto_segs[tcn == 0 & cf_clonal, ])
    f_homdels_clonal <- sum(auto_segs[tcn == 0 & cf_clonal, length])/fcna_output$autosomal_genome
    
    # Number of unique copy-number states
    n_cn_states = nrow(distinct(segs, tcn, lcn))
    n_segs = nrow(segs)
    
    # Number of NA lcns
    n_lcn_na = nrow(segs[is.na(lcn), ])
    n_loh <- nrow(segs[lcn == 0,])
    f_loh <- sum(segs[lcn == 0, length])/fcna_output$interrogated_genome
    
    ### fraction of CNAs that are sublonal;
    n_segs_subclonal = nrow(segs[cf < facets_output$purity, ])
    f_segs_subclonal = sum(segs[cf < facets_output$purity, length])/fcna_output$interrogated_genome
    
    # mean.cnlr.residual
    facets_output$snps[, cnlr_residual :=  cnlr - median(cnlr), seg]
    cnlr_residual_mean <- facets_output$snps[, mean(cnlr_residual)]
    cnlr_residual_sd <- facets_output$snps[, sd(cnlr_residual)]
    
    # conc = calculate_vaf_cn_concordance_rate(facets_output, algorithm)
    
    # get purity at diploid regions (for now, only 2-1s)
    if ( !is.null(maf)) {
      maf <- 
        maf %>% 
        mutate(Chromosome = ifelse(Chromosome == "X", 23, as.integer(Chromosome))) %>%
        data.table
      
      setkey(maf, Chromosome, Start_Position, End_Position)
      setkey(facets_output$segs, chrom, start, end )
      maf_ann = foverlaps(maf, facets_output$segs, mult="first", nomatch=NA)
      
      maf_ann %>% 
        filter(tcn == 2, lcn==1) %>% filter(cf.em > 0.8) %>% 
        select(t_alt_freq)%>% 
        summarise(med_vaf = median(t_alt_freq))
      
    }
    
    # Count segments where logOR shows balanced but tcn/lcn is imbalanced and vice-versa.
    icn_discordant_with_cnlor <-
      segs %>%
      filter(cf_clonal, !is.na(lcn), !is.na(mcn)) %>%
      rowwise %>%
      ### NOTE: using 0.05 instead of 0.025 to be less-permissive in flagging segments as discordant.
      mutate(icn_bal_mafr_high = ifelse(lcn == mcn && mafR > 0.05, T, F),
             icn_imbal_mafr_low = ifelse(lcn != mcn && mafR < 0.05, T, F)) %>% 
      filter(icn_bal_mafr_high || icn_imbal_mafr_low) %>%
      data.table
    
    n_icn_cnlor_discordant = nrow(icn_discordant_with_cnlor)
    f_icn_cnlor_discordant <- icn_discordant_with_cnlor[, sum(end - start)]/fcna_output$autosomal_genome
    
    # Output
    list(
        n_diploid_balanced_segs = n_dip_bal_segs,
        f_diploid_balanced_segs = f_dip_bal_segs,
        n_diploid_imbalanced_segs = n_dip_imbal_segs,
        f_diploid_imbalanced_segs = f_dip_imbal_segs,
        n_amps = n_amps,
        n_dels = n_dels,
        n_homdels = n_homdels,
        f_homdels = f_homdels,
        n_homdels_clonal = n_homdels_clonal,
        f_homdels_clonal = f_homdels_clonal,        
        n_distinct_states = n_cn_states,
        n_lcn_na = n_lcn_na,
        n_loh = n_loh,
        f_loh = f_loh,
        is_wgd = fcna_output$genome_doubled,
        f_altered = fcna_output$frac_altered,
        f_diploid = fcna_output$frac_diploid,
        n_segs_subclonal = n_segs_subclonal,
        f_segs_subclonal = f_segs_subclonal,
        cnlr_residual_mean = cnlr_residual_mean,
        cnlr_residual_sd = cnlr_residual_sd,
        n_icn_cnlor_discordant = n_icn_cnlor_discordant,
        f_icn_cnlor_discordant = f_icn_cnlor_discordant,
        interrogated_genome = fcna_output$interrogated_genome
        # vaf_tn_conc_n_snps = conc$total,
        # vaf_tn_conc_n_snps_conc = conc$concordant,
        # vaf_tn_conc_rate = conc$rate,
        # vaf_tn_conc_n_snps_conc_99 = conc$concordant_99,
        # vaf_tn_conc_rate_99 = conc$rate_99,
        # vaf_tn_conc_n_ai_loh_alt = conc$num_ai_loh_alt,
        # vaf_tn_conc_n_ai_loh_ref = conc$num_ai_loh_ref
    )
}

get_qc_for_impact_fit = function(quality_for_fit_df) {
  quality_for_fit_df %>%
    ### Filter 1: clonal homdels should be < 2% of the autosomal genome
    mutate(homdel_filter_pass = ifelse(em_f_homdels_clonal < 0.02, T, F),
           homdel_filter_note = paste0('% genome clonal-homdel: ', round(em_f_homdels_clonal * 100, 2), '% (expected <2%), and,',
                                       '% genome (any)-homdel: ', round(em_f_homdels * 100, 2), '% (expected <5%)')) %>%
    
    ### Filter 2: 'number of' and fraction of genome within balanced and imbalanced diploid regions.
    mutate(diploid_bal_seg_filter_pass = ifelse(em_f_diploid_balanced_segs > 0.01 & em_n_diploid_balanced_segs > 0, T, F),
           diploid_imbal_seg_filter_pass = ifelse(em_f_diploid_imbalanced_segs > 0.05 & em_n_diploid_imbalanced_segs > 1, T, F),
           diploid_seg_filter_note = paste0('One of these should be true:',
                                            '\tfrac. of diploid genome that is balanced: ', round(em_f_diploid_balanced_segs * 100, 2), '% (expected: atleast 1%)\n',
                                            '\t# of segments that are diploid and balanced: ', em_n_diploid_balanced_segs, ' (expected: at least 1)\n',
                                            '\nor\n',
                                            '\tfrac. of diploid genome that is imbalanced: ', round(em_f_diploid_imbalanced_segs * 100, 2), '% (expected: atleast 5%)\n',
                                            '\t# of segments that are diploid and imbalanced: ', em_n_diploid_imbalanced_segs, ' (expected: at least 2)\n')) %>%
    
    ### Filter 3: Waterfall Flag: pattern where the variance of logR ratio is very high; typically attributed to assay artifact
    mutate(waterfall_filter_pass = ifelse((is.na(purity) | purity < 0.5) && em_cnlr_residual_sd > 1, F, T),
           waterfall_filter_note = paste0('SD of residuals from cnlr: ', round(em_cnlr_residual_sd, 3), ' (expected atleast 50% purity or em_cnlr_residual_sd < 1)')) %>%
    
    ### Filter 4: Hypersegmentation Flag: Heuristic filter to flag hypersegmented fits that do not have sufficient fraction of the 
    ### genome that is balanced (note kind of arbitrary criteria w.r.t total # of diploid segs or fraction diploid. Maybe 
    ### too stringent)
    mutate(hyper_seg_filter_pass = ifelse(n_segs > 65 && 
                                            (em_n_diploid_balanced_segs + em_n_diploid_imbalanced_segs) < 5 && 
                                            max(em_f_diploid_balanced_segs, em_f_diploid_imbalanced_segs) < 0.1, F, T),
           hyper_seg_filter_note = paste0('# segments: ', n_segs, ' (fail if n_segs > 65 and insufficient fraction of the genome that is diploid)')) %>%
    
    ### Filter 5: ploidy-too-high. Flag if: purity is >>7, or purity is high (>5) and sample is low purity or has too small of a fraction
    ### of genome that is balanced and diploidy
    mutate( high_ploidy_filter_pass = ifelse( ploidy > 5 && (ploidy > 7 || purity < 0.1 || em_f_diploid_balanced_segs < 0.05), F, T),
            high_ploidy_filter_note = paste0('ploidy: ', round(ploidy, 2), 
                                             ' (Fail if ploidy > 7, or, if ploidy > 5 and is low purity or insufficient fraction of genome that is diploid and balanced ')) %>%
    mutate( diploid_seg_filter_pass = ifelse((diploid_bal_seg_filter_pass || diploid_imbal_seg_filter_pass), T, F) )  %>%
    mutate( valid_purity_filter_pass = ifelse(is.na(purity) || purity == 0.3, F, T),
            valid_purity_filter_note = paste0('purity: ', round(purity, 3), ' (expected: purity is not 0.3 or NA)')) %>%
    mutate( facets_suite_qc = ifelse(homdel_filter_pass & diploid_seg_filter_pass & 
                                waterfall_filter_pass & hyper_seg_filter_pass &
                                high_ploidy_filter_pass & valid_purity_filter_pass, T, F) )
}

annotate_maf_with_facets_cn = function(maf) {
  # maf <- 
  #   maf_all %>% 
  #   filter(Tumor_Sample_Barcode == "P-0020686-T01-IM6") %>% 
  #   mutate(Chromosome = ifelse(Chromosome == "X", 23, as.integer(Chromosome))) %>%
  #   data.table
  maf <- 
    maf %>% 
    mutate(Chromosome = ifelse(Chromosome == "X", 23, as.integer(Chromosome))) %>%
    data.table
  
  setkey(maf, Chromosome, Start_Position, End_Position)
  setkey(segs, chrom, start, end )
  maf_ann = foverlaps(maf, segs, mult="first", nomatch=NA)
}

