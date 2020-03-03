# FACETS QC criteria

Starting with `v2`, the `facets QC` is versioned independent of the `facets-preview`. The version \# is also logged \(with column `facets_qc_version`\) in `facets_qc.txt` and `facets_review.manifest`.

## v1.0 

_TODO: add more details_

1. Too low fraction of the genome diploidy \(2-0 or 1-0\)
2. `dipLogR` is too high \(high homdels\)
3. `dipLogR` is too low \(high ploidy\)
4. High `ploidy` filter
5. Hypersegmentation
6. Fragment size difference between T/N \("waterfall"\)
7. Invalid purity \(`purity` = 0.3 or NA\)
8. Large discordance in TCN between EM and CNCF
9. Too large fraction of genome subclonal
10. Discordance between integer copy number and `logOR`.
11. Contamination filter \(homozygous SNPs\)

## v0.0

Initial version of facets QC. 

