# Release notes

**v2.1.0 \(March 2020\)**

* **Major release. Required: facets-suite v2.0.3 or higher. facets\_qc v1.0 or higher.**
* Introducing "repositories" \(or "repo"\) that can be loaded into `facets-preview`. Quite simply, a "repo" is a folder with all facets runs/fits that be queried and loaded into the app through a "sample manifest" file.
* QC script is now independent of the app and is versioned separately. Multiple versions of QC calls can now co-exist
* Added new QC filters \(therefore, require facets-suite v2.0.3 or higher\)
* Parameterized `facets-preview` to load options from a confg file \(.json\). Can be configured to load repositories, set QC scripts, specific version of facets-suite. 
* Allowing more expansive options for "refit".

**v2.0.0a \(December 2019\)**

* Integration with IMPACT facets repository maintained by CCS.
* QC criteria evaluating each fit \(see xxx\)
* Migration to `juno`.
  * Now requires `/juno` to be mounted.
  * All refits now performed on `juno`
* Enhanced features for detailed annotation of fits to enable downstream analyses \(_see screenshots below_\)
  * Choose fit at `purity` or `hisens` level. Useful when determining cancer cell fractions \(CCFs\)
  * Enable adjusting `tcn` or `lcn` for some segments. Especially useful to set `lcn` for segments where there are not enough het-snps to estimate allele-specific copy number.
  * Manually set purity estimate \(where determinable\) for fits with `purity=NA/0.3`
* Allow refits to run both `purity` and `hisens` runs for a new `dipLogR`.
* Forces refit to use the same facets version used as the default fit.

**v1.0.0 \(October 2017\)**

* initial release \(see `version1.0.0` branch\)

