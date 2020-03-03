---
description: Two options to generate CCFs and copy number calls
---

# Generate CCFs/gene-level calls

## 1. via `facets-suite`

* use `annotate-maf-wrapper.R` to generate CCFs 
* use `run-facets-wrapper.R` with `--everything` flag to generate gene/arm level calls

NOTE: Neither one is "review-aware". That is if a fit has manual reviews, then it is up to the analyst to choose the right now. 

## 2. via `facets-preview`

The `facets-preview` package contains two functions that are "review-aware" and enable generation of genomic calls both at sample- as well as cohort-level independent of the GUI.

### 2.1 `facetsPreview::generate_genomic_annotations`

This function generate `ccf.maf`, `gene-level` and `arm-level` calls for **every fit** for a given sample. For each fit manual reviews are incorporated and the "use purity run only?", "used edited cncf", "use purity" values are taken into consideration when generating ccfs. By default, the facets\_qc.txt is regenerated, although this is not always necessary. **Note**: mutations are expected to be in the file `<sample_id>.maf` in the top level directory. CCF annotation is ignored if the .maf is not found.

```text
facetsPreview::generate_genomic_annotations(sample_id, sample_path, config_file, regerate_qc = T)
```

### 2.2 `facetsPreview::compile_cohort_annotations`

Iterates through each sample in `samples_to_annotate` \(a table with two columns: `sample_id` and `sample_path`\), identifies the best fit and generates cohort level `gene-level.txt`, `arm-level.txt` and `ccf.maf`files. Note that this does not generate the genomic annotations and expects as a pre-requisite that `generate_genomic_annotations()` is already run for each sample. A fit is identified from the `facets_qc.txt`with the following order of precedence: \(1\) if a manually reviewed best fit is available, \(2\) if there is a `default` fit, and, if neither, \(3\) then choose an arbitrary fit. 

This function generates four output files:

* &lt;output\_prefix&lt;.ccf.maf
* &lt;output\_prefix&gt;.gene\_level.txt
* &lt;output\_prefix&gt;.arm\_level.txt
* &lt;output\_prefix&gt;.cohort.txt. This file contains the fit that was chosen.

_TODO_: Need to allow choosing a fit independent of the facets\_review. For example, through a column called `selected_fit` added to `samples_to_annotate`.

```text
facetsPreview::compile_cohort_annotations(samples_to_annotate, output_prefix, ncores=10)
```

