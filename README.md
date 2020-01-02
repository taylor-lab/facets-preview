
# facets-preview
Load, review and adjust facets fits from your local machine (mac OS only)

## Table of Contents
- [Release Notes](#release-notes)
- [Installation](#installation)
- [Usage](#usage)
  * [0. Launching `facets-preview`](#launching-facets-preview) 
  * [1. Input requirements](#input-requirements) 
  * [2. Sample `facets` command](#sample-command-to-generate) 
  * [3. Loading facets runs](#loading-facets-runs) 
  * [4. Samples manifest](#samples-manifest) 
  * [5. Review fits](#review-fits) 
    + [5.1 Submit refits](#submit-refits)
    + [5.2 QC Summary](#qc-summary)	
    + [5.3 Close Up](#close-up)
    + [5.4 Segments](#segments)	
    + [5.5 Segments (editable)](#segments-editable)
    + [5.6 Review notes](#review-notes)	
  * [6. Generating genomic annotations](#generating-genomic-annotations) 
  * [7. Compile cohort-level genomic annotations](#compile-cohort-level-genomic-annotations) 

   
## Release notes

#### v2.0.0a 
 - Integration with IMPACT facets repository maintained by CCS.
 - QC criteria evaluating each fit (see xxx)
 - Migration to `juno`.
	 - Now requires `/juno` to be mounted. 
	 - All refits now performed on `juno`
 - Enhanced features for detailed annotation of fits to enable downstream analyses (*see screenshots below*)
   - Choose fit at `purity` or `hisens` level. Useful when determining cancer cell fractions (CCFs)
   - Enable adjusting `tcn` or `lcn` for some segments. Especially useful to set `lcn` for segments where there are not enough het-snps to estimate allele-specific copy number. 
   - Manually set purity estimate (where determinable) for fits with `purity=NA/0.3`
 - Allow refits to run both `purity` and `hisens` runs for a new `dipLogR`.
 - Forces refit to use the same facets version used as the default fit.
 
#### v1.0.0 (October 2017)
- initial release (see `version1.0.0` branch)

## Installation

#### 1. Install XQuartz
via https://www.xquartz.org/, or, if using Homebrew, install `Cairo` with `brew install cairo`

#### 2. Install `FUSE for macOS`
To use facets-preview ```juno``` needs to be mounted on your machine through ```sshfs``` . You need to install "FUSE for OS X" from http://osxfuse.github.io first before you install "SSHFS" from https://github.com/osxfuse/osxfuse/wiki/SSHFS) 

***If using Homebrew, execute the following to install FUSE:***

    brew install autoconf automake libtool gettext
    brew link --force gettext
    
```
mkdir ~/juno
sshfs $(whoami)@juno.mskcc.org:/juno ~/juno -o auto_cache -o defer_permissions -o local -o IdentityFile=/users/$(whoami)/.ssh/id_rsa -o reconnect -o transform_symlinks -o follow_symlinks
cd /
sudo ln -s ~/juno
```
#### 3. Install `facetsSuite` package
```
devtools::install_github("taylor-lab/facets-suite", ref = "2.0.1beta")
```

#### 4. Install `facetsPreview` package
```
devtools::install_github("taylor-lab/facets-preview", ref = "version_2_preRelease")
```




## Usage

### 0. Launching `facets-preview`
```
Rscript -e "library(facetsPreview); facetsPreview::launch_application_browser()"   
```
#### Example input file:
```
/juno/work/ccs/bandlamc/facets_review_app/test_input
``` 

### 1. Input requirements
`facets-preview` only requires the path to the facets run to load a sample and therefore requires a very specific directory structure. It parses the sample name (tag) from the folder name in the path. For example,  in`/juno/work/...../P-0007584-T01-IM5_P-0007584-N01-IM5`, the sample name is `P-0007584-T01-IM5_P-0007584-N01-IM5`. The required directory/file structure is as follows (screenshot with example below):
* all runs for that sample will be in individual directories and the runs will be recognized only if the folder names have the format: `default`, `facets.*`, `alt_diplogR.*` or `refit_.*`
* the counts file name should be: `countsMerged____<sample_name>.dat.gz`
* `.Rdata`, `.cncf.txt`, `.png` and `.out` files are required to load the runs

### 2. Sample command to generate facets output compatible with `facets-preview`
```
# install facetsSuite v2
# use run-facets-wrapper.sh in the facets-suite github repo
# NOTE: 'legacy-output' flag needs be set to 'T'. 'facets-lib-path' is required.
# Example command: 
/juno/work/ccs/bandlamc/software/R_libs/facetsSuite/2.0.1-beta/run-facets-wrapper.R 
	--facets-lib-path /juno/work/ccs/bandlamc/software/R_libs/facets/0.5.14/ 
	--counts-file /juno/work/ccs/bandlamc/tmp/P-0007584-T01-IM5_P-0007584-N01-IM5/countsMerged____P-0007584-T01-IM5_P-0007584-N01-IM5.dat.gz
	--sample-id P-0007584-T01-IM5_P-0007584-N01-IM5
	--directory /juno/work/ccs/bandlamc/tmp/P-0007584-T01-IM5_P-0007584-N01-IM5/default/
	--snp-window-size 250 
	--normal-depth 35 
	--min-nhet 15 
	--purity-min-nhet 15 
	--seed 100
	--cval 50 
	--purity-cval 100 
	--legacy-output T 
	--genome hg19 
```

### 3. Loading facets runs
<img src="https://github.com/taylor-lab/facets-preview/blob/version_2_preRelease/images/facets-preview-1.png?raw=true" width="900">

### 4. Samples manifest 
<img src="https://github.com/taylor-lab/facets-preview/blob/version_2_preRelease/images/facets-preview-2.png?raw=true" width="900">

### 5. Review fits

#### *5.1. Submit refits*
<img src="https://github.com/taylor-lab/facets-preview/blob/version_2_preRelease/images/facets-preview-3-refit-panel.png?raw=true" width="900">

#### *5.2. QC Summary*
<img src="https://github.com/taylor-lab/facets-preview/blob/version_2_preRelease/images/facets-preview-4-qc-panel.png?raw=true" width="900">

#### *5.3. Close Up*
<img src="https://github.com/taylor-lab/facets-preview/blob/version_2_preRelease/images/facets-preview-5-closeup.png?raw=true" width="900">

#### *5.4. Segments*

#### *5.5. Segments (editable)*
<img src="https://github.com/taylor-lab/facets-preview/blob/version_2_preRelease/images/facets-preview-6-cncf_edit.png?raw=true" width="900">

#### *5.6. Review notes*
<img src="https://github.com/taylor-lab/facets-preview/blob/version_2_preRelease/images/facets-preview-7-review_panel.png?raw=true" width="900">

### 6. Generating genomic annotations
To generate gene/arm-level and CCF annotations for every facets run within `sample_path`:
```
library(facetsSuite)
facetsPreview::generate_genomic_annotations(sample_id, sample_path)
```
Note: mutations are expected to be in the file `<sample_id>.maf` in the top level.

### 7. Compile cohort-level genomic annotations

Iterates through each sample in `samples_to_annotate` (a table with two columns: `sample_id` and `sample_path`), identifies the best fit and generates cohort level `gene-level.txt`, `arm-level.txt` and `ccf.maf`files. Note that this does not generate the genomic annotations and expects as a pre-requisite that `generate_genomic_annotations()` is already run for each sample. Best fit is identified from the `facets_suite_qc.txt`. If the sample has not been reviewed, then the `default` fit is chosen as the best fit. If the sample does not have fit called `default`, then an arbitrary sample is selected. 

TODO: Need to allow choosing a fit independent of the facets_review. For example, through a column called `selected_fit` added to `samples_to_annotate`.
```
library(facetsSuite)
facetsPreview::compile_cohort_annotations(samples_to_annotate, output_prefix, ncores=10)
```
