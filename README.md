
# facets-preview
Load, review and adjust facets fits

## Release notes

#### v2.0.0a 
 - Integration with IMPACT facets repository maintained by CCS.
 - QC criteria evaluating each fit
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
- initial release

## Installation

#### Pre-requirement for Mac OS X users: if you don't have X11 (XQuartz) installed, please install it first either via https://www.xquartz.org/ or, if using Homebrew, install `Cairo` with `brew install cairo`


```
devtools::install_github("taylor-lab/facets-suite", ref = "2.0.1beta_cb")
devtools::install_github("taylor-lab/facets-preview", ref = "version_2_preRelease")
```


## Usage

#### Requirement: To use facets-preview ```juno``` needs to be mounted on your machine through ```sshfs``` . You may need to install "FUSE for OS X" from http://osxfuse.github.io first before you install "SSHFS" from https://github.com/osxfuse/osxfuse/wiki/SSHFS) 
#### For Homebrew users on MacOS, execute the following to install FUSE:

    brew install autoconf automake libtool gettext
    brew link --force gettext
    
```
mkdir ~/juno
sshfs $(whoami)@juno.mskcc.org:/juno ~/juno -o auto_cache -o defer_permissions -o local -o IdentityFile=/users/$(whoami)/.ssh/id_rsa -o reconnect -o transform_symlinks -o follow_symlinks
cd /
sudo ln -s ~/juno
```


#### 


    

#### Launch facets-preview (opens in browser)
```
Rscript -e "library(facetsPreview); facetsPreview::launch_application_browser()"   
```




#### Example input file:
```
/juno/work/ccs/bandlamc/facets_review_app/test_input
```

## What does it do? -- TO BE UPDATED!
#### 1. Load one or more facets runs
<img src="https://github.com/taylor-lab/facets-preview/blob/master/images/facets-preview-panel1.png?raw=true" width="600">

#### 2. Review samples
<img src="https://github.com/taylor-lab/facets-preview/blob/master/images/facets-preview-panel2.png?raw=true" width="600">


#### 3. Review fits, submit re-fits to ```luna```, and re-load. 
<img src="https://github.com/taylor-lab/facets-preview/blob/master/images/facets-preview-panel3.png?raw=true" width="600">

 

