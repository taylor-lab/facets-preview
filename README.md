[![Build Status](https://travis-ci.com/taylor-lab/facets-preview.svg?token=4kBAQAEEc39zo9ACoThH&branch=master)](https://github.com/taylor-lab/facets-preview)

# facets-preview
Load, review and adjust facets fits

## What does it do?
#### 1. Load one or more facets runs
<img src="https://github.com/taylor-lab/facets-preview/blob/master/images/facets-preview-panel1.png?raw=true" width="600">

#### 2. Review samples
<img src="https://github.com/taylor-lab/facets-preview/blob/master/images/facets-preview-panel2.png?raw=true" width="600">


#### 3. Review fits, submit re-fits to ```luna```, and re-load. 
<img src="https://github.com/taylor-lab/facets-preview/blob/master/images/facets-preview-panel3.png?raw=true" width="600">

 


## Setup
#### For Mac OS X users: if you don't have X11 (XQuartz) installed, please install it first either via https://www.xquartz.org/ or, if using Homebrew, install `Cairo` with 

```
brew install cairo
```

## Installation

```
git clone git@github.com:taylor-lab/facets-preview.git
cd facets-preview
Rscript -e 'if(!require("devtools")) install.packages("devtools", repo="https://cloud.r-project.org");devtools::install_github("hadley/devtools"); devtools::install()'
```


## Usage
Launch it (opens in browser)
```
Rscript -e "library(facetsPreview); library(dplyr); facetsPreview::launch_application_browser()"   
```
#### NOTE: If you would like to access ```ifs``` when using facets-preview, mount ```ifs``` onto your labtop through ```sshfs``` 
```
mkdir ~/ifs
sshfs $(whoami)@luna.mskcc.org:/ifs ~/ifs -o auto_cache -o defer_permissions -o local -o IdentityFile=/users/$(whoami)/.ssh/id_rsa -o reconnect -o transform_symlinks -o follow_symlinks
cd /
sudo ln -s ~/ifs
```

#### You may need to install "FUSE for OS X" from http://osxfuse.github.io first before you install "SSHFS" from https://github.com/osxfuse/osxfuse/wiki/SSHFS) 

#### For Homebrew users on MacOS, execute the following to install FUSE:

    brew install autoconf automake libtool gettext
    brew link --force gettext
    


#### Example input file:
```
/ifs/res/taylorlab/bandlamc/facets_review_app/test_input
```



