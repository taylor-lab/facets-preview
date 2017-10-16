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
```
git clone git@github.com:taylor-lab/facets-preview.git
cd facets-preview
Rscript -e 'if(!require("devtools")) install.packages("devtools", repo="https://cloud.r-project.org"); devtools::install()'
```


## Usage
Launch it (opens in browser)
```
R -e "library(facetsPreview); facetsPreview::launch_application_browser()"   
```

Example input file:
```
/ifs/res/taylorlab/bandlamc/facets_review_app/test_input
```

#### NOTE:  To make things easier, make sure ```/ifs``` is mounted on your laptop through ```sshfs```

```
sshfs <username>@luna.mskcc.org:/ifs /ifs -o auto_cache -o defer_permissions -o local -o IdentityFile=/users/<username>/.ssh/id_rsa -o reconnect -o transform_symlinks -o follow_symlinks
```



