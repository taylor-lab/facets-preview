# facets-preview
Load, review and adjust facets fits

### What does it do?
* Load one or more facets runs
* A viewer to review fits
* Submits refit jobs to ```luna``` and loads the new fits


### Setup
```
git clone git@github.com:taylor-lab/facets-preview.git
cd facets-preview
Rscript -e "devtools::install_deps()"
R CMD INSTALL .
```


### Usage
Launch it (opens in browser)
```
R -e "library(facetsPreview); facetsPreview::launch_application_browser()"   
```

Example input file:
```
/ifs/res/taylorlab/bandlamc/facets_review_app/test_input
```

##### NOTE:  To make things easier, make sure ```/ifs``` is mounted on your laptop through ```sshfs```

```
sshfs <username>@luna.mskcc.org:/ifs /ifs -o auto_cache -o defer_permissions -o local -o IdentityFile=/users/<username>/.ssh/id_rsa -o reconnect -o transform_symlinks -o follow_symlinks
```
