# Installation

## 1. Install XQuartz

via [https://www.xquartz.org/](https://www.xquartz.org/), or, if using `Homebrew`, install `Cairo` with `brew install cairo`

## 2. Install `facets-preview`

```r
devtools::install_github("taylor-lab/facets-preview", ref = "version_2_preRelease")
```

## 3. Use `sshfs` to mount `juno`

_**Rationale for this**_: `facets-preview` loads facets runs from sample level directories with facets outputs. Often, these runs are kept on network storage such as `/juno`. To seamlessly access the runs on local machine without tinkering the paths, it is convenient to remotely mount the file system and simply access the facets run with its full path. For example, `/juno/work/ccs/bandlamc/facets_runs/P-0012345-T01-IM6_P-0012345-N01-IM6/` can be accessed from local machine simply mounting `/juno` locally.   Similarly, if the facets re-fit jobs are to be run on `/juno`, this mounting step is required. 

First, install "FUSE for OS X" from [http://osxfuse.github.io](http://osxfuse.github.io/). If using `Homebrew`, execute the following to install FUSE:

```bash
brew install autoconf automake libtool gettext
brew link --force gettext
```

Next, install "SSHFS" from [https://github.com/osxfuse/osxfuse/wiki/SSHFS](https://github.com/osxfuse/osxfuse/wiki/SSHFS)

Finally, setup mount with the following instructions:

```bash
mkdir ~/juno
sshfs $(whoami)@juno.mskcc.org:/juno ~/juno -o auto_cache -o defer_permissions -o local -o IdentityFile=/users/$(whoami)/.ssh/id_rsa -o reconnect -o transform_symlinks -o follow_symlinks
cd /
sudo ln -s ~/juno
```

## 4. Configuration file

Example .json config file:

```markup
{
  "repo": [
    {
      "name": "Clinical series IMPACT",
      "manifest_file": "/juno/work/....../impact_facets_manifest_latest.txt.gz",
      "sample_name_format": "^P-\\d{7}-T\\d+-IM\\d(,P-\\d{7}-T\\d+-IM\\d)*$",
      "counts_file_format": "countsMerged____{sample_id}.dat.gz"
    },
    {
      "name": "BRCA exomes",
      "manifest_file": "/juno/work/......./brca_exomes_manifest.txt.gz",
      "sample_name_format": "",
      "counts_file_format": "countsMerged____{sample_id}.dat.gz
    }
  ],
  "watcher_dir": "/juno/work/...../facets_refit_watcher/",
  "facets_lib": [
    {
      "version": "0.5.6",
      "lib_path": "/juno/work/ccs/bandlamc/software/R_libs/facets/0.5.6/"
    },
    {
      "version": "0.5.14",
      "lib_path": "/juno/work/ccs/bandlamc/software/R_libs/facets/0.5.14/"
    },
    {
      "version": "0.6.0",
      "lib_path": "/juno/work/ccs/bandlamc/software/R_libs/facets/0.6.0/"
    }
  ],
  "facets_suite_lib": "/juno/work/ccs/bandlamc/software/R_libs/facetsSuite/2.0.3/",
  "facets_suite_run_wrapper": "/juno/work/.../R_libs/facetsSuite/2.0.2/run-facets-wrapper.R",
  "facets_qc_script": "/juno/work/ccs/bandlamc/git/facets-preview/facets_qc/v1.0/facets_fit_qc.R"
}

```

* **`repo`** - facets repository
  * `manifest_file`: A tab-delimited file with only three required columns. See [input requirements](input-requirements.md#1-manifest-file) for more detail. 
    * "sample\_id" column. eg: P-0012345-T01-IM6\_P-0012345-N01-IM6 
    * "sample\_path" column. eg: /juno/work/ccs/bandlamc/facets\_runs/P-0012345-T01-IM6\_P-0012345-N01-IM6/
    * "tumor\_sample" column. eg: P-0012345-T01-IM6
  * `sample_name_format`: \(optional\) a regex to ensure tumor samples are entered in proper format.
  * `counts_file_format`: \(required\) format of the counts file name. 
* **`watcher_dir`**: directory to which facets-preview writes out refit commands. This directory is monitored by a daemon that picks up the command and executes it. For more information, see section: **setup refit watcher** below.
* **`facets_lib`**: these versions are available during refit
* **`facets_suite_lib`**: use this version of `facets-suite` for preview. NOTE: This path should contain the `run-facets-wrapper.sh` script.
* **`facets_qc_script`**: specify the precise path to the version of facets QC script to be run with `facets-preview`

## 5. Setup 'refit watcher'

Refit watcher is simply a script that launches a daemon that monitors a folder for new job files \(with the file name `facets_refit_cmd_.*sh`created in the `refit_jobs` directory. Below is the directory structure for the "refit watcher".  Once this structure is setup, simply modify  the WATCHER\_DIR variable in the  `refit_watcher.sh` script and run it. This works with [inotify](https://github.com/inotify-tools/inotify-tools/wiki) version 3.14. Binary \(for `juno`\) is on facets-preview github repo. 

```bash
refit_watcher/
├── bin
│   ├── inotifywait
│   └── refit_watcher.sh
├── refit_jobs
│   ├── facets_refit_cmd_TumorID_Normal_ID_c50_pc100_diplogR_-0.09.sh
│   ├── facets_refit_cmd_TumorID_Normal_ID_c50_pc100_diplogR_-0.09.sh.err
│   ├── facets_refit_cmd_TumorID_Normal_ID_c50_pc100_diplogR_-0.09.sh.log
├── watcher.err
└── watcher.log
```

