# Input requirements

## 0. Loading a sample

At minimum only the facets run path is required to load a sample. For example, inputting the path: `/juno/work/ccs/facets_runs/tumorID_normalID/`, lets preview infer the `sample_id` as tumorID\_normalID

## 1. Manifest file

The manifest file is required to have at least three columns with an additional column that is optional:

* `sample_id` column. eg: tumorID\_normalID 
* `sample_path` column. eg: /juno/work/ccs/facets\_runs/tumorID\_normalID/
* `tumor_sample` column. eg: tumorID
* `dmp_id` column \(optional\): eg: P-0000000 if the  tumorID is from DMP patient P-0000000. This is only ever used to build the cBioPortal link.

## 2. Facets output structure



