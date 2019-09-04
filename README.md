# Shanghai_HSR_DMSP_VIIRS
This repository contains the code implementations for the manuscript "The Impact of High-Speed Rail on Urban Growth: A Multi-Scale Space-Time Approach" submitted to Regional Studies. The code Shanghai_DID_git.R is an R code for panel data OLS, IV-2SLS, and VCE for DMSP and VIIRS night-time light data (1x1km scale) for years after operations of HH10 and NH13. near_udf.py is the python code implementated on linux command line for space-time covariance matrix sampling.

## Data 
Source data is stored in https://uofi.app.box.com/folder/86303978440 . vdif_1204_1205.asc is the a sample of night-time difference matrix from 2012.4 - 2012.5 . rs_rev_design.RData is the design matrix needed for Shanghai_DID_git.R

## Implementation
Please change the working directories in the code to your local directories and make correct reference to the data file. The R file can be directly run from UI. TO run the python file, enter the following command-line code in a linux systen (a memory of over 128 GB is needed - usually on an HPC cluster).

```
python near_udf.py
```
