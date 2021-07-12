The code in this repository produces the results presented in **N. Todd, M. Lerch, Socioeconomic development predicts a weaker contraceptive effect of breastfeeding, _Proc. Natl. Acad. Sci. U.S.A._, 2021. [doi:10.1073/pnas.2025348118](https://doi.org/10.1073/pnas.2025348118)**.


**Folder `R`**

The code in folder `R` reads and curates data from World Fertility Surveys (WFS) and Demographic and Health Surveys (DHS), computes summary statistics and draws simple plots. It also explores trends in the association between *exclusive* breastfeeding and postpartum amenorrhea (`amenorrhea_BEX.R`).

WFS data can be downloaded from [here](https://wfs.dhsprogram.com) and should be placed in `data/WFS`.

DHS data are downloaded directly from the [DHS website](https://dhsprogram.com/) thanks to the `rdhs` package. Simply replace `my_email` and `my_project` in `rdhs_config.R` by valid user and project names.

All scripts are called from `main.R`.


**Folder `R_cluster`**

Standard errors for breastfeeding and amenorrhea durations at survey and energy group levels are estimated using bootstrap (see `summarize_surv_f.R`). Given the long computation time, we used the [GWDG](https://www.gwdg.de/) computer cluster (workload manager: Slurm). Data and code are uploaded to the cluster, and results downloaded from it, using the code in `preparation.R`

Computations are parallelized using a job array. The submission script is `run_all_bar.slurm` and each job runs `computation.R`.

Results are then analyzed in `analysis_country.R` (respectively `analysis_energy4_parity.R`), that runs and explores the meta-regression model written in `stan/country.stan` (respectively `stan/hdi_energy4_parity.stan`).
