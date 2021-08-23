#!/usr/bin/bash

# update plots
Rscript R/r_vs_inc.r --source cases
Rscript R/r_plots.r --source cases
Rscript R/r_vs_inc.r --source admissions
Rscript R/r_plots.r --source admissions
Rscript R/r_vs_inc.r --source deaths
Rscript R/r_plots.r --source deaths
Rscript R/adm_vs_cases.r
