
# Set-up ------------------------------------------------------------------

setwd("[omitted]")
assign(".lib.loc", c(.libPaths(), "[omitted]"), envir = environment(.libPaths))
library(data.table)
library(RODBC)

# Building cohort spine ---------------------------------------------------

source("scripts/00b_prelim.r")
source("scripts/01a_identify_npd_cohort.r")

# Get data ----------------------------------------------------------------

source("scripts/00b_prelim.r")
load("processed/1_cohort_spine_npd.rda")

source("scripts/02_npd_demographic_modals.r")
source("scripts/03_npd_demographic_year7.r")
source("scripts/04_npd_demographic_ever.r")
source("scripts/05_hes_birth_characteristics.r")
source("scripts/06_chc_diagnoses.r")
source("scripts/07_enrolment.r")
source("scripts/08_exclusion.r")
source("scripts/09_absence.r")
source("scripts/10_exams.r")
source("scripts/11_outpatient_data.r")

# Combine -----------------------------------------------------------------

source("scripts/00b_prelim.r")

files <- list.files("processed/")
files <- files[!(files %in% c("2_cohort_spine_complete.rda"))] # in case it has already been created
                                  
for (file in files) { load(paste0("processed/", file)) }
rm(file, files)

source("scripts/12_combine.r")
