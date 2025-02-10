
print("Creating flags and merging data")

print("NPD demographics modals")
cohort_spine <-
  merge(
    cohort_spine,
    demo_modals,
    by = "PupilMatchingRefAnonymous",
    all.x = T
  )

# Only needed if you started with an NPD inception:
cohort_spine[, dob_approx := as.Date(paste0(yearofbirth, "-", monthofbirth, "-15"))]


print("NPD demographics 7y")
cohort_spine <-
  merge(cohort_spine,
        demo_y7,
        by = "PupilMatchingRefAnonymous",
        all.x = T
  )


print("NPD demographics ever")
cohort_spine[!is.na(PupilMatchingRefAnonymous), fsm_ever_primary := PupilMatchingRefAnonymous %in%
               demographics_ever[fsm_ever_primary == 1]$PupilMatchingRefAnonymous]

cohort_spine[!is.na(PupilMatchingRefAnonymous), highest_sen_primary := factor("None",
                                             levels = c("None",
                                                        "Support",
                                                        "S/EHCP",
                                                        "Specialist provision"))]

cohort_spine[PupilMatchingRefAnonymous %in% demographics_ever[specialist_school == T]$PupilMatchingRefAnonymous,
             highest_sen_primary := "Specialist provision"]

cohort_spine[highest_sen_primary != "Specialist provision" &
               PupilMatchingRefAnonymous %in% demographics_ever[senprovision == "sehcp"]$PupilMatchingRefAnonymous,
             highest_sen_primary := "S/EHCP"]

cohort_spine[highest_sen_primary != "Specialist provision" & highest_sen_primary != "S/EHCP" &
               PupilMatchingRefAnonymous %in% demographics_ever[senprovision == "support"]$PupilMatchingRefAnonymous,
             highest_sen_primary := "Support"]

cohort_spine[, ever_sen_primary := ifelse(highest_sen_primary == "None", F, T)]

print("HES demographic data")
# Remember to join these data on, if you extracted any!


print("HES birth characteristics")
# Only needed if you started with an NPD inception:
cohort_spine <-
  merge(
    cohort_spine,
    birth_records,
    by = "tokenpersonid",
    all.x = T
  )


print("CHC exposure")
phen_groups <- names(table(phen_codes$phen_group))

for (chc in phen_groups) {
    
  new_col <- paste0("chc_", chc)
  print(paste0("Now doing ", new_col))
  
  cohort_spine[!is.na(tokenpersonid), ((new_col)) := as.logical(NA)]
  cohort_spine[!is.na(tokenpersonid), ((new_col)) := tokenpersonid %in% phen_codes[phen_group == chc]$tokenpersonid]
  
}

cohort_spine[, chc_any :=
               chc_hardelid == T |
               chc_psych == T |
               chc_internalising == T |
               chc_externalising == T |
               chc_thought_dis == T |
               chc_selfharm == T]


print("Non-enrolment")
enrolments_spring <- enrolments[census %in% c("Spring", "AP")]

cohort_spine[!is.na(PupilMatchingRefAnonymous), not_enrolled_y8 := !(PupilMatchingRefAnonymous %in% enrolments_spring[year == 8]$PupilMatchingRefAnonymous)]
cohort_spine[!is.na(PupilMatchingRefAnonymous), not_enrolled_y9 := !(PupilMatchingRefAnonymous %in% enrolments_spring[year == 9]$PupilMatchingRefAnonymous)]
cohort_spine[!is.na(PupilMatchingRefAnonymous), not_enrolled_y10 := !(PupilMatchingRefAnonymous %in% enrolments_spring[year == 10]$PupilMatchingRefAnonymous)]
cohort_spine[!is.na(PupilMatchingRefAnonymous), not_enrolled_y11 := !(PupilMatchingRefAnonymous %in% enrolments_spring[year == 11]$PupilMatchingRefAnonymous)]

cohort_spine[!is.na(PupilMatchingRefAnonymous), ever_not_enrolled := not_enrolled_y8 | not_enrolled_y9 | not_enrolled_y10 | not_enrolled_y11]
cohort_spine[!is.na(PupilMatchingRefAnonymous), not_enrolled_n_yr := sum(not_enrolled_y8,
                                                                         not_enrolled_y9,
                                                                         not_enrolled_y10,
                                                                         not_enrolled_y11), by = PupilMatchingRefAnonymous]



print("Exclusions")
cohort_spine[!is.na(PupilMatchingRefAnonymous), excluded_y7 := PupilMatchingRefAnonymous %in% exclusions[year == 7]$PupilMatchingRefAnonymous]
cohort_spine[!is.na(PupilMatchingRefAnonymous), excluded_y8 := PupilMatchingRefAnonymous %in% exclusions[year == 8]$PupilMatchingRefAnonymous]
cohort_spine[!is.na(PupilMatchingRefAnonymous), excluded_y9 := PupilMatchingRefAnonymous %in% exclusions[year == 9]$PupilMatchingRefAnonymous]
cohort_spine[!is.na(PupilMatchingRefAnonymous), excluded_y10 := PupilMatchingRefAnonymous %in% exclusions[year == 10]$PupilMatchingRefAnonymous]
cohort_spine[!is.na(PupilMatchingRefAnonymous), excluded_y11 := PupilMatchingRefAnonymous %in% exclusions[year == 11]$PupilMatchingRefAnonymous]

cohort_spine[, ever_excluded := excluded_y7 | excluded_y8 | excluded_y9 | excluded_y10 | excluded_y11]


print("Absence")
absence_rates <-
  dcast(
    absence,
    PupilMatchingRefAnonymous ~ year,
    value.var = c("abs_auth_rate", "abs_unauth_rate",
                  "abs_overall_rate", "pa10",
                  "abs_illness_rate",
                  "abs_appts_rate",
                  "abs_illness_appts_rate",
                  "sessionspossible_annual"
    )
  )

cohort_spine <-
  merge(
    cohort_spine,
    absence_rates,
    by = "PupilMatchingRefAnonymous",
    all.x = T
  )

cohort_spine[, ever_pa10 := pa10_7 | pa10_8 | pa10_9 | pa10_10 | pa10_11]


print("Exams")
cohort_spine <-
  merge(
    cohort_spine,
    ks4,
    by = "PupilMatchingRefAnonymous",
    all.x = T
  )


print("Outpatients")
cohort_spine[!is.na(tokenpersonid), ever_attended_cpc := tokenpersonid %in% opa_data$tokenpersonid]

opa_data <- opa_data[order(tokenpersonid, apptdate)]
opa_data <- opa_data[!duplicated(tokenpersonid)]
setnames(opa_data, "apptdate", "date_first_cpc")

cohort_spine <-
  merge(
    cohort_spine,
    opa_data[, c("tokenpersonid", "date_first_cpc")],
    by = "tokenpersonid",
    all.x = T
  )

print("CSC")
print("Loading CiN data")
cin <- 
  fread(
    "csc_data/cin.csv",
    stringsAsFactors = F,
    header = T
  )

cin <- cin[pupilmatchingrefanonymous %in% cohort_spine$PupilMatchingRefAnonymous]

print("Loading CLA data")
cla <- 
  fread(
    "csc_data/cla.csv",
    stringsAsFactors = F,
    header = T
  )

cla <- cla[pupilmatchingrefanonymous %in% cohort_spine$PupilMatchingRefAnonymous]

print("Subsetting to primary school")
cin <-
  merge(
    cin,
    cohort_spine[, c("PupilMatchingRefAnonymous", "y7_year")],
    by.x = "pupilmatchingrefanonymous",
    by.y = "PupilMatchingRefAnonymous",
    all.x = T
  )

cla <-
  merge(
    cla,
    cohort_spine[, c("PupilMatchingRefAnonymous", "y7_year")],
    by.x = "pupilmatchingrefanonymous",
    by.y = "PupilMatchingRefAnonymous",
    all.x = T
  )

cin <- cin[referraldate >= paste0(y7_year - 7, "-09-01") & referraldate < paste0(y7_year - 1, "-08-31")]
cla <- cla[date_episode_started >= paste0(y7_year - 7, "-09-01") & date_episode_started < paste0(y7_year - 1, "-08-31")]

print("Create CSC categorical variable")
cohort_spine[!is.na(PupilMatchingRefAnonymous), ever_cin := PupilMatchingRefAnonymous %in% cin[need_episode == T]$pupilmatchingrefanonymous]
cohort_spine[!is.na(PupilMatchingRefAnonymous), ever_cpp := PupilMatchingRefAnonymous %in% cin[cpp == T]$pupilmatchingrefanonymous]
cohort_spine[!is.na(PupilMatchingRefAnonymous), ever_cla := PupilMatchingRefAnonymous %in% cla$pupilmatchingrefanonymous]

cohort_spine[!is.na(PupilMatchingRefAnonymous), highest_csc := factor("None",
                                     levels = c("None",
                                                "CiN",
                                                "CPP",
                                                "CLA"))]

cohort_spine[ever_cla == T & !is.na(PupilMatchingRefAnonymous), highest_csc := "CLA"]
cohort_spine[ever_cpp == T & ever_cla == F & !is.na(PupilMatchingRefAnonymous), highest_csc := "CPP"]
cohort_spine[ever_cin == T & ever_cpp == F & ever_cla == F & !is.na(PupilMatchingRefAnonymous), highest_csc := "CiN"]


print("Creating eligibilty flag")
cohort_spine[, born_in_year := dob_approx >= paste0(min(birth_cohorts) - 1, "-09-01") & dob_approx < paste0(max(birth_cohorts), "-09-01")]
cohort_spine[, eligible := !is.na(tokenpersonid) & born_in_year] # NPD inception cohort
# cohort_spine[, eligible := !is.na(PupilMatchingRefAnonymous) & born_in_year] # HES inceptionc cohort
cohort_spine <- cohort_spine[order(PupilMatchingRefAnonymous, y7_year)]


print("Saving and clearing memory")
save(cohort_spine, file = "processed/2_cohort_spine_complete.rda")
rm(list = ls()); gc()
