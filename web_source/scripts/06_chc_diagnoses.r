
print("Phenotype codes")

extract_hes_data <- function(years) {
  
  dbhandle <- odbcDriverConnect(conn_str)
  
  tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")
  
  keep <- tables$TABLE_NAME[grepl("APC", tables$TABLE_NAME)]
  tables <- tables[tables$TABLE_NAME %in% keep, ]
  
  keep <- tables$TABLE_NAME[grepl(paste0(years, collapse = "|"), tables$TABLE_NAME)]
  tables <- tables[tables$TABLE_NAME %in% keep, ]
  
  hes_source <- data.table()
  
  for (table_name in unique(tables$TABLE_NAME)) {
    
    gc()
    print(paste0("Now doing table: ", table_name))
  
    temp <- data.table(
      sqlQuery(
        dbhandle, paste0(
          "SELECT TOKEN_PERSON_ID, EPISTART, EPIEND, ",
          "ADMIDATE, DISDATE, ADMIMETH, STARTAGE, ", 
          paste0("DIAG_", sprintf("%02d", 1:20), collapse = ", "), ", ",
          paste0("OPERTN_", sprintf("%02d", 1:24), collapse = ", "),
          " FROM ", table_name)
      )
      )
    
    temp <- temp[TOKEN_PERSON_ID %in% cohort_spine$tokenpersonid]
    temp <- temp[!duplicated(temp)]
    hes_source <- rbind(hes_source, temp)
    
  }
  
  setnames(hes_source, colnames(hes_source), tolower(colnames(hes_source)))
  setnames(hes_source, "token_person_id", "tokenpersonid")
  
  return(hes_source)
  
}




print("Loading HES data")
hes_data <- extract_hes_data(2008:2016) # Financial years
hes_data <- hes_data[!duplicated(hes_data)]


print("Cleaning dates")
hes_data <- clean_hes_dates(hes_data)

# drop episodes before reception and beyond year 6 by cohort
hes_data <-
  merge(
    hes_data,
    cohort_spine[, c("tokenpersonid", "y7_year")],
    by = "tokenpersonid",
    all.x = T
  )

hes_data <- 
  hes_data[spell_start >= paste0(y7_year - 7, "-09-01") & spell_start < paste0(y7_year - 1, "-08-31")]


print("Calculating length of stay")
hes_data[, spell_los_nights := as.integer(difftime(spell_end, spell_start, units = "days"))]

# Idnetify phenotypes -----------------------------------------------------

print("Identifying phenotypes")

print("Converting to long format")
hes_data_long <-
  melt(
    hes_data,
    id.vars = c("tokenpersonid", "y7_year",
                "epistart", "spell_start", "admimeth", "startage",
                "spell_los_nights"),
    variable.name = "field",
    value.name = "code"
  )

hes_data_long <- hes_data_long[code != "" & !is.na(code)]
hes_data_long[, code_type := gsub("_[0-9].*", "", field)]
hes_data_long[, code_no := as.integer(gsub("[a-z]*._", "", field))]
hes_data_long[, code := substr(code, 1, 4)]

diagnoses <- hes_data_long[code_type == "diag"]
operations <- hes_data_long[code_type == "opertn"]



print("Hardelid et al: any CHC")

hardelid <-
  fread(
    "codelists/chc_hardelid_v1.csv",
    header = T,
    stringsAsFactors = F
  )

hardelid[, code := gsub("\\.", "", code)]

diagnoses[, chc_hardelid := F]
diagnoses[, diag_for_link := code]

diagnoses[substr(code, 1, 3) %in% hardelid[nchar(code) == 3]$code, chc_hardelid := T]
diagnoses[substr(code, 1, 3) %in% hardelid[nchar(code) == 3]$code, diag_for_link := substr(code, 1, 3)]

diagnoses[substr(code, 1, 4) %in% hardelid[nchar(code) == 4]$code, chc_hardelid := T]
diagnoses[substr(code, 1, 4) %in% hardelid[nchar(code) == 4]$code, diag_for_link := substr(code, 1, 4)]

hardelid_diagnoses <- diagnoses[chc_hardelid == T]
hardelid_diagnoses[, chc_hardelid := NULL]

hardelid_diagnoses <-
  merge(
    hardelid_diagnoses,
    hardelid[, c("code", "flag", "group")],
    by.x = "diag_for_link",
    by.y = "code",
    all.x = T
  )

rm(hardelid)

# deal with flags
hardelid_diagnoses[startage >= 7001, startage := 0]
hardelid_diagnoses[, drop := F]
hardelid_diagnoses[flag == "LOS3" & spell_los_nights < 3, drop := T]
hardelid_diagnoses[flag == "AGE10" & startage < 10, drop := T]
hardelid_diagnoses <- hardelid_diagnoses[drop == F]
hardelid_diagnoses[, drop := NULL]

# tidy
hardelid_diagnoses[, phen_group := "hardelid"]
hardelid_diagnoses <-
  hardelid_diagnoses[, c("tokenpersonid", "y7_year",
                         "code", "group", "epistart",
                         "phen_group")]

phen_codes <- data.table()
phen_codes <- rbind(phen_codes, hardelid_diagnoses)
rm(hardelid_diagnoses)




print("Ní Chobhthaigh et al: SRPs")

diagnoses <- hes_data_long[code_type == "diag"]
operations <- hes_data_long[code_type == "opertn"]

srp_nichobhthaigh <-
  fread(
    "codelists/srp_nichobhthaigh_v2.csv",
    stringsAsFactors = F
  )

srp_nichobhthaigh[, code := gsub("\\.", "", code)]

em_adm <-
  fread(
    "codelists/emergency_admissions_v1.csv",
    stringsAsFactors = F
  )

diagnoses <- diagnoses[admimeth %in% em_adm$code]
rm(em_adm)

# Define indicators
diagnoses[, psych := F]         # Potentially psychosomatic
diagnoses[, internalising := F] # Internalising
diagnoses[, externalising := F] # Externalising
diagnoses[, thought_dis := F]   # Thought disorders
diagnoses[, selfharm := F]      # Self-harm

# Potentially psychosomatic codes
diagnoses[substr(code, 1, 3) %in%
            srp_nichobhthaigh[nchar(code) == 3 & group == "potentially_psych"]$code &
            code_no == 1, # All first diagnostic position only
          psych := T]

diagnoses[substr(code, 1, 4) %in%
            srp_nichobhthaigh[nchar(code) == 4 & group == "potentially_psych"]$code &
            code_no == 1, # All first diagnostic position only
          psych := T]

# These codes have a med_surg flag, meaning it should not be counted where one of
# the exclusion codes is present. These include diag and procedure codes.
med_surg <-
  fread(
    "codelists/srp_nichobhthaigh_v2_medical.csv",
    stringsAsFactors = F
  )

med_surg[, code := gsub("\\.", "", code)]

diagnoses[, med_surg_diag := F]
diagnoses[substr(code, 1, 3) %in% med_surg[code_type == "icd10" & nchar(code) == 3]$code, med_surg_diag := T]
diagnoses[substr(code, 1, 4) %in% med_surg[code_type == "icd10" & nchar(code) == 4]$code, med_surg_diag := T]
diagnoses[, med_surg_diag := max(med_surg_diag), by = .(tokenpersonid, epistart)]

operations[, med_surg_opertn := F]
operations[substr(code, 1, 4) %in% med_surg[code_type == "opcs4"]$code, med_surg_opertn := T]
operations[, med_surg_opertn := max(med_surg_opertn), by = .(tokenpersonid, epistart)]
operations <- operations[med_surg_opertn == T]
operations <- operations[, c("tokenpersonid", "epistart", "med_surg_opertn")]
operations <- operations[!duplicated(operations)]

diagnoses <-
  merge(
    diagnoses,
    operations,
    by = c("tokenpersonid", "epistart"),
    all.x = T
  )

# Now set to FALSE where the code is R10 and the episode is medical or surgical.
diagnoses[psych == T & (med_surg_diag == T | med_surg_opertn == T), psych := F]
diagnoses[, med_surg_diag := NULL]
diagnoses[, med_surg_opertn := NULL]
rm(operations, med_surg)


# Internalising
diagnoses[substr(code, 1, 3) %in%
            srp_nichobhthaigh[nchar(code) == 3 & group == "internalising"]$code &
            code_no == 1, # All first diagnostic position only
          internalising := T]

diagnoses[substr(code, 1, 4) %in%
            srp_nichobhthaigh[nchar(code) == 4 & group == "internalising"]$code &
            code_no == 1, # All first diagnostic position only
          internalising := T]


# Externalising
diagnoses[substr(code, 1, 3) %in%
            srp_nichobhthaigh[nchar(code) == 3 & group == "externalising" & flag2 != "selfharm_xz_codes"]$code &
            code_no == 1, # All first diagnostic position only
          externalising := T]

diagnoses[substr(code, 1, 4) %in%
            srp_nichobhthaigh[nchar(code) == 4 & group == "externalising" & flag2 != "selfharm_xz_codes"]$code &
            code_no == 1, # All first diagnostic position only
          externalising := T]


# Thought disorder
diagnoses[substr(code, 1, 3) %in%
            srp_nichobhthaigh[nchar(code) == 3 & group == "thought_disorder"]$code &
            code_no == 1, # All first diagnostic position only
          thought_dis := T]

diagnoses[substr(code, 1, 4) %in%
            srp_nichobhthaigh[nchar(code) == 4 & group == "thought_disorder"]$code &
            code_no == 1, # All first diagnostic position only
          thought_dis := T]


# Self-harm
diagnoses[substr(code, 1, 3) %in%
            srp_nichobhthaigh[nchar(code) == 3 & group == "selfharm" & flag2 != "selfharm_xz_codes"]$code,
          selfharm := T]

diagnoses[substr(code, 1, 4) %in%
            srp_nichobhthaigh[nchar(code) == 4 & group == "selfharm" & flag2 != "selfharm_xz_codes"]$code,
          selfharm := T]


# Now aggregate to episode level.
diagnoses <- diagnoses[order(tokenpersonid, epistart)]
diagnoses[, psych_episode := as.logical(max(psych)), by = .(tokenpersonid, epistart)]
diagnoses[, internalising_episode := as.logical(max(internalising)), by = .(tokenpersonid, epistart)]
diagnoses[, externalising_episode := as.logical(max(externalising)), by = .(tokenpersonid, epistart)]
diagnoses[, thought_dis_episode := as.logical(max(thought_dis)), by = .(tokenpersonid, epistart)]
diagnoses[, selfharm_episode := as.logical(max(selfharm)), by = .(tokenpersonid, epistart)]

diagnoses <- diagnoses[psych_episode | internalising_episode | externalising_episode |
                         thought_dis_episode | selfharm_episode]

diagnoses[, group := factor(NA,
                            levels = c("psych",
                                       "internalising",
                                       "externalising",
                                       "thought_dis",
                                       "selfharm"))]

diagnoses[psych_episode == T, group := "psych"]
diagnoses[internalising_episode == T, group := "internalising"]
diagnoses[externalising_episode == T, group := "externalising"]
diagnoses[thought_dis_episode == T, group := "thought_dis"]
diagnoses[selfharm_episode == T, group := "selfharm"]

diagnoses[, phen_group := group]
diagnoses <- diagnoses[, c("tokenpersonid", "y7_year",
                                   "code", "group", "epistart",
                                   "phen_group")]

phen_codes <- rbind(phen_codes, diagnoses)



print("Saving")

save(phen_codes, file = "processed/phen_codes.rda")

rm(phen_codes, diagnoses, extract_hes_data, srp_nichobhthaigh,
   hes_data, hes_data_long); gc()
