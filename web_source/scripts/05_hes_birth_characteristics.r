print("Birth characteristics")

# Define functions
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
          "EPITYPE, CLASSPAT, NEOCARE, HRGNHS, ",
          "NUMBABY, DISMETH, EPIKEY, BIRSTAT_1, MATAGE, ",
          "BIRORDR_1, GESTAT_1, BIRWEIT_1, IMD04_DECILE, ",
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
birth_records <- extract_hes_data(c(birth_cohorts[1] - 1, birth_cohorts))
birth_records <- birth_records[!duplicated(birth_records)]

birth_records <-
  merge(
    birth_records,
    cohort_spine[, c("tokenpersonid", "y7_year")],
    by = "tokenpersonid",
    all.x = T
  )

birth_records <-
  birth_records[epistart >= paste0(y7_year - 13, "-09-01") | epistart <= paste0(y7_year - 12, "-09-01")]


print("Cleaning dates")
birth_records <- clean_hes_dates(birth_records)


print("Identifying birth episodes")
births <- fread("codelists/births_zylb_v1.csv", stringsAsFactors = F)

# startage
birth_records <- birth_records[startage %in% 7001:7002] # startage 7001 = <1 day, 7002 = 1 to 6 days

# diag
diag_cols <- names(birth_records)[grepl("^diag", names(birth_records))]

diagnoses <- melt(
  birth_records[, c("tokenpersonid",
                    "epikey",
                    diag_cols),
                with = F],
  id.vars = c("tokenpersonid",
              "epikey"),
  variable.name = "diag_n",
  value.name = "code"
  )

diagnoses <- diagnoses[code != ""]

diagnoses[, birth_diag := F]
diagnoses[substr(code, 1, 3) %in% births[field == "diag"]$code, birth_diag := T]
diagnoses <- diagnoses[birth_diag == T]
diagnoses <- diagnoses[, c("tokenpersonid", "epikey", "birth_diag")]
diagnoses <- diagnoses[!duplicated(diagnoses)]

birth_records <-
  merge(
    birth_records,
    diagnoses,
    by = c("tokenpersonid", "epikey"),
    all.x = T
  )

birth_records[, birth_episode := F]
birth_records[birth_diag == T, birth_episode := T] 
birth_records[, birth_diag := NULL]

rm(diagnoses)

# other fields
birth_records[hrgnhs %in% births[field == "hrg"]$code, birth_episode := T]
birth_records[epitype %in% c(3, 6), birth_episode := T]
birth_records[classpat == 5, birth_episode := T]
birth_records[admimeth %in% c("82", "83", "2C"), birth_episode := T]
birth_records[neocare %in% 0:3, birth_episode := T]

birth_records <- birth_records[birth_episode == T]

# exclude still and multiple births
birth_records[, still_termination_multiple := F]

still <- fread("codelists/multiple_still_births_zylb_v1.csv")
still[, code := gsub("\\.", "", code)]

# diag
diagnoses <-
  melt(
    birth_records[, c("tokenpersonid",
                      "epikey",
                      diag_cols),
                  with = F],
    id.vars = c("tokenpersonid",
                "epikey"),
    variable.name = "diag_n",
    value.name = "code"
    )

diagnoses <- diagnoses[code != ""]

diagnoses[, stillbirth := F]
diagnoses[, termination := F]
diagnoses[, multiple := F]

diagnoses[substr(code, 1, 3) %in% still[field == "diag" & group == "stillbirth" & nchar(code) == 3]$code, stillbirth := T]
diagnoses[substr(code, 1, 4) %in% still[field == "diag" & group == "stillbirth" & nchar(code) == 4]$code, stillbirth := T]
diagnoses[substr(code, 1, 4) %in% still[field == "diag" & group == "termination"]$code, termination := T]
diagnoses[substr(code, 1, 4) %in% still[field == "diag" & group == "multiple_births"]$code, multiple := T]

# De-duplicate the data for merge
diagnoses[, stillbirth := max(stillbirth), by = .(tokenpersonid)]
diagnoses[, termination := max(termination), by = .(tokenpersonid)]
diagnoses[, multiple := max(multiple), by = .(tokenpersonid)]

diagnoses <- diagnoses[, c("tokenpersonid", "stillbirth", "termination", "multiple")]
diagnoses <- diagnoses[!duplicated(diagnoses)]

birth_records <-
  merge(
    birth_records,
    diagnoses,
    by = "tokenpersonid",
    all.x = T
  )

birth_records[stillbirth == T | termination == T | multiple == T, still_termination_multiple := T]
birth_records[, stillbirth := NULL]
birth_records[, termination := NULL]
birth_records[, multiple := NULL]
rm(diagnoses)

# other frields
birth_records[birordr_1 == "" | birordr_1 == "X", birordr_1 := NA]
birth_records[birordr_1 > 1, still_termination_multiple := T]
birth_records[dismeth == 5, still_termination_multiple := T]
birth_records[birstat_1 %in% 2:4, still_termination_multiple := T]
birth_records[numbaby == "" | numbaby == "X", numbaby := NA]
birth_records[numbaby > 1, still_termination_multiple := T]

# We can now drop rows that have exclusions indicated 
table(birth_records$still_termination_multiple, useNA = "always")
birth_records <- birth_records[still_termination_multiple == F]
birth_records[, still_termination_multiple := NULL]

print("Cleaning birth characteristics")
birth_records <- birth_records[, c("tokenpersonid",
                                   "epistart",
                                   "gestat_1",
                                   "birweit_1",
                                   "matage",
                                   "imd04_decile")]

birth_records[gestat_1 == 99, gestat_1 := NA]
birth_records[gestat_1 < 22 | gestat_1 > 45, gestat_1 := NA]

birth_records[birweit_1 == 9999, birweit_1 := NA]
birth_records[birweit_1 < 200, birweit_1 := NA]

birth_records[matage < 10 | matage > 60, matage := NA]

birth_records[, imd_decile := as.integer(NA)]
birth_records[imd04_decile == "Most deprived 10%", imd_decile := 1]
birth_records[imd04_decile == "More deprived 10-20%", imd_decile := 2]
birth_records[imd04_decile == "More deprived 20-30%", imd_decile := 3]
birth_records[imd04_decile == "More deprived 30-40%", imd_decile := 4]
birth_records[imd04_decile == "More deprived 40-50%", imd_decile := 5]
birth_records[imd04_decile == "Less deprived 40-50%", imd_decile := 6]
birth_records[imd04_decile == "Less deprived 30-40%", imd_decile := 7]
birth_records[imd04_decile == "Less deprived 20-30%", imd_decile := 8]
birth_records[imd04_decile == "Less deprived 10-20%", imd_decile := 9]
birth_records[imd04_decile == "Least deprived 10%", imd_decile := 10]
birth_records[, imd04_decile := NULL]

print("Deduplicating")
# will take the first of each but may want to create a flag for conlicting info to drop later
birth_records <- birth_records[order(tokenpersonid, epistart)]

# first dedup on all fields except epistart
birth_records <- birth_records[!duplicated(birth_records[, c("tokenpersonid",
                                                             "gestat_1",
                                                             "birweit_1",
                                                             "matage",
                                                             "imd_decile")])]

birth_records[, gestat_conflict := length(unique(gestat_1[!is.na(gestat_1)])) > 1, by = .(tokenpersonid)]
birth_records[gestat_conflict == F, gestat := gestat_1[!is.na(gestat_1)][1], by = .(tokenpersonid)]

birth_records[, birweit_conflict := length(unique(birweit_1[!is.na(birweit_1)])) > 1, by = .(tokenpersonid)]
birth_records[birweit_conflict == F, birweit := birweit_1[!is.na(birweit_1)][1], by = .(tokenpersonid)]

birth_records[, matage_conflict := length(unique(matage[!is.na(matage)])) > 1, by = .(tokenpersonid)]
birth_records[matage_conflict == F, matage := matage[!is.na(matage)][1], by = .(tokenpersonid)]

birth_records[, imd_conflict := length(unique(imd_decile[!is.na(imd_decile)])) > 1, by = .(tokenpersonid)]
birth_records[imd_conflict == F, imd_decile := imd_decile[!is.na(imd_decile)][1], by = .(tokenpersonid)]

birth_records <- birth_records[!duplicated(tokenpersonid)]

birth_records <- birth_records[, c("tokenpersonid",
                                   "gestat",
                                   "birweit",
                                   "matage",
                                   "imd_decile")]

print("Saving and cleaning up workspace")
save(birth_records, file = "processed/birth_records.rda")

rm(birth_records, births, still, extract_hes_data, diag_cols); gc()
