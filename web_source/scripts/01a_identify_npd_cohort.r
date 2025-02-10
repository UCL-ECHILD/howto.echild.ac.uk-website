
# School censuses ---------------------------------------------------------

print(paste0("Getting year 7 enrolment for birth cohorts ", paste(birth_cohorts, collapse = ", "),
      "; year 7 in ", paste(year7_cohorts, collapse = ", ")))

dbhandle <- odbcDriverConnect(conn_str)


tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")
keep <- tables$TABLE_NAME[grepl("Autumn|Spring|Summer", tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]

keep <- tables$TABLE_NAME[grepl(paste0(year7_cohorts, collapse = "|"), tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]

keep <- tables$TABLE_NAME[!grepl("Schools", tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]

rm(keep)

generate_npd_source <- function() {
  
  npd_source <- data.table()
  
  for(table_name in unique(tables$TABLE_NAME)) {
    
    gc()
    print(paste0("Now doing table: ", table_name))
    
    temp <-
      sqlQuery(
        dbhandle,
        paste0("SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME =  '", table_name, "'")
      )
    
    temp_columns <- temp$COLUMN_NAME
    temp_columns_lower <- tolower(temp_columns)
    
    pupil_column <- grepl("pupilmatchingrefanonymous", temp_columns_lower)
    age_column <- grepl("^ageatstartofacademicyear", temp_columns_lower)
    ncyearactualcolumn <- grepl("ncyearactual", temp_columns_lower)
    
    pupil_column <- temp_columns[pupil_column]
    age_column <- temp_columns[age_column]
    ncyearactualcolumn <- temp_columns[ncyearactualcolumn]
    
    temp <- data.table(
      sqlQuery(
        dbhandle, paste0(
          "SELECT ",
          pupil_column , ", ",
          age_column, ", ",
          ncyearactualcolumn, " FROM ", table_name, 
          " WHERE ", ncyearactualcolumn, " = '7' OR ",
          " (", ncyearactualcolumn, " = 'X' AND ", age_column, " = '11')")
        )
      )
    
    temp[, census := tolower(table_name)]
    temp[, census_year := gsub("[a-z]*_census_", "", census)]
    temp[, census_term := gsub("_census_[0-9].*", "", census)]
    temp[, census := NULL]
    
    colnames(temp) <-
      c("PupilMatchingRefAnonymous",
        "ageatstartofacademicyear",
        "ncyearactual",
        "census_year",
        "census_term"
      )
    
    npd_source <- rbind(npd_source, temp)
    
  }
  
  npd_source[, census_year := as.integer(census_year)]
  return(npd_source)
}

cohort_spine <- generate_npd_source()

cohort_spine <-
  cohort_spine[
    !duplicated(cohort_spine[, c("PupilMatchingRefAnonymous", "census_year")])
  ]

length(unique(cohort_spine$PupilMatchingRefAnonymous)); nrow(cohort_spine)

cohort_spine[, n_record := seq_len(.N), by = PupilMatchingRefAnonymous]
cohort_spine <- cohort_spine[n_record == 1]
length(unique(cohort_spine$PupilMatchingRefAnonymous)); nrow(cohort_spine)

cohort_spine[, census_term := NULL]
cohort_spine[, n_record := NULL]
cohort_spine[, ageatstartofacademicyear := NULL]
cohort_spine[, ncyearactual := NULL]


# Alternative provision ---------------------------------------------------

ap <-
  data.table(
    sqlQuery(
      dbhandle,
      paste0(
        "SELECT AP_PupilMatchingRefAnonymous, AP_ACADYR FROM AP_Census_2008_to_2022
                 WHERE AP_ACADYR IN ('2014/2015', '2015/2016', '2016/2017')
                 AND AP_Age_Start = 11"
      )
    )
  )

setnames(ap, c("PupilMatchingRefAnonymous", "census_year"))
ap[, census_year := as.integer(substr(census_year, 6, 9))]
ap <- ap[!(ap$PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous)]
length(unique(ap$PupilMatchingRefAnonymous)); nrow(ap)
ap <- ap[!duplicated((ap$PupilMatchingRefAnonymous))]

cohort_spine <- rbind(cohort_spine, ap)
cohort_spine <- cohort_spine[order(PupilMatchingRefAnonymous, census_year)]
length(unique(cohort_spine$PupilMatchingRefAnonymous)); nrow(cohort_spine)



# Pupil referral units ----------------------------------------------------

# We do not need the PRU census as it ended before our cohorts reached year 7

# pru <-
#   data.table(
#     sqlQuery(
#       dbhandle,
#       paste0(
#         "SELECT [PRU_PupilMatchingRefAnonymous] ,[PRU_AcademicYear]
#             FROM [dbo].[PRU_Census_2010_to_2013]
#             WHERE [PRU_AcademicYear] = '2012/2013'
#             AND ([PRU_NCyearActual] = '7' OR ([PRU_NCyearActual] = 'X' AND [PRU_AgeAtStartOfAcademicYear] = 11))"
#         )
#       )
#     )
# 
# setnames(pru, c("PupilMatchingRefAnonymous", "census_year"))
# 
# pru[, census_year := as.integer(substr(census_year, 6, 9))]
# pru <- pru[!(pru$PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous)]
# length(unique(pru$PupilMatchingRefAnonymous)); nrow(pru)
# 
# cohort_spine <- rbind(cohort_spine, pru)
# cohort_spine <- cohort_spine[order(PupilMatchingRefAnonymous, census_year)]
# length(unique(cohort_spine$PupilMatchingRefAnonymous)); nrow(cohort_spine)
# 
# rm(tables, ap, pru); gc()



# Get token person ID -----------------------------------------------------

print("Adding linkage key")

linkage_spine <-
  data.table(
    sqlQuery(
      dbhandle,
      paste0(
        "SELECT * FROM nic381972_echild_bridging"
      )
    )
  )

setnames(
  linkage_spine,
  names(linkage_spine),
  c("PupilMatchingRefAnonymous", "tokenpersonid")
)

linkage_spine <-
  linkage_spine[PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]
length(unique(linkage_spine$PupilMatchingRefAnonymous)); nrow(linkage_spine)
length(unique(linkage_spine$tokenpersonid)); nrow(linkage_spine)

linkage_spine <- linkage_spine[order(tokenpersonid)]
linkage_spine[, nrow := seq_len(.N), by = rleid(tokenpersonid)]
# View(linkage_spine[tokenpersonid %in% linkage_spine[nrow > 1]$tokenpersonid])
linkage_spine <- linkage_spine[nrow == 1]

cohort_spine <-
  merge(
    cohort_spine,
    linkage_spine[, c("PupilMatchingRefAnonymous", "tokenpersonid")],
    by = "PupilMatchingRefAnonymous",
    all.x = T
  )

rm(linkage_spine); gc()

print("Changing census_year to y7_year")
setnames(cohort_spine, "census_year", "y7_year")

print("Saving")
save(cohort_spine, file = "processed/1_cohort_spine_npd.rda")
rm(list = ls()); gc()
