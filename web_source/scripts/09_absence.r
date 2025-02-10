
print("Loading absence data")

generate_npd_source <- function(years) {
  
  dbhandle <- odbcDriverConnect(conn_str)
  tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")
  
  keep <- tables$TABLE_NAME[grepl("Absence", tables$TABLE_NAME)]
  tables <- tables[tables$TABLE_NAME %in% keep, ]
  keep <- tables$TABLE_NAME[grepl(paste0(years, collapse = "|"), tables$TABLE_NAME)]
  tables <- tables[tables$TABLE_NAME %in% keep, ]
  
  npd_source <- data.table()
  
  for(table_name in unique(tables$TABLE_NAME)) {
    
    gc()
    print(paste0("Now doing table: ", table_name))
    
    temp <- sqlQuery(dbhandle, paste0("SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME =  '", table_name, "' AND TABLE_SCHEMA = 'dbo'"))
    temp_columns <- temp$COLUMN_NAME
    temp_columns_lower <- tolower(temp_columns)
    
    pupil_column <- grepl("pupilmatchingrefanonymous", temp_columns_lower)
    year_column <- grepl("^academicyear", temp_columns_lower) # ^ = starting with as there are some other cols with AcademicYear in the name
    
    sessions_possible_aut_column <- grepl("sessionspossible_autumn", temp_columns_lower)
    sessions_possible_spr_column <- grepl("sessionspossible_spring", temp_columns_lower)
    sessions_possible_sum_column <- grepl("sessionspossible_summer_", temp_columns_lower)
    sessions_possible_sum6_column <- grepl("sessionspossible_summer6th", temp_columns_lower)
    
    auth_aut_column <- grepl("^authorisedabsence_autumn", temp_columns_lower)
    auth_spr_column <- grepl("^authorisedabsence_spring", temp_columns_lower)
    auth_sum_column <- grepl("^authorisedabsence_summer_", temp_columns_lower)
    auth_sum6_column <- grepl("^authorisedabsence_summer6th", temp_columns_lower)
    
    unauth_aut_column <- grepl("^unauthorisedabsence_autumn", temp_columns_lower)
    unauth_spr_column <- grepl("^unauthorisedabsence_spring", temp_columns_lower)
    unauth_sum_column <- grepl("^unauthorisedabsence_summer_", temp_columns_lower)
    unauth_sum6_column <- grepl("^unauthorisedabsence_summer6th", temp_columns_lower)
    
    overall_aut_column <- grepl("^overallabsence_autumn", temp_columns_lower)
    overall_spr_column <- grepl("^overallabsence_spring", temp_columns_lower)
    overall_sum_column <- grepl("^overallabsence_summer_", temp_columns_lower)
    overall_sum6_column <- grepl("^overallabsence_summer6th", temp_columns_lower)
    
    illness_aut_column <- grepl("termlyreasoni_autumn", temp_columns_lower)
    illness_spr_column <- grepl("termlyreasoni_spring", temp_columns_lower)
    illness_sum_column <- grepl("termlyreasoni_summer_", temp_columns_lower)
    illness_sum6_column <- grepl("termlyreasoni_summer6th_", temp_columns_lower)
    
    appts_aut_column <- grepl("termlyreasonm_autumn", temp_columns_lower)
    appts_spr_column <- grepl("termlyreasonm_spring", temp_columns_lower)
    appts_sum_column <- grepl("termlyreasonm_summer_", temp_columns_lower)
    appts_sum6_column <- grepl("termlyreasonm_summer6th_", temp_columns_lower)
    
    pupil_column <- temp_columns[pupil_column]
    year_column <- temp_columns[year_column]
    
    sessions_possible_aut_column <- temp_columns[sessions_possible_aut_column]
    sessions_possible_spr_column <- temp_columns[sessions_possible_spr_column]
    sessions_possible_sum_column <- temp_columns[sessions_possible_sum_column]
    sessions_possible_sum6_column <- temp_columns[sessions_possible_sum6_column]
    
    auth_aut_column <- temp_columns[auth_aut_column]
    auth_spr_column <- temp_columns[auth_spr_column]
    auth_sum_column <- temp_columns[auth_sum_column]
    auth_sum6_column <- temp_columns[auth_sum6_column]
    
    unauth_aut_column <- temp_columns[unauth_aut_column]
    unauth_spr_column <- temp_columns[unauth_spr_column]
    unauth_sum_column <- temp_columns[unauth_sum_column]
    unauth_sum6_column <- temp_columns[unauth_sum6_column]
    
    overall_aut_column <- temp_columns[overall_aut_column]
    overall_spr_column <- temp_columns[overall_spr_column]
    overall_sum_column <- temp_columns[overall_sum_column]
    overall_sum6_column <- temp_columns[overall_sum6_column]
    
    illness_aut_column <- temp_columns[illness_aut_column]
    illness_spr_column <- temp_columns[illness_spr_column]
    illness_sum_column <- temp_columns[illness_sum_column]
    illness_sum6_column <- temp_columns[illness_sum6_column]
    
    appts_aut_column <- temp_columns[appts_aut_column]
    appts_spr_column <- temp_columns[appts_spr_column]
    appts_sum_column <- temp_columns[appts_sum_column]
    appts_sum6_column <- temp_columns[appts_sum6_column]
    
    temp <- data.table(sqlQuery(dbhandle, paste0("SELECT ",
                                                 pupil_column , ", ",
                                                 year_column, ", ",
                                                 
                                                 sessions_possible_aut_column, ", ",
                                                 sessions_possible_spr_column, ", ",
                                                 sessions_possible_sum_column, ", ",
                                                 sessions_possible_sum6_column, ", ",
                                                 
                                                 auth_aut_column, ", ",
                                                 auth_spr_column, ", ",
                                                 auth_sum_column, ", ",
                                                 auth_sum6_column, ", ",
                                                 
                                                 unauth_aut_column, ", ",
                                                 unauth_spr_column, ", ",
                                                 unauth_sum_column, ", ",
                                                 unauth_sum6_column, ", ",
                                                 
                                                 overall_aut_column, ", ",
                                                 overall_spr_column, ", ",
                                                 overall_sum_column, ", ",
                                                 overall_sum6_column, ", ",
                                                 
                                                 illness_aut_column, ", ",
                                                 illness_spr_column, ", ",
                                                 illness_sum_column, ", ",
                                                 illness_sum6_column, ", ",
                                                 
                                                 appts_aut_column, ", ",
                                                 appts_spr_column, ", ",
                                                 appts_sum_column, ", ",
                                                 appts_sum6_column,
                                                 " FROM ", table_name)))
    
    temp <- temp[get(pupil_column) %in% cohort_spine$PupilMatchingRefAnonymous]
    
    colnames(temp) <- c("PupilMatchingRefAnonymous",
                        "AcademicYear",
                        
                        "sessionspossible_aut",
                        "sessionspossible_spr",
                        "sessionspossible_sum",
                        "sessionspossible_sum6",
                        
                        "authorised_aut",
                        "authorised_spr",
                        "authorised_sum",
                        "authorised_sum6",
                        
                        "unauthorised_aut",
                        "unauthorised_spr",
                        "unauthorised_sum",
                        "unauthorised_sum6",
                        
                        "overall_aut",
                        "overall_spr",
                        "overall_sum",
                        "overall_sum6",
                        
                        "illness_aut", 
                        "illness_spr", 
                        "illness_sum", 
                        "illness_sum6",
                        
                        "appts_aut",
                        "appts_spr",
                        "appts_sum",
                        "appts_sum6")
    
    npd_source <- rbind(npd_source, temp)
    
  }
  
  return(npd_source)
  
}

absence <- generate_npd_source(2015:2021)

absence[, AcademicYear := as.integer(substr(AcademicYear, 6, 9))]

print("Subsetting")
absence <-
  merge(
    absence,
    cohort_spine[, c("PupilMatchingRefAnonymous", "y7_year")],
    by = "PupilMatchingRefAnonymous",
    all.x = T
  )

absence[, year := AcademicYear - (y7_year - 7)]

absence <- absence[year >= 7 & year <= 11]

# Deal with dual enrolments
print("Dealing with dual enrolment")
absence <- absence[order(PupilMatchingRefAnonymous, AcademicYear)]

for (i in 3:(ncol(absence)-2)) {
  current_col <- names(absence)[i]
  print(paste0("Now doing col ", i, ": ", current_col))
  absence[, (current_col) := sum(get(current_col), na.rm = T), by = .(PupilMatchingRefAnonymous, AcademicYear)]
}
rm(i)

absence <- absence[!duplicated(absence)]

print("Removing summer term in year 11")
sum_cols <- names(absence)
sum_cols <- sum_cols[grepl("sum", sum_cols)]

for (sum_col in sum_cols) {
  absence[year == 11, (sum_col) := 0]
}

rm(sum_col, sum_cols)


print("Calculating annual absence")
absence[, sessionspossible_annual := sum(sessionspossible_aut,
                                         sessionspossible_spr,
                                         sessionspossible_sum,
                                         sessionspossible_sum6,
                                         na.rm = T),
        by = .(PupilMatchingRefAnonymous, AcademicYear)]

absence[, authorised_annual := sum(authorised_aut,
                                   authorised_spr,
                                   authorised_sum,
                                   authorised_sum6,
                                   na.rm = T),
        by = .(PupilMatchingRefAnonymous, AcademicYear)]

absence[, unauthorised_annual := sum(unauthorised_aut,
                                     unauthorised_spr,
                                     unauthorised_sum,
                                     unauthorised_sum6,
                                     na.rm = T),
        by = .(PupilMatchingRefAnonymous, AcademicYear)]

absence[, overall_annual := sum(overall_aut,
                                overall_spr,
                                overall_sum,
                                overall_sum6,
                                na.rm = T),
        by = .(PupilMatchingRefAnonymous, AcademicYear)]

absence[, illness_annual := sum(illness_aut,
                                illness_spr,
                                illness_sum,
                                illness_sum6,
                                na.rm = T),
        by = .(PupilMatchingRefAnonymous, AcademicYear)]

absence[, appts_annual := sum(appts_aut,
                              appts_spr,
                              appts_sum,
                              appts_sum6,
                              na.rm = T),
        by = .(PupilMatchingRefAnonymous, AcademicYear)]

absence[, sessionspossible_aut := NULL]
absence[, sessionspossible_spr := NULL]
absence[, sessionspossible_sum := NULL]
absence[, sessionspossible_sum6 := NULL]

absence[, authorised_aut := NULL]
absence[, authorised_spr := NULL]
absence[, authorised_sum := NULL]
absence[, authorised_sum6 := NULL]

absence[, unauthorised_aut := NULL]
absence[, unauthorised_spr := NULL]
absence[, unauthorised_sum := NULL]
absence[, unauthorised_sum6 := NULL]

absence[, overall_aut := NULL]
absence[, overall_spr := NULL]
absence[, overall_sum := NULL]
absence[, overall_sum6 := NULL]

absence[, illness_aut := NULL]
absence[, illness_spr := NULL]
absence[, illness_sum := NULL]
absence[, illness_sum6 := NULL]

absence[, appts_aut := NULL]
absence[, appts_spr := NULL]
absence[, appts_sum := NULL]
absence[, appts_sum6 := NULL]

absence[, illness_appts_annual := illness_annual + appts_annual]


print("Dropping where >420 sessions")
# remove records  with > a year's worth of sessions as may be an error (as per DfE methodology)
# a year max 380. 420 is 40 sessions more = 20 days = 4 school weeks
absence <- absence[sessionspossible_annual <= 420]

print("Ensuring overall absence <= sessions possible")
if (all(absence$overall_annual <= absence$sessionspossible_annual)) {
  print("All ok")
} else {
  print("All NOT ok")
}

print("Calculating rates")
absence[, abs_auth_rate := round((authorised_annual / sessionspossible_annual) * 100, 1)]
absence[, abs_unauth_rate := round((unauthorised_annual / sessionspossible_annual) * 100, 1)]
absence[, abs_overall_rate := round((overall_annual / sessionspossible_annual) * 100, 1)]
absence[, pa10 := abs_overall_rate >= 10]

absence[, abs_illness_rate := round((illness_annual / sessionspossible_annual) * 100, 1)]
absence[, abs_appts_rate := round((appts_annual / sessionspossible_annual) * 100, 1)]
absence[, abs_illness_appts_rate := round((illness_appts_annual / sessionspossible_annual) * 100, 1)]

print("Saving")

absence[, authorised_annual := NULL]
absence[, unauthorised_annual := NULL]
absence[, overall_annual := NULL]
absence[, illness_annual := NULL]
absence[, appts_annual := NULL]
absence[, illness_appts_annual := NULL]
absence[, y7_year := NULL]
absence[, AcademicYear := NULL]

save(absence, file = "processed/absence.rda")
rm(absence, i, current_col); gc()
