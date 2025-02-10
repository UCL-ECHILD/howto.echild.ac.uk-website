
# load
print("Loading enrolment data")

dbhandle <- odbcDriverConnect(conn_str)

# main censuses
generate_npd_source <- function(years) {

  tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")
  
  keep <- tables$TABLE_NAME[grepl("Autumn|Spring|Summer", tables$TABLE_NAME)]
  tables <- tables[tables$TABLE_NAME %in% keep, ]
  
  keep <- tables$TABLE_NAME[grepl(paste0(years, collapse = "|"), tables$TABLE_NAME)]
  tables <- tables[tables$TABLE_NAME %in% keep, ]
  
  keep <- tables$TABLE_NAME[!grepl("Schools", tables$TABLE_NAME) & !grepl("Disability", tables$TABLE_NAME)]
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
    
    pupil_column <- temp_columns[pupil_column]
    year_column <- temp_columns[year_column]
    
    temp <- data.table(sqlQuery(dbhandle, paste0("SELECT ", pupil_column , ", ", year_column, " FROM ", table_name)))
    temp <- temp[get(pupil_column) %in% cohort_spine$PupilMatchingRefAnonymous]
    
    colnames(temp) <- c("PupilMatchingRefAnonymous", "AcademicYear")
    temp <- temp[!duplicated(temp)]
    temp$census <- table_name
    npd_source <- rbind(npd_source, temp)
    
  }
  
  return(npd_source)
  
}

enrolments <- generate_npd_source(2016:2021)

enrolments[, AcademicYear := as.integer(substr(AcademicYear, 6, 9))]
enrolments[, census := gsub("_.*", "", census)]

enrolments <-
  merge(
    enrolments,
    cohort_spine[, c("PupilMatchingRefAnonymous", "y7_year")],
    by = "PupilMatchingRefAnonymous",
    all.x = T
  )

enrolments[, year := AcademicYear - (y7_year - 7)]

# drop anything other than years 8 to 11
enrolments <- enrolments[year >= 8 & year <= 11]

# AP census
print("Adding AP")
temp <-
  data.table(
    sqlQuery(
      dbhandle,
      paste0("SELECT AP_PupilMatchingRefAnonymous,
                   AP_ACADYR
                   FROM AP_Census_2008_to_2022
                   WHERE AP_ACADYR IN
                   ('2015/2016',
                   '2016/2017',
                   '2017/2018',
                   '2018/2019',
                   '2019/2020',
                   '2020/2021')"
             )
    )
  )

temp[, AcademicYear := as.integer(substr(AP_ACADYR, 6, 9))]
temp[, census := "AP"]

temp <- temp[, c("AP_PupilMatchingRefAnonymous", "AcademicYear", "census")]
setnames(temp, "AP_PupilMatchingRefAnonymous", "PupilMatchingRefAnonymous")

temp <- temp[PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]
temp <- temp[!duplicated(temp)]

print("Subsetting")
temp <-
  merge(
    temp,
    cohort_spine[, c("PupilMatchingRefAnonymous", "y7_year")],
    by = "PupilMatchingRefAnonymous",
    all.x = T
  )

temp[, year := AcademicYear - (y7_year - 7)]

temp <- temp[year >= 8 & year <= 11]

enrolments <- rbind(enrolments, temp)
rm(temp)

enrolments <- enrolments[order(PupilMatchingRefAnonymous, year, census)]

print("Saving")
save(enrolments, file = "processed/enrolments.rda")
rm(enrolments); gc()
