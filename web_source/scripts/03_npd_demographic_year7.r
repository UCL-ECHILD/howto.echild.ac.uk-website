
print("Getting demographics - year 7 values")

# At year 7:
# FSM
# idaci_rank

dbhandle <- odbcDriverConnect(conn_str)

tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")

keep <- tables$TABLE_NAME[grepl("Autumn|Spring|Summer", tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]

keep <- tables$TABLE_NAME[grepl(paste0(year7_cohorts, collapse = "|"), tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]

keep <- tables$TABLE_NAME[!grepl("Schools", tables$TABLE_NAME) & !grepl("Disability", tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]
rm(keep)

generate_npd_source <- function() {
  npd_source <- data.table()
  
  for(table_name in unique(tables$TABLE_NAME)) {
    
    gc()
    print(paste0("Now doing table: ", table_name))
    
    temp <- sqlQuery(dbhandle, paste0("SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME =  '", table_name, "' AND TABLE_SCHEMA = 'dbo'"))
    temp_columns <- temp$COLUMN_NAME
    temp_columns_lower <- tolower(temp_columns)
    
    pupil_column <- grepl("pupilmatchingrefanonymous", temp_columns_lower)
    fsm_column <- grepl("fsmeligible", temp_columns_lower)
    idaci_column <- grepl("idaci_", temp_columns_lower)
    age_column <- grepl("^ageatstartofacademicyear", temp_columns_lower)
    ncyearactualcolumn <- grepl("ncyearactual", temp_columns_lower)
    
    pupil_column <- temp_columns[pupil_column]
    fsm_column <- temp_columns[fsm_column]
    idaci_column <- temp_columns[idaci_column]
    age_column <- temp_columns[age_column]
    ncyearactualcolumn <- temp_columns[ncyearactualcolumn]
    
    temp <- data.table(
      sqlQuery(
        dbhandle,
        paste0(
          "SELECT ",
          pupil_column , ", ",
          fsm_column, ", ",
          idaci_column,
          " FROM ", table_name, 
          " WHERE ", ncyearactualcolumn, " = '7' OR ",
          " (", ncyearactualcolumn, " = 'X' AND ", age_column, " = '11')")
      )
    )

    colnames(temp) <- c("PupilMatchingRefAnonymous", "fsmeligible_y7", "idaci_decile_y7")
    temp <- temp[PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]
    npd_source <- rbind(npd_source, temp)
    
  }
  
  return(npd_source)
}

demo_y7 <- generate_npd_source()

print("Adding AP and PRU")
ap <- data.table(
  sqlQuery(
    dbhandle,
    paste0(
      "SELECT AP_PupilMatchingRefAnonymous, AP_FSMeligible FROM AP_Census_2008_to_2022
                 WHERE AP_ACADYR IN ('2014/2015', '2015/2016', '2016/2017')
                 AND AP_Age_Start = 11")
  )
)

setnames(ap, c("PupilMatchingRefAnonymous", "fsmeligible_y7"))

ap <- ap[ap$PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]
ap[, idaci_decile_y7 := NA]

ap <- ap[!duplicated((ap$PupilMatchingRefAnonymous))]

demo_y7 <- rbind(demo_y7, ap)
rm(ap)

# pru <- data.table(sqlQuery(dbhandle,
#                            paste0(
#   "SELECT [PRU_PupilMatchingRefAnonymous]
#       ,PRU_FSMeligible
#       ,idaci_2010_rank
#   FROM [dbo].[PRU_Census_2010_to_2013]
#   WHERE [PRU_AcademicYear] = '2012/2013'
#   AND ([PRU_NCyearActual] = '7' OR ([PRU_NCyearActual] = 'X' AND [PRU_AgeAtStartOfAcademicYear] = 11))")))
# 
# setnames(pru, c("PupilMatchingRefAnonymous", "fsmeligible_y7", "idaci_decile_y7"))
# pru <- pru[pru$PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]
# 
# demo_y7 <- rbind(demo_y7, pru)
# rm(pru)


print("Deduplicating")
length(unique(demo_y7$PupilMatchingRefAnonymous)); nrow(demo_y7)

set.seed(200)
demo_y7 <- demo_y7[!duplicated(demo_y7)]
demo_y7[, fsmeligible_y7 := max(fsmeligible_y7), by = PupilMatchingRefAnonymous] # take max fsm (Ever fsm in year)
demo_y7[, idaci_decile_y7 := mode_fun(idaci_decile_y7), by = PupilMatchingRefAnonymous] # modal IDACI
demo_y7 <- demo_y7[!duplicated(demo_y7)]
length(unique(demo_y7$PupilMatchingRefAnonymous)); nrow(demo_y7)


print("Cleaning up workspace")
save(demo_y7, file = "processed/demographics_y7.rda")
rm(demo_y7, dbhandle, tables); gc()
