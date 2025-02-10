# Binary indicators across primary to year 6:
# Ever FSM 
# SEN

print("Getting demographics - ever FSM in primary to year 6")


years <- 2008:2016
dbhandle <- odbcDriverConnect(conn_str)

tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")

keep <- tables$TABLE_NAME[grepl("Autumn|Spring|Summer", tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]

keep <- tables$TABLE_NAME[grepl(paste0(years, collapse = "|"), tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]

keep <- tables$TABLE_NAME[!grepl("Disability", tables$TABLE_NAME)]
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
    age_column <- grepl("^ageatstartofacademicyear", temp_columns_lower)
    ncyearactualcolumn <- grepl("ncyearactual", temp_columns_lower)
    sen_column <- grepl("senprovision_", temp_columns_lower)
    #urn_column <- grepl("urn", temp_columns_lower)
    
    pupil_column <- temp_columns[pupil_column]
    fsm_column <- temp_columns[fsm_column]
    age_column <- temp_columns[age_column]
    ncyearactualcolumn <- temp_columns[ncyearactualcolumn]
    sen_column <- temp_columns[sen_column]
    #urn_column <- temp_columns[urn_column]
    
    temp <- data.table(
      sqlQuery(
        dbhandle,
        paste0(
          "SELECT ",
          pupil_column , ", ",
          fsm_column, ", ",
          sen_column, #", ",
          # urn_column,
          " FROM ", table_name, 
          " WHERE ", ncyearactualcolumn, " IN ('R', '1', '2', '3', '4', '5', '6') OR ",
          " (", ncyearactualcolumn, " = 'X' AND ", age_column, " < '11')")
      )
    )
    
    
    #colnames(temp) <- c("PupilMatchingRefAnonymous", "fsmeligible", "senprovision", "urn")
    colnames(temp) <- c("PupilMatchingRefAnonymous", "fsmeligible", "senprovision")
    temp <- temp[PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]
    temp[, census := table_name]
    temp <- temp[!duplicated(temp)]
    
    npd_source <- rbind(npd_source, temp)
    
  }
  
  return(npd_source)
}

demographics_ever <- generate_npd_source()


print("Adding AP and PRU")
ap <- data.table(
  sqlQuery(
    dbhandle,
    paste0(
      "SELECT [AP_PupilMatchingRefAnonymous]
   		,[AP_FSMeligible]
   		,[AP_SENProvision]
	    FROM [dbo].[AP_Census_2008_to_2022]
	    WHERE RIGHT(AP_ACADYR, 4) <= 2014
      AND AP_Age_Start < 11")
  )
)

setnames(ap, names(ap), c("PupilMatchingRefAnonymous", "fsmeligible", "senprovision"))

ap <- ap[PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]
ap <- ap[!duplicated(ap)]
#ap[, urn := "AP"]
ap[, census := "AP"]
demographics_ever <- rbind(demographics_ever, ap)
rm(ap)

pru <- data.table(
  sqlQuery(
    dbhandle,
    paste0(
      "SELECT [PRU_PupilMatchingRefAnonymous]
      ,[PRU_FSMeligible]
      ,[PRU_SENprovision]
      FROM [dbo].[PRU_Census_2010_to_2013]
      WHERE [PRU_NCyearActual] IN ('R', '1', '2', '3', '4', '5', '6') OR ([PRU_NCyearActual] = 'X' AND [PRU_AgeAtStartOfAcademicYear] < 11)")
  )
)

setnames(pru, names(pru), c("PupilMatchingRefAnonymous", "fsmeligible", "senprovision"))

pru <- pru[PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]
pru <- pru[!duplicated(pru)]
#pru[, urn := "AP"]
pru[, census := "AP"]
demographics_ever <- rbind(demographics_ever, pru)
rm(pru)


# print("Adding specialist school enrolment")
# gias <- fread("P:/Working/Matt/3_DESCRIPTIVE_STUDY/schools_data/DR200604.02B_public_data_gias_data_25102021_20220614.csv")
# #gias <- gias[URN %in% demographics_ever$urn]
# 
# gias <- gias[, c("URN", "TypeOfEstablishment (name)", "EstablishmentTypeGroup (name)")]
# setnames(gias, names(gias), c("urn", "type1", "type2"))
# 
# gias[, type1 := tolower(type1)]
# gias[, type2 := tolower(type2)]
# 
# gias[, specialist_school := grepl("special|alternative|pupil referral unit", type1) |
#        grepl("special|alternative|pupil referral unit", type2)]
# 
# gias <- gias[specialist_school == T]
# gias[, urn := as.character(urn)]
# 
# demographics_ever <- merge(demographics_ever,
#                            gias[, c("urn", "specialist_school")],
#                            by = "urn",
#                            all.x = T)


print("Cleaning and creating flags")
table(demographics_ever$senprovision, useNA = "always")
demographics_ever[senprovision %in% c("A", "P", "K"), senprovision := "support"]
demographics_ever[senprovision %in% c("E", "e", "S"), senprovision := "sehcp"]
demographics_ever[senprovision == "N", senprovision := NA]
demographics_ever[census %in% c("AP", "PRU"), specialist_school := T]

demographics_ever[, fsm_ever_primary := max(fsmeligible), by = PupilMatchingRefAnonymous]

demographics_ever <- demographics_ever[fsm_ever_primary == 1 | !is.na(senprovision) | !is.na(specialist_school)]

demographics_ever <- demographics_ever[, c("PupilMatchingRefAnonymous",
                                           "fsm_ever_primary",
                                           "senprovision",
                                           "specialist_school")]
demographics_ever <- demographics_ever[!duplicated(demographics_ever)]

print("Cleaning up workspace")
save(demographics_ever, file = "processed/demographics_ever.rda")
rm(tables, demographics_ever, years, gias, dbhandle); gc()
