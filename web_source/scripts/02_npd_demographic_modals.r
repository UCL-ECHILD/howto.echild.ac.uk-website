
print("Getting demographics - modal values")

years <- 2008:2017
dbhandle <- odbcDriverConnect(conn_str)

tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")

keep <- tables$TABLE_NAME[grepl("Autumn|Spring|Summer", tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]

keep <- tables$TABLE_NAME[grepl(paste0(years, collapse = "|"), tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]

keep <- tables$TABLE_NAME[!grepl("Schools", tables$TABLE_NAME) & !grepl("Disability", tables$TABLE_NAME)]
tables <- tables[tables$TABLE_NAME %in% keep, ]

rm(keep)

# skip ethnicity in autumn 2012 and summer 2011 onward (variable missing)
skipstr <- paste0(paste0("Autumn_Census_", 2012:2017, collapse = "|"), "|",
                  paste0("Summer_Census_", 2011:2017, collapse = "|"))
skips <- tables$TABLE_NAME[grepl(skipstr, tables$TABLE_NAME)]

generate_npd_source <- function() {
  
  npd_source <- data.table()
  
  for(table_name in unique(tables$TABLE_NAME)) {
    
    gc()
    print(paste0("Now doing table: ", table_name))
    
    temp <- sqlQuery(dbhandle, paste0("SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME =  '", table_name, "' AND TABLE_SCHEMA = 'dbo'"))
    temp_columns <- temp$COLUMN_NAME
    temp_columns_lower <- tolower(temp_columns)
    
    pupil_column <- grepl("pupilmatchingrefanonymous", temp_columns_lower)
    gender_column <- grepl("gender", temp_columns_lower)
    eth_column <- grepl("ethnicgroup", temp_columns_lower)
    language_column <- grepl("languagegroup|firstlanguage", temp_columns_lower)
    year_birth_column <- grepl("yearofbirth", temp_columns_lower)
    month_birth_column <- grepl("monthofbirth", temp_columns_lower)
    
    age_column <- grepl("^ageatstartofacademicyear", temp_columns_lower)
    ncyearactualcolumn <- grepl("ncyearactual", temp_columns_lower)
    
    pupil_column <- temp_columns[pupil_column]
    gender_column <- temp_columns[gender_column]
    eth_column <- temp_columns[eth_column]
    language_column <- temp_columns[language_column]
    year_birth_column <- temp_columns[year_birth_column]
    month_birth_column <- temp_columns[month_birth_column]
    
    age_column <- temp_columns[age_column]
    ncyearactualcolumn <- temp_columns[ncyearactualcolumn]
    
    if (!(table_name %in% skips)) {
      
      temp <- data.table(
        sqlQuery(
          dbhandle,
          paste0(
            "SELECT ",
            pupil_column , ", ",
            gender_column, ", ",
            eth_column, ", ",
            language_column, ", ",
            year_birth_column, ", ",
            month_birth_column,
            " FROM ", table_name, 
            " WHERE ", ncyearactualcolumn, " IN ('R', '1', '2', '3', '4', '5', '6', '7') OR ",
            " (", ncyearactualcolumn, " = 'X' AND ", age_column, " <= '11')")
        )
      )
      
      
    } else {
      
      temp <- data.table(
        sqlQuery(
          dbhandle,
          paste0(
            "SELECT ",
            pupil_column , ", ",
            gender_column, ", ",
            language_column, ", ",
            year_birth_column, ", ",
            month_birth_column,
            " FROM ", table_name, 
            " WHERE ", ncyearactualcolumn, " IN ('R', '1', '2', '3', '4', '5', '6') OR ",
            " (", ncyearactualcolumn, " = 'X' AND ", age_column, " <= '11')")
        )
      )
      
      temp[, ethnicgroupmajor := NA]
      
      col_order <- c(pupil_column,
                     gender_column,
                     "ethnicgroupmajor",
                     language_column,
                     year_birth_column,
                     month_birth_column)
      
      temp <- temp[, ..col_order]
      
    }
    

    colnames(temp) <- c("PupilMatchingRefAnonymous", "gender", "ethnicgroupmajor",
                        "languagegroupmajor", "yearofbirth", "monthofbirth")
    temp <- temp[PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]
    
    npd_source <- rbind(npd_source, temp)
    
  }
  
  return(npd_source)
  
}

demo_modals <- generate_npd_source()

print("Adding AP and PRU")

ap <-
  data.table(
    sqlQuery(
      dbhandle,
      paste0(
        "SELECT [AP_PupilMatchingRefAnonymous]
   		          ,[AP_Gender]
		            ,[AP_EthnicGroupMajor]
		            ,[AP_YearOfBirth]
		            ,[AP_MonthOfBirth]
                FROM [dbo].[AP_Census_2008_to_2022]
                WHERE RIGHT(AP_ACADYR, 4) <= 2017 AND
                AP_Age_Start <= 11"
      )
    )
  )

setnames(ap, names(ap), c("PupilMatchingRefAnonymous",
                          "gender",
                          "ethnicgroupmajor",
                          "yearofbirth",
                          "monthofbirth"))

ap <- ap[PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]
ap[, languagegroupmajor := NA]
ap <- ap[, c("PupilMatchingRefAnonymous",
             "gender",
             "ethnicgroupmajor",
             "languagegroupmajor",
             "yearofbirth",
             "monthofbirth")]

demo_modals <- rbind(demo_modals, ap)
rm(ap)


pru <- data.table(
  sqlQuery(
    dbhandle,
    paste0(
      "SELECT [PRU_PupilMatchingRefAnonymous]
      ,[PRU_Gender]
	    ,[PRU_EthnicGroupMajor]
      ,[PRU_LanguageGroupMajor]
      ,[PRU_YearOfBirth]
      ,[PRU_MonthOfBirth]
      FROM [dbo].[PRU_Census_2010_to_2013] 
      WHERE PRU_NCyearActual in ('R', '1', '2', '3', '4', '5', '6') OR
      (PRU_NCyearActual = 'X' AND PRU_AgeAtStartOfAcademicYear <= '11')"
    )
  )
)

setnames(pru, names(pru), c("PupilMatchingRefAnonymous",
                          "gender",
                          "ethnicgroupmajor",
                          "languagegroupmajor",
                          "yearofbirth",
                          "monthofbirth"))

pru <- pru[PupilMatchingRefAnonymous %in% cohort_spine$PupilMatchingRefAnonymous]
demo_modals <- rbind(demo_modals, pru)
rm(pru)


print("Cleaning")
# Gender
demo_modals[gender %in% c("0", "9"), gender := NA]
demo_modals[, female := ifelse(gender == "F" | gender == "2", 1, 0)]
demo_modals[, gender := NULL]

# Ethnicity
demo_modals[, ethnicgroupmajor := trimws(tolower(ethnicgroupmajor))]
table(demo_modals$ethnicgroupmajor, useNA = "always")

white <- c("any other white background",
           "british (white)",
           "irish (white)",
           "white",
           "irish",
           "whit",
           "white british",
           "wbri",
           "wiri",
           "woth")

black <- c("african (black or black british)",
           "any other black background",
           "carribean (black or black british)",
           "black - african",
           "black - carribean",
           "black - other",
           "african",
           "blac",
           "caribbean",
           "bafr",
           "bcrb",
           "both")

mixed <- c("any other mixed background",
           "white and asian (mixed)",
           "white and black african (mixed)",
           "white and black carribean (mixed)",
           "mixd",
           "white and asian",
           "white and black african",
           "white and black caribbean",
           "moth",
           "mwas",
           "mwba",
           "mwbc")

asian <- c("any other asian background",
           "bangladeshi (asian or asian british)",
           "indian (asian or asian british)",
           "pakistani (asian or asian british)",
           "bangladeshi",
           "indian",
           "pakistani",
           "asia",
           "aban",
           "aind",
           "aoth",
           "apkn")

other <- c("any other ethnic group",
           "chinese (other ethnic group)",
           "chinese",
           "aoeg",
           "chin",
           "gypsy / romany",
           "traveller of irish heritage",
           "chne",
           "ooth",
           "wirt",
           "wrom")

missing <- c("information not obtained",
             "pre 2002 code",
             "refused",
             "uncl",
             "inva",
             "miss",
             "n/a",
             "nobt",
             "refu")

demo_modals[, ethnicgroup := factor(NA,
                                    levels = c("white",
                                               "black",
                                               "mixed",
                                               "asian",
                                               "other"))]

demo_modals[ethnicgroupmajor %in% white, ethnicgroup := "white"]
demo_modals[ethnicgroupmajor %in% black, ethnicgroup := "black"]
demo_modals[ethnicgroupmajor %in% mixed, ethnicgroup := "mixed"]
demo_modals[ethnicgroupmajor %in% asian, ethnicgroup := "asian"]
demo_modals[ethnicgroupmajor %in% other, ethnicgroup := "other"]

# table(demo_modals$ethnicgroupmajor, demo_modals$ethnicgroup, useNA = "always")
demo_modals[, ethnicgroupmajor := NULL]

# Language
#table(demo_modals$languagegroupmajor, useNA = "always")
demo_modals[, languagegroup := factor(NA,
                                      levels = c("English",
                                                 "Not English"))]
demo_modals[languagegroupmajor %in% c("1_ENG", "ENG", "ENB"), languagegroup := "English"]
demo_modals[languagegroupmajor %in% c("2_OTH", "OTH", "OTB"), languagegroup := "Not English"]
# table(demo_modals$languagegroupmajor, demo_modals$languagegroup, useNA = "always")
demo_modals[, languagegroupmajor := NULL]

# Year and month of birth
# table(demo_modals$yearofbirth, useNA = "always")
# table(demo_modals$monthofbirth, useNA = "always")


print("Getting modal values")

set.seed(100)
demo_modals[, female := mode_fun(female), by = PupilMatchingRefAnonymous]
demo_modals[, ethnicgroup := mode_fun(ethnicgroup), by = PupilMatchingRefAnonymous]
demo_modals[, languagegroup := mode_fun(languagegroup), by = PupilMatchingRefAnonymous]
demo_modals[, yearofbirth := mode_fun(yearofbirth), by = PupilMatchingRefAnonymous]
demo_modals[, monthofbirth := mode_fun(monthofbirth), by = PupilMatchingRefAnonymous]

# table(demo_modals$female, useNA = "always")
# table(demo_modals$ethnicgroup, useNA = "always")
# table(demo_modals$languagegroup, useNA = "always")
# table(demo_modals$yearofbirth, useNA = "always")
# table(demo_modals$monthofbirth, useNA = "always")


print("Deduplicating")
demo_modals <- demo_modals[!duplicated(demo_modals)]
length(unique(demo_modals$PupilMatchingRefAnonymous)); nrow(demo_modals)


print("Cleaning up workspace")
save(demo_modals, file = "processed/demographics_modals.rda")
rm(demo_modals, tables, asian, black, dbhandle,
   missing, mixed, other, skips, skipstr,
   white, years); gc()
