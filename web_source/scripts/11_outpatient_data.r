print("Outpatient data")

extract_hes_data <- function(years) {
  
  dbhandle <- odbcDriverConnect(conn_str)
  
  tables <- subset(sqlTables(dbhandle), TABLE_SCHEM == "dbo")
  
  keep <- tables$TABLE_NAME[grepl("OP_", tables$TABLE_NAME)]
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
          "SELECT TOKEN_PERSON_ID, APPTDATE, FIRSTATT, ATTENDED, TRETSPEF ",
          "FROM ", table_name, " ",
          "WHERE TRETSPEF IN ('191', '241')") # 191 and 241 = pain management services
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

print("Loading OPA data")
opa_data <- extract_hes_data(2008:2016) # Financial years
opa_data <- opa_data[!duplicated(opa_data)]
opa_data[, apptdate := as.Date(apptdate, format = "%Y-%m-%d")]

# drop episodes before reception and beyond year 6 by cohort
opa_data <-
  merge(
    opa_data,
    cohort_spine[, c("tokenpersonid", "y7_year")],
    by = "tokenpersonid",
    all.x = T
  )

opa_data <-
  opa_data[
    apptdate >= paste0(y7_year - 7, "-09-01") & apptdate < paste0(y7_year - 1, "-08-31")
  ]

print("Deduplicate")
opa_data <- opa_data[!duplicated(opa_data)]

print("Saving")
save(opa_data, file = "processed/opa_data.rda")
rm(opa_data); gc()
