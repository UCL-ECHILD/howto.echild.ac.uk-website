
print("Loading KS4 data")
dbhandle <- odbcDriverConnect(conn_str)

ks4 <-
  data.table(
    sqlQuery(
      dbhandle,
      paste0(
        "SELECT KS4_PupilMatchingRefAnonymous, 
        KS4_ACADYR, 
        KS4_ATT8, 
        KS4_GLEVEL2EM_PTQ_EE, 
        KS4_EBACC_94
        FROM KS4Pupil_2015_to_2021
        WHERE KS4_ACADYR IN ('2017/2018', '2018/2019', '2019/2020', '2020/2021')"
      )
    )
  )

setnames(ks4, names(ks4), tolower(gsub("KS4_", "", names(ks4))))
ks4 <- ks4[pupilmatchingrefanonymous %in% cohort_spine$PupilMatchingRefAnonymous]

# Check for duplication
ks4 <- ks4[order(pupilmatchingrefanonymous, acadyr)]
# ks4[, nrow := seq_len(.N), by = pupilmatchingrefanonymous]
# View(ks4[pupilmatchingrefanonymous %in% ks4[nrow > 1]$pupilmatchingrefanonymous])

ks4[, att8 := max(att8), by = pupilmatchingrefanonymous]
ks4[, glevel2em_ptq_ee := max(glevel2em_ptq_ee), by = pupilmatchingrefanonymous]
ks4[, ebacc_94 := max(ebacc_94), by = pupilmatchingrefanonymous]

ks4 <- ks4[!duplicated(pupilmatchingrefanonymous),
           c("pupilmatchingrefanonymous",
             "att8",
             "glevel2em_ptq_ee",
             "ebacc_94")]

setnames(ks4, "pupilmatchingrefanonymous", "PupilMatchingRefAnonymous")

print("Saving")
save(ks4, file = "processed/ks4.rda")
rm(ks4, dbhandle); gc()
