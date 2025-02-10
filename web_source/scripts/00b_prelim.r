
rm(list = ls()); gc()

birth_cohorts <- 2003:2005
year7_cohorts <- birth_cohorts + 12

conn_str <- "[omitted]"

mode_fun <- function(x) {
  v <- x[!is.na(x)]
  ux <- unique(v)
  tab <- tabulate(match(v, ux))
  md <- ux[tab == max(tab)]
  if (length(md) == 1) {
    return(md)
  } else {
    return(md[which(rmultinom(1, 1, rep(1/length(md), length(md))) == 1)])
  }
}

clean_hes_dates <- function(hes_source) {
  
  hes_source[epistart == "", epistart := NA]
  hes_source[epiend == "", epiend := NA]
  hes_source[admidate == "", admidate := NA]
  hes_source[disdate == "", disdate := NA]
  
  hes_source[, epistart := as.Date(epistart, format = "%Y-%m-%d")]
  hes_source[, epiend := as.Date(epiend, format = "%Y-%m-%d")]
  hes_source[, admidate := as.Date(admidate, format = "%Y-%m-%d")]
  hes_source[, disdate := as.Date(disdate, format = "%Y-%m-%d")]
  
  hes_source[epistart <= as.Date("1801-01-01"), epistart := NA]
  hes_source[epiend <= as.Date("1801-01-01"), epiend := NA]
  hes_source[admidate <= as.Date("1801-01-01"), admidate := NA]
  hes_source[disdate <= as.Date("1801-01-01"), disdate := NA]
  
  # table(is.na(hes_source$admidate) & !is.na(hes_source$epistart)) # small n and all unknown admimeth - drop
  hes_source <- hes_source[!is.na(epistart) & !is.na(admidate)]
  
  # table(is.na(hes_source$admidate) & !is.na(hes_source$epistart))
  # hes_source[is.na(admidate)]$admidate <- hes_source[is.na(admidate)]$epistart
  
  # table(is.na(hes_source$epiend) & !is.na(hes_source$disdate))
  hes_source[is.na(epiend) & !is.na(disdate), epiend := disdate]
  
  #View(hes_source[tokenpersonid %in% hes_source[is.na(epiend)]$tokenpersonid])
  # largely seems to be empty or duplicate info - drop
  hes_source <- hes_source[!is.na(epiend)]  
  
  # check whether admidate > disdate - dates seem the other way around
  # table(hes_source[!is.na(disdate)]$admidate > hes_source[!is.na(disdate)]$disdate)
  
  # they need to be turned around
  hes_source[, admidate_tmp := admidate]
  hes_source[, disdate_tmp := disdate]
  hes_source[, epistart_tmp := epistart]
  hes_source[, epiend_tmp := epiend]
  
  hes_source[admidate_tmp > disdate_tmp & !is.na(disdate)]$epiend <-
    hes_source[admidate_tmp > disdate_tmp & !is.na(disdate)]$epistart_tmp
  
  hes_source[admidate_tmp > disdate_tmp & !is.na(disdate)]$epistart <-
    hes_source[admidate_tmp > disdate_tmp & !is.na(disdate)]$epiend_tmp
  
  hes_source[admidate_tmp > disdate_tmp & !is.na(disdate)]$admidate <-
    hes_source[admidate_tmp > disdate_tmp & !is.na(disdate)]$disdate_tmp
  
  hes_source[admidate_tmp > disdate_tmp & !is.na(disdate)]$disdate <-
    hes_source[admidate_tmp > disdate_tmp & !is.na(disdate)]$admidate_tmp
  
  hes_source[, admidate_tmp := NULL]
  hes_source[, disdate_tmp := NULL]
  hes_source[, epistart_tmp := NULL]
  hes_source[, epiend_tmp := NULL]
  
  # check episode start and end dates are sensible
  # table(hes_source$epistart > hes_source$epiend)
  hes_source[epistart > epiend]$epistart <- hes_source[epistart > epiend]$admidate
  hes_source[epistart > epiend]$epiend <- hes_source[epistart > epiend]$epistart
  
  #table(is.na(hes_source$disdate))
  
  hes_source[, max_disdate := max(disdate, na.rm = T), by = .(tokenpersonid, admidate)]
  hes_source[, max_epiend := max(epiend, na.rm = T), by = .(tokenpersonid, admidate)]
  
  # table(is.na(hes_source$max_disdate))
  hes_source[is.na(max_disdate)]$max_disdate <- hes_source[is.na(max_disdate)]$max_epiend
  
  
  print("Joining spells tegether")
  hes_source <- hes_source[order(tokenpersonid, admidate, disdate, epistart, epiend)]
  hes_source <- hes_source[!duplicated(hes_source)]
  
  hes_source[, epi_n := seq_len(.N), by = rleid(tokenpersonid)]
  hes_source[, admi_n := frank(admidate, ties.method = "dense"), by = tokenpersonid]
  
  flags <- hes_source$max_disdate >= shift(hes_source$admidate, type = "lead")
  flags[hes_source$tokenpersonid != shift(hes_source$tokenpersonid, type = "lead")] <- F
  flags[hes_source$admi_n == shift(hes_source$admi_n, type = "lead")] <- F
  hes_source[, dis_flag := flags]
  rm(flags)
  table(hes_source$dis_flag)
  
  ids_flagged <- unique(hes_source[dis_flag == T]$tokenpersonid)
  ids <- hes_source$tokenpersonid
  admidate <- hes_source$admidate
  disdate <- hes_source$disdate
  max_disdate <- hes_source$max_disdate
  tablen <- max(table(ids, hes_source$dis_flag)[, 2]) + 1
  
  max_spellend <- as.Date(rep(NA, length(ids)))
  
  date_cond <- max_disdate >= shift(admidate, type = "lead")
  id_cond_1 <- ids %in% ids_flagged
  id_cond_2 <- ids == shift(ids, type = "lead")
  
  
  print("Looping...")
  pb <- txtProgressBar(min = 1, max = length(ids) - 1, style = 3)

  for (i in 1:(length(ids) - 1)) {
    if (id_cond_1[i]) {
      if (id_cond_2[i]) {
        index_disdate <- rep(as.Date("1800-01-01"), tablen)
        if (date_cond[i]) {
          j <- i
          k <- i
          n <- 1
          while (max_disdate[j] >= (admidate[j + 1]) & ids[j] == ids[j + 1]) {
            index_disdate[n] <- max_disdate[j]
            j <- j + 1
            n <- n + 1
          }
          index_disdate[n] <- max_disdate[j]
          max_spellend[k:(k+n-1)] <- max(index_disdate)
        }
      }
    }
    setTxtProgressBar(pb, i)
  }
  
  hes_source$spell_end <- max_spellend
  
  hes_source[is.na(spell_end), spell_end := max_disdate]
  hes_source[, dis_flag := NULL]
  
  hes_source[, spell_start := min(admidate), by = .(tokenpersonid, spell_end)]
  hes_source[spell_end < epiend, spell_end := epiend]
  hes_source[, spell_end := max(spell_end), by = .(tokenpersonid, spell_start)]
  
  return(hes_source)
  
}
