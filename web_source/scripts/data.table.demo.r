
setwd("[omitted]")
library(data.table)
library(arrow) # for Parquet

# Simulate some data ------------------------------------------------------

n <- 1000000
chc_groups <- c("respiratory", "cardiac", "neurological", "infection", "mental")

set.seed(2104753)

tokenpersonid <- as.character()
for (i in 1:n) {
  tokenpersonid[i] <-
    paste(sample(c(LETTERS, 0:9), 10, replace = T), collapse = "")
}

dt_dataframe <-
  data.frame(
    tokenpersonid = tokenpersonid,
    age_yr = trunc(rnorm(n, 10, 2)),
    female = rbinom(n, 1, 0.45),
    chc_group = sample(chc_groups, n, replace = T),
    epistart = sample(
      seq(as.Date('2000-04-01'), as.Date('2020-03-31'), by = "day"),
      n, replace = T
    )
  )

dt_dataframe$epiend <- dt_dataframe$epistart + rpois(n, 2)

dt_datatable <- data.table(dt_dataframe)

rm(i, n, tokenpersonid)

# Calculate saving times and size on disk
system.time(fwrite(dt_datatable, file = "dt.csv", row.names = F))
system.time(write.csv(dt_datatable, file = "dt_write_csv.csv", row.names = F))
system.time(save(dt_datatable, file = "dt.rda"))

system.time(write_parquet(dt_datatable, sink = "dt.parqet"))
system.time(write_parquet(dt_datatable, sink = "dt.gzip.parqet", compression = "gzip"))


# Code snippets -----------------------------------------------------------

# Return the 1st row and 4th column
dt_dataframe[1, 4]
dt_datatable[1, 4]

# Return just the 1st row
dt_dataframe[1, ]
dt_datatable[1, ]
dt_datatable[1]

# Return just the 4th column
dt_dataframe[, 4]
dt_datatable[, 4]

# Return all columns for a specific group of children (in this case, all girls):
dt_dataframe[dt_dataframe$female == 1, ]
dt_datatable[female == 1]

# Return all rows for specified columns
dt_dataframe[, c("tokenpersonid", "age_yr")]
dt_datatable[, c("tokenpersonid", "age_yr")]
dt_datatable[, .(tokenpersonid, age_yr)]

# Ordering
dt_dataframe <- dt_dataframe[order(dt_dataframe$chc_group, dt_dataframe$epistart), ]
dt_datatable <- dt_datatable[order(chc_group, epistart)]

# Add a new variable
dt_dataframe$age_yr_sq <- dt_dataframe$age_yr ^ 2
dt_datatable[, age_yr_sq := age_yr ^ 2]

# Add a new variable to a data.table in a loop
periods <- seq(365, 365 * 10, by = 365)

for (period in periods) {
  new_var <- paste0("epistart_plus_", period, "_days")
  dt_datatable[, (new_var) := epistart + period]
}

rm(period, periods, new_var)

# Reshape to long
epistart_vars <- names(dt_datatable)[grepl("epistart", names(dt_datatable))]

dt_datatable_long <-
  melt(
    dt_datatable[, c("tokenpersonid", epistart_vars), with = F],
    id.vars = "tokenpersonid",
    value.name = "date"
  )

dt_datatable_long <- dt_datatable_long[order(tokenpersonid)]

# Reshape (back) to wide
dt_datatable_wide <-
  dcast(
    dt_datatable_long,
    formula = tokenpersonid ~ variable
  )


# Messy code
epistart_vars<-names(dt_datatable )[grepl("epistart" ,names(dt_datatable ))]
dt_datatable_long <-melt(dt_datatable[,c("tokenpersonid",epistart_vars),with=F],id.vars="tokenpersonid",value.name="date")
dt_datatable_long<- dt_datatable_long[order(tokenpersonid)]
# Reshape (back) to wide
dt_datatable_wide <-dcast(dt_datatable_long,formula= tokenpersonid ~variable
  )
