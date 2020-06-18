library(pool)
library(data.table)
library(magrittr)
library(ggplot2)
library(writexl)

format_nor_perc <- function(x) paste0(fhiplot::format_nor(x, digits=1),"%")

db_config <- list(
  driver = Sys.getenv("DB_DRIVER", "Sql Server"),
  server = Sys.getenv("DB_SERVER", "dm-prod"),
  db = Sys.getenv("DB_DB", "Sykdomspulsen_surv"),
  port = as.integer(Sys.getenv("DB_PORT", 1433)),
  user = Sys.getenv("DB_USER", "root"),
  password = Sys.getenv("DB_PASSWORD", "example")
)

if(db_config$driver %in% c("ODBC Driver 17 for SQL Server")){
  # linux
  pool <- dbPool(
    drv = odbc::odbc(),
    driver = db_config$driver,
    server = db_config$server,
    database = db_config$db,
    port = db_config$port,
    uid = db_config$user,
    Pwd = db_config$password#,
    #trusted_connection = "yes"
  )
} else if(db_config$driver %in% c("Sql Server")){
  # windows
  pool <- dbPool(
    drv = odbc::odbc(),
    driver = db_config$driver,
    server = db_config$server,
    database = db_config$db,
    port = db_config$port,
    trusted_connection = "yes"
  )
} else {
  pool <- dbPool(
    drv = odbc::odbc(),
    driver = db_config$driver,
    server = db_config$server,
    port = db_config$port,
    user = db_config$user,
    password = db_config$password,
    encoding = "utf8"
  )
}
DBI::dbExecute(pool, glue::glue({"USE {db_config$db};"}))

config_update_dates <- function(config){
  config$start_date_norsyss_standard_weekly <- as.Date("2018-01-01")
  config$start_date <- as.Date("2020-03-09")
  val <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
    dplyr::summarize(date = max(date)) %>%
    dplyr::collect()
  config$max_date_uncertain <- as.Date(val$date[1])
  config$min_date_uncertain <- config$max_date_uncertain-6
}

config <- new.env()
config$ages <- list(
  "total",
  "0-4",
  "5-14",
  "15-19",
  "20-29",
  "30-64",
  "65+"
)

config$small_location_codes <- unique(fhidata::norway_population_b2020[,.(pop=sum(pop)),keyby=.(year,location_code)][pop<500]$location_code)

config_update_dates(config = config)

x <- pool %>% dplyr::tbl("data_norsyss_recent") %>%
  dplyr::distinct(tag_outcome) %>%
  dplyr::collect()

# choices_location_code
x_choices <- fhidata::norway_locations_long_b2020
setorder(x_choices,location_code)
x_choices <- rbind(
  x_choices[granularity_geo=="nation"],
  x_choices[granularity_geo=="county"],
  x_choices[granularity_geo=="municip"],
  x_choices[granularity_geo=="ward"]
)

x_choices[
  fhidata::norway_locations_b2020,
  on="location_code==municip_code",
  county_name := county_name
  ]

x_choices[
  fhidata::norway_locations_ward_b2020,
  on="location_code==ward_code",
  municip_name := municip_name
  ]

x_choices[granularity_geo== "county", location_name := paste0(location_name, " (fylke)")]
x_choices[granularity_geo== "municip", location_name := paste0(location_name, " (kommune i ", county_name,")")]
x_choices[granularity_geo== "ward", location_name := paste0(location_name, " (bydel i ", municip_name,")")]

choices_location_with_ward <- x_choices$location_code
names(choices_location_with_ward) <- x_choices$location_name

choices_location_without_ward <- x_choices[granularity_geo %in% c("nation","county","municip")]$location_code
names(choices_location_without_ward) <- x_choices[granularity_geo %in% c("nation","county","municip")]$location_name

config$choices_location_with_ward <- choices_location_with_ward
config$choices_location_without_ward <- choices_location_without_ward

choices_location_daily <- x_choices[granularity_geo %in% c("nation","county")]$location_code
names(choices_location_daily) <- x_choices[granularity_geo %in% c("nation","county")]$location_name
config$choices_location_daily <- choices_location_daily

config$choices_norsyss_tag <- list(
  "Luftveisinfeksjoner (R05, R74, R78, R83)" = "respiratoryexternal_vk_ot",
  "Magetarm (D11, D70, D73)" = "gastro_vk_ot"
)

get_granularity_geo <- function(location_code){
  granularity_geo <- NULL
  if(location_code == "norge"){
    granularity_geo <- "nation"
  } else if(stringr::str_detect(location_code, "^county")){
    granularity_geo <- "county"
  } else if(stringr::str_detect(location_code, "^municip")){
    granularity_geo <- "municip"
  } else if(stringr::str_detect(location_code, "^ward")){
    granularity_geo <- "ward"
  }
  shiny::req(granularity_geo)
  return(granularity_geo)
}

get_dependent_location_codes <- function(location_code){
  granularity_geo <- get_granularity_geo(location_code = location_code)

  if(location_code %in% c("county03", "municip0301")){
    location_codes <- unique(c("county03", "municip0301", fhidata::norway_locations_ward_b2020[municip_code=="municip0301"]$ward_code))
  } else if(granularity_geo == "nation"){
    location_codes <- c("norge",unique(fhidata::norway_locations_b2020[,.(county_code)])$county_code)
  } else if(granularity_geo == "county"){
    location_codes <- c(location_code, fhidata::norway_locations_b2020[county_code==location_code]$municip_code)
  } else if(granularity_geo == "municip"){
    # this is a municip with ward
    if(location_code %in% c("municip0301")){ #fhidata::norway_locations_b2020$municip_code){
      location_codes <- unique(c(location_code, fhidata::norway_locations_ward_b2020[municip_code==location_code]$ward_code))
    } else {
      x_county_code <- fhidata::norway_locations_b2020[municip_code==location_code]$county_code
      location_codes <- unique(c(location_code, fhidata::norway_locations_b2020[county_code==x_county_code]$municip_code))
    }
  } else if(granularity_geo == "ward"){
    x_municip_code <- fhidata::norway_locations_ward_b2020[ward_code==location_code]$municip_code
    location_codes <- unique(c(
      location_code,
      fhidata::norway_locations_ward_b2020[municip_code==x_municip_code]$ward_code,
      x_municip_code
    ))
  }

  return(location_codes)
}

get_free_days <- function(date_start, date_end){
  dates <- seq.Date(
    from = as.Date(date_start),
    to = as.Date(date_end),
    by = 1
  )
  weekends <- dates[lubridate::wday(dates, week_start = 1) %in% c(6,7)]
  holidays <- fhidata::norway_dates_holidays[date %in% dates & is_holiday==TRUE]$date

  free_days <- sort(unique(c(weekends,holidays)))

  return(free_days)
}
