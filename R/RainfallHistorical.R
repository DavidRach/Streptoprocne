#' Processes historical rainfall data
#' 
#' @param url The URL to the weather.gov forecast
#' 
#' @importFrom rvest read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table 
#' @importFrom lubridate ymd
#' @importFrom lubridate hm
#' @importFrom dplyr select
#' @importFrom dplyr relocate
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom lubridate hours
#' 
#' @return A data.frame of data
#' 
#' @export
RainfallHistorical <- function(url){

url <- read_html(url)
nodes <- url |> html_nodes('tbody')
TheTable <- html_table(nodes)

TableNames <- c("Date", "Time", "WindMPH", "VisMi", "Weather",
 "SkyCond", "AirTempF", "Dwpt", "Min6hr", "Max6hr", "RelativeHumidity",
  "WindChillF", "HeatIndexF", "AltimeterIn", "SeaLevelmb", "Precip1hr",
  "Precip3hr", "Precip6hr")

TheTable <- data.frame(TheTable)
colnames(TheTable) <- TableNames

TheTable$Date <- as.character(TheTable$Date)

current_time <- Sys.time()
current_date <- as.Date(current_time)
current_month <- format(as.Date(current_date), "%Y-%m")

if (any(TheTable$Date == 1)) {
  LastMonth <- current_date - months(1)
  LastMonth <- format(LastMonth, "%Y-%m")
  
  if (any(TheTable$Date >= 28)) {
    TheTable$Date <- paste(LastMonth, TheTable$Date, sep = "-")
  } else if (any(TheTable$Date >= 1 & TheTable$Date < 28)) {
    TheTable$Date <- paste(current_month, TheTable$Date, sep = "-")
  }
} else {TheTable$Date <- paste(current_month, TheTable$Date, sep = "-")}

TheTable$Date <- ymd(TheTable$Date)

  
TheInitialData <- TheTable #|> select(Date, Time, AirTempF, Precip1hr)

max_y <- max(TheInitialData$Precip1hr, na.rm = TRUE)

TheInitialData <- TheInitialData |> mutate(datetime = ymd(Date) + hm(Time)) |> 
  relocate(datetime, .before = Date)
TheInitialData$Precip1hr[is.na(TheInitialData$Precip1hr)] <- 0
  
return(TheInitialData)
}
