#' Returns data from the forty eight hour forecast
#'
#' @param x URL to the 48 hour forecast
#'
#' @importFrom rvest read_html
#' @importFrom magrittr %>%
#' @importFrom rvest html_nodes
#' @importFrom rvest html_table
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#'
#' @return A data.frame a data
#' @export
#'
#' @examples
#'
#' url <- "https://forecast.weather.gov/MapClick.php?lat=39.2736&lon=-76.6264&unit=0&lg=english&FcstType=digital"
#' HourlyWeatherForecast <- FortyEightHour(url)
FortyEightHour <- function(x){
  url <- read_html(x)
  nodes <- url %>% html_nodes('body')
  tablenodes <- nodes %>% html_nodes('table')
  TheTable <- tablenodes[5] %>% html_table()
  TheTable <- TheTable[[1]]
  TheTable <- TheTable[-1,]
  #TheTable

  row_indices <- which(TheTable[, 1] == "Date")

  if (length(row_indices) == 2){
    EndFirstTable <- row_indices[[2]]
    LastRow <- nrow(TheTable)

    Table1 <- TheTable[1:(EndFirstTable -1),]
    Table2 <- TheTable[EndFirstTable:LastRow,]

    TheTables <- list(Table1, Table2)
  }

  TheAssembledData <- map(.x=TheTables, .f=TableProcessing) %>% bind_rows()

  return(TheAssembledData)

}

#' Internal for FortyEightHour
#'
#' @param x Passed parameter
#'
#' @importFrom stringr str_split
#' @importFrom magrittr %>%
#' @importFrom dplyr pull
#' @importFrom Luciernaga NameCleanUp
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom tidyselect matches
#'
#' @return A data.frame row
#'
#' @noRd
TableProcessing <- function(x){
  TableX <- x
  col_indices <- which(grepl("/", TableX[1, ]))

  if (!any(col_indices== 2)){
    TheSoleSurvivor <- TableX[1, col_indices[1]]
    TheBits <- str_split(TheSoleSurvivor, "/") %>% unlist()
    TheNiblets <- as.integer(TheBits[2])
    if(TheNiblets != 1){TheNiblets <- TheNiblets -1
    TheNiblets <- as.character(TheNiblets)
    TheNiblets <- paste0("0", TheNiblets)}
    Reassembled <- paste0(TheBits[1], "/", TheNiblets)
    TableX[1,2] <- Reassembled
    col_indices <- which(grepl("/", TableX[1, ]))
  }

  if(length(col_indices) == 2){
    StartIndex <- col_indices[1]
    SecondIndex <- col_indices[2]
  }

  EndIndex <- ncol(TableX)
  TheFirstCol <- TableX[,1]
  TheFirstCol <- data.frame(TheFirstCol)
  TheFirstCol <- TheFirstCol %>% pull(.)
  TheStrings <- c("(%)", "(°F)", "(EDT)", "(mph)")
  TheFirstCol <- NameCleanUp(TheFirstCol, TheStrings)
  TheFirstCol <- NameCleanUp(TheFirstCol, " ")
  TheFirstCol <- data.frame(TheFirstCol)

  TheFirstSet <- TableX[,StartIndex:(SecondIndex - 1)]
  TheSecondSet <- TableX[,SecondIndex:EndIndex]

  FirstDate <- TheFirstSet[1,1]
  TheFirstSet[1,] <- FirstDate
  FirstComplete <- cbind(TheFirstCol, TheFirstSet)
  FirstComplete <- t(FirstComplete)
  FirstComplete <- data.frame(FirstComplete)
  colnames(FirstComplete) <- FirstComplete[1,]
  FirstComplete <- FirstComplete[-1,]
  row.names(FirstComplete) <- NULL

  SecondDate <- TheSecondSet[1,1] #Add Conditional Workaround
  TheSecondSet[1,] <- SecondDate
  SecondComplete <- cbind(TheFirstCol, TheSecondSet)
  SecondComplete <- t(SecondComplete)
  SecondComplete <- data.frame(SecondComplete)
  colnames(SecondComplete) <- SecondComplete[1,]
  SecondComplete <- SecondComplete[-1,]
  row.names(SecondComplete) <- NULL

  if (all(colnames(FirstComplete) %in% colnames(SecondComplete))){
    TheDataFrame <- bind_rows(FirstComplete, SecondComplete)
    TheDataFrame <- try({TheDataFrame <- TheDataFrame %>%
      select(-matches("\\.\\.\\.14"))}, silent = TRUE)
  }

  return(TheDataFrame)
}
