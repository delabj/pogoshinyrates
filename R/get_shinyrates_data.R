#' Get shiny rates data
#'
#' @description scrapes the data from shinyrates.com and formats it as a dataframe
#'
#' @param page: the page to pull the shinyrates JSON file from (default is https://shinyrates.com/data/rate)
#'
#' @return A data.frame with the following columns:
#'      \item{date_recorded}{ Date the data was pulled}
#'      \item{pokemon_ID}{National Pokédex Number}
#'      \item{pokemon_name}{Name of the Pokemon}
#'      \item{shiny_rate_frac}{Shiny rate as a fraction as pulled from shinyrates.com}
#'      \item{shiny_rate}{Decimal shiny rate calubluated from `shiny_rate_frac`}
#'      \item{sample_size}{Number of that species of pokemon observed in the last 24 hours}
#'
#' @examples
#' get_shinyrates_data()
#'
#' @export
#' @importFrom dplyr %>%
get_shinyrates_data <- function(
  page="https://shinyrates.com/data/rate"
){


  # get the date
  suppressMessages(
    now <- timestamp(
    stamp = Sys.Date(),
    prefix = "",
    suffix = ""
    )
  )



  #scrape the webpage
  rates_df <- scrape_shinyrates_website(page)

  # Format the data
  result <- format_shinyrates_data(rates_df, timestamp=now)

  # Return the data
  return(result)

}


#' Scrapes shinyrates.com
#'
#' @description scrapes the data from shinyrates.com and formats it as a dataframe
#'
#' @param page: the page to pull the shinyrates JSON file from (default is https://shinyrates.com/data/rate)
#'
#' @return A data.frame with the following columns:
#'      \item{pokemon_ID}{National Pokédex Number}
#'      \item{pokemon_name}{Name of the Pokemon}
#'      \item{shiny_rate_frac}{Shiny rate as a fraction as pulled from shinyrates.com}
#'      \item{sample_size}{Number of that species of pokemon observed in the last 24 hours}
#'
#' @examples
#' scrape_shinyrates_website()
#'
#' @export
#' @importFrom dplyr %>%
scrape_shinyrates_website <- function(page){
  # Read in as a list
  rates_list <-  rjson::fromJSON(readLines(page,warn=FALSE))

  #convert to a data.frame
  rates_df <- data.frame(matrix(unlist(rates_list), nrow=length(rates_list), byrow=TRUE))

  #set names
  names(rates_df) <- c("pokemon_ID", "pokemon_name", "shiny_rate_frac", "sample_size")

  return(rates_df)
}



#' Format the shinyrates data
#'
#' @description Formats the data from shinyrates.com
#'
#' @param df: a data frame to format
#' @param timestamp: the specific timestamp to use
#'
#' @return A data.frame with the following columns:
#'      \item{date_recorded}{ Date the data was pulled}
#'      \item{pokemon_ID}{National Pokédex Number}
#'      \item{pokemon_name}{Name of the Pokemon}
#'      \item{shiny_rate_frac}{Shiny rate as a fraction as pulled from shinyrates.com}
#'      \item{shiny_rate}{Decimal shiny rate calubluated from `shiny_rate_frac`}
#'      \item{sample_size}{Number of that species of pokemon observed in the last 24 hours}
#'
#' @examples
#' date <- Sys.Date()
#' df <- scrape_shinyrates_website()
#' format_shinyrates_data(df=df, timestamp=date)
#'
#' @export
#' @importFrom dplyr %>%
format_shinyrates_data <- function(df, timestamp){

  result <- df %>%
    tidyr::separate(
      shiny_rate_frac,
      into = c("numerator", "denominator"),
      sep = "/",
      remove =FALSE
    ) %>%
    dplyr::mutate(
      numerator = sub(",", "", numerator),
      denominator=sub(",", "", numerator),
      shiny_rate = as.numeric(numerator)/as.numeric(denominator),
      date_recorded = timestamp
    ) %>%
    dplyr::select(
      date_recorded,
      pokemon_ID,
      pokemon_name,
      shiny_rate_frac,
      shiny_rate,
      sample_size
    )

  return(result)
}

write_shinyrates_data <- function(df, name="shinyrates.csv" ){

  write.csv(df, name)

}

