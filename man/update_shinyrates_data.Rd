% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_shinyrates_data.R
\name{update_shinyrates_data}
\alias{update_shinyrates_data}
\title{Updates the shinyrates data}
\usage{
update_shinyrates_data(
  df,
 
    old_file = "https://raw.githubusercontent.com/delabj/pogoshinyrates/master/data/shinyrates.csv"
)
}
\arguments{
\item{df:}{a data frame to write}

\item{old_file:}{filepath to the old version of the shinyrates data (defaults to github)ro}
}
\description{
Updates the data from shinyrates.com
}
\examples{
date <- Sys.Date()
df <- scrape_shinyrates_website()
df <- format_shinyrates_data(df=df, timestamp=date)
newdf <- write_shinyrates_data(df)
update_shinyrates_data(newdf)
}
