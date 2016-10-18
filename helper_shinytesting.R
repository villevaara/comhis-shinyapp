
library(stringi)
library(plyr)

get_publications_yearly <- function(catalog_data) {
  
  catalog_years <- catalog_data$publication_year
  years_range <- 1450:1850
  years_titles <- vector("integer", length(years_range))
  
  i <- 1
  for (year in years_range) {
    years_titles[i] <- length(which(catalog_years == year))
    i <- i + 1 
  }
  
  publications_yearly <- data.frame(years = years_range,
                                            titles = years_titles)

  return(publications_yearly)
}


get_hits_yearly <- function(catalog_data, years, keyword) {

  years_range <- years[1]:years[2]
  yearly_hits <- vector("integer", length(years_range))
  keyword <- tolower(keyword)
    
  items_considered <- subset(catalog_data,
                             publication_year >= years[1] &
                             publication_year <= years[2])

  items_hit <- subset(items_considered,
                      grepl(keyword,
                            tolower(items_considered$title)))

  i <- 1
  for (year in years_range) {
    yearly_hits[i] <- length(which(items_hit$publication_year == year))
    i <- i + 1 
  }

  keyword_hits_yearly <- data.frame(years = years_range,
                                    hits = yearly_hits)
  
  return(keyword_hits_yearly)
}

get_hits_per_author <- function (catalog_data,
                                 years,
                                 keyword,
                                 publications_yearly) {

  get_author_totals <- function(items_hit) {
    authors <- items_hit$author
    authors_count <- count(authors)
    # remove NA
    authors_count <- authors_count[!(is.na(authors_count$x)), ]
    # remove < 4 letter names
    authors_count <- subset(authors_count, stri_length(x) > 3)
    # order, most hits first
    print(head(authors_count, 10))
    authors_count <- authors_count[order(authors_count$freq,
                                         decreasing = TRUE), ]
    colnames(authors_count) <- c("author", "hits")
    print(head(authors_count, 10))
    return(authors_count)
  }

  # years_range <- years[1]:years[2]
  # yearly_hits <- vector("integer", length(years_range))
  keyword <- tolower(keyword)

  items_within_timeframe <- subset(catalog_data,
                               publication_year >= years[1] &
                               publication_year <= years[2])

  items_hit <- subset(items_within_timeframe,
                      grepl(keyword,
                            tolower(items_within_timeframe$title)))

  top_10_authors <- head(get_author_totals(items_hit), 10)

  return(top_10_authors)
}

