
library(stringi)
library(plyr)


get_publications_yearly <- function(catalog_data, years) {
  
  catalog_years <- catalog_data$publication_year
  years_range <- years[1]:years[2]
  years_titles <- vector("integer", length(years_range))
  publications_yearly <- data.frame(year = years_range,
                                    titles = years_titles)
  
  catalog_years_count <- count(catalog_years) # plyr
  colnames(catalog_years_count) <- c("year", "titles")
  # discard out of years_range
  catalog_years_count <- subset(catalog_years_count,
                                year >= years_range[1] &
                                  year <= years_range[length(years_range)])
  
  # combine dataframes to fill in years with zero titles
  publications_yearly$titles <-
    catalog_years_count[match(publications_yearly$year,
                              catalog_years_count$year), 2]
  # fill na with 0
  publications_yearly$titles[is.na(publications_yearly$titles)] <- 0
  
  return(publications_yearly)
}


# should make use of get_publications_yearly, lots of overlap
get_hits_yearly <- function(catalog_data, years, keyword) {

  keyword <- tolower(keyword)
    
  items_considered <- subset(catalog_data,
                             publication_year >= years[1] &
                             publication_year <= years[2])

  items_hit <- subset(items_considered,
                      grepl(keyword,
                            tolower(items_considered$title)))

  keyword_hits_yearly <- get_publications_yearly(items_hit,
                                                 years)
    
  return(keyword_hits_yearly)
}


get_title_hits_average <- function (catalog_data,
                                    years,
                                    keyword,
                                    publications_yearly) {

  hits_yearly <- get_hits_yearly(catalog_data, years, keyword)
  
  publications_yearly_subset <- subset(publications_yearly,
                                       year >= years[1] &
                                       year <= years[2])

  averages_yearly <- hits_yearly
  averages_yearly["totals"] <- publications_yearly_subset["titles"]
  averages_yearly["averages"] <-
    averages_yearly["titles"] / averages_yearly["totals"]
  
  return (averages_yearly)
}


get_top10_authors <- function (catalog_data,
                                 years,
                                 keyword) {

  get_author_totals <- function(items_hit) {
    authors <- items_hit$author
    authors_count <- count(authors) # plyr
    colnames(authors_count) <- c("author", "hits")
    authors_count[, 'author'] <- as.character(authors_count[ ,'author'])
    # remove NA
    authors_count <- authors_count[!(is.na(authors_count$author)), ]
    # remove < 4 letter names
    authors_count <- subset(authors_count,
                            stri_length(author) > 3) # stringi
    # order, most hits first
    authors_count <- authors_count[order(authors_count$hits,
                                         decreasing = TRUE), ]
    # rownames(authors_count) <- NULL # resets rownames. not needed
    return(authors_count)
  }

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

