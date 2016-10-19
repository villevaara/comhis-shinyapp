
library(stringi)
library(plyr)

# should make use of get_publications_yearly, lots of overlap
get_hits_yearly <- function(catalog_data, years, keyword) {

  years_range <- years[1]:years[2]
  yearly_hits <- vector("integer", length(years_range))
  keyword_hits_yearly <- data.frame(year = years_range,
                                    titles = yearly_hits)
  
  keyword <- tolower(keyword)
    
  items_considered <- subset(catalog_data,
                             publication_year >= years[1] &
                             publication_year <= years[2])

  items_hit <- subset(items_considered,
                      grepl(keyword,
                            tolower(items_considered$title)))

  items_hit_pubyears_count <- count(items_hit$publication_year)
  colnames(items_hit_pubyears_count) <- c("year", "titles")
  
  keyword_hits_yearly$titles <-
    items_hit_pubyears_count[match(keyword_hits_yearly$year,
                                   items_hit_pubyears_count$year), 2]
  # fill na with 0
  keyword_hits_yearly$titles[is.na(keyword_hits_yearly$titles)] <- 0
  
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


get_hits_per_author <- function (catalog_data,
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
    # print(head(authors_count, 10))
    authors_count <- authors_count[order(authors_count$hits,
                                         decreasing = TRUE), ]
    # rownames(authors_count) <- NULL # resets rownames. not needed
    # print(head(authors_count, 10))
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

