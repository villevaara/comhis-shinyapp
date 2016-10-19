
library(plyr)

get_publications_yearly <- function(catalog_data) {
  
  catalog_years <- catalog_data$publication_year
  years_range <- 1450:1850
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
