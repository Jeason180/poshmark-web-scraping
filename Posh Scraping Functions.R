# Posh Closet Scraping Functions



# Nodes to Text Wrapper
NodesToText <- function(page, tag) {
  html_data <- html_nodes(page, tag)
  text <- html_text(html_data) %>%
    as.character() %>%
    trimws()
  if (length(text) == 0) text <- NA
  return(text)
}



# Function to scrape an individual page
ScrapeItem <- function(item_url) {
  i <<- i + 1
  # cat(paste(i, "\n"))
  if (floor(i / 10) == i / 10) cat(paste("Scraping item", i, "\n"))

  now <- Sys.time()
  webpage <- try(read_html(item_url))
  if (unique(class(webpage) == "try-error")) {
    cat(paste("Item", i, "URL was not accessible, returning NULL \n"))
    return(NULL)
  }

  # Grabbing key information
  title <- NodesToText(webpage, ".listing__title .fw--light")
  brand <- NodesToText(webpage, ".listing__brand")
  price <- NodesToText(webpage, ".listing__ipad-centered h1")
  price <- strsplit(price, "\n")[[1]][1] %>%
    gsub("$", "", ., fixed = T) %>%
    gsub(",", "", ., fixed = T) %>%
    as.numeric()

  size <- NodesToText(webpage, ".listing__size-selector-con")
  description <- NodesToText(webpage, ".listing__description")

  market <- NodesToText(webpage, "a[data-et-name*='listing_details_category']")[1]
  category <- NodesToText(webpage, "a[data-et-name*='listing_details_category']")[2]
  subcategory <- NodesToText(webpage, paste0("a[data-et-name*='listing_details_category'] + .btn"))
  subcategory <- ifelse(length(subcategory == 2), subcategory[2], NA)

  color1 <- NodesToText(webpage, "a[data-et-name*='listing_details_color']")[1]
  color2 <- NodesToText(webpage, "a[data-et-name*='listing_details_color']")[2]

  user <- NodesToText(webpage, ".listing__header a[data-et-name *='username']") %>%
    unique() %>%
    setdiff("")
  last_updated <- NodesToText(webpage, ".header__section .timestamp")[1]


  # Determine whether item is available or sold
  statusnode <- html_nodes(webpage, ".listing__inventory-status")
  availablenode <- html_nodes(webpage, ".commerce-actions__container")

  # Initial settings
  soldstatus <- NA
  availablestatus <- NA

  # If not available, separate into sold vs. not for sale
  if (length(statusnode) > 0) {
    soldtext <- html_text(statusnode) %>% trimws()
    if (soldtext == "This item is sold" | soldtext == "This item is sold out") soldstatus <- "Sold"
    if (soldtext == "Not for sale") soldstatus <- "Not For Sale"
  }

  # If available, mark as available
  if (length(availablenode) > 0) availablestatus <- "Available"

  # If sold is populated, use sold, then check if available is populated
  # If neither one is populated, replace with NA
  status <- ifelse(!is.na(soldstatus), soldstatus,
    ifelse(!is.na(availablestatus), availablestatus, NA)
  )

  # If both are populated, replace with NA
  if (!is.na(soldstatus) & !is.na(availablestatus)) status <- NA




  if (brand == "Meet the Posher" & !is.na(brand)) status <- "Not For Sale"

  itemnode <- html_nodes(webpage, "a[data-et-prop-listing_id]")
  item_id <- html_attr(itemnode[1], "data-et-prop-listing_id")

  imagenode <- html_nodes(webpage, ".carousel__inner .img__container img")[1]
  image_url <- html_attr(imagenode, "src")
  date_posted <- str_extract(image_url, "[[:digit:]]{4}/[[:digit:]]{2}/[[:digit:]]{2}") %>%
    as.Date(format = "%Y/%m/%d")

  nwt_boutique <- NodesToText(webpage, ".listing__title .condition-tag")

  nwt <- ifelse(nwt_boutique == "NWT" & !is.na(nwt_boutique), TRUE, FALSE) %>% ifelse(length(.) == 0, NA, .)
  boutique <- ifelse(nwt_boutique == "Boutique" & !is.na(nwt_boutique), TRUE, FALSE) %>% ifelse(length(.) == 0, NA, .)



  results <- data.frame(
    user = user,
    item_id = item_id,
    date_scraped = now,
    title = title,
    brand = brand,
    category = category,
    subcategory = subcategory,
    price = price,
    status = status,
    date_posted = date_posted,
    last_updated = last_updated,
    nwt = nwt,
    boutique = boutique,
    size = size,
    market = market,
    color1 = color1,
    color2 = color2,
    description = description,
    item_url = item_url,
    stringsAsFactors = F
  )

  if (nrow(results) != 1) cat(paste("Warning: item", title, "does not have 1 entry \n"))

  q <- rnorm(1, sd = 0.5)
  # cat(paste("sleeping", 3+q, "seconds \n"))
  Sys.sleep(3 + q)

  return(results)
}


# Function to scrape each box in the search results
ScrapeSearchResults <- function(node) {
  i <<- i + 1
  if (floor(i / 100) == i / 100) cat(paste("Scraping item", i, "\n"))


  link <- html_nodes(node, ".tile__covershot") %>%
    html_attr("href") %>%
    as.character()

  # If it doesn't work, skip and move on
  if (length(link) == 0) {
    return(NULL)
    next
  }

  # Key info for each item
  title <- NodesToText(node, ".title__condition__container a")
  brand <- NodesToText(node, "[data-et-name=listing_brand]")

  price <- NodesToText(node, ".p--t--1")
  price <- gsub("$", "", price, fixed = T) %>%
    gsub(",", "", ., fixed = T) %>%
    as.numeric()

  size <- NodesToText(node, ".tile__details__pipe__size.ellipses") %>%
    gsub("Size: ", "", ., fixed = T)

  user <- html_nodes(node, ".user-image") %>%
    html_attr("alt")

  item_id <- html_nodes(node, ".tile__covershot") %>%
    html_attr("data-et-prop-listing_id")

  date_posted_raw <- html_nodes(node, ".img__container img") %>%
    html_attr("data-src")

  date_posted <- date_posted_raw %>%
    str_extract("[[:digit:]]{4}/[[:digit:]]{2}/[[:digit:]]{2}") %>%
    as.Date(format = "%Y/%m/%d")

  # Determine whether item is New With Tags or Boutique
  nwtnode <- html_nodes(node, ".condition-tag")
  nwt_text <- html_text(nwtnode) %>% trimws()
  nwt_text <- ifelse(length(nwt_text) == 0, NA, nwt_text)

  nwt <- ifelse(nwt_text == "NWT" & !is.na(nwt_text), TRUE, FALSE)
  boutique <- ifelse(nwt_text == "BOUTIQUE" & !is.na(nwt_text), TRUE, FALSE)

  # Determine if item is sold/sold out, not for sale, or available
  soldnode <- html_nodes(node, ".sold-tag")
  soldoutnode <- html_nodes(node, ".sold-out-tag")
  nfsnode <- html_nodes(node, ".not-for-sale-tag")

  status <- ifelse(length(soldnode) > 0 | length(soldoutnode) > 0, "Sold",
    ifelse(length(nfsnode) > 0, "Not For Sale", "Available")
  )

  if (brand == "Meet the Posher" & !is.na(brand)) status <- "Not For Sale"

  # Collect results into a data frame
  results <- data.frame(
    user = user,
    item_id = item_id,
    title = title,
    brand = brand,
    price = price,
    size = size,
    status = status,
    nwt = nwt,
    boutique = boutique,
    date_posted = date_posted,
    item_url = link,
    stringsAsFactors = F
  )
  if (nrow(results) != 1) cat(paste("Warning: item", title, "does not have 1 entry \n"))

  return(results)
}

## Wrapper Functions
ScrapeSearchResultsWrap <- function(search_url, n = "all") {
  cat(paste("Scraping ", search_url, "\n"))

  webpage <- try(read_html(search_url))
  if (unique(class(webpage) == "try-error")) {
    cat(paste("URL was not accessible, returning URL \n"))
    return(search_url)
  }

  scrape_time <- Sys.time()
  q <- rnorm(1, sd = 0.5)
  Sys.sleep(3 + q)


  box_html <- html_nodes(webpage, ".tile")
  i <<- 0
  num <- ifelse(n == "all", length(box_html), n)
  # cat(paste("Scraping ", num, "items: \n"))

  if (length(box_html) < 16) {
    test_num <- length(box_html)
  } else {
    test_num <- 16
  }

  test_result <- ScrapeSearchResults(box_html[test_num])

  if (length(test_result) > 0) {
    scraped_results <- lapply(box_html[1:num], ScrapeSearchResults) %>% bind_rows()

    scraped_results$scrape_time <- scrape_time
    scraped_results$search_url <- search_url

    return(scraped_results)
  } else {

    # rescrapeurls <<- c(rescrapeurls, search_url)
    # return(NULL)
    return(search_url)
  }
}



# Function to scrape item - wrapper
ScrapeItemWrap <- function(links) {
  i <<- 0
  scraped_results <- lapply(links, ScrapeItem) %>% bind_rows()
  return(scraped_results)
}


# Function to clean the last updated field to get the date sold
DateSold <- function(item, closet) {

  # Function only applies to sold items
  status <- closet %>%
    filter(item_id == item) %>%
    pull(status)
  if (status != "Sold") {
    date_sold <- NA
    return(date_sold)
    next
  }

  updated <- closet %>%
    filter(item_id == item) %>%
    pull(last_updated)
  
  scrape <- closet %>%
    filter(item_id == item) %>%
    pull(date_scraped)
  
  listing_date <- closet %>%
    filter(item_id == item) %>%
    pull(date_posted)

  time_words <- c("hours", "days", "hour", "day", "Yesterday", "mins", "secs", "moments")
  time_regex <- paste(time_words, collapse = "|")

  # Indicator for whether the item was updated [X time] ago
  updated_ago <- grepl(time_regex, updated, ignore.case = TRUE)

  # For listings updated [X time] ago
  if (updated_ago == TRUE) {

    # Extract the unit and amount of time
    unit <- str_extract(updated, time_regex)
    amount <- str_extract(updated, "[[:digit:]]") %>% as.numeric()

    # Adjustment for "an hour ago"
    if (unit == "hour" & is.na(amount) & grepl(" an ", updated)) amount <- 1

    # Calculate date sold
    if (unit == "hours" | unit == "hour") date_sold <- scrape - hours(amount)
    if (unit == "days" | unit == "day") date_sold <- scrape - days(amount)
    if (unit == "mins") date_sold <- scrape - minutes(amount)
    if (unit == "secs") date_sold <- scrape - seconds(amount)
    if (unit == "Yesterday") date_sold <- scrape - days(1)
    if (unit == "moments") date_sold <- scrape

    date_sold <- as.Date(date_sold)
  } else {
    # For listings updated at a particular date/time
    months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    month_regex <- paste(months, collapse = "|")

    # Extract the month from the string and match it to the numerical value
    month <- str_extract(updated, month_regex)
    month <- match(month, month.abb)

    # Extract day and begin by assuming year is the same year as the listing
    day <- str_extract(updated, "[[:digit:]]+") %>% as.numeric()
    year <- year(listing_date)

    # Corrections for leap years
    if (month == 2 & day == 29 & year == 2015) year <- 2016
    if (month == 2 & day == 29 & year == 2019) year <- 2020


    # Combine the pieces
    date_sold <- paste(month, day, year, sep = "-")
    date_sold <- as.Date(date_sold, format = "%m-%d-%Y")

    if (is.na(date_sold)) cat(paste("date sold is NA, item is", item, "\n"))

    # If the imputed sold date is before the actual listing date,
    # then the item must have been sold the next calendar year
    if (date_sold < listing_date) {
      year <- year(listing_date) + 1
      date_sold <- paste(month, day, year, sep = "-")
      date_sold <- as.Date(date_sold, format = "%m-%d-%Y")
    }
  }
  return(date_sold)
}
