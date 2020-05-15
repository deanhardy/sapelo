rm(list=ls())

## for implementing zillow scraping later
## https://github.com/notesofdabbler/blog_notesofdabbler/blob/master/learn_rvest/exploreZillow_w_rvest.R
## http://thatdatatho.com/2018/12/14/an-introduction-to-scraping-real-estate-data-with-rvest-and-rselenium/

url <- "https://www.zillow.com/homes/
  for_sale/?searchQueryState=%7B%22usersSearchTerm%22%3A%22Sapelo+Island%2C+
  GA%22%2C%22mapBounds%22%3A%7B%22west%22%3A-81.36082585009765%2C%22east%22%3A-
  81.12393314990234%2C%22south%22%3A31.371044424632895%2C%22north%22%3A31.54003024291742
  %7D%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A54314%2C%22regionType%22%3A6%7D%5D%2C%22
  mapZoom%22%3A12%2C%22filterState%22%3A%7B%22sortSelection%22%3A%7B%22value%22%3A%22globalrelevanceex
  %22%7D%7D%2C%22savedSearchEnrollmentId%22%3A%22X1-SS0h1ut76czy191000000000_4hjdl%22%7D"

sapply(2:38, function(x) {
  url <- "https://www.zillow.com/homes/
  for_sale/?searchQueryState=%7B%22usersSearchTerm%22%3A%22Sapelo+Island%2C+
  GA%22%2C%22mapBounds%22%3A%7B%22west%22%3A-81.36082585009765%2C%22east%22%3A-
  81.12393314990234%2C%22south%22%3A31.371044424632895%2C%22north%22%3A31.54003024291742
  %7D%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A54314%2C%22regionType%22%3A6%7D%5D%2C%22
  mapZoom%22%3A12%2C%22filterState%22%3A%7B%22sortSelection%22%3A%7B%22value%22%3A%22globalrelevanceex
  %22%7D%7D%2C%22savedSearchEnrollmentId%22%3A%22X1-SS0h1ut76czy191000000000_4hjdl%22%7D"
  paste0(url, x) }) -> urls

library(RSelenium)

driver <- rsDriver(browser=c("chrome"))
remDr <- driver[["client"]]

df_all <- data.frame()
for(i in 1:(length(urls))) {
  remDr$navigate(paste0(urls[[i]]))
  Sys.Sleep(1)
  links <- remDr$findElements(using = "xpath", value = "//*[@class = 'plink']")
  df <- data.frame(link = unlist(sapply(links, function(x){x$getElementAttribute('href')})))
  Sys.sleep(1)
  df_all <- rbind(df_all, df)
}
