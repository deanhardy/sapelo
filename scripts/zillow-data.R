rm(list=ls())

## for implementing zillow scraping later
## https://github.com/notesofdabbler/blog_notesofdabbler/blob/master/learn_rvest/exploreZillow_w_rvest.R
## http://thatdatatho.com/2018/12/14/an-introduction-to-scraping-real-estate-data-with-rvest-and-rselenium/
## http://jonkatz2.github.io/2016/05/13/Web-scraping-with-R

library(rvest)

## define data directory
datadir <- '/Users/dhardy/Dropbox/r_data/sapelo'

url <- read_html("https://www.zillow.com/homes/for_sale/?searchQueryState=%7B%22usersSearchTerm%22%3A%22Sapelo+Island%2C+GA%22%2C%22mapBounds%22%3A%7B%22west%22%3A-81.36082585009765%2C%22east%22%3A-81.12393314990234%2C%22south%22%3A31.371044424632895%2C%22north%22%3A31.54003024291742%7D%2C%22regionSelection%22%3A%5B%7B%22regionId%22%3A54314%2C%22regionType%22%3A6%7D%5D%2C%22mapZoom%22%3A12%2C%22filterState%22%3A%7B%22sortSelection%22%3A%7B%22value%22%3A%22globalrelevanceex%22%7D%7D%2C%22savedSearchEnrollmentId%22%3A%22X1-SS0h1ut76czy191000000000_4hjdl%22%7D")

zID <- url %>%
  html_nodes("article") %>%
  html_attr("id")

addr <- url %>%
  html_nodes("article address") %>%
  html_text()

price <- url %>%
  html_nodes("article .list-card-info .list-card-heading .list-card-price") %>%
  html_text()

date <- url %>%
  html_nodes("article .list-card-top .list-card-variable-text") %>%
  html_text()

link <- url %>%
  html_nodes("article .list-card-info a") %>%
  html_attr("href")

zdata <- do.call(rbind, Map(data.frame, zID=zID, addr=addr, price=price, date=date, link=link))
rownames(zdata) <- c()

parcel_id <- NULL

for(i in zdata$link) {
  OUT <- read_html(i) %>%
    html_nodes("ul .sc-kPVwWT") %>%
    html_text() %>%
    str_split(., "Number: ") %>%
    as.data.frame() %>%
    dplyr::select(starts_with("c")) %>%
    rename(parcel.id = starts_with("c")) %>%
    top_n(-1)
parcel_id <- rbind(parcel_id, OUT)
}

zdata2 <- cbind(zdata, parcel_id) %>%
  rename(parcel_id = parcel.id)

zdata3 <- data.frame(lapply(zdata2, as.character), stringsAsFactors=FALSE)
zdata4 <- zdata3 %>% mutate(parcel_id = str_squish(parcel_id))

write.csv(zdata4, file.path(datadir, 'zdata.csv'))

