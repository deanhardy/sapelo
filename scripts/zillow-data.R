rm(list=ls())

## for implementing zillow scraping later
## https://github.com/notesofdabbler/blog_notesofdabbler/blob/master/learn_rvest/exploreZillow_w_rvest.R
## http://thatdatatho.com/2018/12/14/an-introduction-to-scraping-real-estate-data-with-rvest-and-rselenium/
## http://jonkatz2.github.io/2016/05/13/Web-scraping-with-R

library(rvest)
library(tidyverse)
library(stringr)

## define data directory
datadir <- '/Users/Rebecca/Dropbox/r_data/sapelo'

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
# i <- "https://www.zillow.com/homedetails/LOT-C-Portion-Of-LOT-C-Sapelo-Island-GA-31327/2079288306_zpid/"
i <- "https://www.zillow.com/homedetails/15XA-TR3-E-Perimeter-Rd-Sapelo-Island-GA-31327/2077662942_zpid/"

## usually every time need to verify html_nodes on Zillow
## in Firefox, turn on "inspector" and click on "Parcel Number..." on listing page

css.path <- 'html.zsg-theme-modernized.null.fonts-stage-2 body.actionbar-inline.srp-page-container.zsg-layout_full.responsive-search-page.nav-full-width.tengage.wide.znav-search-bar.map-visible.hdp-double-scroll-layout div#wrapper.main-wrapper div#home-detail-lightbox-container div#search-detail-lightbox.home-detail-lightbox div#details-page-container.detail-page.details-page-container.react.active-view div div.ds-wrapper.znav-force-mobile-layout div#ds-container.ds-container.ds-mobile-single-scroll.ds-container-lightboxed.is-data-forward div.ds-data-col.ds-white-bg.ds-data-col-data-forward div#ds-data-view.c5a12t-0.ffdLrh ul.sc-1f5d78c-0.ieUXpt.ds-data-view-list.zsg-tooltip-viewport li.ds-data-view-item div.ds-home-facts-and-features.reso-facts-features.sheety-facts-features div.sc-19crqy3-2.ccXidy div.sc-19crqy3-4.hKgoUm div.sc-1cravqs-0.jHwnPQ div.sc-1cravqs-2.cmGGck div.sc-1cravqs-3.cuQEKD ul.sc-1cravqs-4.elgsYU li span.Text-aiai24-0.cZksOw'

for(i in zdata$link) {
  OUT <- read_html(i) %>%
    # html_node(css.path)
    # html_nodes('h5 div ul li span')
    # html_nodes('span .Text-aiai24-0.cZksOw')
    # html_nodes('/html/body/div[1]/div[6]/div/div[1]/div/div/div[2]/div[4]/div[6]/ul/li[4]/div/div/div[2]/div[2]/div/div[2]/ul/li[5]/span')
    # html_nodes('h5 div ul li span')
    # html_nodes("ul .sc-1cravqs-4 .elgsYU")
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

## manual repair of pages with info following parcel id
## need to fix in for loop above for long-term
zdata4$parcel_id[2] <- '0101A 0097'

write.csv(zdata4, file.path(datadir, 'zdata.csv'))

