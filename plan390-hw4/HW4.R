library(tidyverse)
library(rvest)
library(purrr)
library(polite)
library(sf)

# Find URLs of senate pages
session = bow("https://www.ncleg.gov/Members/MemberList/S")
res = scrape(session)
elements = html_elements(res, "div.member-col")
Urls = map_dfr(elements, function(element) {
  link = html_element(element, "div.col-8 a") %>% html_attr("href")
  return(list(
    "link" = link
  ))
})
next_page = TRUE
current_page = 1
results = list()
session2 = bow("https://www.ncleg.gov")
while(next_page){
  session = nod(session2, paste0(Urls[current_page,1]))
  res = scrape(session)
  element2 = html_elements(res, "main.col-12.body-content.ncga-container-gutters")
  senate = map_dfr(element2, function(element) {
    name = html_element(element, "h1.section-title") %>% html_text2()
    district = html_element(element, "h6:contains('District')") %>% html_text2()
    term = html_element(element, "div.col-12.col-md-7.col-lg-9.col-xl-6") %>% html_text2()
    return(list(
      "Name" = name,
      "District" = district,
      "Terms" = term
    ))
  })
  results[[current_page]] = senate
  current_page = current_page + 1
  has_next_page = !is.na(html_element(res, "a.pagination__link--next"))
}
all_senate = bind_rows(results)

# Extract political party and terms served in office   
all(str_detect(all_senate$District, "^[[:alnum:]]+ - District [[:alnum:]]+$"))  
all_senate[,c("party", "district")] = str_match(all_senate$District, "^([[:alnum:]]+) - District ([[:alnum:]]+)$")[,2:3]
all(str_detect(all_senate$Terms, "^[0-9]*"), na.rm = T) 
all_senate[,c("Terms_in_Senate")] = str_match(all_senate$Terms, "^[0-9]+")
write_csv(all_senate, "all_senate.csv")
Valid = filter(all_senate, !is.na(all_senate$Terms_in_Senate))

average_term = group_by(Valid, party) %>%
  summarize(Average = mean(as.numeric(Terms_in_Senate))
  )









