## ------------------------1.Data Acquisition-----------------------------------

source("https://michael-weylandt.com/STA9750/load_helpers.R")
mp_start(N=01)
load_netflix_data <- function(){
  # Note to students: Make sure you have installed the tidyverse and fs packages
  #                   before beginning this assignment. If you aren't sure,
  #                   simply run `library(tidyverse)` at your R Console and, if
  #                   you don't see an error message, `tidyverse` has been 
  #                   installed. (Same for `fs`) If you still need to install
  #                   either package, use the `install.packages` function as 
  #                   demonstrated in class.
  library(tidyverse)
  library(rvest)
  library(readxl)
  library(fs)
  
  data_dir <- path("data", "mp01")
  
  if(!dir_exists(data_dir)){
    dir_create(data_dir, showWarnings=FALSE, recurse=TRUE)
  }
  
  NETFLIX_URLS <- list(
    `2023H1` = "https://about.netflix.com/news/what-we-watched-a-netflix-engagement-report",
    `2023H2` = "https://about.netflix.com/news/what-we-watched-the-second-half-of-2023",
    `2024H1` = "https://about.netflix.com/news/what-we-watched-the-first-half-of-2024",
    `2024H2` = "https://about.netflix.com/news/what-we-watched-the-second-half-of-2024",
    `2025H1` = "https://about.netflix.com/news/what-we-watched-the-first-half-of-2025",
    `2025H2` = "https://about.netflix.com/news/what-we-watched-the-second-half-of-2025",
    `2026H1` = "https://about.netflix.com/news/what-we-watched-the-first-half-of-2026"
  )
  
  download_if_needed <- function(url, local){
    if(!file_exists(local)){
      download.file(url, local, quiet = TRUE, mode = "wb")
    }
    
    return(local)
  }
  
  NETFLIX_URLS |> 
    map(read_html) |> 
    map(html_element, xpath = "//a[contains(@href, '.xlsx')]") |> 
    map_chr(html_attr, "href") |> 
    enframe("period", "url") |> 
    mutate(path = path(data_dir, paste0(period, ".xlsx")), 
           path = map2(url, path, download_if_needed), 
           data = map(path, read_excel, skip=3, na="*"), 
           data = map(data, \(x) x |> mutate(`Release Date` = as.Date(`Release Date`)))) |>
    unnest(data) |> 
    select(-url, -path) |> 
    mutate(year=str_sub(period, 1, 4), 
           half=str_sub(period, start=6), 
           period_start = make_date(year, if_else(half==1, 1, 7), 1), 
           period_end = make_date(year, if_else(half==1, 6, 12), if_else(half==1, 30, 31))) |> 
    filter(!is.na(`Hours Viewed`)) |> 
    mutate(live_event = !is.na(Runtime) & str_detect(Runtime, "\\d+:\\d+\\*"), 
           clean_time = str_remove(Runtime, fixed("*")),
           Runtime = 60 * hour(hm(clean_time)) + minute(hm(clean_time))) |>
    select(-clean_time) |>
    mutate(Runtime = coalesce(Runtime, mean(Runtime, na.rm=TRUE)), 
           .by=Title, 
           Title = stringi::stri_trans_general(Title, "Latin-ASCII"),
           Title = str_remove(Title, " // .*"), 
           Title = str_remove(Title, "[^[:ascii:]].*"), 
           Title = str_trim(Title)) |>
    filter_out(Title == "") |>
    select(year, 
           period, 
           period_start, 
           period_end, 
           Title, 
           -half, 
           Runtime, 
           Views, 
           `Hours Viewed`, 
           `Release Date`,
           `Available Globally?`, 
           live_event)
}

NETFLIX_DATA <- load_netflix_data()

NETFLIX_DATA

## ------------2.Fix Column Names and Types-------------------------------------

NETFLIX_DATA <- NETFLIX_DATA |>
  rename(
    title = Title,
    runtime = Runtime,
    views = Views,
    hours_viewed = `Hours Viewed`,
    release_date = `Release Date`,
    available_globally = `Available Globally?`
  )

NETFLIX_DATA <- NETFLIX_DATA |>
  mutate(
    year = as.integer(year),
    available_globally = available_globally == "Yes"
  )


##------------3. Exploratory Data Analysis--------------------------------------

glimpse(NETFLIX_DATA)
summary(NETFLIX_DATA)

install.packages("skimr")
library(skimr)
skim(NETFLIX_DATA)


#1. What date range is covered by this data set?
NETFLIX_DATA |>
  summarize(
    start_date = min(period_start),
    end_date = max(period_end)
  ) #The data covers the period from January 1, 2023 through June 30, 2026

#2. How many different shows and movies are covered in this data set?
NETFLIX_DATA |>
  summarize(
    n_titles = n_distinct(title)
  ) #If you include different seasons of shows, there are 24,427 different shows/movies in this data set.

#3. How many total hours have been spent watching shows from the Bridgerton universe?

NETFLIX_DATA |>
  filter(str_detect(title, "Bridgerton")) |> #includes Bridgerton and Queen Charlotte
  summarize(
    bridgerton_total_hours = sum(hours_viewed)
  ) #4,862,800,000 hours have been spent watching the shows from the Bridgerton universe.

#4. How many total hours have been spent watching the five seasons of Stranger Things?

NETFLIX_DATA |>
  filter(str_detect(title, "Stranger Things")) |>
  distinct(title) #First find the actual names of seasons 1-5

NETFLIX_DATA |>
  filter(str_detect(title, "Stranger Things")) |>
  filter_out(str_detect(title, "Beyond Stranger Things")) |>
  filter_out(str_detect(title, "Tales From '85")) |>
  filter_out(str_detect(title, "Fireplace")) |>
  summarize(
    st_total_hours = sum(hours_viewed)
  ) #5,548,500,000 hours have been spent watchning the five seasons of Stranger Things.


#5. What percentage of total watch time was spent on the Top 10 titles of 2026H1?

NETFLIX_DATA |>
  filter(period == "2026H1") |>
  arrange(desc(hours_viewed)) |>
  slice_head(n = 10) |>
  summarize(
    top_10_titles_hours = sum(hours_viewed) #this give us the total hours for the top 10 titles in 2026H1
  ) #4,977,900,000 hours

NETFLIX_DATA |>
  filter(period == "2026H1") |>
  summarize(
    total_hours = sum(hours_viewed) #total hours watched for 2026H1
  ) #74,333,600,000 total hours watched

pct_top_ten = (4977900000 / 74333600000) * 100
pct_top_ten #6.6967%
#The top 10 titles of the first half of 2026 accounted for 6.6967% of the total hours watched.


#6. What fraction of Netflix’s catalog is globally available? (Answer this both by title and weighted by hours viewed)

NETFLIX_DATA |>
  filter(!is.na(available_globally)) |> #this is to filter out the two NaN values we have
  summarize(
    global_fraction = mean(available_globally) #Calculates the fraction (1=True, 0=False)
  ) #35.1% of Netflix's catalog is globally available.

##---------------------4. Exploratory Table Answers-----------------------------
#1.What was the top title (by total hours viewed) in each period and what fraction of total viewership did it capture?

install.packages("gt")
library(gt)

period_totals <- NETFLIX_DATA |>
  group_by(period) |>
  summarize(
    total_hours = sum(hours_viewed) #helps us find total hours per period
  )
top_titles <- NETFLIX_DATA |>
  group_by(period) |>
  summarize(
    title = title[which.max(hours_viewed)],
    hours_viewed = max(hours_viewed) #helps us find maximum hours viewed per period.
  ) #
top_titles

top_titles <- top_titles |>
  left_join(period_totals, by = "period") |>
  mutate(
    fraction = hours_viewed / total_hours
  )
top_titles 

top_titles |>
  select(period, title, fraction) |>
  gt() |>
  cols_label(
    period = "Period",
    title = "Top Title",
    fraction = "Share of Viewership"
  ) |>
  fmt_percent(
    fraction,
    decimals = 1
  ) |>
  tab_header(
    title = md("**Top Netflix Title by Viewing Period**"),
    subtitle = "Share of total watch time captured by the most-watched title"
  )

#2. How has Netflix’s catalog changed in size over the periods for which we have data? You should consider both the number of titles and the total runtime of content.

catalog_size <- NETFLIX_DATA |>
  group_by(period) |>
  summarize(
    n_titles = n_distinct(title),
    total_runtime = sum(runtime, na.rm = TRUE)
  )

catalog_size |>
  gt() |>
  cols_label(
    period = "Period",
    n_titles = "Number of Titles",
    total_runtime = md("Total Runtime (*minutes*)")
  ) |>
  fmt_number(
    n_titles,
    decimals = 0
  ) |>
  fmt_number(
    total_runtime,
    decimals = 0
  ) |>
  tab_header(
    title = md("**Netflix Catalog Size Over Time**"),
    subtitle = "Number of titles and total runtime by reporting period"
  )


#3. Which titles have had the most consistently high viewership across all periods? These shows are likely to be ‘staples’ that viewers fall back on as ‘comfort food’, e.g., re-runs of Friends.

consistent_titles <- NETFLIX_DATA |>
  filter(!is.na(views)) |>
  group_by(title) |>
  summarize(
    avg_views = mean(views),
    min_views = min(views),
    sd_views = sd(views),
    n_periods = n_distinct(period)
  )

consistent_titles |>
  count(n_periods)

consistent_titles |>
  arrange(desc(avg_views))

consistent_titles |>
  filter(n_periods >= 3) |>
  arrange(desc(avg_views))

consistent_high <- consistent_titles |>
  filter(
    n_periods >= 3,
    avg_views >= 20000000,
    min_views >= 10000000,
    sd_views <= 20000000
  ) |>
  arrange(desc(avg_views))

consistent_high
consistent_high |>
  gt() |>
  cols_label(
    title = "Title",
    avg_views = "Average Views",
    min_views = "Minimum Views",
    sd_views = "Standard Deviation",
    n_periods = "Periods"
  ) |>
  fmt_number(
    columns = c(avg_views, min_views, sd_views),
    decimals = 0
  ) |>
  tab_header(
    title = md("**Titles with Consistently High Viewership**"),
    subtitle = "High average and minimum viewership with relatively low variation"
  )

#4.Within each period, what fraction of time was spent on ‘shows’ vs movies?

show_movie |>
  count(show) |>
  gt() |>
  cols_label(
    show = "Show",
    n = "Number of Titles"
  ) |>
  tab_header(
    title = md("**Netflix Content Classification**"),
    subtitle = "Titles classified using runtime and 'Season' heuristics"
  )
