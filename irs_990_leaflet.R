
# Library setup ---------------------------------------------------
library(civis.r.client)
library(dplyr)
library(radiant.data) # etl - which.pmax - rowwise max column 
library(scales) # currency hover text
library(plyr) # rounding to 1000
library(datasets) # state abbreviations
library(leaflet) # interactive mapping
library(geojson) # state spatial data
library(geojsonio) # state spatial data
library(htmlwidgets) # html export

# Download data ---------------------------------------------------
# Org level metadata
coalesced <- read_civis('irs_990.parsed_2017_coalesced', database = 'redshift-general') %>%
  filter(!is.na(ein)) %>% # drop ein NAs 
  mutate(org_state = as.character(org_state))
# Org topic modeling
tfidf <- read_civis('irs_990.scores_2017_tfidf', database = 'redshift-general') %>%
  filter(!is.na(ein)) # drop ein NAs
# State name to state code crosswalk
states_xwalk <- data.frame(code = state.abb, state = state.name) %>%
  rbind(data.frame(code = c('DC', 'PR', 'VI', 'GU', 'AP'), 
                   state = c('District of Columbia', 'Puerto Rico', 'Virgin Islands',
                             'Guam', 'Armed Forces Pacific'))) %>%
  mutate(code = as.character(code), state = as.character(state))

# Create crosstab for topic number and description ---------------------------------------------------
topic_num <- names(tfidf)[2:16]
topic_name <- c('Youth Character Development', 'Sports and Youth Sports', 'Healthcare', 'Economic Development',
                'Veterans and Animals', 'Labor and Workers\' Rights', 'Christianity', 
                'Benefits, Insurance, and Wellbeing', 'Vulnerable Populations', 'Relief and Food Relief',
                'Troubled Teens and Drug Addiction', 'Affordable Housing', 
                'Conservation and the Environment', 'Culture and History', 'Scholarships')
topic_xwalk <- data.frame(tid = seq(0, 14), topic_num, topic_name, stringsAsFactors = FALSE)

# Define most likely topic for each organization
tfidf <- tfidf %>%
  mutate(max_topic = colnames(select(., -ein))[max.col(rowwise(select(., -ein)))],
         primary_topic = as.integer(substr(max_topic, 7, nchar(max_topic))))

# Merge into one dataset
df_all <- coalesced %>%
  left_join(tfidf, by = 'ein') %>%
  left_join(topic_xwalk, by = c("primary_topic" = "tid")) %>%
  select(ein, org_name, primary_topic, topic_name, org_city, org_state, org_zipcode, org_total_revenue, 
         org_type) %>%
  dplyr::rename(topic_num = primary_topic) %>%
  left_join(states_xwalk, by = c('org_state'='code'))

# Total # of non-profits by state
state_totals <- df_all %>%
  group_by(org_state) %>%
  dplyr::summarise(state_orgs = n(),
                   state_rev = sum(org_total_revenue, na.rm = TRUE))

# Largest Number of Orgs by State
# % state organizations with majority topic
state_topics_leaf <- df_all %>%
  group_by(org_state, topic_name) %>%
  dplyr::summarise(topic_orgs = n()) %>%
  group_by(org_state) %>%
  filter(topic_orgs == max(topic_orgs),
         !(org_state %in% c('VI', 'GU', 'AP'))) %>%
  left_join(state_totals, by = "org_state") %>%
  mutate(topic_org_percent = round((topic_orgs/state_orgs)*100,1)) %>%
  left_join(states_xwalk, by = c('org_state'='code')) %>%
  ungroup() %>%
  select(state, topic_popular_name = topic_name, topic_popular_percent = topic_org_percent) %>%
  mutate(state = as.character(state))

# Largerst Average Revenue of Topic by State
# Average state revenue with majority topic
state_rev_leaf <- df_all %>%
  select(ein, topic_name, org_state, org_total_revenue) %>%
  drop_na(ein, org_total_revenue, topic_name) %>%
  group_by(org_state, topic_name) %>%
  dplyr::summarise(topic_rev_avg = mean(org_total_revenue, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(org_state) %>%
  filter(topic_rev_avg == max(topic_rev_avg))  %>% 
  filter(org_state %in% c(state.abb, "DC")) %>%
  left_join(states_xwalk, by = c('org_state'='code')) %>%
  mutate(topic_rev_avg_round = round_any(topic_rev_avg, 1000)) %>%
  ungroup() %>%
  select(state, topic_revenue_name = topic_name, topic_revenue_avg = topic_rev_avg_round) %>%
  mutate(state = as.character(state))


# Leaflet Mapping ---------------------------------------------------
# Load states spatial data
url <- "http://leafletjs.com/examples/choropleth/us-states.js"
# read as text file
doc <- readLines(url)
# remove the javascript assignment at the front 
doc2 <- gsub("var statesData = ", "", doc)
# write out as a temp file and read
write(doc2, file = "tempgeo.json")
states <- geojson_read("tempgeo.json", what = "sp")

# Join irs990 data to states spatial data
states@data <- states@data %>%
  mutate(name = as.character(name)) %>%
  left_join(state_topics_leaf, by = c('name'='state')) %>%
  left_join(state_rev_leaf, by = c('name'='state')) %>%
  filter(name != 'Puerto Rico')

# Set up states map
m <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addTiles()

# Set up color palette
pal <- colorFactor(
  palette = c("#22556F","#1D99B1","#80d0c7","#986679","#98CF6F","#EFEE69","#fcb729", "#929292"),
  levels = c('Vulnerable Populations', 'Affordable Housing', 'Economic Development',
             'Youth Character Development', 'Healthcare', 'Benefits, Insurance, and Wellbeing',
             'Culture and History', 'Labor and Workers\' Rights'))

# Set up custom hover labels
label_popular <- sprintf(
  "<strong>%s</strong><br/>Most frequent topic: %s<br/>Percent of organizations focused on this topic: %g&#37;",
  states$name, states$topic_popular_name, states$topic_popular_percent) %>% 
  lapply(htmltools::HTML)
label_revenue <- sprintf(
  "<strong>%s</strong><br/>Category with largest annual revenue: %s<br/>Avg annual revenue of organizations in this category: $%g million",
  states$name, states$topic_revenue_name, round(states$topic_revenue_avg/1000000,1)) %>% 
  lapply(htmltools::HTML)

# Generate maps
# Most popular topics
plot_popular <- m %>% addPolygons(fillColor = ~pal(topic_popular_name),
                                  weight = 2,  opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
                                  # Highlights
                                  highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.7, 
                                                               bringToFront = TRUE),
                                  # Labels
                                  label = label_popular,
                                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                              textsize = "15px", direction = "auto"))
# Title and legend
plot_popular <- addControl(plot_popular, 
                           html = '<strong><big>Most Frequent Charity and Nonprofit Focus, by State</big></strong>', 
                           position = 'topright') %>%
  addLegend(pal = pal, values = ~topic_popular_name, 
            opacity = 0.7, title = 'Organizational Focus', position = "topright")
plot_popular
saveWidget(plot_popular, file="plot_popular.html")

# Most revenued topics
plot_revenue <- m %>% addPolygons(
  fillColor = ~pal(topic_revenue_name),
  weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
  # Highlights
  highlight = highlightOptions(weight = 5, color = "#666", dashArray = "",
                               fillOpacity = 0.7,bringToFront = TRUE),
  # Labels
  label = label_revenue,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px", direction = "auto"))
# Title and legend
plot_revenue <- addControl(plot_revenue, 
                           html = "<strong><big>Charity and Nonprofit Focus with Greatest Average Revenue, by State</strong></big>",
                           position = "topright") %>%
  addLegend(pal = pal, values = ~topic_revenue_name, opacity = 0.7, title = 'Organizational Focus',
            position = "topright")
plot_revenue
saveWidget(plot_revenue, file="plot_revenue.html")
