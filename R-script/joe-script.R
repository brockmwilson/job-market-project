
# Packages ----

library(pacman)

p_load(ggrepel, data.table, tidyverse)

p_load(data.table, dtplyr, dplyr, ggplot2, tm,
       SnowballC, wordcloud, RColorBrewer,
       knitr, kableExtra, fixest, modelsummary, 
       broom, purrr, Synth, qte, ggthemes, ggridges, here)

# Data ----

joe_data = readxl::read_xlsx("./data/joe_2021.xlsx")

# Settings ----

graph_theme = theme_pander()

# Data Manipulation ----

# Adding new columns

joe_data$date = as.Date(joe_data$Date_Active, format = "%Y-%m-%d")
joe_data$count = 1
joe_data$loc = str_extract_all(joe_data$locations, "[A-Z]{5}", simplify = T)[,1]
joe_data$academic = grepl(joe_data$jp_title, pattern = "Professor|professor")
joe_data$academic2 = grepl(joe_data$jp_title, pattern = "Assistant Professor|Assistant professor|Post|post")
joe_data$field = substr(joe_data$JEL_Classifications, 1, 1)

# Creating different data aggregations

joe_data_agg = joe_data %>%
  arrange(date) %>%
  group_by(date) %>%
  summarise(obs = n()) %>%
  mutate(cumsum = cumsum(obs))

joe_loc_data = joe_data %>%
  arrange(loc, date) %>%
  group_by(date, loc) %>%
  summarise(obs = n()) %>%
  group_by(loc) %>%
  mutate(cumsum = cumsum(obs))

joe_field_data = joe_data %>%
  arrange(field, date) %>%
  group_by(date, field) %>%
  summarise(obs = n()) %>%
  group_by(field) %>%
  mutate(cumsum = cumsum(obs))

joe_acad_data = joe_data %>%
  arrange(academic, date) %>%
  group_by(date, academic) %>%
  summarise(obs = n()) %>%
  group_by(academic) %>%
  mutate(cumsum = cumsum(obs)) %>%
  mutate(cumsum_pct = cumsum/max(cumsum)*100)

joe_acad2_data = joe_data %>%
  arrange(academic2, date) %>%
  group_by(date, academic2) %>%
  summarise(obs = n()) %>%
  group_by(academic2) %>%
  mutate(cumsum = cumsum(obs)) %>%
  mutate(cumsum_pct = cumsum/max(cumsum)*100)

# Visualizations ----

cutoff_date = as.Date("2022-01-01")
ref_date = as.Date("2021-09-25")

ggplot(data = joe_acad2_data %>%
         filter(date < cutoff_date),
       aes(x = date,
           color = factor(academic2),
           y = cumsum)) +
  geom_line() +
  geom_point() +
  graph_theme

ggplot(data = joe_loc_data %>%
         filter(date < cutoff_date),
       aes(x = date,
           y = cumsum)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ loc, scales = "free_y") +
  graph_theme

ggplot(data = joe_field_data %>%
         filter(date < cutoff_date),
       aes(x = date,
           y = cumsum)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ field, scales = "free_y") +
  graph_theme

ggplot(data = joe_data2,
       aes(x = date,
           y = obs)) +
  geom_col(position = "stack") +
  graph_theme

ggplot(data = joe_data2,
       aes(x = date,
           y = cumsum)) +
  geom_col(position = "stack") +
  geom_vline(xintercept = ref_date, color = "red") +
  graph_theme

ggplot(data = joe_data,
       aes(x = date,
           y = cumsum)) +
  geom_line() +
  geom_vline(xintercept = ref_date, color = "red") +
  graph_theme

ggplot(data = joe_data %>%
         filter(date < cutoff_date),
       aes(x = date,
           y = cumsum/max(cumsum))) +
  geom_line() +
  geom_vline(xintercept = ref_date, color = "red") +
  graph_theme

ggplot(data = joe_data %>%
         filter(date < cutoff_date),
       aes(x = date,
           y = cumsum)) +
  geom_line() +
  geom_vline(xintercept = ref_date, color = "red") +
  graph_theme

ggplot(data = joe_data %>%
         filter(date < cutoff_date),
       aes(x = date,
           y = cumsum)) +
  geom_line() +
  geom_vline(xintercept = ref_date, color = "red") +
  graph_theme

# Word Map ----

## All jobs ----

joe_text = paste(joe_data$jp_full_text, collapse = " ")

write_file(joe_text, "./txt/joe_text.txt")

# NOTE EDIT BELOW

joe_text <- Corpus(DirSource("./txt"))

# Strip unnecessary whitespace
joe_text <- tm_map(joe_text, stripWhitespace)
# # Convert to lowercase
joe_text <- tm_map(joe_text, tolower)
# # Remove conjunctions etc.
joe_text <- tm_map(joe_text, removeWords, stopwords("english"))
# # Remove suffixes to the common 'stem'
# joe_text <- tm_map(joe_text, stemDocument)
# # Remove commas etc.
# joe_text <- tm_map(joe_text, removePunctuation)

wordcloud(joe_text
          , scale=c(5,0.5)     # Set min and max scale
          , max.words=100      # Set top n words
          , random.order=FALSE # Words in decreasing freq
          , rot.per=0.35       # % of vertical words
          , use.r.layout=FALSE # Use C++ collision detection
          , colors=brewer.pal(8, "Dark2"))

## Academic jobs ----

# BELOW NEEDS TO BE REDONE

# 
# joe_text_academic = paste(joe_data[joe_data$academic2 == T, ]$jp_full_text, collapse = " ")
# 
# write_file(mooncloud, "./txt/joe_text_academic.txt")
# 
# # NOTE EDIT BELOW
# 
# mooncloud <- Corpus(DirSource("./text_dir"))
# 
# # Make sure it has loaded properly - have a look!
# inspect(mooncloud)
# 
# # Strip unnecessary whitespace
# mooncloud <- tm_map(mooncloud, stripWhitespace)
# # # Convert to lowercase
# mooncloud <- tm_map(mooncloud, tolower)
# # # Remove conjunctions etc.
# mooncloud <- tm_map(mooncloud, removeWords, stopwords("english"))
# # # Remove suffixes to the common 'stem'
# # mooncloud <- tm_map(mooncloud, stemDocument)
# # # Remove commas etc.
# # mooncloud <- tm_map(mooncloud, removePunctuation)
# 
# #(optional) arguments of 'tm' are converting the document to something other than text, to avoid, run this line
# # mooncloud <- tm_map(mooncloud, PlainTextDocument)
# 
# wordcloud(mooncloud
#           , scale=c(5,0.5)     # Set min and max scale
#           , max.words=100      # Set top n words
#           , random.order=FALSE # Words in decreasing freq
#           , rot.per=0.35       # % of vertical words
#           , use.r.layout=FALSE # Use C++ collision detection
#           , colors=brewer.pal(8, "Dark2"))
# 
# # Joe data
# 
