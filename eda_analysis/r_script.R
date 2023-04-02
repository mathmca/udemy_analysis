library(tidyverse)
library(ggplot2)
library(janitor)
library(GGally)
library(gridExtra)
library(ggwordcloud)
library(glue)
library(stringi)

# --Set working directory--
setwd("./dataset")


original <- read.csv("udemy_courses.csv")
original_df <- data.frame(original)


# Transformation
final_df <- mutate(original_df,
  year = as.integer(format(as.Date(published_timestamp, format="%Y-%m-%d"), "%Y")),
  month = as.integer(format(as.Date(published_timestamp, format="%Y-%m-%d"), "%m")),
  lecture_duration = round((content_duration/num_lectures) * 100, 0)) %>%
  select(!c(1, 2, 3, 4, 11)) %>%
  clean_names()


# Wrangling
aggone <- final_df %>%
  group_by(year, subject) %>%
  summarise(
    count = n(),
    avg_price = mean(price, na.rm = TRUE),
    subscribers = sum(num_subscribers, na.rm = TRUE),
    nsubject_subs = round(subscribers / count, 0)
  ) %>%
  dplyr::filter(year > '2011')


# Functions
splitter <- function(column) {
  # Returns a vector of words splitted from the column's row.
  final <- c()
  
  for (i in 1:length(column[[1]])) {
    value <- column[[1]][i]
    strg <- strsplit(value, " ")[[1]]
    final <- append(final, tolower(strg))
  }
  
  return(final)
}

get_sample <- function(df, filter_col, filter_val, sample_size) {
  # Return a sample from a specified column extracted from "original_df"
  set.seed(42)
  vector <- sample_n(filter(select(df, c(2, 5, 12)), filter_col == filter_val),
                     sample_size)[1]
  
  return(vector)
}


# Top Courses by Subject
music <- get_sample(original_df, original_df$subject, "Musical Instruments", 680)
webdev <- get_sample(original_df, original_df$subject, "Web Development", 1200)
graphdev <- get_sample(original_df, original_df$subject, "Graphic Design", 603)
busfin <- get_sample(original_df, original_df$subject, "Business Finance", 1195)

subject_words <- list(music = splitter(music),
                      webdev = splitter(webdev),
                      graphdev = splitter(graphdev),
                      busfin = splitter(busfin))
subject_df <- as.data.frame(do.call(cbind, subject_words))

c1 <- subject_df %>% 
  group_by(busfin) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(50)

c2 <- subject_df %>% 
  group_by(music) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(50)

c3 <- subject_df %>% 
  group_by(webdev) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(50)

c4 <- subject_df %>% 
  group_by(graphdev) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  head(50)

# Saving Table
df_wordcount <- cbind(c1, c2, c3, c4)
write.csv(df_wordcount, file = "word-counter.csv")


# Charts

## Top 100 Most Expensive Subjects
top_words <- get_sample(original_df, original_df$price, 200, 100)

words_df <- as.data.frame(list(words = splitter(top_words)))
word_tracker <- words_df %>% 
  group_by(words) %>% 
  summarise(count = n())

## Chart 1
set.seed(42)
w1 <- ggplot(word_tracker, mapping=aes(label = words, size=count)) +
  geom_text_wordcloud(aes(
    color = factor(sample.int(2, nrow(word_tracker), replace = TRUE))
  )) +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#0D1117", color="#0D1117"),
        plot.title = element_text(size = 15, hjust=.5, color = "white")) +
  labs(title = "The 100 Most Expensive Courses")

## Top 100 Cheapest Subjects
least_words <- get_sample(original_df, original_df$price, 20, 100)

words_df2 <- as.data.frame(list(words = splitter(least_words)))
word_tracker2 <- words_df2 %>% 
  group_by(words) %>% 
  summarise(count = n())

# Chart 
set.seed(42)
w2 <- ggplot(word_tracker2, mapping=aes(label = words, size=count)) +
  geom_text_wordcloud(aes(
    color = factor(sample.int(2, nrow(word_tracker2), replace = TRUE))
  )) +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#0D1117", color="#0D1117"),
        plot.title = element_text(size = 15, hjust=.5, color = "white")) +
  labs(title = "Top 100 Cheapest Subjects")


title_conf <- element_text(size = 15, hjust=.5, color = "black")


## Chart 3
p1 <- group_by(final_df, subject) %>%
  summarise(subs = sum(num_subscribers, na.rm = TRUE)) %>% 
  ggplot(mapping=aes(x = subject, y = subs)) +
  geom_col(width = 0.4, fill = "purple") + 
  geom_text(aes(label = subs),  vjust=1.3, color="white", size = 4) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = title_conf) +
  labs(title = "Amount of Subscribers by Subject", y = NULL, x = NULL)

## Chart 4
p2 <- group_by(final_df, subject) %>%
  summarise(count = n()) %>% 
  ggplot(mapping=aes(x = subject, y = count)) +
  geom_col(width = 0.4, fill = "purple") + 
  geom_text(aes(label = count),  vjust = 1.3, color = "white", size = 4) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = title_conf) +
  labs(title = "Total Amount of Courses", y = NULL, x = NULL)

p12 <- grid.arrange(p1, p2, ncol = 2)

## Chart 5
p3 <- final_df %>% 
  group_by(month, subject) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  ggplot(mapping=aes(x = month, y = avg_price)) +
  geom_line(aes(color = subject), size = 1) +
  theme(legend.position = "bottom", plot.title = title_conf) +
  annotate("rect", xmin = 6, xmax = 7, ymin =35, ymax =100,
           alpha = .2) +
  annotate("segment", x = 11, y = 35, xend = 11, yend = 93,
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 11, y = 95, label = "Peak") +
  scale_x_discrete(limit = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                            "Aug", "Oct", "Nov", "Dec")) +
  labs(title = "Price Sazonality by Subject", x = NULL, y = "Average Price")

## Chart 6
p4 <- final_df %>% 
  dplyr::filter(price == 0) %>% 
  ggplot(mapping = aes(x = year)) +
  geom_bar(fill = "purple", width = 0.3, alpha = 0.4, color = "purple") +
  labs(title = "Free courses by Year", x = NULL, y = NULL) +
  theme(plot.title = title_conf)

## Chart 7
### positively skewed distribution (most prices between 0 and 50)
p5 <- ggpairs(final_df, columns = c(1:4, 10), aes(color = subject, alpha = 0.3),
        upper = list(continuous = "points"))

## Chart 8
p6 <- final_df %>% 
  ggplot(mapping=aes(x=subject, price)) +
  geom_violin(fill="purple", color="purple", alpha=0.4) +
  facet_wrap(~level, ncol = 2, nrow = 2) +
  labs(title = "Price Distribution", x = NULL, y = NULL) +
  theme(plot.title = title_conf)

## Chart 9
p62 <- final_df %>% 
  ggplot(mapping=aes(x=subject, price)) +
  geom_violin(fill="purple", color="purple", alpha=0.4) +
  labs(title = "Price Distribution", x = NULL, y = NULL) +
  theme(plot.title = title_conf)

## Chart 10
p7 <- final_df %>% 
  ggplot(mapping=aes(x = fct_rev(fct_infreq(level)))) +
  geom_bar(color="purple", fill="purple", alpha=0.4) +
  facet_wrap(~ subject) +
  labs(title = "Number of Courses by Level", x = NULL, y = NULL) +
  theme(plot.title = title_conf)


# Saving plots

save_plots <- function(plots, file_prefix, file_format = "png", width = 10, height = 6) {
  for (i in seq_along(plots)) {
    file_name <- paste0(file_prefix, i, ".", file_format)
    ggsave(file_name, plot = plots[[i]], width = width, height = height)
  }
}

setwd("../plots")

plots <- list(p3, p4, p5, p6, p62, p7)
save_plots(plots, "graph_")
