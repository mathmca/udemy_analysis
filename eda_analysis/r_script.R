library(tidyverse)
library(ggplot2)
library(janitor)
library(GGally)
library(gridExtra)
library(ggwordcloud)
library(glue)

# --Set working directory--
setwd("./dataset")


original <- read.csv("udemy_courses.csv")
original_df <- data.frame(original)

final_df <- mutate(original_df,
  year = as.integer(format(as.Date(published_timestamp, format="%Y-%m-%d"), "%Y")),
  month = as.integer(format(as.Date(published_timestamp, format="%Y-%m-%d"), "%m")),
  lecture_duration = round((content_duration/num_lectures) * 100, 0)) %>%
  select(!c(1, 2, 3, 4, 11)) %>%
  clean_names()

aggone <- final_df %>%
  group_by(year, subject) %>%
  summarise(
    count = n(),
    avg_price = mean(price, na.rm = TRUE),
    subscribers = sum(num_subscribers, na.rm = TRUE),
    nsubject_subs = round(subscribers / count, 0)
  ) %>%
  dplyr::filter(year > '2011')

# Chart 1
p1 <- group_by(final_df, subject) %>%
  summarise(subs = sum(num_subscribers, na.rm = TRUE)) %>% 
  ggplot(mapping=aes(x = subject, y = subs)) +
  geom_col(width = 0.4, fill = "purple") + 
  geom_text(aes(label = subs),  vjust=1.3, color="white", size = 4) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(title = "Amount of Subscribers by Suject", y = NULL, x = NULL)

p2 <- group_by(final_df, subject) %>%
  summarise(count = n()) %>% 
  ggplot(mapping=aes(x = subject, y = count)) +
  geom_col(width = 0.4, fill = "purple") + 
  geom_text(aes(label = count),  vjust = 1.3, color = "white", size = 4) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  labs(title = "Total Amount of Courses", y = NULL, x = NULL)

p12 <- grid.arrange(p1, p2, ncol = 2)

# Chart 2

p3 <- final_df %>% 
  group_by(month, subject) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  ggplot(mapping=aes(x = month, y = avg_price)) +
  geom_line(aes(color = subject), size = 1) +
  theme(legend.position = "bottom") +
  annotate("rect", xmin = 6, xmax = 7, ymin =35, ymax =100,
           alpha = .2) +
  annotate("segment", x = 11, y = 35, xend = 11, yend = 93,
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 11, y = 95, label = "Peak") +
  scale_x_discrete(limit = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                            "Aug", "Oct", "Nov", "Dec")) +
  labs(title = "Price Sazonality by Subject", x = NULL, y = "Average Price")

# Chart 3

p4 <- final_df %>% 
  dplyr::filter(price == 0) %>% 
  ggplot(mapping = aes(x = year)) +
  geom_bar(fill = "purple", width = 0.3, alpha = 0.4, color = "purple") +
  labs(title = "Free courses by Year", x = NULL, y = NULL)

# Chart 4
# positively skewed distribution (most prices between 0 and 50)
p5 <- ggpairs(final_df, columns = c(1:4, 10), aes(color = subject, alpha = 0.3),
        upper = list(continuous = "points"))

# Chart 5
p6 <- final_df %>% 
  ggplot(mapping=aes(x=subject, price)) +
  geom_violin(fill="purple", color="purple", alpha=0.4) +
  facet_wrap(~level, ncol = 2, nrow = 2) +
  labs(title = "Price Distribution", x = NULL, y = NULL)

# Chart 6
p7 <- final_df %>% 
  ggplot(mapping=aes(x = fct_rev(fct_infreq(level)))) +
  geom_bar(color="purple", fill="purple", alpha=0.4) +
  facet_wrap(~ subject) +
  labs(title = "Number of Courses by Level", x = NULL, y = NULL)


# Word cloud
words <- head(arrange(select(original_df, c(2, 5)), desc(price)), 100)[1]

splitter <- function(column) {
  final <- c()
  
  for (i in 1:length(column[[1]])) {
  value <- column[[1]][i]
  strg <- strsplit(value, " ")[[1]]
  final <- append(final, strg)
  }
  
  return(final)
}

words_df <- as.data.frame(list(words = splitter(words)))
word_tracker <- words_df %>% 
  group_by(words) %>% 
  summarise(count = n())

set.seed(42)
ggplot(word_tracker, mapping=aes(label = words, size=count)) +
  geom_text_wordcloud(aes(
    color = factor(sample.int(2, nrow(word_tracker), replace = TRUE))
  )) +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#1E2327", color="#1E2327"))
  # labs(title = "Most Expensive Topics") +
  # theme(
  #   plot.background = element_rect(fill = "#262323"),
  #   panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(),
  #   plot.title = element_text(size = 18, hjust=.5,
  #                             color = "white"))


# Saving plots
