library(tidyverse)
library(caret)
library(dslabs)

################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)



# Q1 Rows and columns
dim(edx)
summary(edx)
edx %>% head()
nollat <- edx$rating == 0
sum(nollat)
kolmoset <- edx$rating == 3
sum(kolmoset)
# Mallivastaus
edx %>% filter(rating == 0) %>% tally()
edx %>% filter(rating == 3) %>% tally()

# Q3 how many different movies
edx %>%
  count(n_distinct(movieId))
# Mallivastaus
n_distinct(edx$movieId)

# Q4 how many different users
n_distinct(edx$userId)

# Q5 How many ratings per genre
sum(edx$genres %like% "Drama")
sum(edx$genres %like% "Comedy")
sum(edx$genres %like% "Thriller")
sum(edx$genres %like% "Romance")
# Mallivastaus
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Q6 Greatest number of ratings
ratings <- edx %>% select(title) %>% group_by(title) %>% add_tally()
ratings %>% arrange(desc(n)) %>% head(5)
# Mallivastaus
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Q7 most given ratings
edx %>% group_by(rating) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
# Mallivastaus
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count))  

# Q8 mod
# Laskin edellisest?
# graafinen mallivastaus
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()
