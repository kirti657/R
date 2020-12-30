# R
genre <-  read.csv("C:/Users/BALI COMPUTER HOUSE/Desktop/fma-rock-vs-hiphop.csv")
View(genre)
summary(genre)
genre1 <- read.csv("C:/Users/BALI COMPUTER HOUSE/Desktop/echonest.csv")
View(genre1)
head(genre)
head(genre1)

total_genre <- merge(genre,genre1,by="track_id")
View(total_genre)
str(total_genre)
summary(total_genre)
glimpse(total_genre)

num_cols <- unlist(lapply(total_genre,is.numeric))
num_cols
data_num <-total_genre[, num_cols]
data_num
standardise  = function(x){
  return((x - mean(x,na.rm = TRUE ))/sd(x, na.rm = TRUE))
  }
std_data =as.data.frame(apply(data_num,2,standardise))
View(std_data)
mypca <- prcomp(std_data)
mypca
summary(mypca)
plot(mypca)
install.packages("tidyverse")
library(tidyverse)
predict(mypca) %>% round(2)
plot(mypca , type = 'l')
biplot(mypca, scale = 0)
str(mypca)


genre2 <- cbind(std_data,mypca$x)
head(genre2)
library(ggplot2)

ggplot(genre2, aes(PC1, PC2)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point()+
  geom_smooth()


#md.pattern(total_genre)
#install.packages("pacman")
#pacman :: p_load(pacman, dplyr,GGally,ggplot2,ggthemes,ggvis,httr,lubridate,plotly,rio,rmarkdown,shiny,stringr,tidyr)
#install.packages("rjson")

#library("rjson")
#result <- fromJSON(file = "C:/Users/BALI COMPUTER HOUSE/Desktop/echonest-metrics.json")
#json_data_frame <- as.data.frame(result)
#print(json_data_frame)
#View(result)
#head(result)
#joined_genre <- merge(genre,result, by.x = "track_id", 
 #                  by.y = "Code", all.x = TRUE, all.y = FALSE)
install.packages("caTools")
library(caTools)
set.seed(123)
split <- sample.split(std_data,SplitRatio = 0.65)
split
training_new <- subset(total_genre,split==TRUE)
View(training_new)
test_new <- subset(total_genre,split==FALSE)
View(test_new)


#feature scaling 
#training_new[,1:2]=scale(training_new[,1:2])
#View(training_new)
#test_new[,1:2]=scale(test_new[,1:2])
#View(test_new)

classifier =glm(formula= track_id~bit_rate+comments+composer+date_created+date_recorded+duration+favorites+genre_top+genres+genres_all+information+interest+language_code+license+listens+lyricist+number+publisher+tags+title+X+acousticness+danceability+energy+instrumentalness+liveness+speechiness+tempo+valence,
                family(binomial(link = "logit")),
                data = training_new)
classifier

regressor = lm(formula = data_num ~,
               data = training_new )
y_pred = predict(regressor, newdata = test_set)
total_genre %>%
  select(data_num) %>%
  scale() %>%
  cor() %>%
  corrplot::corrplot(method = 'color', 
                     order = 'hclust', 
                     type = 'upper', 
                     diag = FALSE, 
                     tl.col = 'black',
                     addCoef.col = "grey30",
                     number.cex = 0.6,
                     col = colorRampPalette(colors = c(
                       kp_cols('red'), 
                       'white', 
                       kp_cols('dark_blue')))(200),
                     main = 'Audio Feature Correlation',
                     mar = c(2,2,2,2),
                     family = 'Avenir')

avg_genre_matrix <- total_genre %>%
    group_by(num_cols) %>%
    summarise_if(is.numeric, median, na.rm = TRUE) %>%
    ungroup() 
  
avg_genre_cor <- avg_genre_matrix %>%
    select(data_num, -mode) %>% 
    scale() %>%
    t() %>%
    as.matrix() %>%
    cor() 
  
colnames(avg_genre_cor) <- avg_genre_matrix$genre
row.names(avg_genre_cor) <- avg_genre_matrix$genre
  
  avg_genre_cor %>% corrplot::corrplot(method = 'color', 
                                       order = 'hclust',
                                       type = 'upper',
                                       tl.col = 'black',
                                       diag = FALSE,
                                       addCoef.col = "grey40",
                                       number.cex = 0.75,
                                       col = colorRampPalette(colors = c(
                                         kp_cols('red'), 
                                         'white', 
                                         kp_cols('dark_blue')))(200),
                                       mar = c(2,2,2,2),
                                       main = 'Correlation Between Median Genre Feature Values',
                                       family = 'Avenir')  
playlist_songs_scaled <- data_num %>%
mutate_if(is.numeric, scale)
  
set.seed(123)
training_songs <- sample(1:nrow(data_num), nrow(data_num)*.80, replace = FALSE)
train_set <- playlist_songs_scaled[training_songs, c('genre', data_num)] 
test_set <- playlist_songs_scaled[-training_songs, c('genre', data_num)] 
  
train_resp <- playlist_songs_scaled[training_songs, 'genre']
test_resp <- playlist_songs_scaled[-training_songs, 'genre']

set.seed(123)
model_dt <- rpart(genre ~ ., data = train_set)

rpart.plot(model_dt, 
           type = 5, 
           extra = 104,
           box.palette = list(purple = "#490B32",
                              red = "#9A031E",
                              orange = '#FB8B24',
                              dark_blue = "#0F4C5C",
                              blue = "#5DA9E9",
                              grey = '#66717E'),
           leaf.round = 0,
           fallen.leaves = FALSE, 
           branch = 0.3, 
           under = TRUE,
           under.col = 'grey40',
           family = 'Avenir',
           main = 'Genre Decision Tree',
           tweak = 1.2)  
#checking accuracy
accuracy_rf %>%
rbind(accuracy_dt) %>%
rbind(accuracy_gb) %>%
filter(match == TRUE) %>%
select(model, accuracy) %>%
mutate(accuracy = percent(accuracy,2)) %>%
knitr::kable()

