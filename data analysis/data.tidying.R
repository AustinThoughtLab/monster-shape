############## Data Formatting Script ##############

library(tidyverse)
library(stringr)

setwd("/Users/beliz/OneDrive/Desktop/Austin Thought Lab/honors thesis/data")


files <- list.files(path='C:/Users/beliz/OneDrive/Desktop/Austin Thought Lab/honors thesis/data') 
df <- data.frame()
for(i in 1:length(files)){
  df0 <- read.csv(files[i]) %>% 
    select(-success, -timeout, -failed_images, -failed_audio, -failed_video, -rt, 
           -time_elapsed, -question_order, -accuracy, -internal_node_id, -correct_response) %>% 
    mutate(correct = as.character(correct))
  id <- df0 %>%
    filter(task_part == "id-question") %>% 
    select(response) %>%
    str_extract("participant_ID.*") %>%
    str_remove("participant_ID\":\"") %>%
    str_remove('"\\}')
  df1 <- df0 %>%
    mutate(id = id)
  df <- bind_rows(df,df1)
}



## get rid of unnecessary columns and reorganize

df2 <- subset(df, trial_type!="video-button-response" & trial_type!= "fullscreen" & trial_type!= "preload"
              & task_part != "demo")
View(df2)

# add pID col

# branch test







# NOTE:
## "A1" = "monkey-shape"
## "A2" = "monster-path"
## "A3" = "robot-path"
## "A4" = "monkey-path"
## "B1" = "alien-shape"
## "B2" = "monster-ball"
## "B3" = "robot-ball"
## "B4" = "alien-ball"
## "C1" = "monster-shape"
## "C2" = "robot-shape"
## "C3" = "monster-hiding"
## "C4" = "robot-hiding"


# dataset without all of the task choices

subdf <- df2 %>% 
  subset(task_part!="day-1" & task_part!= "day-2" & task_part!= "day-3" &
           task_part!="hiding_1" & task_part!= "hiding_2" & task_part!= "hiding_3" &
           task_part!= "followup-day-1" & task_part!="followup-day-2" & task_part!= "followup-day-3")

View(subdf)














