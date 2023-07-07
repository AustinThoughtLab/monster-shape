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

# misc rows added in by script
df2 <- subset(df, trial_type!="video-button-response" & trial_type!= "fullscreen" & trial_type!= "preload"
              & task_part != "demo" & task_part != "id-question" & task_part != "version" 
              & stimulus != "<p>Now I have some questions for you.</p>"
              & stimulus != "<p>Now, we’re going to choose a new game to play.</p><p>We’re going to look at different games, and for each set of games</p><p>you’ll choose the one you want to play the most.</p><p>Here are the options:</p>"
              & stimulus != "<p>Alright, here are the next three options:</p>"
              & stimulus != "<p>Alright, here are the last three options:</p>"
              & stimulus != "<p>Let's play the next game!</p>"
              & stimulus != "<p> Do you want to hear the instructions again or are you ready to play the game?</p>",
)
View(df2)

# fix pid
df2$version <- df2$id

colnames(df2)[colnames(df2) == "version"] ="participant.id"
df2 <- df2 %>%
  select(-id, -followup, -stimulus)

# reorder columns
col_order <- c("participant.id", "condition", "counterbalancing",
               "order", "task_part", "response", "trial_type", "trial_index", "choices", "correct")
lt <- df2[, col_order]

## recode thingsss

# rename counterbalancing and order 
lt$counterbalancing <- recode(lt$counterbalancing, "C1" = "monster","C2" = "robot")

lt$order <- lt$order %>%
  str_remove("o")

# shape game survey responses

lt$response <- lt$response %>%
  str_replace_all("A lot","3") %>%
  str_replace_all("A little","2") %>%
  str_replace_all("Not at all","1") %>%
  str_replace_all("More each day","3") %>%
  str_replace_all("Same each day","2") %>%
  str_replace_all("Fewer each day","1") %>%
  str_replace_all("More", "3") %>% 
  str_replace_all("Same", "2") %>% 
  str_replace_all("Less", "1") %>% 
  str_remove("\\{\"liked\":\"") %>%
  str_remove("\"\\}") %>%
  str_remove("\\{\"learned\":\"") %>%
  str_remove("\"\\}") %>%
  str_remove("\\{\"stars\":\"") %>%
  str_remove("\"\\}") %>%
  str_remove("\\{\"learned-q\":\"") %>%
  str_remove("\"\\}") %>%
  str_remove("\\{\"guesses\":\"")
 
# as.numeric()


options <- df %>%
  filter(task_part=="options") %>%
  select(response,choices) %>%
  filter(choices!='["Continue"]') %>%
  filter(choices!='[]') %>%
  mutate(choices2 = choices %>%
           str_remove('\\[') %>%
           str_remove('\"]') %>%
           str_remove_all('img/opt/opt') %>%
           str_remove_all('\"') %>%
           str_remove_all('.png'))




## if o1 and 2, then "option"
## correct T/F standardized
## task_part == day-1, task_part ==day-2, task_part == day=3 --> choices...
## make text/ data csvs



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
