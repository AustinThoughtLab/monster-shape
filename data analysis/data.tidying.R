############## Data Formatting Script ##############

library(tidyverse)
library(dplyr)
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
 
# recode options to be their choices

lt2 <- lt

lt2$choices <- lt2$choices %>%
  str_remove('\\[') %>%
  str_remove('\"]') %>%
  str_remove_all('img/opt/opt') %>%
  str_remove_all('\"') %>%
  str_remove_all('.png') %>% 
  str_remove_all("img/shape/") %>%
  str_replace_all(",img/shape/", " ") 

# after strsplit we should use something like nth(as.numeric(response)+1) to get the right item from the vector

lt2.5 <- lt2 %>% 
  filter(task_part== "options" & trial_type == "html-button-response2") %>% 
  mutate(response = recode(response, "0" = substr(choices, 1, 2), 
                           "1" = substr(choices, 4, 5), 
                           "2" = substr(choices, 7, 8),
                           .default = NA_character_))

lt2.5$response <- lt2.5$response %>% 
  str_replace_all("A1", "monkey-shape") %>% 
  str_replace_all("A2", "monster-path") %>% 
  str_replace_all("A3", "robot-path") %>% 
  str_replace_all("A4", "monkey-path") %>% 
  str_replace_all("B1", "alien-shape") %>% 
  str_replace_all("B2", "monster-ball") %>% 
  str_replace_all("B3", "robot-ball") %>% 
  str_replace_all("B4", "alien-ball") %>% 
  str_replace_all("C1", "monster-shape") %>% 
  str_replace_all("C2", "robot-shape") %>% 
  str_replace_all("C3", "monster-hiding") %>% 
  str_replace_all("C4", "robot-hiding")
  


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

options_ans <- options %>%
  filter(choices2!="") %>%
  mutate(response2 = response %>%
           as.numeric() + 1
  ) %>%
  select(response2,choices2)

choice_1 <- options_ans$choices2[1] %>%
  str_split(",") %>%
  unlist() %>%
  nth(options_ans$response2[1]) %>%
  recode("A1" = "monkey-shape",
         "A2" = "monster-path",
         "A3" = "robot-path",
         "A4" = "monkey-path")

choice_2 <- options_ans$choices2[2] %>%
  str_split(",") %>%
  unlist() %>%
  nth(options_ans$response2[2]) %>%
  recode("B1" = "alien-shape",
         "B2" = "monster-ball",
         "B3" = "robot-ball",
         "B4" = "alien-ball")

choice_3 <- options_ans$choices2[3] %>%
  str_split(",") %>%
  unlist() %>%
  nth(options_ans$response2[3]) %>%
  recode("C1" = "monster-shape",
         "C2" = "robot-shape",
         "C3" = "monster-hiding",
         "C4" = "robot-hiding")

options_exp <- options %>%
  filter(choices2=="") %>%
  pull(response) %>%
  str_remove("\\{\"choice-.\":\"") %>%
  str_remove('"\\}')

choice_1_exp <- options_exp[1]
choice_2_exp <- options_exp[2]
choice_3_exp <- options_exp[3]

View(options)

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
