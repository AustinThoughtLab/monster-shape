####################### data.tidying graveyard ################################


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

# need to merge lt2.5 -> lt2 regardless of differing row #s... ugggghhghghghgh

# not working -> 

f <- merge(x = lt3.5, y = lt3, by = "participant.id", all.x=TRUE)



lt3 <- lt2
lt3.5 <- lt2.5

lt3[match(lt3.5$participant.id, lt3$participant.id), ] <- lt3.5

lt3[match(lt3.5$task_part, lt3$task_part, incomparables = NULL), ] <- lt3.5

# join if participant ID matches and task_part = options AND trial_type = html-button-response-2 
# "participant.id", "condition", "counterbalancing", "order", "task_part", "response", "trial_type", "trial_index", "choices", "correct"

