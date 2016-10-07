library(dplyr)
test_timeline <- gloograph::test_timeline   

test_timeline_reversed_connections  <- test_timeline %>%
  filter(action == "connects") %>%
  rename(owner_id = object_id, object_id = owner_id)

test_timeline %<>% 
  rbind(test_timeline_reversed_connections) %>%
  arrange(time)

devtools::use_data(test_timeline, test_timeline, overwrite = T)

