require(tidyverse)
require(jsonlite)

mydata <- fromJSON(txt="../quizdata.json")

quiz_questions <- mydata$values

quiz_tibble <- as_tibble_col(quiz_questions) 


vec <- setNames(rep("", 12), unlist(quiz_tibble[[1, 1]]))
my_tibble <- bind_rows(vec)[0, ]

for (i in 2:498){
  new_data <- setNames(as_tibble(t(unlist(quiz_tibble[[i, 1]]))), unlist(quiz_tibble[[1, 1]]))
  my_tibble <- my_tibble |>
    bind_rows(new_data)
}

