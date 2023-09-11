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

qti_tibble <- tribble(~qtype, ~legacy, ~pval, ~body, ~answer, ~choice1, ~choice2, ~choice3, ~choice4, ~choice5)

for (i in c(37, 39:42, 75, 76, 78, 80, 81, 135, 137, 138, 142, 143, 315:319)){
  qti_tibble <- qti_tibble |>
    add_row(qtype = "MC",
            legacy = " ",
            pval = 1,
            body = paste0(my_tibble$passage[[i]]," QUESTION: ",my_tibble$question[[i]]),
            answer = 1,
            choice1 = my_tibble$choice1[[i]],
            choice2 = my_tibble$choice2[[i]],
            choice3 = my_tibble$choice3[[i]],
            choice4 = my_tibble$choice4[[i]],
            choice5 = my_tibble$choice5[[i]]
    )
}

write_csv(qti_tibble, "quiz_one_questions.csv", append=FALSE, col_names=FALSE)