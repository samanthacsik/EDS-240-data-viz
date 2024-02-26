#..........................load packages.........................
library(googlesheets4)
library(tidyverse)

#.................import spreadsheet of students.................
#students <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UnGwZ6TMr8ik3PpEo65GJU1R9Pk-xmkjlMn3kOqfrvY/edit?usp=sharing")
# roster <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1guUVFA4FeHZOuWFdQ1uGaE6RK1Y5lABp8MDDd9hdCYU/edit?usp=sharing")
roster <- read_csv(here::here("student-groups", "eds240-students.csv"))

#............................wrangle.............................

students <- roster |>
  select(name)

#................randomly generate group numbers.................

group_num <- c(1, 1, 1,
               2, 2, 2,
               3, 3, 3,
               4, 4, 4,
               5, 5, 5,
               6, 6, 6,
               7, 7, 7,
               8, 8, 8,
               9, 9, 9,
               10, 10, 10,
               11, 11, 11,
               12, 12, 12,
               13, 13)

group_num <- sample(group_num)

#....................cbind numbers to roster.....................

student_groups <- cbind(students, group_num) |>
  arrange(group_num)

#..........................write out csv.........................
# write_csv(x = student_groups, file = here::here("student-groups", "week8_learning_partners.csv"))

