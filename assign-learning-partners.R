#..........................load packages.........................
library(googlesheets4)
library(tidyverse)

#.................import spreadsheet of students.................
students <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1UnGwZ6TMr8ik3PpEo65GJU1R9Pk-xmkjlMn3kOqfrvY/edit?usp=sharing")

#......................initialize empty df.......................
all_groups <- data.frame()

#..........................create groups.........................
generate_groups <- function(data){

  for (i in 1:14) { # for a class size of 40: 13 groups of three, 1 group of four

    # create group number ----
    group_number <- paste0("group", i)

    # assign group members----
    random_group <- students |>
      slice_sample(n = 3, replace = FALSE) |>
      mutate(group_assignment = rep(group_number))

    # add group to df ----
    all_groups <- rbind(all_groups, random_group)

  }

}


n_students <- 40
x <- sample(1:n_students)
levels <- 14
groupings <- split(x, x%%levels)
