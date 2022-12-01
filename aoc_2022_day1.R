# Advent of Code
# Day 1

library(tidyverse)
library(readr)

df1ex <- read_lines("C:/Users/sawhi/Desktop/aoc2022_1ex.txt")

df1 <- read_lines("C:/Users/sawhi/Desktop/aoc2022_1.txt")

df1b <- tibble(data = df1) %>% 
  mutate(elf_split = if_else(data == "", 1, 0),
         elf = cumsum(elf_split),
         calories = as.numeric(data)) %>% 
  group_by(elf) %>% 
  summarise(total_calories = sum(calories, na.rm = TRUE)) %>% 
  arrange(desc(total_calories))

df1_answer <- slice_head(df1b)
df1_answer$total_calories

df2 <- df1b %>% 
  slice(1:3) %>% 
  summarise(top_3 = sum(total_calories))

df2

