# Advent of Code
# Day 4

library(tidyverse)

df4ex <- read_lines("C:/Users/sawhi/Desktop/aoc2022_4ex.txt")

df4 <- read_lines("C:/Users/sawhi/Desktop/aoc2022_4.txt")

df1a <- tibble(data = df4) %>% 
  separate(data, c("A_start", "A_end", "B_start", "B_end")) %>% 
  mutate(across(everything(), as.numeric))%>% 
  mutate(overlap = case_when(
    A_start <= B_start & A_end >= B_end ~ 1,
    B_start <= A_start & B_end >= A_end ~ 1,
    TRUE ~ 0 )) %>% 
  summarise(Total = sum(overlap))

df1a$Total  

df2a <- tibble(data = df4) %>% 
  separate(data, c("A_start", "A_end", "B_start", "B_end")) %>% 
  mutate(across(everything(), as.numeric))%>% 
  mutate(overlap = case_when(
    A_start <= B_start & A_end >= B_start ~ 1,
    B_start <= A_start & B_end >= A_start ~ 1,
    TRUE ~ 0 )) %>% 
  summarise(Total = sum(overlap))

df2a$Total
