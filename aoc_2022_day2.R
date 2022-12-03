# Advent of Code
# Day 2

library(tidyverse)

df1ex <- read_lines("C:/Users/sawhi/Desktop/aoc2022_2ex.txt")

df1 <- read_lines("C:/Users/sawhi/Desktop/aoc2022_2.txt")

df1b <- tibble(data = df1) %>% 
  separate(data, c("Elf", "Me")) %>% 
  mutate(
    Item_score = case_when(
      Me == "X" ~ 1,
      Me == "Y" ~ 2,
      Me == "Z" ~ 3 ),
    Result = case_when(
      Elf == "A" & Me == "Y" ~ "Win",
      Elf == "B" & Me == "Z" ~ "Win",
      Elf == "C" & Me == "X" ~ "Win",
      Elf == "A" & Me == "X" ~ "Draw",
      Elf == "B" & Me == "Y" ~ "Draw",
      Elf == "C" & Me == "Z" ~ "Draw",
      TRUE ~ "Lose" ),
    Result_score = case_when(
      Result == "Win" ~ 6,
      Result == "Draw" ~ 3,
      TRUE ~ 0 )) %>% 
  mutate(Total_score = Item_score + Result_score) %>% 
  summarise(Total = sum(Total_score))

df1b$Total

df2 <- df1b %>% 
  slice(1:3) %>% 
  summarise(top_3 = sum(total_calories))

df2 <- tibble(data = df1) %>% 
  separate(data, c("Elf", "Result")) %>% 
  mutate(
    Result_score = case_when(
      Result == "X" ~ 0,
      Result == "Y" ~ 3,
      Result == "Z" ~ 6),
    Me = case_when(
      Result == "Z" & Elf == "A" ~ "Paper",
      Result == "Z" & Elf == "B" ~ "Scissors",
      Result == "Z" & Elf == "C" ~ "Rock",
      Result == "Y" & Elf == "A" ~ "Rock",
      Result == "Y" & Elf == "B" ~ "Paper",
      Result == "Y" & Elf == "C" ~ "Scissors",
      Result == "X" & Elf == "A" ~ "Scissors",
      Result == "X" & Elf == "B" ~ "Rock",
      Result == "X" & Elf == "C" ~ "Paper"),
    My_score = case_when(
      Me == "Rock" ~ 1,
      Me == "Paper" ~ 2,
      Me == "Scissors" ~ 3)) %>% 
  mutate(Total_score = Result_score + My_score) %>% 
  summarise(Total = sum(Total_score))

df2$Total
