# Advent of Code
# Day 3

library(tidyverse)

df3ex <- read_lines("C:/Users/sawhi/Desktop/aoc2022_3ex.txt")

df3 <- read_lines("C:/Users/sawhi/Desktop/aoc2022_3.txt")

df1b <- tibble(data = df3) %>% 
  mutate(length = str_length(data),
         half_length = length / 2) %>% 
  mutate(part1 = str_sub(data, start = 1, end = half_length ),
         part2 = str_sub(data, start = half_length + 1, end = length)) %>% 
  mutate(Letter = NA_character_)

# s <- "vJrwpWtwJgWr"
# t <- "hcsFMMfFFhFp"

for (r in 1:nrow(df1b)) {
  s <- df1b$part1[r]
  t <- df1b$part2[r]
  for (i in 1:nchar(s)) {
    letter <- str_sub(s, i, i)
    
    result <- grepl(pattern = letter, t)
    
    if (result == TRUE)
    {
      print(letter)
      
      df1b$Letter[r] = letter
      
      break
    }
  }
}

df1c <- tibble(letters_a = c(letters, LETTERS),
               values_a = seq(1:52))

df1d <- df1b %>% 
  left_join(df1c, by = c("Letter" = "letters_a")) %>% 
  summarise(Total = sum(values_a))

df1d$Total

## Part 2 -------------------------------------------------------------------
df2b <- tibble(data = df3ex) %>% 
  mutate(group = rep(1:(nrow(df2b)/3), each = 3),
         elf = rep(1:3, nrow(df2b))) %>% 
  pivot_wider(names_from = elf,
              names_prefix = "group",
              values_from = data) %>% 
  mutate(Letter = NA_character_)

for (r in 1:nrow(df2b)) {
  s <- df2b$group1[r]
  t <- df2b$group2[r]
  u <- df2b$group3[r]
  for (i in 1:nchar(s)) {
    letter <- str_sub(s, i, i)
    
    result_t <- grepl(pattern = letter, t)
    
    if (result_t == TRUE)
    {
      result_u <- grepl(pattern = letter, u)
      
      if (result_u == TRUE)
      {
        print(letter)
        
        df2b$Letter[r] = letter
      }
    }
  }
}

df2c <- df2b %>% 
  left_join(df1c, by = c("Letter" = "letters_a")) %>% 
  summarise(Total = sum(values_a))

df2c$Total

