setwd("C:/FP/North_Valley/src")

library(tidyverse)
library(dplyr)
library(fingerPro)
library(readxl)
library(corrr)

# Пример исходных данных
data <- catchment

# 1) ЧТЕНИЕ и подготовка ДАННЫХ

df_north <-
  read_excel(path = "../data/North_Valley_data.xlsx") %>% # чтение данных
  filter(Name != "Размерность") %>% # убираем строку с размерностью
  select(-X, -Y) %>% # убираем столбцы с координатами
  rename(id = Name) %>% # переименовываем name в id
  mutate_at(vars(-Source), ~ as.numeric(.)) %>% # преобразуем в числовой тип
  na_if(0) %>% # заменяем нули на na
  as.data.frame() # преобразуем к типу дата фрейм из тибла

# список элементов, которых нет в мишени
mix_na <-
  df_north %>% 
  filter(Source == "Mix") %>% # оставляем только строку целевого образца
  select_if(is.na) %>% # выбираем столбцы с na
  gather(var, val) %>% # переменную и значение ориентируем вертикально
  pull(var) # извлекаем вектор названий

# получение итогового набора данных
df <-
  df_north %>% 
  select(!all_of(mix_na)) %>%  # оставляем только те элементы, которые есть в целевом образце
  mutate_all(~replace(., is.na(.), 0)) # заменяем na на 0



  



