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

# 2) ПРОВЕРКА НА КОЛЛИНЕАРНОСТЬ
collinears <-
  df %>% 
  select(-id, -Source) %>% 
  correlate(method = "spearman") %>% 
  rearrange() %>% 
  shave() %>% 
  mutate_if(is.numeric, ~abs(.)) %>% 
  filter_if(is.numeric, any_vars(. > 0.85)) %>% 
  pull(term)

collinears

# 3) LDA
boxPlot(df, columns = 1:6, ncol = 3)
correlationPlot(df, columns = 1:7, mixtures = TRUE)

df_lda <-
  df %>% 
  select(!any_of(collinears)) # создаем дф без коллинеарных элементов

df_lda %>% 
  LDAPlot(text = T)

# 4) ВЫБОР ТРАССЕРОВ
df_lda %>% 
  rangeTest() %>% 
  KWTest(pvalue = 0.25)

DFATest(df_lda, niveau = 0.25)

# 5) БОКСПЛОТЫ
df %>% 
  select(id, Source, plagioklaz, kps) %>% 
  gather(elem, cons, -id, -Source) %>% 
  ggplot(aes(x = Source,
             y = cons,
             color = Source)) +
  geom_boxplot() +
  geom_text(aes(label = id)) +
  facet_wrap(~elem,
             scales = "free_y")

# 6) ПОДТВЕРЖДЕНИЕ ТРАССЕРОВ
df_lda %>% 
  select(id, Source, plagioklaz, kps) %>% 
  LDAPlot(text = T)

# 7) Размешивание
results <- 
  df_lda %>% 
  select(id, Source, plagioklaz, kps) %>% 
  unmix(samples = 100, iter = 1000)

results %>% 
  plotResults()


