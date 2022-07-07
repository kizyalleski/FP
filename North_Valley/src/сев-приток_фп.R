library(tidyverse)
library(fingerPro)
library(readxl)
library(corrr)

# 1) Подготовить данные под формат ----------------------------------------
data <- catchment

# 2) Чтение входных данных ------------------------------------------------
# 2020 год
df_2020 <-
  read_excel("data/don/chem/2022/01_сев-приток.xlsx") %>% 
  filter(Name != "Размерность") %>% 
  dplyr::select(-X, -Y) %>% 
  rename(id = Name) %>%
  mutate_at(vars(-id, -Source),
            ~as.numeric(.)) %>% 
  na_if(0) %>%
  as.data.frame()

raz_2020 <- 
  read_excel("data/don/chem/2022/01_сев-приток.xlsx") %>% 
  filter(Name == "Размерность") %>% 
  dplyr::select(-Name:-Source) %>% 
  gather(val, raz20) %>% 
  filter(raz20 != "%")
  
raz_2020 %>% 
  print(n = 100)

# Целевой образец D29 из 2021 года
# обратить внимание на размерность
df_2021 <- 
  read_excel("data/don/chem/DON_FP_2020-2021.xlsx",
             sheet = "2021") %>% 
  filter(Name == "D18") %>% 
  mutate(Source = "mix",
         .after = Name) %>% 
  rename(id = Name) %>% 
  dplyr::select(-X, -Y) %>% 
  mutate_at(vars(-id, -Source),
            ~as.numeric(.)) %>% 
  na_if(0) %>% 
  as.data.frame()

raz_2021 <-
  read_excel("data/don/chem/DON_FP_2020-2021.xlsx",
             sheet = "2021") %>% 
  filter(Name == "Размерность") %>% 
  dplyr::select(-Name, -Y) %>% 
  gather(val, raz21) %>% 
  filter(raz21 != "%")

raz_2021 %>% 
  print(n = 100)

# 3) Сравнить размерности -------------------------------------------------
raz_2020 %>% 
  full_join(raz_2021,
            by = "val") %>%
  filter(raz20 != raz21)

# Объединяем датасеты
df <- 
  df_2020 %>% 
  bind_rows(df_2021)

# Список элементов которых нет в мишени
mix_na <- 
  df %>% 
  filter(Source == "mix") %>% 
  select_if(is.na) %>% 
  gather(var, val) %>% 
  pull(var)


# 4) Сохранеям итоговый набор данных --------------------------------------
df_final <- 
  df %>% 
  select(!all_of(mix_na)) %>% 
  # dplyr::select(-P) %>% 
  mutate_all(~replace(., is.na(.), 0))

# 5) Проверка на коллинеарность -------------------------------------------
collinears <-
   df_final %>% 
   dplyr::select(-id, -Source) %>% 
   correlate(method = "spearman") %>%  
   rearrange() %>% 
   shave() %>% 
   mutate_if(is.numeric,
             ~abs(.)) %>% 
   # dplyr::select(-term) %>% 
   filter_if(is.numeric,
             any_vars(. > .85)) %>% 
   pull(term)

collinears

# 6) LDA ------------------------------------------------------------------
df_lda <- 
  df_final %>% 
  # filter(!id %in% c("2010", "2012", "2013")) %>%
  # filter(Source != "Moraine") %>% 
  select(!any_of(collinears)) 

df_lda %>% 
  LDAPlot(text = T)

# 7) Выбор трассеров ------------------------------------------------------
df_lda %>% 
  rangeTest() %>%
  KWTest(pvalue = 0.2)

DFATest(df_lda, niveau = 0.2)

# Построение боксплотов
df_final %>% 
  dplyr::select(id, Source, Ti, plagioklaz, kvarc, hlorit) %>% 
  gather(element, concent, -id, -Source) %>% 
  ggplot(aes(x = Source, 
             y = concent,
             color = Source)) +
  geom_boxplot() +
  geom_text(aes(label = id)) +
  facet_wrap(~element,
             scales = "free_y")

# 8) Подтверждение трассеров ----------------------------------------------
df_lda %>% 
  dplyr::select(id, Source, plagioklaz, kvarc, hlorit) %>% 
  LDAPlot(text = T)

# 9) Размешивание ---------------------------------------------------------
results <- 
  df_lda %>% 
  dplyr::select(id, Source, plagioklaz, kvarc) %>% 
  unmix(samples = 100, iter = 3000)

results %>% 
  fingerPro::plotResults()

