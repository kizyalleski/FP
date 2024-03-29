setwd("C:/FP/Whole_Catchment/src/geology")

library(tidyverse)
library(dplyr)
library(fingerPro)
library(readxl)
library(corrr)

# ������ �������� ������
data <- catchment

# 1) ������ � ���������� ������

df_whole <-
  read_excel(path = "../../data/geology_mix-2025.xlsx") %>% # ������ ������
  filter(Name != "�����������") %>% # ������� ������ � ������������
  select(-X, -Y) %>% # ������� ������� � ������������
  rename(id = Name) %>% # ��������������� name � id
  mutate_at(vars(-Source), ~ as.numeric(.)) %>% # ����������� � �������� ���
  na_if(0) %>% # �������� ���� �� na
  as.data.frame() # ����������� � ���� ���� ����� �� �����

# ������ ���������, ������� ��� � ������
mix_na <-
  df_whole %>% 
  filter(Source == "Mix") %>% # ��������� ������ ������ �������� �������
  select_if(is.na) %>% # �������� ������� � na
  gather(var, val) %>% # ���������� � �������� ����������� �����������
  pull(var) # ��������� ������ ��������

# ��������� ��������� ������ ������
df <-
  df_whole %>% 
  select(!all_of(mix_na)) %>%  # ��������� ������ �� ��������, ������� ���� � ������� �������
  mutate_all(~replace(., is.na(.), 0)) # �������� na �� 0

# 2) �������� �� ��������������
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
# boxPlot(df, columns = 1:6, ncol = 3)
# correlationPlot(df, columns = 1:7, mixtures = TRUE)

df_lda <-
  df %>% 
  select(!any_of(collinears)) # ������� �� ��� ������������ ���������

df_lda <-
  df_lda %>%
  filter(id != 2014) %>%
  filter(id != 2015) %>%
  filter(id != 2028) %>%
  filter(id != 4004) %>%
  filter(id != 15) %>%
  filter(id != 17) %>%
  filter(id != 2019) %>%
  filter(id != 7) %>% 
  filter(id != 4) %>%
  filter(id != 28) %>%
  filter(id != 20) %>%
  filter(id != 27) %>%
  filter(id != 2021) %>%
  filter(id != 2013) %>% 
  filter(id != 2005) %>%
  filter(id != 2020) %>% 
  filter(id != 2001) %>%
  filter(id != 2017) %>%
  filter(id != 2007) %>% 
  filter(id != 24) %>%
  filter(id != 9) %>% 
  filter(id != 2003) %>% 
  filter(id != 2016) %>%
  filter(id != 2029) %>%
  filter(id != 21) %>% 
  filter(id != 6) %>%
  filter(id != 23) %>% 
  filter(id != 2004)

df_lda %>% 
  LDAPlot(text = T)

# 4) ����� ���������
df_lda %>% 
  rangeTest() %>% 
  KWTest(pvalue = 0.05)

DFATest(df_lda, niveau = 0.05)

# 5) ���������
df %>% 
  select(id, Source, Al, Ti, Zn, kps, plagioklaz, smektit) %>% 
  gather(elem, cons, -id, -Source) %>% 
  ggplot(aes(x = Source,
             y = cons,
             color = Source)) +
  geom_boxplot() +
  geom_text(aes(label = id)) +
  facet_wrap(~elem,
             scales = "free_y")

# 6) ������������� ���������
df_lda %>% 
  select(id, Source, Al, Ti, Zn, kps, plagioklaz, smektit) %>% 
  LDAPlot(text = T)

# 7) ������������
results <- 
  df_lda %>% 
  select(id, Source, Al, Ti, Zn, kps, plagioklaz, smektit) %>% 
  unmix(samples = 200, iter = 300)

results %>% 
  plotResults()
