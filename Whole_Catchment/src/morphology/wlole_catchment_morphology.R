setwd("C:/FP/Whole_Catchment/src/morphology")

library(tidyverse)
library(dplyr)
library(fingerPro)
library(readxl)
library(corrr)

# ������ �������� ������
data <- catchment

# 1) ������ � ���������� ������

df_whole <-
  read_excel(path = "../../data/data.xlsx") %>% # ������ ������
  filter(Name != "�����������") %>% # ������� ������ � ������������
  select(-X, -Y) %>% # ������� ������� � ������������
  rename(id = Name) %>% # ��������������� name � id
  mutate_at(vars(-Source), ~ as.numeric(.)) %>% # ����������� � �������� ���
  na_if(0) %>% # �������� ���� �� na
  as.data.frame() %>%  # ����������� � ���� ���� ����� �� �����
  filter(id != 2024) %>% 
  filter(id != 2023)
  
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
  filter(id != 2001) %>%
  filter(id != 20) %>%
  filter(id != 27) %>%
  filter(id != 2021) %>%
  filter(id != 2005) %>%
  filter(id != 2015) %>%
  filter(id != 3004) %>%
  filter(id != 24) %>% 
  filter(id != 2007) %>%
  filter(id != 28) %>%
  filter(id != 2020) %>%
  filter(id != 2019) %>% 
  filter(id != 3003) %>%
  filter(id != 2028) %>%
  filter(id != 2029)
  
df_lda %>% 
  LDAPlot(text = T)

# 4) ����� ���������
df_lda %>% 
  rangeTest() %>% 
  KWTest(pvalue = 0.05)

DFATest(df_lda, niveau = 0.05)

# 5) ���������
df %>% 
  select(id, Source, Al, Zn, kps, plagioklaz) %>% 
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
  select(id, Source, Al, Zn, kps, plagioklaz) %>% 
  LDAPlot(text = T)

# 7) ������������
results <- 
  df_lda %>% 
  select(id, Source, Al, Zn, kps, plagioklaz) %>% 
  unmix(samples = 200, iter = 300)

results %>% 
  plotResults()
