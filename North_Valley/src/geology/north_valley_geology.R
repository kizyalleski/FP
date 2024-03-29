setwd("C:/FP/North_Valley/src/geology")

library(tidyverse)
library(dplyr)
library(fingerPro)
library(readxl)
library(corrr)

# 1) ������ � ���������� ������

df_north_geology <-
  read_excel(path = "../../data/geology.xlsx") %>% # ������ ������
  filter(Name != "�����������") %>% # ������� ������ � ������������
  select(-X, -Y) %>% # ������� ������� � ������������
  rename(id = Name) %>% # ��������������� name � id
  mutate_at(vars(-Source), ~ as.numeric(.)) %>% # ����������� � �������� ���
  na_if(0) %>% # �������� ���� �� na
  as.data.frame() # ����������� � ���� ���� ����� �� �����

# ������ ���������, ������� ��� � ������
mix_na <-
  df_north_geology %>% 
  filter(Source == "Mix") %>% # ��������� ������ ������ �������� �������
  select_if(is.na) %>% # �������� ������� � na
  gather(var, val) %>% # ���������� � �������� ����������� �����������
  pull(var) # ��������� ������ ��������

# ��������� ��������� ������ ������
df <-
  df_north_geology %>% 
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
  select(!any_of(collinears))   # ������� �� ��� ������������ ���������

df_lda %>% 
  LDAPlot(text = T)

# 4) ����� ���������
df_lda %>% 
  rangeTest() %>% 
  KWTest(pvalue = 0.3)

DFATest(df_lda, niveau = 0.3)

# 5) ���������
df %>% 
  select(id, Source, plagioklaz, hlorit, Ca, Zn, sluda) %>% #Ca, Zn, sluda) %>% 
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
  select(id, Source, plagioklaz, hlorit) %>%   #Ca, Zn, sluda) %>% 
  LDAPlot(text = T)

# 7) ������������
results <- 
  df_lda %>% 
  select(id, Source, plagioklaz, hlorit) %>% 
  unmix(samples = 100, iter = 1000)

results %>% 
  plotResults()


