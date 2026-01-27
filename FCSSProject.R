#Statistical analysis on sentiment data. Replace file paths with correct ones for your PC. 
#Code taken from https://www.datanovia.com/en/lessons/anova-in-r/#two-way-independent-anova

install.packages(c("rstatix", "ggpubr"), dependencies = TRUE)

library(tidyverse)
library(ggpubr)
library(rstatix)
library(emmeans)

df <- read.csv("C:/Users/kateb/OneDrive/Desktop/df.csv")
anova_df <- read.csv("C:/Users/kateb/OneDrive/Desktop/anova_df.csv")

measure.aov <- anova_df %>% anova_test(score ~ candidate * measure)
measure.aov

model <- lm(score ~ candidate * measure, data = anova_df)
anova_df %>%
  group_by(measure) %>%
  anova_test(score ~ candidate, error = model)

pwc <- anova_df %>% 
  group_by(measure) %>%
  emmeans_test(score ~ candidate, p.adjust.method = "bonferroni") 
pwc

state.aov <- df %>% anova_test(sentiment ~ candidate * state_code)
state.aov

model2 <- lm(sentiment ~ candidate * state_code, data = df)
df %>%
  group_by(state_code) %>%
  anova_test(sentiment ~ candidate, error = model)

pwc2 <- df %>% 
  group_by(state_code) %>%
  emmeans_test(sentiment ~ candidate, p.adjust.method = "bonferroni") 
pwc2