---
title: "Summary Statistics"
author: "Nashra Ahmad"
date: '2024-02-20'
output: 
  html_document:
    toc: false
    number_sections: true
    theme: flatly
    highlight: espresso
editor_options: 
  markdown: 
    wrap: 72
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Initialise

## Libraries

```{r  libraries,warning=FALSE,message=FALSE}
library(emmeans)
library(dplyr)
library(ggplot2)
library(ggforce)
library(tidyverse)
library(ggplot2)
library(stringr)
library(lme4)
library(writexl)

```

## Read data

Reading the outlier removed data

```{r readdata}
dw2<-read.csv('Westerndata_outlierremoved.csv',skip = 0,header = TRUE)
dw2 <- subset(dw2, select = -Familiarity)
di2<-read.csv('Indiandata_outlierremoved.csv',skip = 0,header = TRUE)
```

## Combine data

```{r combine}
dw2$Familiarity <- "Non-Indian"
di2$Familiarity <- "Indian"
dcom <- rbind(dw2, di2)
dcom <- dcom %>%filter(type != "Naturll" & type != "Naturl2") #remove testing stimuli
```

1.  Recode type into Natural, Metrical (Basic, Complex, SP1) or Altered
    (SD1)

```{r,results='asis'}
dcom$type<-factor(dcom$type)
dcom$Alteration_Type <- factor(dcom$type,levels = c("Basic", "Complex", "Naturl", "sd1", "sp1"),labels = c("Metrical", "Metrical","Natural", "Altered", "Metrical"))

```

Note: Alteration_Type is Stimuli-Types, sd1 is Structurally Same, sd is
Altered.

## First, looking at the differences between the metrically same category

```{r, check metrical}

# Define patterns, conditions, and types
types_to_compare <- c("Basic", "Complex", "sp1")

# Pre-filter dataset for relevant columns and types
data_filtered <- dcom %>%
  filter(type %in% types_to_compare) %>%
  select(ResponseId, Musicianship, Familiarity,pattern, condition, type, similarity)

# LMM for all participant groups between Stimuli-Type
lm_met_m <- lmer(similarity ~ condition * pattern * Familiarity* Musicianship*type + (1 | ResponseId), data = data_filtered)
emm_met_m <- emmeans (lm_met_m,~  + condition+ pattern +Familiarity +   Musicianship +type+ pattern : type)
jt_met_m<-joint_tests(emm_met_m)
print(knitr::kable(jt_met_m))
pairwise_met_m <- pairs(emm_met_m, by = , adjust = "bonferroni")
pairwise_summary_m <- summary(pairwise_met_m)
jt_pairwise_m_df <- as.data.frame(pairwise_summary_m)
write_xlsx(jt_pairwise_m_df, "Stimuli Type Comparisons.xlsx")
```

As we found no significant interactions in Test, We combine all three
into Metrical

```{r, visualise metrical}

#Visualisation of only Test
vis_m <- data_filtered %>%
  filter(condition == "Test", type !="sd1") %>%
  mutate(Familiarity = ifelse(Familiarity == "Indian", "Familiar",
        ifelse(Familiarity == "Non-Indian", "Unfamiliar",Familiarity)),
  Groups = paste(Familiarity, Musicianship, sep = " "),
    pattern = ifelse(pattern == "Rupak", "Rupaktal (NI)", pattern),
    pattern = ifelse(pattern == "Teental", "Teental (ISO)", pattern),
  type = factor(type, levels = c("Basic", "Complex", "sp1"))
  ) %>%
  group_by(condition, Groups, pattern, type) %>%
  summarise(M = mean(similarity), n = n(), SE = sd(similarity) / sqrt(n), .groups = 'drop')

g_m <- ggplot(vis_m, aes(x = Groups, y = M, fill = type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.2, position = position_dodge(width = 0.7)) +
  facet_wrap(~ pattern, scales = "fixed") +
  ylab("Similarity Ratings (Mean, SE)") +
  xlab("Participant Groups") +
  ggtitle(" Test Learning Condition") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
  labs(fill = "Metrical") +
  scale_fill_brewer(palette = "Set5")
g_m

#Visualisation of only Base
vis_m2 <- data_filtered %>%
  filter(condition == "Base", type !="sd1") %>%
  mutate(Familiarity = ifelse(Familiarity == "Indian", "Familiar",
        ifelse(Familiarity == "Non-Indian", "Unfamiliar",Familiarity)),
  Groups = paste(Familiarity, Musicianship, sep = " "),
    pattern = ifelse(pattern == "Rupak", "Rupaktal (NI)", pattern),
    pattern = ifelse(pattern == "Teental", "Teental (ISO)", pattern),
  type = factor(type, levels = c("Basic", "Complex", "sp1"))
  ) %>%
  group_by(condition, Groups, pattern, type) %>%
  summarise(M = mean(similarity), n = n(), SE = sd(similarity) / sqrt(n), .groups = 'drop')

g_m2 <- ggplot(vis_m2, aes(x = Groups, y = M, fill = type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.2, position = position_dodge(width = 0.7)) +
  facet_wrap(~ pattern, scales = "fixed") +
  ylab("Similarity Ratings (Mean, SE)") +
  xlab("Participant Groups") +
  ggtitle(" Base Learning Condition") +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
  labs(fill = "Metrical") +
  scale_fill_brewer(palette = "Set5")
g_m2
```

# Analysis

## Change in Metrical Structure on Similarity perception

Visualizations

```{r,echo=FALSE,fig.width=10,fig.height=6}
#Test

#Participant vise comparison for only Base condition
vis_meter2 <- dcom %>%
  filter(condition == "Base") %>%
  mutate(
    Familiarity = ifelse(Familiarity == "Indian", "Familiar",
                         ifelse(Familiarity == "Non-Indian", "Unfamiliar", Familiarity)),
    Groups = paste(Familiarity, Musicianship, sep = " "),
    Alteration_Type = factor(Alteration_Type, levels = c("Natural", "Metrical", "Altered")),
    pattern = ifelse(pattern == "Rupak", "Rupaktal (NI)", pattern),
    pattern = ifelse(pattern == "Teental", "Teental (ISO)", pattern)
  ) %>%
  group_by(Groups, pattern, Alteration_Type) %>%
  summarise(M = mean(similarity), n = n(), SE = sd(similarity) / sqrt(n), .groups = 'drop')

g_meter2 <- ggplot(vis_meter2, aes(x = Groups, y = M, fill = Alteration_Type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.2, position = position_dodge(width = 0.7)) +
  facet_wrap(~ pattern, scales = "fixed") +
  ylab("Similarity Ratings") +
  xlab("Participant Groups") +
  ggtitle("Base Learning Condition") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
  labs(fill = "Stimuli Types") +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, 7.5)
g_meter2

#Participant vise comparison for only Test condition
vis_meter2.1 <- dcom %>%
  filter(condition == "Test") %>%
  mutate(
    Familiarity = ifelse(Familiarity == "Indian", "Familiar",
                         ifelse(Familiarity == "Non-Indian", "Unfamiliar", Familiarity)),
    Groups = paste(Familiarity, Musicianship, sep = " "),
    Alteration_Type = factor(Alteration_Type, levels = c("Natural", "Metrical", "Altered")),
    pattern = ifelse(pattern == "Rupak", "Rupaktal (NI)", pattern),
    pattern = ifelse(pattern == "Teental", "Teental (ISO)", pattern)
  ) %>%
  group_by(Groups, pattern, Alteration_Type) %>%
  summarise(M = mean(similarity), n = n(), SE = sd(similarity) / sqrt(n), .groups = 'drop')

g_meter2.1 <- ggplot(vis_meter2.1, aes(x = Groups, y = M, fill = Alteration_Type)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(aes(ymin = M - SE, ymax = M + SE), width = 0.2, position = position_dodge(width = 0.7)) +
  facet_wrap(~ pattern, scales = "fixed") +
  ylab("Similarity Ratings") +
  xlab("Participant Groups") +
  ggtitle("Test Learning Condition") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
  labs(fill = "Stimuli Types") +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, 7.5)
g_meter2.1

```

Statistics:

```{r, analysis meter}
#putting everything in tables
lm_met1.1 <- lmer(similarity ~ condition * pattern * Familiarity* Musicianship*Alteration_Type + (1 | ResponseId), data = dcom)

emm_met1.1 <- emmeans (lm_met1.1,~  + condition+ pattern +Familiarity +   Musicianship +Alteration_Type+ pattern : Alteration_Type)
jt_met1.1<-joint_tests(emm_met1.1)
print(knitr::kable(jt_met1.1))
jt_met1.1_df <- as.data.frame(jt_met1.1)
library(writexl)
write_xlsx(jt_met1.1_df, "jt_met1_1.xlsx")
pairwise_met_overall <- pairs(emm_met1.1, by = , adjust = "bonferroni")
pairwise_summary_overall <- summary(pairwise_met_overall)

#Overall comparison of alteration types for only Test Learning Condition
meter_overall <- dcom %>%
  filter(condition != "Base")
lm_met1 <- lmer(similarity ~ pattern * Familiarity* Musicianship*Alteration_Type + (1 | ResponseId), data = meter_overall)
emm_met1 <- emmeans (lm_met1,~  + pattern +Familiarity +   Musicianship +Alteration_Type+ pattern : Alteration_Type)
jt_met1<-joint_tests(emm_met1)
print(knitr::kable(jt_met1))
#1. for overall pattern and alteration type
emm_met_overall<- emmeans (lm_met1,~  pattern*Alteration_Type )
pairwise_met1 <- pairs(emm_met_overall, by = , adjust = "bonferroni")
pairwise_summary_met1 <- summary(pairwise_met1)
#2. for all factors
pairwise_met2 <- pairs(emm_met1, by = , adjust = "bonferroni")
pairwise_summary_met2 <- summary(pairwise_met2)


```

Checking whether irrespective of the Condition, if there was a
difference in experts vs others

```{r for experts}

emm_subset <- emmeans(lm_met1.1, ~  pattern*Familiarity * Musicianship*Alteration_Type)

pairwise_fam_mus_teental <- pairs(emm_subset, adjust = "bonferroni")
print("All pairwise comparisons for Alteration Type, Familiarity, and Musicianship in Teental pattern:")
#summary(pairwise_fam_mus_teental)
pairwise_summary_Teen <- summary(pairwise_fam_mus_teental)

```

## Accuracy Scores of IS and NI and Effect of Learning

This has not been reported in the paper as we did not want to provide
differences between the rhythmic cycles but is a way to compare ISO and
NI like Hannon et al. (2012).

2.  Plots

```{r, echo=FALSE, visualise}
# Replace 'Indian' with 'Familiar' and 'Non-Indian' with 'Unfamiliar
dcom_acc <- dcom %>%
  mutate(Familiarity = ifelse(Familiarity == "Indian", "Familiar", Familiarity)) %>%
  mutate(Familiarity = ifelse(Familiarity == "Non-Indian", "Unfamiliar", Familiarity))

# Add a new column 'Groups' by combining 'Familiairity' and 'Musicianship'
dcom_acc <- mutate(dcom_acc, Groups = paste(Familiarity, Musicianship, sep = " "))

#Accuracy Scores: Accuracy=(mean of metrical+mean of natural) - (mean of altered)
d_accuracy <- dcom_acc %>%
  group_by(ResponseId, condition, pattern, Groups, Familiarity, Musicianship) %>%
  summarise(
    Difference_Scores = (mean(similarity[type %in% c("Basic", "Naturl", "Complex", "sp1")]) -
                        mean(similarity[type == "sd1"])))

d_accuracy2 <- d_accuracy %>%
  group_by(pattern, Groups, condition) %>%
  summarise(M2 = mean(Difference_Scores), 
            n = n(), 
            SE2 = sd(Difference_Scores) / sqrt(n))
#plot difference scores
pd <- position_dodge(width = 0.8) 
d_accuracy2 <- d_accuracy %>%
  group_by(pattern, Groups, condition) %>%
  summarise(M2 = mean(Difference_Scores), 
            n = n(), 
            SE2 = sd(Difference_Scores) / sqrt(n))
g_acc1 <- ggplot(d_accuracy2, aes(x = Groups, y = M2, fill = pattern)) +
  geom_col(position = pd, width = 0.7) +
  #geom_errorbar(aes(ymin = M2 - SE2, ymax = M2 + SE2), width = 0.2, position = pd) +
  facet_wrap(~ condition, scales = "free") +
  ylab("Accuracy Scores") +  
  xlab("Participant Groups") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(min(d_accuracy2$M2) * 1.1, max(d_accuracy2$M2) * 1.1)  # Adjust ylim

#print(g_acc1)

#Now editing the graph:
# Rename levels of the condition factor for the graph
d_accuracy3<- d_accuracy2 %>%
  mutate(condition = ifelse(condition == "Base", "A. Base", condition)) %>%
  mutate(condition = ifelse(condition == "Test", "B. Test", condition))%>%
mutate(pattern = ifelse(pattern == "Rupak", "Rupaktal (NI)", pattern))%>%
mutate(pattern = ifelse(pattern == "Teental", "Teental (ISO)", pattern))

# Plot with updated facet labels
g_acc2 <- ggplot(d_accuracy3, aes(x = Groups, y = M2, fill = pattern)) +
  geom_col(position = pd, width = 0.7) +
  facet_wrap(~ condition, scales = "free") +
  ylab("Accuracy Scores") +  
  xlab("Participant Groups") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(min(d_accuracy2$M2) * 2.5, max(d_accuracy2$M2) * 1.5)+# Adjust ylim
  labs(fill = "Pattern")

print(g_acc2)
# Save the plot as a PNG file with 300 DPI resolution
file_path <- "g_acc2_plot.png"

# Save the plot
ggsave(filename = file_path, plot = g_acc2, device = "png", dpi = 300)

# Print message confirming the save
print(paste("Plot saved as", file_path, "with 300 DPI"))

```

Statistics for accuracy perception:

```{r, analysis accuracy}
#1. Base

acc_base <- d_accuracy %>%
  filter(condition == "Base")
lm_acc1 <- lm(Difference_Scores ~ pattern * Familiarity*Musicianship, data = acc_base)
emm_acc <- emmeans (lm_acc1,~  pattern*Familiarity*Musicianship )
pairwise_acc <- pairs(emm_acc, by = , adjust = "bonferroni")
pairwise_summary_acc <- summary(pairwise_acc)

#2. Test

acc_Test <- d_accuracy %>%
  filter(condition == "Test")
lm_acc2 <- lm(Difference_Scores ~ pattern * Familiarity*Musicianship, data = acc_Test)
emm_acc2 <- emmeans (lm_acc2,~  pattern*Familiarity*Musicianship )
pairwise_acc2 <- pairs(emm_acc2, by = , adjust = "bonferroni")
pairwise_summary_acc2<- summary(pairwise_acc2)

#overall for effect of short term learning
m2 <- lmer(Difference_Scores ~ pattern * Familiarity * Musicianship  * condition + (1 | ResponseId), data = d_accuracy)
emm_acc3 <- emmeans(m2, ~  pattern + Familiarity + condition + pattern + Musicianship : Familiarity + pattern : condition + Musicianship : condition )
jt_acc3<-joint_tests(emm_acc3)
print(knitr::kable(jt_acc3))
pairwise_acc3 <- pairs(emm_acc3, by = , adjust = "bonferroni")
pairwise_summary_acc3 <- summary(pairwise_acc3)

```

One could add the p-values to the plot above using annotate function.
