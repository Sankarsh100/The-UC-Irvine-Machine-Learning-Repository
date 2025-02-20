library(mlbench) 
library(tidyverse)
library(corrplot)
library(e1071)
library(caret)

data(Glass)

Glass %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + 
  geom_histogram(bins = 15) + 
  facet_wrap(~key, scales = 'free') +
  ggtitle("Histograms of Numerical Predictors")

Glass %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + 
  geom_boxplot() + 
  facet_wrap(~key, scales = 'free') +
  ggtitle("Boxplots of Numerical Predictors")

Glass %>%
  keep(is.numeric) %>%
  cor() %>%
  corrplot() 

Glass %>%
  ggplot() +
  geom_bar(aes(x = Type)) +
  ggtitle("Distribution of Types of Glass")

#b

Glass %>%
  keep(is.numeric) %>%
  apply(., 2, skewness) %>%
  round(4)

#c
Glass %>%
  keep(is.numeric) %>%
  mutate_all(funs(BoxCoxTrans(.)$lambda)) %>%
  head(1)


#3.2
#a
data(Soybean)

columns <- colnames(Soybean)

lapply(columns,
       function(col) {
         ggplot(Soybean, 
                aes_string(col)) + geom_bar() + coord_flip() + ggtitle(col)})

#b
Soybean %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(), names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y = variables, x=n, fill = missing))+
  geom_col(position = "fill") +
  labs(title = "Proportion of Missing Values",
       x = "Proportion") +
  scale_fill_manual(values=c("grey","red"))





Soybean %>%
  group_by(Class) %>%
  mutate(class_Total = n()) %>%
  ungroup() %>%
  filter(!complete.cases(.)) %>%
  group_by(Class) %>%
  mutate(Missing = n(),
         Proportion =  Missing / class_Total) %>% 
  ungroup()%>%
  select(Class, Proportion) %>%
  distinct()




Soybean %>%
  filter(!Class %in% c("phytophthora-rot", "diaporthe-pod-&-stem-blight", "cyst-nematode",
                       "2-4-d-injury", "herbicide-injury")) %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(), names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y = variables, x=n, fill = missing))+
  geom_col(position = "fill") +
  labs(title = "Proportion of Missing Values with Missing Classes Removed",
       x = "Proportion") +
  scale_fill_manual(values=c("grey","red"))

