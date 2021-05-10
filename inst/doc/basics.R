## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  library(windsoraiR)
#  my_data <-
#    windsor_fetch(
#    api_key = "your api key",
#    date_preset = "last_7d",
#    fields = c("source", "campaign", "clicks",
#               "medium", "sessions", "spend")
#  )

## ---- echo = FALSE------------------------------------------------------------
library(windsoraiR)

## -----------------------------------------------------------------------------
str(my_data)

## -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)

top_10 <- 
  my_data %>% 
  filter(data.clicks > 0) %>% 
  group_by(data.campaign) %>% 
  summarise(n_clicks = sum(data.clicks)) %>% 
  ungroup %>% 
  arrange(desc(n_clicks)) %>% 
  slice_head(n = 10)

knitr::kable(top_10)

## ---- fig.width=7-------------------------------------------------------------
ggplot(top_10, aes(x = n_clicks, y = data.campaign)) +
  geom_col()

## -----------------------------------------------------------------------------
top_10 <- 
  my_data %>% 
  filter(data.clicks > 0) %>% 
  group_by(data.source, data.campaign) %>% 
  summarise(n_clicks = sum(data.clicks)) %>% 
  ungroup %>% 
  arrange(desc(n_clicks)) %>% 
  slice_head(n = 10)

knitr::kable(top_10)

## ---- fig.width=7-------------------------------------------------------------
ggplot(top_10, aes(x = n_clicks, y = data.campaign, fill = data.source)) +
  geom_col()

## ---- fig.width= 7------------------------------------------------------------
my_data %>%
  filter(data.clicks > 0) %>%
  group_by(data.campaign) %>%
  summarise(sum_spend = sum(as.numeric(data.spend))) %>%
  ungroup %>%
  arrange(desc(sum_spend)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = sum_spend, y = data.campaign)) +
  geom_col()

## ---- fig.width=10------------------------------------------------------------
library(tidyr)

my_data %>%
  filter(data.clicks > 0) %>%
  group_by(data.campaign) %>%
  summarise(n_clicks = sum(data.clicks), sum_spend = sum(as.numeric(data.spend))) %>%
  arrange(desc(sum_spend)) %>%
  slice_head(n = 10) %>%
  pivot_longer(cols = c("n_clicks", "sum_spend"), names_to = "aggreg", values_to = "values") %>%  
  ggplot(aes(x = values, y = data.campaign, fill = aggreg)) +
  geom_col() +
  facet_wrap("aggreg", ncol = 2) + 
  theme(legend.position="bottom")

