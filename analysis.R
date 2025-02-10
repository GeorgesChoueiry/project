
library(tidyverse)
library(arrow)
library(readxl)

# read data
articles <- read_parquet("pubmed articles.parquet")
climate <- read_excel("climate change data cleaned.xlsx")


### production trend
####################

articles |> 
  distinct(pmid, .keep_all = TRUE) |> 
  count(pub_year) |> 
  ggplot(aes(x = pub_year, y = n)) +
  geom_segment(aes(xend = pub_year, y = 0, yend = n), color = "#475E74") +
  geom_point(color = "#475E74", size = 4) +
  scale_x_continuous(breaks = 2005:2024, limits = c(2005, 2024)) +
  scale_y_continuous(breaks = seq(0,300,50), limits = c(0,300), expand = c(0,0)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = "dotted", color = "#BEC5AD"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 20)) +
  labs(x = NULL, y = "Article count")


### funding
###########

articles |> 
  distinct(pmid, .keep_all = TRUE) |> 
  count(!is.na(grants)) |> 
  mutate(perc = n / sum(n) * 100)

articles |> 
  distinct(pmid, .keep_all = TRUE) |> 
  separate_longer_delim(funding_country, delim = ";") |> 
  filter(!is.na(funding_country)) |> 
  count(funding_country, sort = TRUE) |> 
  mutate(perc = n / sum(n) * 100)


### topics covered
##################

articles |> 
  filter(level == 1) |> 
  count(gbd_cause)

country_gdp <- 
  climate |> 
  select(country, gdp_per_capita)

articles |> 
  filter(level == 1) |> 
  separate_longer_delim(affiliation_country, delim = ";") |> 
  left_join(country_gdp, join_by(affiliation_country == country)) |> 
  summarize(.by = gbd_cause,
            mean(gdp_per_capita, na.rm = TRUE))


### relationship with article count
###################################

# impute missing values
set.seed(0)
imputed_climate <- missForest::missForest(climate |> select(article_count, extreme_temperature, drought, flood, storm, wildfire, gdp_per_capita) |> as.data.frame())
imputed_climate <- imputed_climate$ximp

# random forest
library(ranger)

set.seed(1)
rf_model <- ranger(article_count ~ extreme_temperature + drought + flood + storm + wildfire + gdp_per_capita, 
                   data = imputed_climate, 
                   num.trees = 500, 
                   importance = "impurity")

# plot variable importance
variable_importance <- rf_model$variable.importance

importance_df <- tibble(
  variable = names(variable_importance),
  importance = variable_importance
) |> 
  mutate(variable = case_when(
    variable == "extreme_temperature" ~ "Extreme temperature events/year",
    variable == "drought" ~ "Droughts/year",
    variable == "flood" ~ "Floods/year",
    variable == "wildfire" ~ "Wildfires/year",
    variable == "storm" ~ "Storms/year",
    variable == "gdp_per_capita" ~ "GDP per capita/year"
  ))

ggplot(importance_df, aes(x = importance, y = reorder(variable, importance))) +
  geom_segment(aes(x = 0, xend = importance, yend = variable), color = "#475E74") +
  scale_x_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(linetype = "dotted", color = "#BEC5AD"),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 17)) +
  labs(y = NULL, x = "Importance")
  

# get over- and under-producers
predictions <- predict(rf_model, imputed_climate)$predictions
climate |> 
  mutate(preds = predictions,
         diff = article_count - preds) |> 
  select(country, diff) |> 
  arrange(desc(diff)) |> 
  slice_head(n = 5)

climate |> 
  mutate(preds = predictions,
         diff = article_count - preds) |> 
  select(country, diff) |> 
  arrange(diff) |> 
  slice_head(n = 5)
