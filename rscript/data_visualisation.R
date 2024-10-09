library(tidyverse)
library(lubridate)
library(fpp3)
library(ggthemes)
#----------------

# read data-------

issues <- read_rds("data/full_issues_correct_amountissues.rds")

# manipulate data---------------
issues_selected <- issues |> select(transaction_date,item,quantity_product_issued,amount_issued)
daily_issues <- issues_selected |> mutate(date=as_date(transaction_date)) |> group_by(date,item) |> 
  summarise(quantity_product_issued=sum(quantity_product_issued), amount_issued=sum(amount_issued)) |> 
  ungroup()

## create a tsibble
daily_issues_tsibble <- daily_issues |> as_tsibble(index = date, key=item) |> 
  fill_gaps(quantity_product_issued = 0, amount_issued=0)


# daily visualisation---------------------

issue_plot <- function(my_item) {
ggplot(data = daily_issues_tsibble |> filter(item == my_item), mapping = aes(x = date, y = quantity_product_issued))+
  geom_point(size=1)+
  geom_line()+
  theme_few()
}


issue_plot("Sulphamethoxazole + Trimethoprim - (200mg + 40mg)/5ml - Suspension")

daily_issues_tsibble_features <- daily_issues_tsibble |> 
  features(quantity_product_issued, feature_set(pkgs = "feasts"))

ggplot(data = daily_issues_tsibble_features, mapping = aes(x = trend_strength, y = seasonal_strength_week))+
  geom_point()+
  theme_few()

  
high_sesonal <- daily_issues_tsibble_features |> filter(seasonal_strength_week==max(seasonal_strength_week)) |> pull(item)

gg_season(daily_issues_tsibble |> filter(item == high_sesonal), period = "week")

## Monthly visualisation

monthly_issues_tsibble <- daily_issues_tsibble |> index_by(month=yearmonth(date)) |> group_by_key() |> 
  summarise(quantity_product_issued=sum(quantity_product_issued),
            amount_issued=sum(amount_issued))

write_rds(monthly_issues_tsibble,"data/monthly_issues_tsibble.rds")


# MONTHLY visualisation--------
items <- unique(monthly_issues_tsibble$item)
issue_plot_monthly <- function(my_item) {
  ggplot(data = monthly_issues_tsibble |> filter(item == my_item), mapping = aes(x = month, y = quantity_product_issued))+
    geom_point(size=1)+
    geom_line()+
    theme_few()+
    labs(title=my_item)
}

my_plot <- list()
for (i in (1:40)) {
  my_plot[[i]] <- issue_plot_monthly(items[i])
}

my_plot
issue_plot_monthly("Sulphamethoxazole + Trimethoprim - (200mg + 40mg)/5ml - Suspension")

gg_season(monthly_issues_tsibble |> 
            filter(item == "Sulphamethoxazole + Trimethoprim - (200mg + 40mg)/5ml - Suspension"))


monthly_issues_tsibble_features <- monthly_issues_tsibble |> 
  features(quantity_product_issued, feature_set(pkgs = "feasts"))

ggplot(data = monthly_issues_tsibble_features, mapping = aes(x = trend_strength, 
                                                             y = seasonal_strength_year))+
  geom_point()

high_sesonal <- monthly_issues_tsibble_features |> filter(!is.na(seasonal_strength_year)) |> 
  filter(seasonal_strength_year==max(seasonal_strength_year)) |> pull(item)

high_trend <- monthly_issues_tsibble_features |> filter(!is.na(seasonal_strength_year)) |> 
  filter(trend_strength==max(trend_strength)) |> pull(item)

low_sesonal <- monthly_issues_tsibble_features |> filter(!is.na(seasonal_strength_year)) |> 
  filter(seasonal_strength_year==min(seasonal_strength_year)) |> pull(item)

low_sesonal <- monthly_issues_tsibble_features |> filter(!is.na(seasonal_strength_year)) |> 
  filter(seasonal_strength_year==min(seasonal_strength_year)) |> pull(item)

gg_season(monthly_issues_tsibble |> filter(item == low_sesonal))

autoplot(monthly_issues_tsibble |> filter(item == low_sesonal))

# PCA


library(broom)
pcs <- monthly_issues_tsibble_features |>
  select(-item) |>
  prcomp(scale = TRUE) |>
  augment(monthly_issues_tsibble_features)
pcs |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2)) +
  geom_point() +
  theme(aspect.ratio = 1)