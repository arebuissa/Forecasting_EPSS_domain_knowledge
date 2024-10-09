library(tidyverse)
library(readxl)
library(lubridate)
# ----read data ----------

# issued_forecast_original <- read_excel("data/forecast_issued-data.xlsx") |> janitor::clean_names()
# write_rds(issued_forecast_original,"data/issued_forecast_original.rds")

full_issues_original <- read_rds("../data/full_issued.rds")

##--------------- preparation

full_issues <- full_issues_original |> mutate(expiry_date = as_date(expiry_date)) |>
select(transaction_date,item,expiry_date,unit_price,unit,quantity_unit_issued=quantity_issued,
       quantity_product_issued=quantity_issued_new_each, amount_issued,environment,region,zone,woreda,institution_id,
       institution_type) |> drop_na()


full_issues_correct_items <- full_issues |> mutate(
  item = case_when(
    item == "Tetracycline - 0.01 - Eye Ointment" ~ "Tetracycline - 1% - Eye Ointment" ,
    item == "Sulphamethoxazole + Trimethoprim - (200mg + 40mg)/5ml - Mixture" ~ "Sulphamethoxazole + Trimethoprim - (200mg + 40mg)/5ml - Suspension",
    item == "Insulin Soluble Human - 100IU/ml in 10ml Vial - Injection(Solution)" ~ "Insulin Soluble Human" ,
    item == "Insulin Soluble / Neutral (HPB) - 100 Units/ml  - Injection" ~ "Insulin Soluble Human" ,
    item == "Propylthiouracil - 100mg - Tablet (Scored)"  ~ "Propylthiouracil - 100mg - Tablet",
    item == "Insuline Isophane Biphasic (Soluble/Isophane Mixture) - (30 + 70)IU/ml in 10ml Vial - Injection (Suspension)"  ~ "Insuline Isophane Biphasic",
    item == "Insuline Isophane Biphasic (Soluble/Isophane Mixture) - (30 + 70)IU/ml in 10ml Vial - Injection(Suspension)"  ~ "Insuline Isophane Biphasic",
    item == "Sulphamethoxazole + Trimethoprim - (200mg +40mg)/5ml - Suspension"  ~  "Sulphamethoxazole + Trimethoprim - (200mg + 40mg)/5ml - Suspension",
    item == "Insulin Soluble Human - 100IU/ml in 10ml Vial - Injection"  ~  "Insulin Soluble Human",
    item == "Insuline Isophane Biphasic (Soluble/Isophane Mixture) - (30 + 70)IU/ml in 10ml Vial - Injection(Suspension)"  ~  "Insuline Isophane Biphasic",
    
    TRUE ~ item)
)


full_issues_correct_price <- full_issues_correct_items |> mutate(unit_price = 
                                      case_when(
                                        (unit_price==0 & item=="Rapid Diagnostic Test") ~ 139.56,
                                        (unit_price==0 & item=="Oral Rehydration Salt") ~ 2.94,
                                        (unit_price==0 & item=="Atrovastatin - 20mg - Tablet") ~ 163.90,
                                        (unit_price==0 & item=="Sodium Chloride (Normal Saline)") ~ 20.90,
                                          TRUE ~ unit_price))

                                                                                    
full_issues_correct_amountissues <- full_issues_correct_price |> 
  mutate(amount_issued = if_else(amount_issued == 0,unit_price*quantity_product_issued  ,amount_issued))


##

write_rds(full_issues_correct_amountissues,"data/full_issues_correct_amountissues.rds")

#--------monthly time series

issues <- read_rds("data/full_issues_correct_amountissues.rds")
# manipulate data---------------
issues_selected <- issues |> select(transaction_date,item,quantity_product_issued,amount_issued)
daily_issues <- issues_selected |> mutate(date=as_date(transaction_date)) |> group_by(date,item) |> 
  summarise(quantity_product_issued=sum(quantity_product_issued), amount_issued=sum(amount_issued)) |> 
  ungroup()

## create a tsibble
daily_issues_tsibble <- daily_issues |> as_tsibble(index = date, key=item) |> 
  fill_gaps(quantity_product_issued = 0, amount_issued=0, .full = end)
monthly_issues_tsibble <- daily_issues_tsibble |> index_by(month=yearmonth(date)) |> group_by_key() |> 
  summarise(quantity_product_issued=sum(quantity_product_issued),
            amount_issued=sum(amount_issued))

write_rds(monthly_issues_tsibble,"data/monthly_issues_tsibble.rds")




