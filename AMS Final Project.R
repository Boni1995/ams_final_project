library(openxlsx)
library(dplyr)
library(tidyr)

# Dataset regarding well being
df_wb <- read.xlsx("C:\\Users\\franc\\Documents\\GitHub\\ams_final_project\\Dataset\\OECD_Well_Being.xlsx")

str(df_wb) # We have 447 tuples and 28 columns (25 are the attributes to analyze)


# Check NAs
wb_na <- sapply(df_wb, function(x) sum(length(which(is.na(x)))))
wb_na # No nulls in this dataset

summary(df_wb) # Some numerical values has character class, so we need to correct them


# We realized that some missing values were replaced with "..", so this may be the problem why we have numerical values as character
wb_missing_values <- sapply(df_wb, function(col) sum(col == "..")/nrow(df_wb)*100)
as.data.frame(wb_missing_values)
max(wb_missing_values) # Max missing values in an attribute is 5,37%, so we consider it low

# Now we will check this percentage of missing values, but by country
missing_values_country <- aggregate(. ~ Country+Region, data = df_wb, function(x) sum(x == ".."))
missing_values_country <- as.data.frame(missing_values_country)

missing_values_country <- aggregate(
  rowSums(df_wb[, 4:ncol(df_wb)] == "..") ~ Country + Region,
  data = df_wb,
  FUN = sum
)

colnames(missing_values_country) <- c("Country", "Region", "n_null")

missing_values_country <- missing_values_country[missing_values_country$n_null > 0, ]

missing_values_country <- table(missing_values_country$Country)
missing_values_country <- as.data.frame(missing_values_country)
colnames(missing_values_country) <- c("Country", "n_cities")

missing_values_country


n_country <- table(df_wb$Country)
n_country <- as.data.frame(n_country)
colnames(n_country) <- c("Country", "n_cities")

missing_by_country <- merge(n_country, missing_values_country, by = "Country")
colnames(missing_by_country) <- c("Country", "n_cities", "n_missing")
missing_by_country$p_na <- (missing_by_country$n_missing/missing_by_country$n_cities)*100

missing_by_country

# We have 3 countries with missing values in all their regions, so we will drop them
df_wb <- df_wb[!(df_wb$Country == "Costa Rica" | df_wb$Country == "Iceland" | df_wb$Country == "Japan"), ]

# The rest of missing values are going to be replaced by the median of each country
df_wb <- df_wb %>% 
  mutate(across(everything(), ~ ifelse(. == "..", NA, .)))

df_wb <- df_wb %>%
  mutate(across(.cols = which(!names(df_wb) %in% names(df_wb)[1:3]), 
                .fns = ~ as.numeric(.)))

df_wb <- df_wb %>% 
  group_by(Country) %>% 
  mutate(across(3:(ncol(df_wb)-1), ~ replace_na(., median(., na.rm = TRUE)))) %>% 
  ungroup()

df_wb <- as.data.frame(df_wb)

summary(df_wb)
head(df_wb, 10)
