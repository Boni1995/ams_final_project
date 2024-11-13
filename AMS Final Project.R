library(openxlsx)

# Dataset regarding well being
df_well_being <- read.xlsx("C:\\Users\\franc\\Documents\\GitHub\\ams_final_project\\Dataset\\Data OECD Europe.xlsx")

str(df_well_being)

wb_na <- sapply(df_well_being, function(x) sum(is.na(x)))
wb_na # No nulls in this dataset

# Dataset regarding heart diseases
df_heart_disease <- read.csv("C:\\Users\\franc\\Documents\\GitHub\\ams_final_project\\Dataset\\cardio_train.csv", sep=";")

str(df_heart_disease)

hd_na <- sapply(df_heart_disease, function(x) sum(is.na(x)))
hd_na # No nulls in this dataset

