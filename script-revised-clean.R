library(readxl); library(dplyr)
library(tidyr)


###################################### DATA
# to protect privacy, ages have been aggregated into categories
#raw data not shared due to PII
#original analysis used field-methods-xlsx
field=read_xlsx('data-raw data.xlsx', sheet='field')
lab=read_xlsx('data-raw data.xlsx', sheet='lab')

#  rename location column in field to match lab
field <- field |> rename(Location = Market)

# mask age to prevent re-identification of individuals
lab <- lab |>
    mutate(
        Age_Numeric = as.numeric(gsub("\\+", "", Age)),
        Age = factor(ifelse(Age_Numeric >= 50, "50+", "Under_50"))
    ) |>
    dplyr::select(-Age_Numeric)

field <- field |>
    mutate(
        Age_Numeric = as.numeric(Age),
        Age = factor(ifelse(Age_Numeric >= 50, "50+", "Under_50"))
    ) |>
    dplyr::select(-Age_Numeric)


# create new Study column and create unique ids from Ind value
# participants in field are distinct individuals from participants in lab
# example: Ind value 1 from field is not the same individual as In value from lab
field <- field |>
    mutate(Study = "Field",
           UID = paste0("F_", Ind)) |>
    select(-Ind)

lab <- lab |>
    mutate(Study = "Lab",
           UID = paste0("L_", Ind))|>
    select(-Ind)

# merge
df <- bind_rows(field, lab)
rm(field, lab)


# relocate
df <- df |>
    relocate(UID, .before = Location) |>
    relocate(Study, .before = Location) |>
    relocate(Age, .before = Odor1)


###################################### SPLIT DATA
forced <- df |>
    select(-starts_with("Free")) |>
    pivot_longer(
        cols = matches("(Odor|Forced)\\d"),
        names_to = c(".value", "Trial"),
        names_pattern = "(Odor|Forced)(\\d)"
    ) |>
    mutate(Trial = as.numeric(Trial))

free <- df |>
    select(Study, UID, Location, Age, Sex, matches("(Odor|Free)\\d")) |>
    pivot_longer(
        cols = matches("(Odor|Free)\\d"),
        names_to = c(".value", "Trial"),
        names_pattern = "(Odor|Free)(\\d)"
    ) |>
    mutate(Trial = as.numeric(Trial))



###################################### SAVE DATA
write.csv(forced, "revised-data-forced.csv", row.names = FALSE)
write.csv(free, "revised-data-free.csv", row.names = FALSE)



######################################TIDY
rm(list=ls())
gc()

