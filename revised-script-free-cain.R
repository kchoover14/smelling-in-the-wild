library(readxl)
library(dplyr) #wrangling
library(tidyr) #separate rows
library(nnet) # multinomial logistic regression
library(car) # anova
library(broom) #tidy output
library(scales) #plot labels
library(ggplot2) #plotting
library(skimr) #look at data distribution

###################################### DATA
field=read_xlsx('data-cain.xlsx', sheet='field')
lab=read_xlsx('data-cain.xlsx', sheet='lab')

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
    relocate(Study, .before = Location)

#### get demographics
free = read.csv("data-free.csv")

#join
df <- df |>
    left_join(free |> select(UID, Study, Location, Sex, Age) |> distinct(),
              by = c("UID", "Study", "Location"))

# Pivot to long format (Odor1-5, Free1-5, Cain1-5 ??? single columns)
df <- df |>
    pivot_longer(
        cols = matches("^(Odor|Free|Cain)\\d$"),
        names_to = c(".value", "trial"),
        names_pattern = "(.+)(\\d)"
    )

# Join with lookup
lookup <- read.csv("lookup-free-codes-final.csv")

df <- df |>
    left_join(lookup |> select(Free, Odor, Code),
              by = c("Free", "Odor"))

# Check join for any unmatched rows
unmatched <- df |> filter(is.na(Code))
if (nrow(unmatched) > 0) {
    cat("WARNING: Unmatched rows:\n")
    print(unmatched)
} else {
    cat("All rows matched successfully!\n")
}

rm(lookup, free, unmatched)




###################################### IDENTIFICATION/ACCURACY
# Tests whether identification rate varies by odor, with location as environmental effect

# Set reference level to 'correct'
df$Cain <- relevel(factor(df$Cain), ref = "correct")

# Preliminary model: Sex + Age + Study
prelim_cain <- multinom(Cain ~ Sex + Age + Study, data = df)
print(summary(prelim_cain))
print(car::Anova(prelim_cain, type = "II"))
#### STUDY SEX AND AGE SIGNIFICANT, SAMPLE SIZES TOO SMALL?


# Check sample sizes by Study x Sex x Age
df |>
    group_by(Study, Sex, Age) |>
    summarise(n = n(), .groups = "drop") |>
    print()

# Check Cain distribution within each group
df |>
    group_by(Study, Sex, Age, Cain) |>
    summarise(n = n(), .groups = "drop") |>
    pivot_wider(names_from = Cain, values_from = n, values_fill = 0) |>
    print()



# Split by Study x Sex x Age (8 groups)
df_field_f_50plus    <- df |> filter(Study == "Field", Sex == "Female", Age == "50+")
df_field_f_under50   <- df |> filter(Study == "Field", Sex == "Female", Age == "Under_50")
df_field_m_50plus    <- df |> filter(Study == "Field", Sex == "Male", Age == "50+")
df_field_m_under50   <- df |> filter(Study == "Field", Sex == "Male", Age == "Under_50")
df_lab_f_50plus      <- df |> filter(Study == "Lab", Sex == "Female", Age == "50+")
df_lab_f_under50     <- df |> filter(Study == "Lab", Sex == "Female", Age == "Under_50")
df_lab_m_50plus      <- df |> filter(Study == "Lab", Sex == "Male", Age == "50+")
df_lab_m_under50     <- df |> filter(Study == "Lab", Sex == "Male", Age == "Under_50")

# Multinomial models: Cain ~ Odor for each group
model1 <- multinom(Cain ~ Odor, data = df_field_f_50plus)
print(summary(model1))
print(car::Anova(model1))

model2 <- multinom(Cain ~ Odor, data = df_field_f_under50)
print(summary(model2))
print(car::Anova(model2))

model3 <- multinom(Cain ~ Odor, data = df_field_m_50plus)
print(summary(model3))
print(car::Anova(model3))

model4 <- multinom(Cain ~ Odor, data = df_field_m_under50)
print(summary(model4))
print(car::Anova(model4))

model5 <- multinom(Cain ~ Odor, data = df_lab_f_50plus)
print(summary(model5))
print(car::Anova(model5))

model6 <- multinom(Cain ~ Odor, data = df_lab_f_under50)
print(summary(model6))
print(car::Anova(model6))

model7 <- multinom(Cain ~ Odor, data = df_lab_m_50plus)
print(summary(model7))
print(car::Anova(model7))

model8 <- multinom(Cain ~ Odor, data = df_lab_m_under50)
print(summary(model8))
print(car::Anova(model8))

print(car::Anova(model1))
print(car::Anova(model2))
print(car::Anova(model3))
print(car::Anova(model4))
print(car::Anova(model5))
print(car::Anova(model6))
print(car::Anova(model7))
print(car::Anova(model8))
##### AGE BIASED SAMPLE, UNDER 50 ONLY HAVE STAT VALIDITY IN SAMPLE SIZE/NA


#### PLOT
# Global odor order from field females (by % correct)
odor_order_cain <- df |>
    filter(Sex == "Female") |>
    group_by(Odor) |>
    summarise(pct_correct = mean(Cain == "correct", na.rm = TRUE)) |>
    arrange(pct_correct) |>
    pull(Odor)

# Calculate proportions by Sex and Odor
cain_summary <- df |>
    count(Sex, Odor, Cain) |>
    group_by(Sex, Odor) |>
    mutate(
        total = sum(n),
        pct = n / total,
        # Reverse factor levels so correct is first (bottom of stack)
        Cain = factor(Cain, levels = c("incorrect", "far miss", "near miss", "correct"))
    ) |>
    ungroup() |>
    mutate(Odor = factor(Odor, levels = odor_order_cain))

# Stacked bar plot
ggplot(cain_summary, aes(x = Odor, y = pct, fill = Cain)) +
    geom_col(position = "stack", width = 0.6) +
    geom_text(aes(label = percent(pct, accuracy = 1)),
              position = position_stack(vjust = 0.5),
              color = "white", size = 3) +
    scale_fill_manual(
        values = c(
            "correct"   = "#1B4F72",
            "near miss" = "#20a486",
            "far miss"  = "#7F8C8D",
            "incorrect" = "#BDC3C7"
        ),
        # Reverse legend order to match visual stack
        breaks = c("correct", "near miss", "far miss", "incorrect")
    ) +
    scale_y_continuous(labels = percent) +
    coord_flip() +
    facet_wrap(~ Sex, nrow = 2) +
    labs(x = "", y = "", fill = "Cain Category",
         caption = "Cain 4-level coding. Under_50 groups show significant odor variation.") +
    theme_classic() +
    theme(
        text = element_text(size = 12),
        axis.text = element_text(color = "black", size = 12),
        legend.position = "top"
    )
ggsave("revised-free-accuracy-cain.png",
       plot = last_plot(),
       width = 8,
       height = 8,
       units = "in",
       dpi = 300)





###################################### OLFACTORY ABILITY
# Tests environmental effect on identification ability

# OA = proportion of odors correctly identified (Source present) per UID x Location
# Weighted OA score per UID x Location
# Weights: correct=1, near miss=0.67, far miss=0.33, incorrect=0
oa_cain <- df |>
    mutate(
        weight = case_when(
            Cain == "correct"   ~ 1.0,
            Cain == "near miss" ~ 0.67,
            Cain == "far miss"  ~ 0.33,
            Cain == "incorrect" ~ 0.0
        )
    ) |>
    group_by(UID, Study, Location, Sex, Age) |>
    summarise(
        oaWeighted = sum(weight, na.rm = TRUE),
        oaPercent = oaWeighted / 5 * 100,  # Convert to 0-100 scale
        .groups = "drop"
    )

#FOR EFFECTS KNOWN EFFECT FROM AGE AND SEX AND UNKNOWN EFFECT FROM STUDY
glm(oaPercent / 100 ~ Sex + Age + Study,
    data = oa_cain,
    family = quasibinomial) |>
    tidy(exponentiate = TRUE) |>
    select(term, OR = estimate, p.value) |>
    print()
#SEX SIGNIFICANT only but reference levels vary by study so split by study and sex

#look at distribution for assumptions prior to glm testing
skim(oa_cain)
ggplot(oa_cain, aes(x = factor(oaPercent))) +
    geom_bar(position = "dodge") +
    labs(x = "", y = "") +
    scale_fill_viridis_d(begin = 0.15, end = 0.65) +
    theme_classic()

# OA by location
# Preliminary GLM: Sex only significant for overall OA level
# BUT location models require Study split - field and lab locations are separate
# Four models: Study x Sex with study-appropriate reference levels

oa_field_f <- oa_cain |> filter(Study == "Field", Sex == "Female")
oa_field_m <- oa_cain |> filter(Study == "Field", Sex == "Male")
oa_lab_f   <- oa_cain |> filter(Study == "Lab",   Sex == "Female")
oa_lab_m   <- oa_cain |> filter(Study == "Lab",   Sex == "Male")

field_female_oa <- oa_field_f |>
    mutate(Location = relevel(factor(Location), ref = "Tate")) |>
    glm(oaPercent / 100 ~ Location, data = _, family = quasibinomial) |>
    tidy(exponentiate = TRUE, conf.int = TRUE) |>
    select(term, OR = estimate, p.value, conf.low, conf.high)

field_male_oa <- oa_field_m |>
    mutate(Location = relevel(factor(Location), ref = "Tate")) |>
    glm(oaPercent / 100 ~ Location, data = _, family = quasibinomial) |>
    tidy(exponentiate = TRUE, conf.int = TRUE) |>
    select(term, OR = estimate, p.value, conf.low, conf.high)

lab_female_oa <- oa_lab_f |>
    mutate(Location = relevel(factor(Location), ref = "Clean")) |>
    glm(oaPercent / 100 ~ Location, data = _, family = quasibinomial) |>
    tidy(exponentiate = TRUE, conf.int = TRUE) |>
    select(term, OR = estimate, p.value, conf.low, conf.high)

lab_male_oa <- oa_lab_m |>
    mutate(Location = relevel(factor(Location), ref = "Clean")) |>
    glm(oaPercent / 100 ~ Location, data = _, family = quasibinomial) |>
    tidy(exponentiate = TRUE, conf.int = TRUE) |>
    select(term, OR = estimate, p.value, conf.low, conf.high)

print(field_female_oa)
print(field_male_oa)
print(lab_female_oa)
print(lab_male_oa)




###################################### DIRECTION ANALYSIS
# Did people change from wrong to right between Tate and Borough for env effect
# can this address the learning issue?

# Tate as baseline
tate_response_cain <- df |>
    filter(Study == "Field", Location == "Tate") |>
    mutate(tate_correct = as.integer(Cain == "correct")) |>
    select(UID, Odor, tate_correct)

direction_cain <- df |>
    filter(Study == "Field", Location != "Tate") |>
    mutate(current_correct = as.integer(Cain == "correct")) |>
    left_join(tate_response_cain, by = c("UID", "Odor")) |>
    mutate(
        direction = case_when(
            tate_correct == 0 & current_correct == 1 ~ "Learned",
            tate_correct == 1 & current_correct == 0 ~ "Distracted",
            tate_correct == 1 & current_correct == 1 ~ "Confirmed correct",
            tate_correct == 0 & current_correct == 0 ~ "Still wrong"
        )
    )

# Summary
direction_summary_cain <- direction_cain |>
    count(Sex, Location, direction) |>
    group_by(Sex, Location) |>
    mutate(
        total = sum(n),
        pct = round(n / total * 100, 1)
    ) |>
    ungroup()
print(direction_summary_cain)

# Plot
direction_cain |>
    count(Sex, Location, direction) |>
    group_by(Sex, Location) |>
    mutate(
        pct = n / sum(n),
        Location = factor(Location, levels = c("Southwark", "Borough")),
        direction = factor(direction,
                           levels = c("Confirmed correct", "Learned",
                                      "Distracted", "Still wrong"))
    ) |>
    ggplot(aes(x = Location, y = pct, fill = direction)) +
    geom_col(position = "stack") +
    geom_text(aes(label = percent(pct, accuracy = 1)),
              position = position_stack(vjust = 0.5),
              color = "white", size = 3.5) +
    scale_fill_manual(values = c(
        "Confirmed correct" = "#1B4F72",
        "Learned"           = "#20a486",
        "Distracted"        = "#7F8C8D",
        "Still wrong"       = "#BDC3C7"
    )) +
    scale_y_continuous(labels = percent) +
    facet_wrap(~ Sex) +
    labs(x = "", y = "", fill = "",
         caption = "Tate excluded (baseline). Field study only. Cain correct.") +
    theme_classic() +
    theme(
        text = element_text(size = 12),
        axis.text = element_text(color = "black", size = 11),
        legend.position = "top",
        plot.caption = element_text(size = 10, color = "#2C3E50")
    )
ggsave("free-direction-cain.png",
       plot = last_plot(),
       width = 5, height = 4, units = "in", dpi = 300)




######################## TIDY
rm(list=ls())
gc()

