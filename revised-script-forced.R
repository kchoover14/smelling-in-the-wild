library(dplyr) #for data wrangling
library(broom) #for tidy tables
library(ggplot2) #for plots
library(irr) #for ICC test
library(tidyr) #for pivot_wider
library(skimr) #look at data distribution

######################## ACCURACY  reco thid b/c of the testing order now
forced = read.csv("data-forced.csv")

# create accuracy: match == TRUE means the forced choice matched the odor
forced  =  forced |>
    mutate(
        Correct = mapply(grepl, pattern = Odor, x = Forced), #T/F
        Correctn = as.numeric(Correct) #binary version of logical
    )

# ACCURACY TEST FOR EFFECTS KNOWN EFFECT FROM AGE AND SEX AND UNKNOWN EFFECT FROM STUDY
check_accuracy <- glm(Correctn ~ Sex + Age + Study,
                   data = forced,
                   family = binomial)
check_accuracy |>
    tidy(exponentiate = TRUE) |>
    select(term, OR = estimate, p.value) # no age but yes to sex and study
#this is the raw data that will be used for oa so test oa too
#KEEP STUDIES AND SEX SEPARATE TO FIND AN ENV EFFECT

# Split datasets
forced_field_f <- forced |> filter(Study == "Field", Sex == "Female")
forced_field_m <- forced |> filter(Study == "Field", Sex == "Male")
forced_lab_f <- forced |> filter(Study == "Lab", Sex == "Female")
forced_lab_m <- forced |> filter(Study == "Lab", Sex == "Male")

# Chi-square for females, Fisher's for males
chisq.test(table(forced_field_f$Odor, forced_field_f$Correctn))
fisher.test(table(forced_field_m$Odor, forced_field_m$Correctn))
chisq.test(table(forced_lab_f$Odor, forced_lab_f$Correctn))
fisher.test(table(forced_lab_m$Odor, forced_lab_m$Correctn))

# Run tests and store results
test_results <- tibble(
    Study = c("Field", "Field", "Lab", "Lab"),
    Sex = c("Female", "Male", "Female", "Male"),
    test = c(
        paste0("\u03C7\u00B2(4) = ", round(chisq.test(table(forced_field_f$Odor, forced_field_f$Correctn))$statistic, 2),
               ", p < 0.001"),
        paste0("Fisher's p = ", round(fisher.test(table(forced_field_m$Odor, forced_field_m$Correctn))$p.value, 3)),
        paste0("\u03C7\u00B2(4) = ", round(chisq.test(table(forced_lab_f$Odor, forced_lab_f$Correctn))$statistic, 2),
               ", p = ", round(chisq.test(table(forced_lab_f$Odor, forced_lab_f$Correctn))$p.value, 3)),
        paste0("Fisher's p = ", round(fisher.test(table(forced_lab_m$Odor, forced_lab_m$Correctn))$p.value, 3))
    )
)

# Global odor order
odor_order <- forced |>
    filter(Study == "Field", Sex == "Female") |>
    group_by(Odor) |>
    summarise(mean = mean(Correctn, na.rm = TRUE)) |>
    arrange(mean) |>
    pull(Odor)

# Plot
forced |>
    group_by(Study, Sex, Odor) |>
    summarise(
        pct_correct = mean(Correctn, na.rm = TRUE),
        n = n(),
        se = sqrt(pct_correct * (1 - pct_correct) / n),
        .groups = "drop"
    ) |>
    mutate(
        Odor = factor(Odor, levels = odor_order),
        group_label = paste(Study, Sex, sep = " - ")
    ) |>
    ggplot(aes(x = Odor, y = pct_correct)) +
    geom_col(fill = "#1B4F72", width = 0.6) +
    geom_errorbar(aes(ymin = pct_correct - se,
                      ymax = pct_correct + se),
                  width = 0.2) +
    geom_text(aes(label = scales::percent(pct_correct, accuracy = 1)),
              hjust = 1.2,
              color = "white",
              size = 3.5) +
    geom_text(data = test_results |>
                  mutate(group_label = paste(Study, Sex, sep = " - ")),
              aes(x = 0.6, y = 0.55, label = test),
              hjust = 0,
              size = 2.8,
              color = "#2C3E50",
              inherit.aes = FALSE) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.2)) +
    coord_flip() +
    facet_wrap(~ group_label, nrow = 2) +
    labs(x = "",
         y = "Percent Correctly Identified",
         caption = "Error bars show standard error.") +
    theme_classic() +
    theme(
        text = element_text(size = 12),
        axis.text = element_text(color = "black", size = 12)
    )
ggsave("revised-forced-accuracy.png",
       plot = last_plot(),
       width = 8,
       height = 8,
       units = "in",
       dpi = 300)


######################## CONSISTENCY
#CONSISTENCY IS HOW OFTEN A PERSON IDENTIFIED AN ODOR USING THE SAME TERM
#create consistency by odor data to see if consistency by odor predicts consistency on another
consistency <- forced |>
    group_by(UID, Odor) |>
    summarize(
        unique_labels = n_distinct(Forced),
        .groups = "drop"
    ) |>
    mutate(is_consistent = ifelse(unique_labels == 1, 1, 0)) |>
    left_join(
        forced |>
            select(UID, Study, Age, Sex) |>
            distinct(UID, .keep_all = TRUE),
        by = "UID"
    )
# TEST FOR EFFECTS KNOWN EFFECT FROM AGE AND SEX AND UNKNOWN EFFECT FROM STUDY
check_consistency <- glm(is_consistent ~ Sex + Age + Study,
                         data = consistency,
                         family = binomial)
check_consistency |>
    tidy(exponentiate = TRUE) |>
    select(term, OR = estimate, p.value) # no age but yes to sex and study
#SEX AND STUDY SIGNIFICANT, AGE NOT; SPLIT BY SEX AND STUDY

#split by study and sex
consistency_field_f <- consistency |> filter(Study == "Field" & Sex == "Female")
consistency_field_m <- consistency |> filter(Study == "Field" & Sex == "Male")
consistency_lab_f <- consistency |> filter(Study == "Lab" & Sex == "Female")
consistency_lab_m <- consistency |> filter(Study == "Lab" & Sex == "Male")

### Individual level question: Are some people more consistent than others?
# ICC for each - reshape to wide first
icc_test <- function(df) {
    df |>
        select(UID, Odor, is_consistent) |>
        pivot_wider(names_from = Odor, values_from = is_consistent) |>
        select(-UID) |>
        icc(model = "twoway", type = "consistency")
}

icc_test(consistency_field_f) #ICC 0.113
icc_test(consistency_field_m) #ICC 0.003
icc_test(consistency_lab_f) #ICC 0.139
icc_test(consistency_lab_m) #ICC 0.576

# test sample sizes
consistency_field_f |> count(Study, Sex, Odor)
consistency_field_m |> count(Study, Sex, Odor)
consistency_lab_f |> count(Study, Sex, Odor)
consistency_lab_m |> count(Study, Sex, Odor)

# Odor-level questions; are people more consistent with some odors?
#samples reasonable enough for testing if small
chisq.test(table(consistency_field_f$Odor, consistency_field_f$is_consistent))
chisq.test(table(consistency_lab_f$Odor, consistency_lab_f$is_consistent))

fisher.test(table(consistency_field_m$Odor, consistency_field_m$is_consistent))
fisher.test(table(consistency_lab_m$Odor, consistency_lab_m$is_consistent))

#### PLOT
# Calculate global order based on overall mean
odor_order <- consistency |>
  group_by(Odor) |>
  summarise(overall_mean = mean(is_consistent, na.rm = TRUE)) |>
  arrange(overall_mean) |>
  pull(Odor)

consistency |>
  group_by(Study, Sex, Odor) |>
  summarise(
    pct_consistent = mean(is_consistent, na.rm = TRUE),
    n = n(),
    se = sqrt(pct_consistent * (1 - pct_consistent) / n),
    .groups = "drop"
  ) |>
  mutate(
    Odor = factor(Odor, levels = odor_order),
    group_label = paste(Study, Sex, sep = " - ")
  ) |>
  ggplot(aes(x = Odor, y = pct_consistent)) +
  geom_col(fill = "#1B4F72", width = 0.6) +
  geom_errorbar(aes(ymin = pct_consistent - se,
                    ymax = pct_consistent + se),
                width = 0.2) +
  geom_text(aes(label = scales::percent(pct_consistent, accuracy = 1)),
            hjust = 1.2,
            color = "white",
            size = 3.5) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2)) +
  coord_flip() +
  facet_wrap(~ group_label, nrow = 2) +
  labs(x = "",
       y = "Percent Consistent",
       caption = "Error bars show standard error.\n* Field females p=0.001 (chi-square); all other groups non-significant.") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(color = "black", size = 12),
    plot.caption = element_text(size = 10, color = "#2C3E50")
  )
ggsave("revised-forced-consistency.png",
       plot = last_plot(),
       width = 8,
       height = 8,
       units = "in",
       dpi = 300)


######################## OLFACTORY ABILITY
# OLFACTORY ABILITY IS HOW MANY ODORS OUT OF 5 WERE CORRECTLY IDENTIFIED AS COUNT AND PERCENT
oa <- forced |>
    group_by(UID, Study, Location, Sex, Age) |>
    summarize(
        oaCount = sum(Correctn, na.rm = TRUE),
        oaPercent = mean(Correctn, na.rm = TRUE) * 100,
        .groups = "drop"
    )


#FOR EFFECTS KNOWN EFFECT FROM AGE AND SEX AND UNKNOWN EFFECT FROM STUDY
glm(oaPercent / 100 ~ Sex + Age + Study,
    data = oa,
    family = quasibinomial) |>
    tidy(exponentiate = TRUE) |>
    select(term, OR = estimate, p.value) |>
    print()
#SEX AND STUDY SIGNIFICANT, AGE NOT


#look at distribution for assumptions prior to glm testing
skim(oa)
ggplot(oa, aes(x = factor(oaPercent))) +
    geom_bar(position = "dodge") +
    labs(x = "", y = "") +
    scale_fill_viridis_d(begin = 0.15, end = 0.65) +
    theme_classic()


# Odds Ratios for Females in the Field
# Field models
field_female_oa <- oa |>
    filter(Study == "Field" & Sex == "Female") |>
    mutate(Location = relevel(factor(Location), ref = "Tate")) |>
    glm(oaPercent/100 ~ Location, data = _, family = quasibinomial) |>
    tidy(exponentiate = TRUE, conf.int = TRUE) |>
    select(term, OR = estimate, p.value, conf.low, conf.high)

field_male_oa <- oa |>
    filter(Study == "Field" & Sex == "Male") |>
    mutate(Location = relevel(factor(Location), ref = "Tate")) |>
    glm(oaPercent/100 ~ Location, data = _, family = quasibinomial) |>
    tidy(exponentiate = TRUE, conf.int = TRUE) |>
    select(term, OR = estimate, p.value, conf.low, conf.high)

# Lab models
lab_female_oa <- oa |>
    filter(Study == "Lab" & Sex == "Female") |>
    mutate(Location = relevel(factor(Location), ref = "Clean")) |>
    glm(oaPercent/100 ~ Location, data = _, family = quasibinomial) |>
    tidy(exponentiate = TRUE, conf.int = TRUE) |>
    select(term, OR = estimate, p.value, conf.low, conf.high)

lab_male_oa <- oa |>
    filter(Study == "Lab" & Sex == "Male") |>
    mutate(Location = relevel(factor(Location), ref = "Clean")) |>
    glm(oaPercent/100 ~ Location, data = _, family = quasibinomial) |>
    tidy(exponentiate = TRUE, conf.int = TRUE) |>
    select(term, OR = estimate, p.value, conf.low, conf.high)

field_female_oa
field_male_oa
lab_female_oa
lab_male_oa



###################################### DIRECTION ANALYSIS
# Did people change from wrong to right between Tate and Borough for env effect
# can this address the learning issue?
# Field only - tracks change from Tate baseline
# Categories: Learned (wrong->right), Distracted (right->wrong),
#             Confirmed correct (right->right), Still wrong (wrong->wrong)

# Tate baseline correct/incorrect per UID x Odor
tate_response <- forced |>
    filter(Study == "Field", Location == "Tate") |>
    select(UID, Odor, tate_correct = Correctn)

# Join non-Tate field rows against Tate baseline
direction_analysis <- forced |>
    filter(Study == "Field", Location != "Tate") |>
    left_join(tate_response, by = c("UID", "Odor")) |>
    mutate(
        direction = case_when(
            tate_correct == 0 & Correctn == 1 ~ "Learned",
            tate_correct == 1 & Correctn == 0 ~ "Distracted",
            tate_correct == 1 & Correctn == 1 ~ "Confirmed correct",
            tate_correct == 0 & Correctn == 0 ~ "Still wrong"
        )
    )

# Summary table
direction_summary <- direction_analysis |>
    count(Sex, Location, direction) |>
    group_by(Sex, Location) |>
    mutate(
        total = sum(n),
        pct = round(n / total * 100, 1)
    ) |>
    ungroup()
print(direction_summary)

# Plot
direction_analysis |>
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
         caption = "Tate excluded (baseline). Field study only.") +
    theme_classic() +
    theme(
        text = element_text(size = 10),
        axis.text = element_text(color = "black", size = 10),
        legend.position = "top",
        plot.caption = element_text(size = 9, color = "black")
    )
ggsave("revised-forced-direction.png",
       plot = last_plot(),
       width = 5, height = 4, units = "in", dpi = 300)





################################ CORRELATION
# Collapse OA to one mean score per UID across locations
# Avoids pseudoreplication from multiple rows per UID in oa
oa_mean <- oa |>
    group_by(UID, Study, Sex, Age) |>
    summarise(mean_oa = mean(oaPercent, na.rm = TRUE),
              .groups = "drop")

oa_consistency <- oa_mean |>
    left_join(
        consistency |>
            group_by(UID) |>
            summarise(mean_consistency = mean(is_consistent, na.rm = TRUE)),
        by = "UID"
    )

# Pooled correlation
cor.test(oa_consistency$mean_oa, oa_consistency$mean_consistency,
         method = "kendall")

# By Study x Sex
oa_consistency |>
    group_by(Study, Sex) |>
    summarise(
        tau = cor.test(mean_oa, mean_consistency,
                       method = "kendall")$estimate,
        p   = cor.test(mean_oa, mean_consistency,
                       method = "kendall")$p.value,
        n   = n(),
        .groups = "drop"
    )
# MALES ARE THE ODD ONE OUT, THE SOUTHWARK DIP


######################## TIDY
rm(list=ls())
gc()
