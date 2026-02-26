library(dplyr) #wrangling
library(tidyr) #separate rows
library(stringr) #handling text
library(tibble) #column_to_rownames
library(FactoMineR) #ca analysis
library(factoextra) #ca plot
library(broom) #tidy output
library(scales) #plot labels
library(ggplot2) #plotting
library(irr) #for ICC test\
library(skimr) #look at data distribution

###################################### DATA
free = read.csv("data-free.csv")
lookup <- read.csv("lookup-free-codes-final.csv")

#### join terms
data_coded <- free |>
    left_join(lookup |> select(Free, Odor, Code),
              by = c("Free", "Odor"))

# Check join for any unmatched rows
unmatched <- data_coded |> filter(is.na(Code))
if (nrow(unmatched) > 0) {
    cat("WARNING: Unmatched rows:\n")
    print(unmatched)
} else {
    cat("All rows matched successfully!\n")
}



###################################### ODOR TERMS
# split multi-codes into separate rows
data_long <- data_coded |>
    separate_rows(Code, sep = "\\+") |>
    mutate(Code = str_trim(Code))

cat("Rows before split:", nrow(data_coded), "\n")
cat("Rows after split:", nrow(data_long), "\n")


#### check category distribution
cat("\nCode distribution:\n")
data_long |>
    count(Code, sort = TRUE) |>
    print()

cat("\nCode by Odor:\n")
data_long |>
    count(Odor, Code) |>
    pivot_wider(names_from = Code, values_from = n, values_fill = 0) |>
    print()


#### Contingency table
# CA table: Odor x Code
# Exclude NoLabel, NoDetect, Valence from CA
# (these are not descriptive of odor character)
ca_data <- data_long |>
  #  filter(!Code %in% c("NoLabel", "NoDetect", "Valence")) |>
    count(Odor, Code)

ca_table <- ca_data |>
    pivot_wider(names_from = Code,
                values_from = n,
                values_fill = 0) |>
    column_to_rownames("Odor")

cat("\nCA contingency table:\n")
print(ca_table)


#### Corresponance analysis
ca_result <- CA(ca_table, graph = FALSE)

cat("\nCA eigenvalues:\n")
print(ca_result$eig)

#### Plot
# Biplot: odors (rows) and codes (columns)
fviz_ca_biplot(ca_result,
               repel = TRUE,
               col.row="#20a486", col.col="#440154",
               title = "") +
    theme_classic() +
    theme(
        text = element_text(size = 14),
        axis.text = element_text(color = "black", size = 12),
        plot.caption = element_text(size = 10, color = "black")
    )

ggsave("revised-free-ca.png",
       plot = last_plot(),
       width = 6,
       height = 6,
       units = "in",
       dpi = 300)
cat("\nCA plot saved!\n")


#### save
write.csv(data_long, "revised-data-ca.csv")
cat("Analysis dataset saved to data-ca.csv\n")





###################################### IDENTIFICATION/ACCURACY
# Tests whether identification rate varies by odor, with location as environmental effect

# Use Source for correct from CA, disregard additional choices
accuracy <- data_coded |>
    mutate(Correct = str_detect(Code, "Source"),
           Correctn = as.integer(Correct))

# Preliminary GLM: known effects (Sex, Age) + unknown effect (Study)
check_accuracy <- glm(Correctn ~ Sex + Age + Study,
                      data = accuracy,
                      family = binomial)
check_accuracy |>
    tidy(exponentiate = TRUE) |>
    select(term, OR = estimate, p.value) |>
    print()
#### ONLY SEX IS SIGNIFICANT, AGE AND STUDY ARE NOT

# Split by Sex
accuracy_f <- accuracy |> filter(Sex == "Female")
accuracy_m   <- accuracy |> filter(Sex == "Male")

# check sample sizes
accuracy_f |>
    count(Location, Correctn) |>
    pivot_wider(names_from = Correctn,
                values_from = n,
                values_fill = 0,
                names_prefix = "correct_") |>
    rename(incorrect = correct_0, correct = correct_1) |>
    mutate(total = incorrect + correct,
           pct_correct = round(correct / total * 100, 1)) |>
    arrange(pct_correct) |>
    print()

accuracy_m |>
    count(Location, Correctn) |>
    pivot_wider(names_from = Correctn,
                values_from = n,
                values_fill = 0,
                names_prefix = "correct_") |>
    rename(incorrect = correct_0, correct = correct_1) |>
    mutate(total = incorrect + correct,
           pct_correct = round(correct / total * 100, 1)) |>
    arrange(pct_correct) |>
    print()


# Chi-square for females, Fisher's for males (small n)
chisq.test(table(accuracy_f$Odor, accuracy_f$Correctn))
chisq.test(table(accuracy_m$Odor, accuracy_m$Correctn))

# Store test results for plot annotation
accuracy_tests <- tibble(
    Sex  = c("Female", "Male"),
    test = c(
        paste0("\u03C7\u00B2(4) = ",
               round(chisq.test(table(accuracy_f$Odor, accuracy_f$Correctn))$statistic, 2),
               ", p = ",
               round(chisq.test(table(accuracy_f$Odor, accuracy_f$Correctn))$p.value, 3)),
        paste0("\u03C7\u00B2(4) = ",
               round(chisq.test(table(accuracy_m$Odor, accuracy_m$Correctn))$statistic, 2),
               ", p = ",
               round(chisq.test(table(accuracy_m$Odor, accuracy_m$Correctn))$p.value, 3))
    )
)
print(accuracy_tests)

# Global odor order from field females (matches forced choice convention)
odor_order_acc <- accuracy |>
    filter(Sex == "Female") |>
    group_by(Odor) |>
    summarise(mean_correct = mean(Correctn, na.rm = TRUE)) |>
    arrange(mean_correct) |>
    pull(Odor)

# Plot: faceted horizontal bar chart
accuracy |>
    group_by(Sex, Odor) |>
    summarise(
        pct_correct = mean(Correctn, na.rm = TRUE),
        n = n(),
        se = sqrt(pct_correct * (1 - pct_correct) / n),
        .groups = "drop"
    ) |>
    mutate(
        Odor = factor(Odor, levels = odor_order_acc)
    ) |>
    ggplot(aes(x = Odor, y = pct_correct)) +
    geom_col(fill = "#1B4F72", width = 0.6) +
    geom_errorbar(aes(ymin = pct_correct - se,
                      ymax = pct_correct + se),
                  width = 0.2) +
    geom_text(aes(label = percent(pct_correct, accuracy = 1)),
              hjust = 1.2,
              color = "white",
              size = 3.5) +
    geom_text(data = accuracy_tests,
              aes(x = 0.6, y = 0.60, label = test),
              hjust = 0,
              size = 2.8,
              color = "#2C3E50",
              inherit.aes = FALSE) +
    scale_y_continuous(labels = percent,
                       limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.2)) +
    coord_flip() +
    facet_wrap(~ Sex, nrow = 2) +
    labs(x = "",
         y = "Percent Correctly Identified (Source present)",
         caption = "Error bars show standard error.") +
    theme_classic() +
    theme(
        text = element_text(size = 12),
        axis.text = element_text(color = "black", size = 12)
    )
ggsave("revised-free-accuracy.png",
       plot = last_plot(),
       width = 8,
       height = 8,
       units = "in",
       dpi = 300)


###################################### CONSISTENCY
#Tests if individuals are consistent in their labels (for free, using coded category)

# Consistent = at least one code category appears in ALL exposures for that UID x Odor
consistency_raw <- data_long |>
    mutate(Code = str_trim(Code)) |>
    group_by(UID, Study, Age, Sex, Odor, Location) |>
    summarise(codes_at_location = list(unique(Code)), .groups = "drop")

consistency <- consistency_raw |>
    group_by(UID, Study, Age, Sex, Odor) |>
    summarise(
        n_exposures   = n(),
        is_consistent = as.integer(
            length(Reduce(intersect, codes_at_location)) > 0
        ),
        .groups = "drop"
    )

# Preliminary GLM: known effects (Sex, Age) + unknown effect (Study)
check_consistency <- glm(is_consistent ~ Sex + Age + Study,
                         data = consistency,
                         family = binomial)
check_consistency |>
    tidy(exponentiate = TRUE) |>
    select(term, OR = estimate, p.value) |>
    print()
#### ONLY STUDY IS DIFFERENT, AGE AND SEX ARE NOT DIFFERENT

# Split by Study x Sex
consistency_field <- consistency |> filter(Study == "Field")
consistency_lab <- consistency |> filter(Study == "Lab")

# ICC test for each group: are odors independent or correlated within persons?
# ICC test: are odors independent or correlated within persons?
icc_test <- function(df) {
    df |>
        select(UID, Odor, is_consistent) |>
        pivot_wider(names_from = Odor, values_from = is_consistent) |>
        select(-UID) |>
        icc(model = "twoway", type = "consistency")
}

print(icc_test(consistency_field)) # icc 0.0382
print(icc_test(consistency_lab)) # icc  0.157

#count sample sizes
cat("\nConsistency counts - Field (Odor x is_consistent):\n")
consistency_field |>
    count(Odor, is_consistent) |>
    pivot_wider(names_from = is_consistent,
                values_from = n,
                values_fill = 0,
                names_prefix = "consistent_") |>
    rename(inconsistent = consistent_0, consistent = consistent_1) |>
    mutate(total = inconsistent + consistent,
           pct_consistent = round(consistent / total * 100, 1)) |>
    arrange(pct_consistent) |>
    print()


### Individual level question: Are some people more consistent than others?
## testing to see if odors are independent or not; if not, no some people are not more consistent than others

consistency_lab |>
    count(Odor, is_consistent) |>
    pivot_wider(names_from = is_consistent,
                values_from = n,
                values_fill = 0,
                names_prefix = "consistent_") |>
    rename(inconsistent = consistent_0, consistent = consistent_1) |>
    mutate(total = inconsistent + consistent,
           pct_consistent = round(consistent / total * 100, 1)) |>
    arrange(pct_consistent) |>
    print()



# Odor-level questions; are people more consistent with some odors?
# Chi-square for both (min cell count > 5 for both studies)
cat("\nConsistency tests by Study:\n")
chisq.test(table(consistency_field$Odor, consistency_field$is_consistent))
chisq.test(table(consistency_lab$Odor,   consistency_lab$is_consistent))

# Store test results for plot annotation
consistency_tests <- tibble(
    Study = c("Field", "Lab"),
    test  = c(
        paste0("\u03C7\u00B2(4) = ",
               round(chisq.test(table(consistency_field$Odor, consistency_field$is_consistent))$statistic, 2),
               ", p = ",
               round(chisq.test(table(consistency_field$Odor, consistency_field$is_consistent))$p.value, 3)),
        paste0("\u03C7\u00B2(4) = ",
               round(chisq.test(table(consistency_lab$Odor, consistency_lab$is_consistent))$statistic, 2),
               ", p = ",
               round(chisq.test(table(consistency_lab$Odor, consistency_lab$is_consistent))$p.value, 3))
    )
)
print(consistency_tests)


# Odor order from overall mean consistency
odor_order_con <- consistency |>
    group_by(Odor) |>
    summarise(overall_mean = mean(is_consistent, na.rm = TRUE)) |>
    arrange(overall_mean) |>
    pull(Odor)

# Plot
consistency |>
    group_by(Study, Odor) |>
    summarise(
        pct_consistent = mean(is_consistent, na.rm = TRUE),
        n = n(),
        se = sqrt(pct_consistent * (1 - pct_consistent) / n),
        .groups = "drop"
    ) |>
    mutate(Odor = factor(Odor, levels = odor_order_con)) |>
    ggplot(aes(x = Odor, y = pct_consistent)) +
    geom_col(fill = "#1B4F72", width = 0.6) +
    geom_errorbar(aes(ymin = pct_consistent - se,
                      ymax = pct_consistent + se),
                  width = 0.2) +
    geom_text(aes(label = percent(pct_consistent, accuracy = 1)),
              hjust = 1.2,
              color = "white",
              size = 3.5) +
    geom_text(data = consistency_tests,
              aes(x = 0.6, y = 0.60, label = test),
              hjust = 0,
              size = 2.8,
              color = "#2C3E50",
              inherit.aes = FALSE) +
    scale_y_continuous(labels = percent,
                       limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.2)) +
    coord_flip() +
    facet_wrap(~ Study, nrow = 2) +
    labs(x = "",
         y = "Percent Consistent (shared category across exposures)",
         caption = "Error bars show standard error.") +
    theme_classic() +
    theme(
        text = element_text(size = 12),
        axis.text = element_text(color = "black", size = 12,),
        plot.caption = element_text(size = 10, color = "#2C3E50")
    )
ggsave("revised-free-consistency.png",
       plot = last_plot(),
       width = 8,
       height = 8,
       units = "in",
       dpi = 300)


###################################### OLFACTORY ABILITY
# Tests environmental effect on identification ability

# OA = proportion of odors correctly identified (Source present) per UID x Location
oa <- accuracy |>
    group_by(UID, Study, Location, Sex, Age) |>
    summarise(
        oaCount   = sum(Correctn, na.rm = TRUE),
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
#SEX SIGNIFICANT only but reference levels vary by study so split by study and sex

#look at distribution for assumptions prior to glm testing
skim(oa)
ggplot(oa, aes(x = factor(oaPercent))) +
    geom_bar(position = "dodge") +
    labs(x = "", y = "") +
    scale_fill_viridis_d(begin = 0.15, end = 0.65) +
    theme_classic()

# OA by location
# Preliminary GLM: Sex only significant for overall OA level
# BUT location models require Study split - field and lab locations are separate
# Four models: Study x Sex with study-appropriate reference levels

oa_field_f <- oa |> filter(Study == "Field", Sex == "Female")
oa_field_m <- oa |> filter(Study == "Field", Sex == "Male")
oa_lab_f   <- oa |> filter(Study == "Lab",   Sex == "Female")
oa_lab_m   <- oa |> filter(Study == "Lab",   Sex == "Male")

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

# Field only - tracks change from Tate baseline
# Categories: Learned (wrong->right), Distracted (right->wrong),
#             Confirmed correct (right->right), Still wrong (wrong->wrong)

# Tate baseline correct/incorrect per UID x Odor
tate_response <- accuracy |>
    filter(Study == "Field", Location == "Tate") |>
    select(UID, Odor, tate_correct = Correctn)

# Join non-Tate field rows against Tate baseline
direction_analysis <- accuracy |>
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
ggsave("revised-free-direction.png",
       plot = last_plot(),
       width = 5, height = 4, units = "in", dpi = 300)



################################ CORRELATION
# Collapse OA to one mean score per UID across locations
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


######################## TIDY
rm(list=ls())
gc()

