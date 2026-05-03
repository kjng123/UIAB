# ============================================================
#  UIAB Appeal Outcomes Analysis
# ============================================================

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)
library(lubridate)
library(broom)
library(rlang)


# ============================================================
# 0.  SHARED THEME & COLOUR PALETTES
# ============================================================

# Benefits outcome palette (allowed / denied / none)
OUTCOME_COLOURS <- c(
  "allowed" = "#2166AC",   # blue  = claimant wins
  "denied"  = "#D6604D",   # red   = claimant loses
  "none"    = "#B0B0B0",   # grey  = no determination
  "other"   = "#92C5DE"    # light blue
)

# ALJ outcome palette (what the board did to the ALJ decision)
ALJ_COLOURS <- c(
  "upheld"    = "#4DAC26",  # green  = ALJ upheld
  "overruled" = "#D01C8B",  # pink   = ALJ overruled
  "modified"  = "#F4A582",  # peach  = ALJ modified
  "remanded"  = "#762A83",  # purple = remanded
  "none"      = "#B0B0B0"
)

# Initial determination palette (what the board did to the initial determination)
# Same semantics as ALJ palette — reuse it
INITIAL_COLOURS <- ALJ_COLOURS

# Consistent minimal theme for legal/policy reports
theme_uiab <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 2),
      plot.subtitle    = element_text(colour = "grey40", size = base_size - 1,
                                      margin = margin(b = 8)),
      plot.caption     = element_text(colour = "grey55", size = base_size - 2,
                                      hjust = 0),
      axis.title       = element_text(size = base_size - 1),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold")
    )
}


# ============================================================
# 1.  LOAD DATA
# ============================================================

path <- "C:/Users/ngkan/OneDrive/Desktop/NLP Project UIAB/uiab_outcomes_v2.csv"

df <- read_csv(
  path,
  na           = c("", "NA", "NaN", "nan", "None"),
  col_types    = cols(appeal_board_no = col_character()),
  show_col_types = FALSE
) %>%
  mutate(year = str_extract(mailed_and_filed_date, "\\b\\d{4}\\b") |> as.integer())

glimpse(df)


# ============================================================
# 2.  CLEAN JUDGE NAMES
# ============================================================

canon_map <- c(
  "MARILYN P OMARA"        = "MARILYN P O'MARA",
  "MARILYN OMARA"          = "MARILYN P O'MARA",
  "MICHAEL T GREASON"      = "MICHAEL T GREASON",
  "MICHAEL GREASON"        = "MICHAEL T GREASON",
  "T GREASON"              = "MICHAEL T GREASON",
  "RANDALL T DOUGLAS"      = "RANDALL T DOUGLAS",
  "GERALDINE A REILLY"     = "GERALDINE A REILLY",
  "GERALDINE A"            = "GERALDINE A REILLY",
  "GERALDINE REILLY"       = "GERALDINE A REILLY",
  "JUNE F ONEILL"          = "JUNE F O'NEILL",
  "LEONARD D POLLETTA"     = "LEONARD D POLLETTA",
  "D POLLETTA"             = "LEONARD D POLLETTA",
  "NARD D POLLETTA"        = "LEONARD D POLLETTA",
  "GEORGE FRIEDMAN"        = "GEORGE FRIEDMAN",
  "KIMBERLY JEANPIERRE"    = "KIMBERLY JEANPIERRE",
  "TANYA R DANIEL"         = "TANYA R DANIEL",
  "TANYA DANIEL"           = "TANYA R DANIEL",
  "TANYA R"                = "TANYA R DANIEL",
  "EILEEN M LONG CHELALES" = "EILEEN M LONG CHELALES",
  "EILEEN LONGCHELALES"    = "EILEEN M LONG CHELALES",
  "EILEEN LONG CHELALES"   = "EILEEN M LONG CHELALES",
  "EILEEN M LONGCHELALES"  = "EILEEN M LONG CHELALES",
  "MARYANN K MCCARTHY"     = "MARYANN K MCCARTHY",
  "MARY K MCCARTHY"        = "MARYANN K MCCARTHY",
  "JAMES S ALESI"          = "JAMES S ALESI"
)

df_long <- df %>%
  mutate(
    board_member = coalesce(board_member, ""),
    judge_list   = board_member %>%
      str_replace_all("\\bMEMBERS?\\b", "") %>%
      str_squish() %>%
      str_split(",")
  ) %>%
  unnest(judge_list) %>%
  mutate(
    judge_key = judge_list %>%
      str_squish() %>%
      str_to_upper() %>%
      str_replace_all("[[:punct:]]", "") %>%
      str_squish(),
    judge = if_else(
      judge_key %in% names(canon_map),
      unname(canon_map[judge_key]),
      judge_key
    )
  ) %>%
  filter(judge_key != "") %>%
  distinct(appeal_board_no, judge, .keep_all = TRUE)


# ============================================================
# 3.  STANDARDISE MISSING OUTCOMES
#
#  Column semantics (from actual data):
#    benefits_outcome : "allowed" | "denied"            → use OUTCOME_COLOURS
#    alj_outcome      : "upheld" | "overruled" |
#                       "modified" | "remanded"          → use ALJ_COLOURS
#    initial_outcome  : "upheld" | "overruled" |
#                       "modified" | "remanded"          → use INITIAL_COLOURS
# ============================================================

df_long <- df_long %>%
  mutate(
    benefits_outcome = if_else(is.na(benefits_outcome), "none",     benefits_outcome),
    alj_outcome      = if_else(is.na(alj_outcome),      "none",     alj_outcome),
    initial_outcome  = if_else(is.na(initial_outcome),  "none",     initial_outcome),
    who_appealed     = if_else(is.na(who_appealed),      "unknown",  who_appealed),
    issue_type       = if_else(is.na(issue_type),        "other",    issue_type),
    decision_mode    = if_else(is.na(decision_mode),     "standard", decision_mode)
  )



# ============================================================
# 4.  HELPER FUNCTIONS
# ============================================================

# -- 4a. Case-level proportion table (one row per case, deduplicated)
proportion_table <- function(data, ...) {
  group_vars <- enquos(...)
  data %>%
    distinct(appeal_board_no, !!!group_vars) %>%
    count(!!!group_vars) %>%
    mutate(pct = round(n / sum(n) * 100, 1))
}

# -- 4b. Stacked proportion bar chart (horizontal)
#        colour_map must match the actual values in outcome_col.
plot_stacked <- function(data,
                         outcome_col,
                         group_col   = judge,
                         top_n       = 10,
                         title       = NULL,
                         subtitle    = NULL,
                         colour_map  = NULL) {
  
  outcome_col <- ensym(outcome_col)
  group_col   <- ensym(group_col)
  
  # Rank groups by total cases; keep top N
  top_groups <- data %>%
    distinct(appeal_board_no, !!group_col) %>%
    count(!!group_col, name = "total") %>%
    slice_max(total, n = top_n) %>%
    arrange(total) %>%
    pull(!!group_col)
  
  plot_df <- data %>%
    filter(!!group_col %in% top_groups) %>%
    distinct(appeal_board_no, !!group_col, !!outcome_col) %>%
    count(!!group_col, !!outcome_col) %>%
    group_by(!!group_col) %>%
    mutate(prop  = n / sum(n),
           total = sum(n)) %>%
    ungroup() %>%
    mutate(!!group_col := factor(!!group_col, levels = top_groups))
  
  p <- ggplot(plot_df,
              aes(x = !!group_col, y = prop, fill = !!outcome_col)) +
    geom_col(width = 0.7) +
    geom_text(
      aes(label = if_else(prop >= 0.05,
                          percent(prop, accuracy = 1), "")),
      position = position_stack(vjust = 0.5),
      size = 3, colour = "white", fontface = "bold"
    ) +
    coord_flip() +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       expand  = expansion(mult = c(0, 0.02))) +
    labs(title    = title,
         subtitle = subtitle,
         x        = NULL,
         y        = "Share of cases",
         fill     = NULL) +
    theme_uiab()
  
  if (!is.null(colour_map)) {
    p <- p + scale_fill_manual(values = colour_map, na.value = "grey80")
  }
  
  # Annotate with case count on right margin
  totals <- plot_df %>%
    distinct(!!group_col, total) %>%
    mutate(!!group_col := factor(!!group_col, levels = top_groups))
  
  p + geom_text(
    data = totals,
    aes(x = !!group_col, y = 1.02,
        label = paste0("n=", scales::comma(total)),
        fill  = NULL),
    hjust = 0, size = 2.8, colour = "grey40"
  )
}

# -- 4c. Point-range plot of judge allow rates (with 95% CI)
plot_judge_ci <- function(data, min_cases = 20) {
  data %>%
    mutate(ben_allowed = as.integer(benefits_outcome == "allowed")) %>%
    group_by(judge) %>%
    summarise(
      n    = n(),
      mean = mean(ben_allowed),
      se   = sqrt(mean * (1 - mean) / n),
      lo95 = pmax(mean - 1.96 * se, 0),
      hi95 = pmin(mean + 1.96 * se, 1),
      .groups = "drop"
    ) %>%
    filter(n >= min_cases) %>%
    mutate(judge = fct_reorder(judge, mean)) %>%
    ggplot(aes(x = mean, y = judge)) +
    geom_vline(
      xintercept = mean(as.integer(data$benefits_outcome == "allowed")),
      linetype = "dashed", colour = "grey50", linewidth = 0.6
    ) +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95),
                   height = 0.3, colour = "grey60") +
    geom_point(aes(size = n), colour = OUTCOME_COLOURS["allowed"]) +
    scale_x_continuous(labels = percent_format(accuracy = 1),
                       limits = c(0, 1)) +
    scale_size_continuous(range = c(2, 7), guide = "none") +
    labs(
      title    = "Claimant win rate by judge (with 95% confidence interval)",
      subtitle = "Dashed line = overall average. Point size = number of cases.",
      x        = "Share of cases where benefits were allowed",
      y        = NULL
    ) +
    theme_uiab()
}


# ============================================================
# 5.  OVERALL SUMMARY TABLES
# ============================================================

cat("\n=== Benefits outcome (overall) ===\n")

proportion_table(df_long, benefits_outcome) |> print()

cat("\n=== ALJ outcome (overall) ===\n")
proportion_table(df_long, alj_outcome) |> print()

cat("\n=== Initial determination (overall) ===\n")
proportion_table(df_long, initial_outcome) |> print()

cat("\n=== Who appealed ===\n")
proportion_table(df_long, who_appealed) |> print()

cat("\n=== Issue type ===\n")
proportion_table(df_long, issue_type) |> print()

cat("\n=== Decision mode ===\n")
proportion_table(df_long, decision_mode) |> print()

# ALJ upheld breakdown
cat("\n=== ALJ 'upheld' — claimant wins vs loses ===\n")
df %>%
  mutate(
    alj_detail = case_when(
      alj_outcome == "upheld" & benefits_outcome == "allowed" ~ "Upheld — claimant wins",
      alj_outcome == "upheld" & benefits_outcome == "denied"  ~ "Upheld — claimant loses",
      alj_outcome == "upheld"                                  ~ "Upheld — outcome unknown",
      TRUE ~ coalesce(alj_outcome, "missing")
    )
  ) %>%
  count(alj_detail) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n)) %>%
  print()


# ============================================================
# 6.  JUDGE-LEVEL WIDE TABLES
# ============================================================

make_judge_wide <- function(data, outcome_col) {
  outcome_col <- ensym(outcome_col)
  data %>%
    distinct(appeal_board_no, judge, !!outcome_col) %>%
    count(judge, !!outcome_col) %>%
    group_by(judge) %>%
    mutate(total = sum(n), pct = round(n / total * 100, 1)) %>%
    ungroup() %>%
    select(judge, !!outcome_col, pct, total) %>%
    pivot_wider(names_from = !!outcome_col, values_from = pct,
                values_fill = 0) %>%
    arrange(desc(total))
}

cat("\n=== Judge × benefits outcome (%) ===\n")
make_judge_wide(df_long, benefits_outcome) |> print(n = 50)

cat("\n=== Judge × ALJ outcome (%) ===\n")
make_judge_wide(df_long, alj_outcome) |> print(n = 50)

cat("\n=== Judge × initial determination (%) ===\n")
make_judge_wide(df_long, initial_outcome) |> print(n = 50)


# ============================================================
# 7.  VISUALISATIONS — JUDGE-LEVEL
# ============================================================

TOP_N <- 13

# 7a. Stacked bar: benefits outcome by judge
#     Colour map: OUTCOME_COLOURS  (allowed / denied / none)
print(
  plot_stacked(
    df_long,
    outcome_col = benefits_outcome,
    top_n       = TOP_N,
    title       = "Benefits outcomes by judge",
    subtitle    = paste0("Top ", TOP_N, " judges by caseload"),
    colour_map  = OUTCOME_COLOURS
  )
)

# 7b. Stacked bar: ALJ outcome by judge
#     Colour map: ALJ_COLOURS  (upheld / overruled / modified / remanded / none)
print(
  plot_stacked(
    df_long,
    outcome_col = alj_outcome,
    top_n       = TOP_N,
    title       = "ALJ outcomes by judge",
    subtitle    = paste0("Top ", TOP_N, " judges by caseload"),
    colour_map  = ALJ_COLOURS
  )
)

# 7c. Stacked bar: initial determination by judge
#     Colour map: INITIAL_COLOURS  (upheld / overruled / modified / remanded / none)
#     NOTE: initial_outcome tracks what the board did to the *initial determination*,
#     not a benefits decision — so it uses ALJ-style labels, not allowed/denied.
print(
  plot_stacked(
    df_long,
    outcome_col = initial_outcome,
    top_n       = TOP_N,
    title       = "Initial determination by judge",
    subtitle    = paste0("Top ", TOP_N, " judges by caseload"),
    colour_map  = INITIAL_COLOURS
  )
)

# 7d. Point-range: claimant win rate with 95% CI
print(plot_judge_ci(df_long, min_cases = 20))


# ============================================================
# 8.  VISUALISATIONS — WHO APPEALED × OUTCOME
# ============================================================

cat("\n=== Benefits outcome × who appealed ===\n")
df_long %>%
  distinct(appeal_board_no, who_appealed, benefits_outcome) %>%
  count(who_appealed, benefits_outcome) %>%
  group_by(who_appealed) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  arrange(who_appealed, desc(n)) %>%
  print()

p_who <- df_long %>%
  filter(who_appealed != "unknown") %>%
  distinct(appeal_board_no, who_appealed, benefits_outcome) %>%
  count(who_appealed, benefits_outcome) %>%
  group_by(who_appealed) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    who_appealed = recode(who_appealed,
                          "claimant"     = "Claimant appealed",
                          "employer"     = "Employer appealed",
                          "commissioner" = "Commissioner appealed"
    )
  ) %>%
  ggplot(aes(x = who_appealed, y = prop, fill = benefits_outcome)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = if_else(prop >= 0.05, percent(prop, accuracy = 1), "")),
    position = position_stack(vjust = 0.5),
    size = 3.5, colour = "white", fontface = "bold"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand  = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = OUTCOME_COLOURS, na.value = "grey80") +
  labs(
    title    = "Benefits outcome by appellant",
    subtitle = "Who initiated the Board appeal affects claimant win rates",
    x        = NULL, y = "Share of cases", fill = NULL
  ) +
  theme_uiab()
print(p_who)


# ============================================================
# 9.  VISUALISATIONS — ISSUE TYPE × OUTCOME
# ============================================================

cat("\n=== Benefits outcome × issue type ===\n")
df_long %>%
  distinct(appeal_board_no, issue_type, benefits_outcome) %>%
  count(issue_type, benefits_outcome) %>%
  group_by(issue_type) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  arrange(issue_type, desc(n)) %>%
  print(n = 60)

print(
  plot_stacked(
    df_long,
    outcome_col = benefits_outcome,
    group_col   = issue_type,
    top_n       = 12,
    title       = "Benefits outcome by issue type",
    subtitle    = "Top 12 issue types by caseload",
    colour_map  = OUTCOME_COLOURS
  )
)

#break up by who makes the appeal

# ============================================================
# 10.  JUDGE × ISSUE TYPE (top judges, % allowed)
# ============================================================

TOP_N_JUDGES <- 10

top_judges <- df_long %>%
  distinct(appeal_board_no, judge) %>%
  count(judge) %>%
  slice_max(n, n = TOP_N_JUDGES) %>%
  pull(judge)

cat("\n=== % allowed: judge × issue type (top judges) ===\n")
df_long %>%
  filter(judge %in% top_judges) %>%
  distinct(appeal_board_no, judge, issue_type, benefits_outcome) %>%
  count(judge, issue_type, benefits_outcome) %>%
  group_by(judge, issue_type) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  filter(benefits_outcome == "allowed") %>%
  select(judge, issue_type, n_cases = n, pct_allowed = pct) %>%
  arrange(judge, issue_type) %>%
  print(n = 100)

# Heatmap: judge × issue type
heatmap_df <- df_long %>%
  filter(judge %in% top_judges) %>%
  distinct(appeal_board_no, judge, issue_type, benefits_outcome) %>%
  group_by(judge, issue_type) %>%
  summarise(
    n           = n(),
    pct_allowed = mean(benefits_outcome == "allowed") * 100,
    .groups = "drop"
  ) %>%
  filter(n >= 5)

p_heatmap <- ggplot(heatmap_df,
                    aes(x = issue_type, y = judge, fill = pct_allowed)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(pct_allowed), "%")),
            size = 2.8, colour = "white", fontface = "bold") +
  scale_fill_gradient2(
    low      = OUTCOME_COLOURS["denied"],
    mid      = "lightyellow",
    high     = OUTCOME_COLOURS["allowed"],
    midpoint = 50,
    labels   = label_percent(scale = 1),
    name     = "% allowed"
  ) +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  labs(
    title    = "Claimant win rate by judge and issue type",
    subtitle = "Cells with fewer than 5 cases suppressed",
    x        = "Issue type", y = NULL
  ) +
  theme_uiab() +
  theme(legend.position = "right")
print(p_heatmap)

#add relative breakdown of issues per judge
# ============================================================
# 11.  TREND OVER TIME
# ============================================================

benefits_by_year <- df %>%
  mutate(benefits_outcome = if_else(
    is.na(benefits_outcome) | benefits_outcome == "", "none", benefits_outcome
  )) %>%
  filter(!is.na(year)) %>%
  distinct(appeal_board_no, year, benefits_outcome) %>%
  count(year, benefits_outcome) %>%
  complete(year, benefits_outcome, fill = list(n = 0)) %>%
  group_by(year) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

print(benefits_by_year)

p_trend <- ggplot(benefits_by_year %>% filter(benefits_outcome != "none"),
                  aes(x = year, y = prop, colour = benefits_outcome)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = OUTCOME_COLOURS) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title    = "Benefits outcomes over time",
    subtitle = "'None' category excluded for clarity",
    x        = "Year", y = "Share of cases", colour = NULL
  ) +
  theme_uiab()
print(p_trend)
#volume over time and by issue types

# ============================================================
# 12.  REMAND RATES
# ============================================================

cat("\n=== Procedural remand rate by year ===\n")
df %>%
  filter(!is.na(year)) %>%
  distinct(appeal_board_no, year, procedural_remand) %>%
  group_by(year) %>%
  summarise(
    n          = n(),
    pct_remand = round(mean(procedural_remand, na.rm = TRUE) * 100, 1),
    .groups    = "drop"
  ) %>%
  print()

cat("\n=== Procedural remand rate by judge (top judges) ===\n")
df_long %>%
  filter(judge %in% top_judges) %>%
  distinct(appeal_board_no, judge, procedural_remand) %>%
  group_by(judge) %>%
  summarise(
    n          = n(),
    pct_remand = round(mean(procedural_remand, na.rm = TRUE) * 100, 1),
    .groups    = "drop"
  ) %>%
  arrange(desc(n)) %>%
  print()


# ============================================================
# 13.  JUDGE-LEVEL SUMMARY STATISTICS
# ============================================================

case_level <- df_long %>%
  mutate(
    ben_allowed         = as.integer(benefits_outcome == "allowed"),
    alj_upheld_denial   = as.integer(alj_outcome == "upheld" & benefits_outcome == "denied"),
    alj_upheld_approval = as.integer(alj_outcome == "upheld" & benefits_outcome == "allowed"),
    alj_overruled       = as.integer(alj_outcome == "overruled")
  )

judge_summary <- case_level %>%
  group_by(judge) %>%
  summarise(
    n_cases              = n(),
    pct_allowed          = mean(ben_allowed,         na.rm = TRUE),
    se_allowed           = sd(ben_allowed,            na.rm = TRUE) / sqrt(n()),
    pct_alj_upheld_deny  = mean(alj_upheld_denial,   na.rm = TRUE),
    se_alj_upheld_deny   = sd(alj_upheld_denial,     na.rm = TRUE) / sqrt(n()),
    pct_alj_upheld_appr  = mean(alj_upheld_approval, na.rm = TRUE),
    pct_alj_overruled    = mean(alj_overruled,        na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_cases >= 20) %>%
  arrange(desc(n_cases))

cat("\n=== Judge-level summary (means + SEs) ===\n")
print(judge_summary, n = 50)

cat("\n=== Cross-judge variation ===\n")
judge_summary %>%
  summarise(across(starts_with("pct_"),
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        sd   = ~ sd(.x,   na.rm = TRUE)),
                   .names = "{.col}_{.fn}")) %>%
  print()

cat("\n=== Judge activity by year ===\n")
case_level %>%
  filter(judge %in% judge_summary$judge) %>%
  group_by(judge) %>%
  summarise(
    n         = n(),
    year_min  = min(year, na.rm = TRUE),
    year_max  = max(year, na.rm = TRUE),
    year_mean = round(mean(year, na.rm = TRUE), 1),
    .groups   = "drop"
  ) %>%
  arrange(desc(n)) %>%
  print()


# ============================================================
# 14.  ANOVA + PAIRWISE TESTS
# ============================================================

cat("\n=== ANOVA: benefits allowed ~ judge ===\n")
case_anova <- case_level %>%
  filter(judge %in% judge_summary$judge) %>%
  mutate(judge = factor(judge))

aov_result <- aov(ben_allowed ~ judge, data = case_anova)
print(summary(aov_result))

cat("\n=== Pairwise t-tests (Bonferroni correction) ===\n")
pairwise.t.test(case_anova$ben_allowed, case_anova$judge,
                p.adjust.method = "bonferroni") |> print()
#adjusted anova

# ============================================================
# 15.  LOGISTIC REGRESSION
# ============================================================

case_logit <- case_anova %>%
  filter(!is.na(ben_allowed), !is.na(year)) %>%
  mutate(
    judge = relevel(factor(judge), ref = "MICHAEL T GREASON"),
    who_appealed = factor(
      if_else(is.na(who_appealed) | who_appealed == "unknown",
              "claimant", who_appealed),
      levels = c("claimant", "employer", "commissioner")
    ),
    issue_type = factor(issue_type)
  )

m1 <- glm(ben_allowed ~ judge,
          data = case_logit, family = binomial)

m2 <- glm(ben_allowed ~ judge + year,
          data = case_logit, family = binomial)

m3 <- glm(ben_allowed ~ judge + year + who_appealed + issue_type,
          data = case_logit, family = binomial)

cat("\n=== Model 1: judge only ===\n");               print(summary(m1))
cat("\n=== Model 2: judge + year ===\n");             print(summary(m2))
cat("\n=== Model 3: full model ===\n");               print(summary(m3))
cat("\n=== AIC comparison ===\n");                    print(AIC(m1, m2, m3))

# Forest plot of Model 3 judge coefficients (odds ratios)
m3_tidy <- tidy(m3, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(str_detect(term, "^judge")) %>%
  mutate(
    judge = str_remove(term, "^judge"),
    judge = fct_reorder(judge, estimate)
  )

p_forest <- ggplot(m3_tidy, aes(x = estimate, y = judge)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.25, colour = "grey60") +
  geom_point(size = 3, colour = OUTCOME_COLOURS["allowed"]) +
  scale_x_log10(labels = label_number(accuracy = 0.01)) +
  labs(
    title    = "Odds ratios for claimant win — judge effects (Model 3)",
    subtitle = "Reference judge: MICHAEL T GREASON. Adjusted for year, appellant, issue type.",
    x        = "Odds ratio (log scale)",
    y        = NULL
  ) +
  theme_uiab()
print(p_forest)


# ============================================================
# 16.  JUDGE × YEAR TRENDS (faceted)
# ============================================================

MIN_CASES_PER_YEAR <- 5

plot_df <- case_level %>%
  filter(judge %in% top_judges, !is.na(year)) %>%
  group_by(judge, year) %>%
  summarise(n_cases = n(), pct_allowed = mean(ben_allowed, na.rm = TRUE),
            .groups = "drop") %>%
  filter(n_cases >= MIN_CASES_PER_YEAR)

judge_corr <- plot_df %>%
  group_by(judge) %>%
  summarise(
    r         = cor(year, pct_allowed, use = "complete.obs"),
    cor_label = paste0("r = ", round(r, 2)),
    .groups   = "drop"
  )

p_facet <- ggplot(plot_df, aes(x = year, y = pct_allowed)) +
  geom_point(aes(size = n_cases), alpha = 0.7, colour = OUTCOME_COLOURS["allowed"]) +
  geom_smooth(method = "lm", se = FALSE, colour = "darkred", linewidth = 0.9) +
  geom_text(
    data        = judge_corr,
    aes(x = -Inf, y = Inf, label = cor_label),
    hjust       = -0.15, vjust = 1.3,
    inherit.aes = FALSE, size = 3, colour = "grey30"
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  facet_wrap(~ judge) +
  labs(
    title    = "Annual claimant win rate by judge",
    subtitle = paste0("Linear trend shown. Years with fewer than ",
                      MIN_CASES_PER_YEAR, " cases excluded."),
    x = "Year", y = "% allowed"
  ) +
  theme_uiab()
print(p_facet)


# ============================================================
# 17.  CORRELATIONS (judge-level means)
# ============================================================

cat("\n=== Correlations between judge-level outcome rates ===\n")
judge_summary %>%
  select(pct_allowed, pct_alj_upheld_deny,
         pct_alj_upheld_appr, pct_alj_overruled) %>%
  cor(use = "complete.obs") %>%
  round(3) %>%
  print()


# ============================================================
# 18.  RANDOM SAMPLE FOR MANUAL VALIDATION
# ============================================================

set.seed(123)
cat("\n=== Random sample (n=30) for manual validation ===\n")
df %>%
  slice_sample(n = 30) %>%
  select(appeal_board_no, mailed_and_filed_date, board_member,
         who_appealed, issue_type, decision_mode,
         benefits_outcome, alj_outcome, initial_outcome) %>%
  print(n = 30)

