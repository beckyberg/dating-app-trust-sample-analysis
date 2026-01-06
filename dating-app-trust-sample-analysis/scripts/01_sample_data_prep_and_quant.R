# ============================================================
# Sample Workflow: Data Prep + One Quant Table (R)
# Purpose: Demonstrate reproducible cleaning + analysis approach.
# Note: Dataset is NOT included in this public repo to protect participant privacy.
# ============================================================

library(tidyverse)
library(janitor)
library(stringr)
library(tidyr)
library(here)

# ---- Project-friendly paths ----
RAW_PATH <- here("data", "TrustData.csv")       # not included publicly
CLEAN_PATH <- file.path("data", "TrustData_clean.csv")   # not included publicly
OUT_DIR    <- "outputs"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

if (!file.exists(RAW_PATH)) {
  stop(
    "Data file not found at: ", RAW_PATH, "\n",
    "Note: The dataset is not included in this public repo to protect participant privacy.\n",
    "To run locally, place your de-identified CSV in /data with the expected filename."
  )
}

# -----------------------------
# 1) Load + clean names
# -----------------------------
dat_raw <- read_csv(RAW_PATH, show_col_types = FALSE)
message("Raw rows (including any empties): ", nrow(dat_raw))

dat <- dat_raw %>%
  clean_names() %>%
  filter(!is.na(timestamp) & timestamp != "")

message("Rows with timestamp (final N): ", nrow(dat))

required_cols <- c("timestamp", "irl_dates_cat", "safety_awareness", "meet_decision_factor")
missing <- setdiff(required_cols, names(dat))
if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))

# -----------------------------
# 2) Recode IRL dates (ordered factor)
# -----------------------------
dat <- dat %>%
  mutate(
    irl_dates_cat_raw = irl_dates_cat,
    irl_dates_cat = recode(
      irl_dates_cat,
      "None"         = "0 dates",
      "2-Jan"        = "1–2 dates",
      "5-Mar"        = "3–5 dates",
      "10-Jun"       = "6–10 dates",
      "More than 10" = "11+ dates",
      .default = irl_dates_cat
    ),
    irl_dates_cat = factor(
      irl_dates_cat,
      levels = c("0 dates", "1–2 dates", "3–5 dates", "6–10 dates", "11+ dates"),
      ordered = TRUE
    )
  )

# Optional: save cleaned dataset locally (not included in public repo)
write_csv(dat, CLEAN_PATH)

# -----------------------------
# 3) One Core Quantitative Output
# -----------------------------
meet_decision_factor_table <- dat %>%
  filter(!is.na(meet_decision_factor) & meet_decision_factor != "") %>%
  count(meet_decision_factor, name = "n") %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))

print(meet_decision_factor_table)

write_csv(meet_decision_factor_table, file.path(OUT_DIR, "table_meet_decision_factor.csv"))

run_summary <- tibble(
  metric = c("raw_rows", "final_rows_with_timestamp"),
  value  = c(nrow(dat_raw), nrow(dat))
)
write_csv(run_summary, file.path(OUT_DIR, "run_summary_quant.csv"))
