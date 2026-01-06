# ============================================================
# Sample Qual Pipeline (R): Fake vs Trusted Profile Language
# Purpose: Demonstrate reproducible text workflow (no raw text shared).
# Note: Open-ended responses and datasets are not included publicly.
# ============================================================

library(tidyverse)
library(tidytext)
library(janitor)
library(stringr)
library(tidyr)
library(topicmodels)

CLEAN_PATH <- file.path("data", "TrustData_clean.csv")   # not included publicly
OUT_DIR    <- "outputs"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

# ---- Guardrail: fail gracefully if data isn't present ----
if (!file.exists(CLEAN_PATH)) {
  stop(
    "Cleaned data not found at: ", CLEAN_PATH, "\n",
    "Note: The dataset is not included in this public repo to protect participant privacy.\n",
    "To run locally, place your cleaned CSV in /data with the expected filename."
  )
}

dat <- read_csv(CLEAN_PATH, show_col_types = FALSE) %>% clean_names()

# ---- Validate required columns ----
required_cols <- c("fake_profile_open", "trusted_profile_open")
missing <- setdiff(required_cols, names(dat))
if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))

# ---- Select text fields + remove missing ----
fake  <- dat %>%
  filter(!is.na(fake_profile_open) & fake_profile_open != "") %>%
  transmute(text = fake_profile_open)

trust <- dat %>%
  filter(!is.na(trusted_profile_open) & trusted_profile_open != "") %>%
  transmute(text = trusted_profile_open)

# ---- Domain stopwords ----
domain_stop <- c("app", "apps", "profile", "profiles")

# ============================================================
# 1) Tokenize + Top Words (safe summary)
# ============================================================
prep_tokens <- function(df) {
  df %>%
    mutate(id = row_number()) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words, by = "word") %>%
    filter(!word %in% domain_stop) %>%
    filter(str_detect(word, "^[a-z']+$"))  # keep simple tokens
}

fake_words  <- prep_tokens(fake)
trust_words <- prep_tokens(trust)

fake_top_words  <- fake_words  %>% count(word, sort = TRUE) %>% slice_head(n = 25)
trust_top_words <- trust_words %>% count(word, sort = TRUE) %>% slice_head(n = 25)

write_csv(fake_top_words,  file.path(OUT_DIR, "fake_top_words.csv"))
write_csv(trust_top_words, file.path(OUT_DIR, "trust_top_words.csv"))

# ============================================================
# 2) Sentiment (safe summary)
# ============================================================
fake_sent <- fake_words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment) %>%
  mutate(percent = round(100 * n / sum(n), 1))

trust_sent <- trust_words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment) %>%
  mutate(percent = round(100 * n / sum(n), 1))

write_csv(fake_sent,  file.path(OUT_DIR, "fake_sentiment_bing.csv"))
write_csv(trust_sent, file.path(OUT_DIR, "trust_sentiment_bing.csv"))

# ============================================================
# 3) Topic Modeling (LDA) — top terms only (safe)
# ============================================================
make_dtm <- function(words_df) {
  words_df %>%
    count(id, word) %>%
    cast_dtm(document = id, term = word, value = n)
}

fake_dtm  <- make_dtm(fake_words)
trust_dtm <- make_dtm(trust_words)

set.seed(123)
k <- 3

# ---- Guardrail: LDA can fail if DTM is too sparse ----
if (nrow(fake_dtm) < 5 || nrow(trust_dtm) < 5) {
  stop("Not enough documents for LDA after preprocessing. Try loosening filters or lowering k.")
}

fake_lda  <- LDA(fake_dtm,  k = k, control = list(seed = 123))
trust_lda <- LDA(trust_dtm, k = k, control = list(seed = 123))

fake_topics <- tidy(fake_lda, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

trust_topics <- tidy(trust_lda, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

write_csv(fake_topics,  file.path(OUT_DIR, "fake_topics_top_terms.csv"))
write_csv(trust_topics, file.path(OUT_DIR, "trust_topics_top_terms.csv"))

# Optional: run summary (nice for reviewers)
run_summary <- tibble(
  metric = c("n_fake_responses", "n_trust_responses", "k_topics"),
  value  = c(nrow(fake), nrow(trust), k)
)
write_csv(run_summary, file.path(OUT_DIR, "qual_run_summary.csv"))

# ============================================================
# 4) Researcher Synthesis (safe, interpretive)
# ============================================================
contrast_table <- tibble(
  theme_type = c("Visual cues", "Behavioral cues", "Conversational cues"),
  fake_signals = c(
    "Inconsistent photos, heavy filters, too polished",
    "Odd timing, evasive answers, low effort",
    "Scripted vibe, vague prompts, mismatched tone"
  ),
  trust_signals = c(
    "Consistent photos, natural look, context-rich images",
    "Responsive, stable communication, effortful engagement",
    "Emotionally coherent conversation, warmth, clarity"
  )
)

write_csv(contrast_table, file.path(OUT_DIR, "contrast_matrix.csv"))
