# install.packages(c("dplyr", "tidyr", "stringr", "readr", 
#                   "purrr", "janitor", "docxtractr"))

# ----------------------------
# Full robust pipeline (R)
# ----------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)
library(janitor)
library(docxtractr)

# ----------------------------
# 1) Read .docx resumes from Resumes/
# ----------------------------
resume_dir <- "Resumes"
docx_files <- list.files(resume_dir, pattern = "\\.docx$", full.names = TRUE)

extract_docx_text <- function(path) {
  doc <- docxtractr::read_docx(path)
  txt_vec <- docxtractr::docx_extract_all(doc)   # returns character vector
  paste(txt_vec, collapse = "\n")
}

# Build df_kaggle-like table
df_kaggle <- tibble(
  file = basename(docx_files),
  raw_text = map_chr(docx_files, extract_docx_text)
) %>%
  mutate(
    # crude name extraction (can improve)
    name = str_extract(raw_text, "(?m)^[A-Za-z][A-Za-z \\-]{1,80}"),
    # try to capture blocks labeled Education / Experience
    education = str_extract(raw_text, regex("(Education|EDUCATION)[\\s\\S]+?(Experience|EXPERIENCE|$)", ignore_case = TRUE)),
    experience = str_extract(raw_text, regex("(Experience|EXPERIENCE)[\\s\\S]+", ignore_case = TRUE))
  ) %>%
  mutate(person_id = paste0("K_", row_number()),
         source = "kaggle") %>%
  select(person_id, name, source, education, experience, file, raw_text)

# ----------------------------
# 2) Read TechWolf CSV safely (force everything to character)
# ----------------------------
# Use base read.csv to more robustly coerce columns; ensure working directory is correct
df_tw_raw <- read.csv("techwolf_workhistories.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE,
                      colClasses = "character",
                      check.names = FALSE,
                      fileEncoding = "UTF-8")

# Normalize column names to lowercase for predictable matching:
names(df_tw_raw) <- tolower(names(df_tw_raw))

# Clean names a bit (optional)
df_tw <- as_tibble(df_tw_raw) %>% clean_names()
# Ensure person_id and source
df_tw <- df_tw %>% mutate(person_id = paste0("T_", row_number()), source = "techwolf")

# ----------------------------
# 3) PERSON table (kaggle + techwolf)
# ----------------------------
person_kaggle <- df_kaggle %>% select(person_id, name, source)
person_tw <- df_tw %>% mutate(name = NA_character_) %>% select(person_id, name, source)
PERSON <- bind_rows(person_kaggle, person_tw)

# ----------------------------
# 4) EDUCATION table (from kaggle docs)
# ----------------------------
EDUCATION_kaggle <- df_kaggle %>%
  select(person_id, education) %>%
  filter(!is.na(education) & education != "") %>%
  separate_rows(education, sep = "\\n|;|,") %>%
  mutate(education = str_trim(education)) %>%
  filter(education != "") %>%
  mutate(
    degree_level = case_when(
      str_detect(tolower(education), "\\b(phd|doctor)\\b") ~ "PhD",
      str_detect(tolower(education), "\\b(master|msc|ma)\\b") ~ "Masters",
      str_detect(tolower(education), "\\b(bachelor|bsc|ba)\\b") ~ "Bachelors",
      str_detect(tolower(education), "diploma") ~ "Diploma",
      str_detect(tolower(education), "certificate|certification") ~ "Certification",
      TRUE ~ "Other"
    ),
    major = str_extract(education, "(?i)(computer science|engineering|business|it|software|math|statistics|marketing|physics|biology|chemistry|finance|design)"),
    university = str_extract(education, "(?i)(university|institute|college)[A-Za-z0-9 \\-&,\\.]*"),
    graduation_year = str_extract(education, "\\b(19|20)\\d{2}\\b")
  )

EDUCATION_tw <- tibble(
  person_id = character(), degree_level = character(),
  major = character(), university = character(), graduation_year = character()
)

EDUCATION <- bind_rows(EDUCATION_kaggle, EDUCATION_tw)

# ----------------------------
# 5) JOB table
#    - Kaggle: extract basic job_title from experience block
#    - Techwolf: pivot columns like title_0, esco_title_0, esco_uri_0, description_0, start_0, end_0
# ----------------------------

# JOB from kaggle docs (basic)
JOB_kaggle <- df_kaggle %>%
  mutate(job_title = str_extract(experience, "(?i)(?<=\\n|^)[A-Za-z][A-Za-z0-9 /\\-&]{2,80}")) %>%
  mutate(job_label_standardized = tolower(job_title)) %>%
  select(person_id, job_title, job_label_standardized)

# JOB from techwolf CSV (robust pivot)
# Build a regex that matches e.g. title_0, esco_title_0, esco_uri_0, description_0, start_0, end_0
col_pattern <- "^(title|description|start|end|esco_title|esco_uri)_\\d+$"

# Only pivot columns that exist
tech_cols_to_pivot <- names(df_tw)[grepl(col_pattern, names(df_tw), ignore.case = TRUE)]

JOB_tw <- df_tw %>%
  select(person_id, all_of(tech_cols_to_pivot)) %>%
  pivot_longer(
    cols = -person_id,
    names_to = c("field", "exp_num"),
    names_pattern = "(.+?)_(\\d+)$",
    values_to = "value"
  ) %>%
  # keep the original field name (e.g. title, esco_title, esco_uri, description, start, end)
  pivot_wider(names_from = field, values_from = value) %>%
  # Avoid using a column named 'title' which may conflict with base::title()
  mutate(
    job_title_raw = if ("title" %in% names(.)) title else if ("job_title" %in% names(.)) job_title else NA_character_,
    esco_title_raw = if ("esco_title" %in% names(.)) esco_title else NA_character_,
    esco_uri_raw = if ("esco_uri" %in% names(.)) esco_uri else NA_character_
  ) %>%
  # Ensure they are characters (they should be)
  mutate(
    job_title = as.character(job_title_raw),
    job_label_standardized = if_else(!is.na(esco_title_raw) & esco_title_raw != "", tolower(as.character(esco_title_raw)), tolower(coalesce(job_title, ""))),
    esco_code = as.character(esco_uri_raw)
  ) %>%
  filter(!is.na(job_title) & job_title != "") %>%
  select(person_id, job_title, job_label_standardized, esco_code)

# Combine JOB tables
JOB <- bind_rows(JOB_kaggle, JOB_tw)

# ----------------------------
# 6) Highest degree per person
# ----------------------------
highest_degree <- EDUCATION %>%
  mutate(rank = case_when(
    degree_level == "PhD" ~ 4,
    degree_level == "Masters" ~ 3,
    degree_level == "Bachelors" ~ 2,
    degree_level == "Diploma" ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(person_id) %>%
  slice_max(order_by = rank, n = 1, with_ties = FALSE) %>%
  ungroup()

# ----------------------------
# 7) Final job per person (first job row)
# ----------------------------
final_job <- JOB %>%
  group_by(person_id) %>%
  slice(1) %>%
  ungroup()

# ----------------------------
# 8) FINAL dataset
# ----------------------------
FINAL_DATASET <- PERSON %>%
  left_join(highest_degree %>% select(person_id, degree_level, major, university),
            by = "person_id") %>%
  left_join(final_job %>% select(person_id, job_label_standardized),
            by = "person_id") %>%
  rename(target_job = job_label_standardized)

# ----------------------------
# 9) Write outputs
# ----------------------------
write.csv(PERSON, "PERSON_table.csv", row.names = FALSE)
write.csv(EDUCATION, "EDUCATION_table.csv", row.names = FALSE)
write.csv(JOB, "JOB_table.csv", row.names = FALSE)
write.csv(FINAL_DATASET, "FINAL_dataset.csv", row.names = FALSE)

# End of script
