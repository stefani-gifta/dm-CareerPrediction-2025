# ----------------------------
# AI-POWERED Resume + TechWolf Pipeline
# Uses Claude API for intelligent resume parsing
# ----------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)
library(janitor)
library(docxtractr)
library(httr)
library(jsonlite)

# ----------------------------
# CONFIGURATION
# ----------------------------
ANTHROPIC_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")  # Set this in your .Renviron file
USE_AI_EXTRACTION <- TRUE  # Set to FALSE to use pattern-matching only

if (USE_AI_EXTRACTION && ANTHROPIC_API_KEY == "") {
  warning("ANTHROPIC_API_KEY not found. Set it with: Sys.setenv(ANTHROPIC_API_KEY='your-key-here')")
  USE_AI_EXTRACTION <- FALSE
}

# ----------------------------
# AI EXTRACTION FUNCTION
# ----------------------------
library(httr)
library(jsonlite)

check_available_models <- function() {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY") # Ensure this is set!
  
  if (api_key == "") stop("ANTHROPIC_API_KEY is missing from environment.")
  
  # We check the standard 'models' endpoint
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models?key=", api_key)
  
  resp <- httr::GET(url)
  
  if (status_code(resp) != 200) {
    cat("Error reaching API:", status_code(resp), "\n")
    print(content(resp, "text"))
    return(NULL)
  }
  
  data <- content(resp, "parsed")
  models <- data$models
  
  # Print nicely
  cat("\n=== AVAILABLE MODELS FOR YOUR KEY ===\n")
  for (m in models) {
    # Filter for 'generateContent' capable models
    if ("generateContent" %in% m$supportedGenerationMethods) {
      cat("Model ID:", m$name, "\n")
    }
  }
}

check_available_models()
# dplyr::last_dplyr_warnings()
extract_resume_with_ai <- function(resume_text, person_id) {
  
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  
  if (api_key == "") {
    warning("No GOOGLE_API_KEY found.")
    return(list(education = list(), jobs = list(), name = NA, success = FALSE))
  }
    
    # 1. SIMPLIFIED PROMPT: We don't need to beg for JSON format as much
    # because the API setting will enforce it.
    prompt_text <- paste0(
      "You are a resume parser. Extract structured information from this resume text.\n",
      "Dates must be YYYY-MM-DD. If a specific day is missing, use YYYY-MM-01.\n\n",
      "Resume Text:\n\"\"\"\n",
      substr(resume_text, 1, 30000), # Flash has a huge context window, use it!
      "\n\"\"\"\n\n",
      "Return data matching this JSON schema:\n",
      "{ \n",
      "  name: string, \n",
      "  education: [{ degree_level, major, university, graduation_year }], \n",
      "  jobs: [{ job_title, company, start_date, end_date }] \n",
      "}"
    )
    
    # 2. CORRECT URL: Use the stable model tag
    url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
    
    # 3. NATIVE JSON MODE: The 'generationConfig' is the secret sauce
    body <- list(
      contents = list(
        list(parts = list(list(text = prompt_text)))
      ),
      generationConfig = list(
        responseMimeType = "application/json",
        temperature = 0.1 # Keep it deterministic
      )
    )
    
    # 4. EXECUTE REQUEST
    resp <- tryCatch({
      httr::POST(
        url = paste0(url, "?key=", api_key),
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        httr::add_headers("Content-Type" = "application/json")
      )
    }, error = function(e) {
      warning(paste("HTTP Request Failed:", person_id, e$message))
      return(NULL)
    })
    
    if (is.null(resp) || httr::status_code(resp) != 200) {
      warning(paste("API Error:", httr::status_code(resp), "-", httr::content(resp, "text", encoding = "UTF-8")))
      return(list(education = list(), jobs = list(), name = NA, success = FALSE))
    }
    
    # 5. ROBUST PARSING (Stripping Markdown)
    out <- httr::content(resp, "parsed")
    raw_text <- out$candidates[[1]]$content$parts[[1]]$text
    
    # Remove markdown backticks if present (e.g. ```json ... ```)
    clean_json <- str_replace_all(raw_text, "```json|```", "")
    
    parsed <- tryCatch({
      jsonlite::fromJSON(clean_json, simplifyVector = FALSE)
    }, error = function(e) {
      warning(paste("JSON Syntax Error for", person_id, ":", e$message))
      return(NULL)
    })
    
    if (is.null(parsed)) {
      return(list(education = list(), jobs = list(), name = NA, success = FALSE))
    }
    
    parsed$success <- TRUE
    parsed
  }

# ----------------------------
# FALLBACK: Pattern-based extraction
# ----------------------------
extract_education_patterns <- function(text) {
  if (is.na(text) || text == "") return(tibble())
  
  edu_patterns <- c(
    "bachelor|bsc|b\\.s\\.|ba|b\\.a\\.",
    "master|msc|m\\.s\\.|ma|m\\.a\\.|mba",
    "phd|ph\\.d\\.|doctorate|doctoral",
    "diploma|associate"
  )
  
  sentences <- str_split(text, "\\n|\\.")[[1]]
  edu_sentences <- sentences[str_detect(tolower(sentences), paste(edu_patterns, collapse = "|"))]
  
  if (length(edu_sentences) == 0) return(tibble())
  
  tibble(
    education_text = edu_sentences,
    degree_level = case_when(
      str_detect(tolower(education_text), "phd|doctorate") ~ "PhD",
      str_detect(tolower(education_text), "master|msc|ma|mba") ~ "Masters",
      str_detect(tolower(education_text), "bachelor|bsc|ba") ~ "Bachelors",
      TRUE ~ "Other"
    ),
    major = str_extract(tolower(education_text), 
                        "(computer science|engineering|business|it|software|math|statistics|marketing|finance|design|data science)"),
    university = str_extract(education_text, "(?i)(university|college|institute) [A-Za-z ]{3,40}"),
    graduation_year = str_extract(education_text, "\\b(19|20)\\d{2}\\b")
  )
}

extract_job_patterns <- function(text) {
  if (is.na(text) || text == "") return(tibble())
  
  job_patterns <- c(
    "(?i)(software|senior|junior|lead|principal) (engineer|developer|analyst|manager)",
    "(?i)(project|product|program) (manager|director|lead)",
    "(?i)(data|business) analyst"
  )
  
  job_titles <- str_extract_all(text, paste(job_patterns, collapse = "|"))[[1]]
  
  if (length(job_titles) == 0) return(tibble())
  
  tibble(
    job_title = unique(job_titles)[1:min(3, length(unique(job_titles)))],
    job_label_standardized = tolower(job_title)
  )
}

# ----------------------------
# 1) Read .docx resumes
# ----------------------------
resume_dir <- "Resumes"
docx_files <- list.files(resume_dir, pattern = "\\.docx$", full.names = TRUE)

if (length(docx_files) == 0) {
  stop("No .docx files found in Resumes/ directory!")
}

# install.packages("readtext")

library(readtext)

extract_docx_text_readtext <- function(path) {
  tryCatch({
    doc <- readtext(path)
    text <- doc$text
    paste(text, collapse = "\n")
  }, error = function(e) {
    warning(paste("Failed:", basename(path), "-", e$message))
    return(NA_character_)
  })
}

df_resumes <- tibble(
  file = basename(docx_files),
  full_path = docx_files,
  raw_text = map_chr(docx_files, extract_docx_text_readtext)
) %>%
  filter(!is.na(raw_text) & nchar(raw_text) > 50) %>%
  mutate(
    person_id = paste0("R_", row_number())
  )

cat("\n=== RESUME EXTRACTION ===\n")
cat("✓ Loaded", nrow(df_resumes), "resume files\n")

df_resumes[1, 'raw_text']

# ----------------------------
# 2) Extract with AI (with progress)
# ----------------------------
cat("\n=== AI EXTRACTION (this may take a minute) ===\n")

if (USE_AI_EXTRACTION && nrow(df_resumes) > 0) {
  
  # df_resumes_small <- df_resumes %>% slice_head(n = 10)
  
  df_results <- df_resumes %>%
    mutate(
      ai_result = map2(
        raw_text,
        person_id,
        function(txt, pid) {
          Sys.sleep(0.8)   # gentle rate-limit
          extract_resume_with_ai(txt, pid)
        }
      )
    )
  
  # ---- extract name ----
  df_results <- df_results %>%
    mutate(name = map_chr(ai_result, ~ .x$name %||% NA_character_))
  
  # ---- extract education ----
  education_df <- df_results %>%
    mutate(education = map(ai_result, ~ .x$education %||% list())) %>%
    select(person_id, education) %>%
    unnest_longer(education, keep_empty = TRUE) %>%
    unnest_wider(education, names_sep = "_")
  
  # ---- extract jobs ----
  jobs_df <- df_results %>%
    mutate(jobs = map(ai_result, ~ .x$jobs %||% list())) %>%
    select(person_id, jobs) %>%
    unnest_longer(jobs, keep_empty = TRUE) %>%
    unnest_wider(jobs, names_sep = "_")
  
  cat("✓ AI extracted", nrow(education_df), "education records\n")
  cat("✓ AI extracted", nrow(jobs_df), "job records\n")
  
} else {
  # Fallback to pattern matching
  cat("Using pattern-based extraction (no API key provided)\n")
  
  EDUCATION_resumes <- df_resumes %>%
    select(person_id, raw_text) %>%
    mutate(edu = map(raw_text, extract_education_patterns)) %>%
    unnest(edu, keep_empty = FALSE) %>%
    select(person_id, degree_level, major, university, graduation_year)
  
  JOB_resumes <- df_resumes %>%
    select(person_id, raw_text) %>%
    mutate(job = map(raw_text, extract_job_patterns)) %>%
    unnest(job, keep_empty = FALSE) %>%
    select(person_id, job_title, job_label_standardized)
  
  df_resumes <- df_resumes %>%
    mutate(name = str_extract(raw_text, "(?m)^[A-Z][A-Za-z '\\-]{2,40}"))
}

# ----------------------------
# 3) REMOVE TECHWOLF SECTION
# ----------------------------
cat("\n=== TECHWOLF DISABLED ===\n")
df_tw <- tibble()
JOB_tw <- tibble()
EDUCATION_tw <- tibble()
PERSON_tw <- tibble()

# ----------------------------
# 5) Create unified tables (RESUME-ONLY)
# ----------------------------

# PERSON table — only resume people
PERSON <- df_results %>%
  select(person_id, name)

# EDUCATION table — only resume education
EDUCATION <- education_df

# JOB table — only resume jobs
JOB <- jobs_df

# ----------------------------
# 6) Create analysis dataset
# ----------------------------
FINAL_DATASET <- PERSON %>%
  left_join(EDUCATION, by="person_id", relationship = "many-to-many") %>%
  left_join(JOB, by="person_id", relationship = "many-to-many")

# ----------------------------
# 7) Summary & Export
# ----------------------------
cat("\n=== FINAL SUMMARY (RESUME-ONLY) ===\n")
cat("Total people:", nrow(PERSON), "\n")
cat("Total education records:", nrow(EDUCATION), "\n")
cat("Total job records:", nrow(JOB), "\n")

# write.csv(PERSON, "PERSON_table.csv", row.names = FALSE)
# write.csv(EDUCATION, "EDUCATION_table.csv", row.names = FALSE)
# write.csv(JOB, "JOB_table.csv", row.names = FALSE)

sapply(FINAL_DATASET, is.list)
FINAL_DATASET <- FINAL_DATASET %>%
  select(-education_graduation_year)
write.csv(FINAL_DATASET, "FINAL_dataset.csv", row.names = FALSE)

cat("\n✓ All files saved successfully!\n")
cat("\nFiles created:\n")
cat("  - PERSON_table.csv\n")
cat("  - EDUCATION_table.csv\n")
cat("  - JOB_table.csv\n")
cat("  - FINAL_dataset.csv\n")
