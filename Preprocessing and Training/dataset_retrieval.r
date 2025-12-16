install.packages(c("jsonlite", "dplyr", "purrr", "stringr", "tidyr"))

# ===============================================================
# PACKAGES
# ===============================================================
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

# ===============================================================
# 1. Load & decode JSONL (with multiple encodings)
# ===============================================================

path <- "master_resumes.jsonl"   # <-- change this to your file name

safe_decode <- function(s) {
  # Decode JSON lines that may be double or triple encoded
  if (is.na(s)) return(NULL)
  s <- trimws(s)

  # Strip outer quotes if present
  if ((startsWith(s, "\"") && endsWith(s, "\"")) ||
      (startsWith(s, "'")  && endsWith(s, "'"))) {
    s <- substr(s, 2, nchar(s) - 1)
  }

  # Try up to 4 times (like the Python loop)
  for (i in 1:4) {
    obj <- tryCatch(
      jsonlite::fromJSON(s, simplifyVector = FALSE),
      error = function(e) NULL
    )

    # If fails, try a "single-quote to double-quote" variant
    if (is.null(obj)) {
      s_json <- gsub("'", "\"", s, fixed = TRUE)
      obj <- tryCatch(
        jsonlite::fromJSON(s_json, simplifyVector = FALSE),
        error = function(e) NULL
      )
    }

    # If we finally get a list/dict: done
    if (!is.null(obj) && (is.list(obj) || is.vector(obj))) {
      # If still encoded as a single string, decode again
      if (is.character(obj) && length(obj) == 1) {
        s <- obj
        next
      }
      return(obj)
    }

    # If obj is a simple string, prepare for next iteration
    if (is.character(obj) && length(obj) == 1) {
      s <- obj
      next
    }

    break
  }

  return(NULL)
}

lines <- readLines(path, encoding = "UTF-8", warn = FALSE)

objs <- map(lines, safe_decode) |>
  discard(~ is.null(.x) || !is.list(.x))

# ===============================================================
# 2. Flattening helpers (skills, experience, education, projects)
# ===============================================================

flatten_experience <- function(exp) {
  if (!is.list(exp)) return("")
  out <- character()

  for (e in exp) {
    if (!is.list(e)) next

    # ---- Responsibilities (list of strings) ----
    resp <- e[["responsibilities"]]
    if (is.list(resp)) {
      out <- c(out, unlist(lapply(resp, as.character)))
    }

    # ---- Technical Environment ----
    tech_env <- e[["technical_environment"]]
    if (is.list(tech_env)) {
      # languages, frameworks, databases, operating_systems, methodologies, tools
      langs <- tech_env[["languages"]]
      if (is.list(langs)) {
        out <- c(out, unlist(lapply(langs, as.character)))
      }

      frameworks <- tech_env[["frameworks"]]
      if (is.list(frameworks)) {
        out <- c(out, unlist(lapply(frameworks, as.character)))
      }

      dbs <- tech_env[["databases"]]
      if (is.list(dbs)) {
        out <- c(out, unlist(lapply(dbs, as.character)))
      }

      oses <- tech_env[["operating_systems"]]
      if (is.list(oses)) {
        out <- c(out, unlist(lapply(oses, as.character)))
      }

      meth <- tech_env[["methodologies"]]
      if (is.list(meth)) {
        out <- c(out, unlist(lapply(meth, as.character)))
      }

      tools <- tech_env[["tools"]]
      if (is.list(tools)) {
        out <- c(out, unlist(lapply(tools, as.character)))
      }
    }
  }

  paste(out, collapse = ", ")
}

# get_job_titles <- function(exp) {
#   if (!is.list(exp)) return("")
#   out <- character()

#   for (e in exp) {
#     if (!is.list(e)) next

#     title <- e[["title"]]
#     if (is.character(title) && length(title) == 1) {
#       out <- c(out, title)
#     }
#   }

#   # join all titles from experience (like ", ".join in Python)
#   paste(out, collapse = ", ")
# }

flatten_education <- function(edu) {
  if (!is.list(edu)) return("")
  out <- character()

  for (e in edu) {
    if (!is.list(e)) next
    sec <- e[["degree"]]
    if (is.list(sec)) {
      # collect all scalar values from degree dict
      for (v in sec) {
        if (is.atomic(v) && length(v) == 1) {
          out <- c(out, as.character(v))
        }
      }
    }
  }

  paste(out, collapse = ", ")
}

flatten_projects <- function(proj) {
  if (!is.list(proj)) return("")
  out <- character()

  for (p in proj) {
    if (!is.list(p)) next

    for (k in c("description", "technologies", "impact", "role")) {
      v <- p[[k]]
      if (is.character(v)) {
        out <- c(out, v)
      }
    }
  }

  paste(out, collapse = ", ")
}

flatten_skills <- function(sk) {
  if (!is.list(sk)) return("")
  out <- character()

  # Loop through top-level categories (but skip "languages")
  for (category in names(sk)) {
    if (identical(category, "languages")) next

    items <- sk[[category]]

    # Add category name
    out <- c(out, category)

    # Items inside category should be a list
    if (is.list(items) && !is.data.frame(items)) {
      for (it in items) {
        if (is.list(it)) {
          name <- it[["name"]]
          if (is.character(name)) {
            out <- c(out, name)
          }
        }
      }
    } else if (is.list(items) || is.data.frame(items)) {
      # handle dict with nested lists, e.g. project_management
      if (is.list(items)) {
        subcats <- names(items)
        for (subcat in subcats) {
          out <- c(out, subcat)
          subitems <- items[[subcat]]
          if (is.list(subitems)) {
            for (it in subitems) {
              if (is.list(it)) {
                name <- it[["name"]]
                if (is.character(name)) {
                  out <- c(out, name)
                }
              }
            }
          }
        }
      }
    }
  }

  paste(out, collapse = ", ")
}

# ===============================================================
# 3. Build final clean dataset from raw JSONL objects
# ===============================================================

rows <- vector("list", length(objs))
for (i in seq_along(objs)) {
  o <- objs[[i]]

  pi <- o[["personal_info"]]
  summary <- ""
  if (is.list(pi) && !is.null(pi[["summary"]])) {
    if (is.list(pi[["summary"]])) {
      summary <- pi[["summary"]][["content"]] %||% ""
    } else if (is.character(pi[["summary"]])) {
      summary <- pi[["summary"]]
    }
  }

  skills <- flatten_skills(o[["skills"]])
  exp    <- flatten_experience(o[["experience"]])
  edu    <- flatten_education(o[["education"]])
  proj   <- flatten_projects(o[["projects"]])

  # job_title <- get_job_titles(o[["experience"]])

  resume_text <- paste(skills, exp, edu, proj)

  rows[[i]] <- data.frame(
    skills        = skills,
    experience    = exp,
    education     = edu,
    projects      = proj,
    combined_text = resume_text,
    # job_title     = job_title,
    stringsAsFactors = FALSE
  )
}

df <- bind_rows(rows)

# Optional: inspect structure (like df.info() / df.head())
str(df)
print(dim(df))

# Save first-stage CSV
write.csv(df, "resumes_clean_labeled.csv", row.names = FALSE)
cat("Saved resumes_clean_labeled.csv\n")