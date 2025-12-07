library(tidyverse)
library(caret)
library(text2vec)
library(stringr)

df <- read.csv('FINAL_dataset.csv')
head(df, 50)

# check missing values
colSums(is.na(df))

df_no_na <- df %>% drop_na()
colSums(is.na(df_no_na))

# check duplicates
sum(duplicated(df_no_na))

write.csv(df_no_na, 'final_dataset_no_na.csv', row.names = FALSE)

table(df_no_na$name)

# check specific duplicates
df_no_na %>% filter(name == 'Samir R. Naik')

# separate data for preprocessing
df_proficiency <- df_no_na %>%
  select(name, total_years, seniority_level, skills_detected, skill_count) %>%
  distinct()

df_education <- df_no_na %>%
  select(name, education_degree_level, education_major, education_university) %>%
  distinct()

df_jobs <- df_no_na %>%
  select(name, jobs_job_title, jobs_company, jobs_start_date, jobs_end_date) %>%
  distinct()


# preprocess proficiency
all_skills <- df_proficiency %>%
  select(skills_detected) %>%
  separate_rows(skills_detected, sep = ",") %>%
  mutate(skills_detected = str_trim(tolower(skills_detected)))
cat("unique skills count:", n_distinct(all_skills$skills_detected), "\n")

# standardize skills mapping
skill_mapping <- c(
  # Programming Languages
  'scala' = 'Scala', 'go' = 'Go', 'r' = 'R', 'java' = 'Java',
  'javascript' = 'Javascript', 'js' = 'Javascript', 'typescript' = 'Typescript',
  'perl' = 'Perl', 'bash' = 'Bash/Shell', 'shell' = 'Bash/Shell',
  'shell scripting' = 'Bash/Shell', 'c++' = 'C++', 'c#' = 'C#',
  'vb.net' = 'Vb.Net', 'ruby' = 'Ruby', 'python' = 'Python',
  'php' = 'Php', 'groovy' = 'Groovy', 'swift' = 'Swift',
  'objective-c' = 'Objective-C', 'kotlin' = 'Kotlin', 'matlab' = 'Matlab',
  
  # Web & Frontend
  'html' = 'HTML', 'html5' = 'HTML', 'css' = 'CSS', 'css3' = 'CSS',
  'ajax' = 'Ajax', 'xml' = 'Xml', 'jquery' = 'Jquery', 'ember' = 'Ember',
  'angular' = 'Angular', 'angularjs' = 'Angular', 'react' = 'React',
  'reactjs' = 'React', 'node' = 'Node.js', 'node.js' = 'Node.js',
  'jsp' = 'Jsp', 'servlet' = 'Servlet', 'express' = 'Express',
  'jsf' = 'Jsf', 'struts' = 'Struts', 'polymer' = 'Polymer',
  'backbone' = 'Backbone', 'extjs' = 'Extjs', 'dojo' = 'Dojo',
  'bootstrap' = 'CSS', 'webpack' = 'Webpack', 'npm' = 'Npm',
  'gulp' = 'Gulp', 'grunt' = 'Grunt',
  
  # Backend & Frameworks
  'spring' = 'Spring', 'spring boot' = 'Spring Boot', 'spring mvc' = 'Spring Mvc',
  'spring security' = 'Spring Security', 'hibernate' = 'Hibernate',
  'jpa' = 'Jpa', 'ejb' = 'Ejb', 'ruby on rails' = 'Ruby',
  'microservices' = 'Microservices', 'bottle' = 'Bottle', 'django' = 'Django',
  'laravel' = 'Laravel', 'symfony' = 'Symfony', 'zend' = 'Zend',
  'codeigniter' = 'Codeigniter', 'cakephp' = 'Cakephp', 'joomla' = 'Joomla',
  'drupal' = 'Drupal', 'wordpress' = 'Wordpress', 'magento' = 'Magento',
  'ionic' = 'Ionic', 'cordova' = 'Cordova',
  
  # Databases
  'sql' = 'Sql', 'sql server' = 'SQL Server', 'oracle' = 'Oracle',
  'db2' = 'Db2', 'mysql' = 'Mysql', 'postgresql' = 'Postgresql',
  'sqlite' = 'Sqlite', 'mongodb' = 'Mongodb', 'cassandra' = 'Cassandra',
  'redis' = 'Redis', 'neo4j' = 'Neo4J', 'dynamodb' = 'Dynamodb',
  'teradata' = 'Teradata', 'data warehouse' = 'Data Warehousing',
  'data warehousing' = 'Data Warehousing', 'olap' = 'Olap', 'oltp' = 'Oltp',
  'mapreduce' = 'Mapreduce', 'hadoop' = 'Hadoop', 'hbase' = 'Hbase',
  'hive' = 'Hive', 'pig' = 'Pig', 'elasticsearch' = 'Elasticsearch',
  'sqoop' = 'Sqoop', 'flume' = 'Flume',
  
  # Cloud
  'aws' = 'Aws', 'ec2' = 'Aws', 's3' = 'Aws', 'cloudfront' = 'Cloudfront',
  'google cloud' = 'Google Cloud', 'azure' = 'Azure', 'openshift' = 'Openshift',
  'cloud foundry' = 'Cloud Foundry', 'lambda' = 'Lambda', 'heroku' = 'Heroku',
  
  # DevOps & Automation
  'docker' = 'Docker', 'kubernetes' = 'Kubernetes', 'jenkins' = 'Jenkins',
  'bamboo' = 'Bamboo', 'puppet' = 'Puppet', 'chef' = 'Chef',
  'terraform' = 'Terraform', 'github' = 'Github', 'bitbucket' = 'Bitbucket',
  'svn' = 'Svn', 'clearcase' = 'Clearcase', 'gitlab' = 'Gitlab',
  'travis ci' = 'Travis Ci', 'ansible' = 'Ansible', 'powershell' = 'Powershell',
  'mercurial' = 'Mercurial', 'ci/cd' = 'CI/CD', 'devops' = 'DevOps',
  
  # Testing Tools
  'selenium' = 'Selenium', 'cucumber' = 'Cucumber', 'junit' = 'Junit',
  'testng' = 'Testng', 'jasmine' = 'Jasmine', 'mocha' = 'Mocha',
  'karma' = 'Karma', 'protractor' = 'Protractor', 'postman' = 'Postman',
  'soap' = 'Soap', 'soap ui' = 'Soap', 'soapui' = 'Soap',
  'bdd' = 'Bdd', 'tdd' = 'Tdd',
  
  # Analytics / BI
  'tableau' = 'Tableau', 'power bi' = 'Power Bi', 'cognos' = 'Cognos',
  'ssrs' = 'Ssrs', 'crystal reports' = 'Crystal Reports',
  'microstrategy' = 'Microstrategy', 'jasper reports' = 'Jasper Reports',
  'qlikview' = 'Qlikview',
  
  # ML & Data Science
  'machine learning' = 'Machine Learning', 'deep learning' = 'Deep Learning',
  'nlp' = 'Nlp', 'tensorflow' = 'Tensorflow', 'pandas' = 'Pandas',
  'numpy' = 'Numpy', 'spark' = 'Spark', 'storm' = 'Storm',
  'kafka' = 'Kafka', 'ab initio' = 'Ab Initio', 'mybatis' = 'Mybatis',
  
  # Operating Systems
  'linux' = 'Linux', 'unix' = 'Unix', 'windows' = 'Windows',
  'solaris' = 'Solaris', 'aix' = 'Aix', 'mac os' = 'MacOS',
  'macos' = 'MacOS', 'ubuntu' = 'Ubuntu', 'centos' = 'Centos',
  'red hat' = 'Red Hat',
  
  # Tools
  'maven' = 'Maven', 'gradle' = 'Gradle', 'jira' = 'Jira',
  'confluence' = 'Confluence', 'ms office' = 'Ms Office',
  'excel' = 'Excel', 'visio' = 'Visio', 'git' = 'Git', 'yum' = 'Linux',
  
  # Monitoring
  'nagios' = 'Nagios', 'splunk' = 'Splunk',
  
  # Reporting / Others
  'etl' = 'Etl', 'informatica' = 'Informatica', 'ssis' = 'Ssis',
  
  # Content Management / DXPs
  'aem' = 'Aem', 'adobe experience manager' = 'Adobe Experience Manager',
  'sitecore' = 'Sitecore',
  
  # Soft / Process Skills
  'agile' = 'Agile', 'scrum' = 'Scrum', 'waterfall' = 'Waterfall',
  'kanban' = 'Kanban', 'soa' = 'Soa'
)

standardize_skill <- function(skill) {
  skill_norm <- tolower(str_trim(skill))
  if (skill_norm %in% names(skill_mapping)) {
    return(skill_mapping[skill_norm])
  } else {
    return(str_to_title(skill))
  }
}

# process skills for each person
process_skills <- function(skills_string) {
  if (is.na(skills_string)) return(character(0))
  
  skills <- str_split(skills_string, ",")[[1]] %>%
    str_trim() %>%
    tolower()
  
  standardized <- sapply(skills, standardize_skill, USE.NAMES = FALSE)
  standardized_unique_skills <- unique(standardized)
  
  return(standardized_unique_skills)
}

df_proficiency$standardized_skills_list <- lapply(df_proficiency$skills_detected, process_skills)

all_skills_flat <- unlist(df_proficiency$standardized_skills_list)
mlb_classes <- sort(unique(all_skills_flat))

skills_encoded <- t(sapply(df_proficiency$standardized_skills_list, function(skills) {
  as.integer(mlb_classes %in% skills)
}))

df_encoded_skills <- as.data.frame(skills_encoded)
colnames(df_encoded_skills) <- mlb_classes

colnames(df_encoded_skills) <- paste0('skill_', colnames(df_encoded_skills))

df_proficiency_reset <- df_proficiency
rownames(df_proficiency_reset) <- NULL
rownames(df_encoded_skills) <- NULL

df_standardized_skills <- cbind(
  df_proficiency_reset[, !names(df_proficiency_reset) %in% c('skills_detected', 'skill_count', 'standardized_skills_list')],
  df_encoded_skills
)

# preprocess education
name_counts <- table(df_education$name)
duplicated_names <- names(name_counts[name_counts > 1])

df_education <- df_education %>%
  mutate(
    education_degree_level = tolower(education_degree_level),
    education_degree_level = str_remove_all(education_degree_level, "\\s*\\(.*?\\)"),
    education_degree_level = str_remove_all(education_degree_level, "\\."),
    education_degree_level = str_remove_all(education_degree_level, "'s")
  )

standardize_degree <- function(degree) {
  degree <- tolower(as.character(degree))
  
  if (str_detect(degree, "phd")) return("Doctorate")
  if (str_detect(degree, "master|mba|ms|mca|business administration")) return("Master")
  if (str_detect(degree, "bachelor|be|bsc|b sc|ba|btech|bcom")) return("Bachelor")
  if (str_detect(degree, "certificate|certification|course|ca|program|training")) return("Certification")
  if (str_detect(degree, "high school|ged")) return("High School")
  if (str_detect(degree, "diploma|post graduate")) return("Diploma")
  
  return(degree)
}

df_education <- df_education %>%
  mutate(standardized_degree_level = sapply(education_degree_level, standardize_degree))

# rank degrees
degree_rank_mapping <- c(
  'High School' = 1,
  'Certification' = 2,
  'Diploma' = 3,
  'Bachelor' = 4,
  'Master' = 5,
  'Doctorate' = 6
)

df_education <- df_education %>%
  mutate(degree_rank = degree_rank_mapping[standardized_degree_level])

# get highest degree per person
highest_degree_per_person <- df_education %>%
  group_by(name) %>%
  slice_max(degree_rank, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(name, standardized_degree_level, education_major, education_university)

df_education <- highest_degree_per_person

# standardize major
df_education <- df_education %>%
  mutate(
    education_major = str_to_title(education_major),
    education_major = str_remove_all(education_major, "\\s*\\(.*?\\)")
  )

standardize_major <- function(major) {
  major <- tolower(as.character(major))
  
  if (str_detect(major, "computer|computing|it|software|network|information system|information technology")) {
    return("Computer Science & IT")
  }
  if (str_detect(major, "business|management|marketing|strategic|corporate|project|operations|analytics|international")) {
    return("Business & Management")
  }
  if (str_detect(major, "commerce|finance|account")) {
    return("Commerce & Finance")
  }
  if (str_detect(major, "engineering|mechanics|electronics|electrical|telecommunication")) {
    return("Engineering")
  }
  if (str_detect(major, "science|life|physics|environment|technology")) {
    return("Applied/Life Sciences")
  }
  if (str_detect(major, "math|stat")) {
    return("Mathematics & Statistics")
  }
  
  return(major)
}

df_education <- df_education %>%
  mutate(standardized_major = sapply(education_major, standardize_major)) %>%
  select(-education_major)

# standardize university
df_education <- df_education %>%
  mutate(
    education_university = str_remove_all(education_university, "\\s*\\(.*?\\)"),
    education_university = str_remove_all(education_university, ","),
    education_university = str_remove_all(education_university, "\\b(India|Usa|Us|Uk|Uae)\\b"),
    education_university = str_trim(education_university)
  )

university_map <- c(
  'Jntu' = 'JNTU',
  'Jntu Hyderabad' = 'JNTU Hyderabad',
  'Jntu Kakinada' = 'JNTU Kakinada',
  'Jntu University' = 'JNTU',
  'Jntu-H' = 'JNTU Hyderabad',
  'Jntuh' = 'JNTU Hyderabad',
  'Jntuk University' = 'JNTU Kakinada',
  'Jntuk' = 'JNTU Kakinada',
  'Jawaharlal Nehru Technological University' = 'JNTU',
  'Jawaharlal Nehru Technological University Hyderabad' = 'JNTU Hyderabad',
  'Jawaharlal Nehru Technological University Kakinada' = 'JNTU Kakinada',
  'Not Specified' = 'Unknown',
  'India' = 'Unknown',
  'Madras University' = 'University Of Madras',
  'University Of Madras' = 'University Of Madras',
  'Bits Pilani' = 'BITS Pilani',
  'Calums Ca Us' = 'California University of Management & Sciences',
  'S.G.T.B Khalsa College University Of Delhi' = 'S.G.T.B Khalsa College',
  'New York Institute Of Technology' = 'New York Institute of Technology',
  'Universitatea De Vest Romania' = 'The West University of Timișoara',
  'Cbit Hyderabad India' = 'Chaitanya Bharathi Institute of Technology',
  'Ou' = 'Osmania University'
)

apply_university_map <- function(name) {
  clean <- str_to_title(str_trim(name))
  if (clean %in% names(university_map)) {
    return(university_map[clean])
  }
  return(clean)
}

df_education <- df_education %>%
  mutate(education_university = sapply(education_university, apply_university_map)) %>%
  select(-education_university)

# preprocess jobs
# get most recent job
df_jobs <- df_jobs %>%
  mutate(jobs_end_date = as.Date(jobs_end_date, format = "%Y-%m-%d")) %>%
  arrange(desc(jobs_end_date)) %>%
  distinct(name, .keep_all = TRUE) %>%
  select(name, jobs_job_title)

df_combined_data <- df_standardized_skills %>%
  full_join(df_education, by = "name") %>%
  full_join(df_jobs, by = "name")

# metrics
calculate_ranking_metrics <- function(y_true, y_scores, k_values = c(1, 3, 5, 10)) {
  results <- list()
  n_samples <- length(y_true)
  n_classes <- ncol(y_scores)
  
  # top-K Accuracy
  for (k in k_values) {
    if (k <= n_classes) {
      top_k_pred <- t(apply(y_scores, 1, function(x) order(x, decreasing = TRUE)[1:k]))
      top_k_acc <- mean(sapply(1:n_samples, function(i) y_true[i] %in% top_k_pred[i,]))
      results[[paste0('top_', k, '_accuracy')]] <- top_k_acc
    }
  }
  
  # mean Reciprocal Rank (MRR)
  reciprocal_ranks <- sapply(1:n_samples, function(i) {
    sorted_indices <- order(y_scores[i,], decreasing = TRUE)
    rank <- which(sorted_indices == y_true[i])[1]
    if (length(rank) > 0) 1.0 / rank else 0.0
  })
  results[['mrr']] <- mean(reciprocal_ranks)
  
  # precision and Recall at K
  for (k in k_values) {
    if (k <= n_classes) {
      top_k_pred <- t(apply(y_scores, 1, function(x) order(x, decreasing = TRUE)[1:k]))
      precision_at_k <- mean(sapply(1:n_samples, function(i) as.integer(y_true[i] %in% top_k_pred[i,])))
      results[[paste0('precision@', k)]] <- precision_at_k
      results[[paste0('recall@', k)]] <- precision_at_k # for single label
    }
  }
  
  # average similarity of correct matches
  correct_similarities <- sapply(1:n_samples, function(i) y_scores[i, y_true[i]])
  results[['avg_correct_similarity']] <- mean(correct_similarities)
  results[['min_correct_similarity']] <- min(correct_similarities)
  
  return(results)
}

# feature engineering
engineer_features <- function(df_combined) {
  skill_cols <- grep("^skill_", names(df_combined), value = TRUE)
  df_combined$total_skills <- rowSums(df_combined[, skill_cols])
  
  degree_mapping <- c(
    'High School' = 1, 'Certification' = 2, 'Diploma' = 3,
    'Bachelor' = 4, 'Master' = 5, 'Doctorate' = 6
  )
  
  df_combined$degree_numeric <- degree_mapping[df_combined$standardized_degree_level]
  df_combined$degree_numeric[is.na(df_combined$degree_numeric)] <- 0
  df_combined$has_masters_plus <- as.integer(df_combined$degree_numeric >= 5)
  
  df_combined$experience_level <- cut(df_combined$total_years,
                                      breaks = c(-1, 2, 5, 10, Inf),
                                      labels = c(0, 1, 2, 3))
  df_combined$experience_level <- as.integer(as.character(df_combined$experience_level))
  
  seniority_mapping <- c('Entry Level' = 0, 'Mid Level' = 1, 'Senior Level' = 2, 'Executive' = 3)
  df_combined$seniority_numeric <- seniority_mapping[df_combined$seniority_level]
  df_combined$seniority_numeric[is.na(df_combined$seniority_numeric)] <- 1
  
  major_mapping <- c(
    'Computer Science & IT' = 1.0, 'Engineering' = 0.9,
    'Mathematics & Statistics' = 0.8, 'Business & Management' = 0.5,
    'Commerce & Finance' = 0.3, 'Applied/Life Sciences' = 0.3
  )
  df_combined$major_relevance <- major_mapping[df_combined$standardized_major]
  df_combined$major_relevance[is.na(df_combined$major_relevance)] <- 0.4
  
  return(df_combined)
}

# create resume text
create_resume_text <- function(row) {
  skill_cols <- grep("^skill_", names(row), value = TRUE)
  skills <- skill_cols[row[skill_cols] == 1]
  skills <- gsub("skill_", "", skills)
  skills <- gsub("_", " ", skills)
  skills_text <- paste(skills, collapse = " ")
  
  education_text <- paste(row['standardized_degree_level'], row['standardized_major'])
  experience_text <- paste(row['seniority_level'], row['total_years'], "years")
  
  combined <- tolower(paste(skills_text, education_text, experience_text))
  combined <- paste(strsplit(combined, "\\s+")[[1]], collapse = " ")
  
  return(combined)
}

# print metrics function
print_metrics <- function(model_name, metrics) {
  cat("\nResults\n")
  
  cat("Top-K Accuracy:\n")
  for (k in c(1, 3, 5, 10)) {
    key <- paste0('top_', k, '_accuracy')
    if (key %in% names(metrics)) {
      cat(sprintf("  Top-%d: %.4f (%.2f%%)\n", k, metrics[[key]], metrics[[key]] * 100))
    }
  }
  
  cat("\nRanking Metrics:\n")
  cat(sprintf("  MRR: %.4f\n", metrics[['mrr']]))
  
  cat("\nPrecision & Recall:\n")
  for (k in c(1, 3, 5, 10)) {
    p_key <- paste0('precision@', k)
    r_key <- paste0('recall@', k)
    if (p_key %in% names(metrics)) {
      cat(sprintf("  @%d: P=%.4f, R=%.4f\n", k, metrics[[p_key]], metrics[[r_key]]))
    }
  }
  
  if ('avg_correct_similarity' %in% names(metrics)) {
    cat("\nSimilarity:\n")
    cat(sprintf("  Average: %.4f\n", metrics[['avg_correct_similarity']]))
    cat(sprintf("  Minimum: %.4f\n", metrics[['min_correct_similarity']]))
  }
  
  if ('cv_mean' %in% names(metrics)) {
    cat("\nCV Score:\n")
    cat(sprintf("  Mean: %.4f (±%.4f)\n", metrics[['cv_mean']], metrics[['cv_std']] * 2))
  }
}

# train model
# TF-IDF + cosine similarity
train_tfidf_cosine <- function(X_train, y_train, X_test, y_test) {
  it_train <- itoken(X_train, preprocessor = tolower, tokenizer = word_tokenizer)
  vocab <- create_vocabulary(it_train, stopwords = stopwords("en"))
  vocab <- prune_vocabulary(vocab, doc_proportion_min = 0.002, doc_proportion_max = 0.8)
  vectorizer <- vocab_vectorizer(vocab)
  
  dtm_train <- create_dtm(it_train, vectorizer)
  tfidf_model <- TfIdf$new()
  dtm_train_tfidf <- fit_transform(dtm_train, tfidf_model)
  
  it_test <- itoken(X_test, preprocessor = tolower, tokenizer = word_tokenizer)
  dtm_test <- create_dtm(it_test, vectorizer)
  dtm_test_tfidf <- transform(dtm_test, tfidf_model)
  
  cat(sprintf("TF-IDF shape: %d x %d\n", nrow(dtm_train_tfidf), ncol(dtm_train_tfidf)))
  
  similarities <- sim2(dtm_test_tfidf, dtm_train_tfidf, method = "cosine")
  
  n_classes <- length(unique(y_train))
  y_scores <- matrix(0, nrow = length(y_test), ncol = n_classes)
  
  for (i in 1:length(y_test)) {
    for (j in 1:length(y_train)) {
      y_scores[i, y_train[j] + 1] <- max(y_scores[i, y_train[j] + 1], similarities[i, j])
    }
  }
  
  metrics <- calculate_ranking_metrics(y_test + 1, y_scores)
  
  return(list(model = tfidf_model, metrics = metrics, y_scores = y_scores))
}

# logistic regression
train_logistic_regression <- function(X_train, y_train, X_test, y_test) {
  it_train <- itoken(X_train, preprocessor = tolower, tokenizer = word_tokenizer)
  vocab <- create_vocabulary(it_train, stopwords = stopwords("en"))
  vocab <- prune_vocabulary(vocab, doc_proportion_min = 0.002, doc_proportion_max = 0.8)
  vectorizer <- vocab_vectorizer(vocab)
  
  dtm_train <- create_dtm(it_train, vectorizer)
  tfidf_model <- TfIdf$new()
  dtm_train_tfidf <- fit_transform(dtm_train, tfidf_model)
  
  it_test <- itoken(X_test, preprocessor = tolower, tokenizer = word_tokenizer)
  dtm_test <- create_dtm(it_test, vectorizer)
  dtm_test_tfidf <- transform(dtm_test, tfidf_model)
  
  cat(sprintf("TF-IDF shape: %d x %d\n", nrow(dtm_train_tfidf), ncol(dtm_train_tfidf)))
  
  # with cv
  lr_model <- cv.glmnet(x = dtm_train_tfidf, y = factor(y_train),
                        family = "multinomial", nfolds = 5, alpha = 1)
  
  y_scores <- predict(lr_model, dtm_test_tfidf, s = "lambda.min", type = "response")
  y_scores <- y_scores[,,1] # extract probability matrix
  
  metrics <- calculate_ranking_metrics(y_test + 1, y_scores)
  metrics[['cv_mean']] <- max(lr_model$cvm)
  metrics[['cv_std']] <- lr_model$cvsd[which.max(lr_model$cvm)]
  
  return(list(model = lr_model, tfidf = tfidf_model, metrics = metrics, y_scores = y_scores))
}

# random forest
train_random_forest <- function(X_train, y_train, X_test, y_test) {
  # scale
  preProcValues <- preProcess(X_train, method = c("center", "scale"))
  X_train_scaled <- predict(preProcValues, X_train)
  X_test_scaled <- predict(preProcValues, X_test)
  
  # train
  rf_model <- randomForest(x = X_train_scaled, y = factor(y_train),
                           ntree = 200, maxnodes = 15,
                           mtry = floor(sqrt(ncol(X_train_scaled))),
                           importance = TRUE)
  
  y_scores <- predict(rf_model, X_test_scaled, type = "prob")
  
  metrics <- calculate_ranking_metrics(y_test + 1, y_scores)
  
  train_control <- trainControl(method = "cv", number = 5)
  cv_model <- train(x = X_train_scaled, y = factor(y_train),
                    method = "rf", trControl = train_control, ntree = 200)
  
  metrics[['cv_mean']] <- mean(cv_model$resample$Accuracy)
  metrics[['cv_std']] <- sd(cv_model$resample$Accuracy)
  
  importance_df <- data.frame(
    feature = rownames(importance(rf_model)),
    importance = importance(rf_model)[, "MeanDecreaseGini"]
  ) %>% arrange(desc(importance)) %>% head(15)
  
  cat("\nTop 15 Important Features:\n")
  for (i in 1:nrow(importance_df)) {
    cat(sprintf("  %-35s %.4f\n", importance_df$feature[i], importance_df$importance[i]))
  }
  
  return(list(model = rf_model, scaler = preProcValues, metrics = metrics, 
              y_scores = y_scores, importance = importance_df))
}

# group job titles 
group_job_titles <- function(job_title) {
  job_title <- tolower(job_title)
  job_title <- gsub("^(sr\\.?|senior|junior|jr\\.?|lead|principal|staff)\\s+", "", job_title)
  
  if (grepl("business analyst", job_title)) return("Business Analyst")
  if (grepl("java.*developer|j2ee", job_title)) return("Java Developer")
  if (grepl("full.*stack|fullstack", job_title)) return("Full Stack Developer")
  if (grepl("data (scientist|analyst)|analytics", job_title)) return("Data Scientist/Analyst")
  if (grepl("devops|site reliability", job_title)) return("DevOps Engineer")
  if (grepl("qa|test|quality assurance", job_title)) return("QA Engineer")
  if (grepl("project manager|scrum master|product owner", job_title)) return("Project Manager")
  if (grepl("software engineer|software developer", job_title)) return("Software Engineer")
  
  return(str_to_title(job_title))
}

# visualization 









