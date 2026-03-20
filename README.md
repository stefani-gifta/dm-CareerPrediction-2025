# Career Path Prediction Based on Resume

This study proposes an automated resume screening system to address the challenges of manual, time-consuming, and inconsistent candidate evaluation in large-scale hiring environments. The system functions as a decision support tool for recruiters, designed to classify candidates into appropriate job roles within the scope of computer science.

## Methodology

Data extraction was done to parse skills, experience, education, and project information from approximately 4,000 resumes using TF-IDF. The core of the approach is a multi-label classification model that allows a single candidate to be recommended for multiple positions. Seven models were evaluated, including classical machine learning algorithms (Logistic Regression, SVM, Naive Bayes, Random Forest, and LightGBM) and transformer-based deep learning models (DistilBERT and BERT Base Multilingual).

## Tech Stack

- **Pipeline**: R and Python
- **Data Format**: JSON (for raw resume data)

## Key Results

- **Top Performance:** LightGBM achieved the highest performance across all evaluation metrics, recording a micro-F1 score of 0.995.
- **Benefit:** The proposed system is expected to significantly reduce manual screening time by approximately 25–40%.
- **Future Direction:** While LightGBM performed best on the current dataset, transformer-based models (DistilBERT and BERT Base Multilingual) are considered more promising for future works involving larger and more linguistically diverse resume datasets.

## Resources

| Resource | Link |
| --- | --- |
| **Source Code** | [Github repository](https://github.com/stefani-gifta/dm-CareerPrediction-2025) |
| **Presentation Slides** | [Canva public view link](https://www.canva.com/design/DAG6LlAdTMg/wFKATm382QH2OY4TjNhunA/view?utm_content=DAG6LlAdTMg&utm_campaign=designshare&utm_medium=link2&utm_source=uniquelinks&utlId=h42fa6d6233) |
| **Dataset Used** | [HuggingFace (datasetmaster/resumes)](https://huggingface.co/datasets/datasetmaster/resumes) |

## Contributors

- Stefani Gifta Ganda
- Ersya Najwa Saskia
- Robert Lyon

## Lecturer

Henry Lucky
