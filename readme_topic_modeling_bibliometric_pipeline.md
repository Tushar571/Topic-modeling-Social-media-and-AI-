# 📘 Topic Modeling & Bibliometric Analysis Pipeline

This repository provides an **end-to-end R-based workflow** for text preprocessing, **Latent Dirichlet Allocation (LDA)** topic modeling, evaluation, visualization, and bibliometric exploration using `bibliometrix`. It is designed for **researchers analyzing scientific literature datasets** (e.g., Scopus or Web of Science exports).

---

## 📁 Project Structure

```
README.md
/data/            # raw input files (e.g., scopus_export.csv)
/scripts/         # main R scripts
/output/          # generated CSVs, plots, and models
/notebooks/       # optional RMarkdown notebooks
/LICENSE
```

---

## 🚀 Workflow Overview

### 1️⃣ Load & Import Data
- Uses: `readtext`, `quanteda`, `textstem`, `tm`, `dplyr`
- Input expected: a CSV or text file containing **Title**, **Abstract**, and **Author Keywords** columns.

```r
x <- readtext(file.choose())
x$data <- paste(x$Title, x$Abstract, x$`Author Keywords`)
```

### 2️⃣ Text Cleaning & Normalization
- Convert text to lowercase.
- Lemmatize using `textstem::lemmatize_strings()`.
- Remove numbers, punctuation, and English stopwords using `tm`.

### 3️⃣ Tokenization & Stopword Filtering
- Tokenize text via `quanteda::tokens()`.
- Remove stopwords and short tokens (`min_nchar = 2`).
- Apply a large **custom stopword list** to remove domain-generic words (e.g., *study*, *paper*, *result*).

### 4️⃣ Phrase Merging
Combine frequent multi-word expressions for improved topic coherence:
```r
t1 <- gsub("\\b(social|medium)\\b", "social_media", t, perl = TRUE)
t1 <- gsub("\\b(machine|learn)\\b", "machine_learning", t1, perl = TRUE)
```

### 5️⃣ Create Document-Feature Matrix (DFM)
```r
d1 <- quanteda::dfm(t1)
topfeatures(d1, 100)
```

---

## 🧠 Topic Modeling (LDA)

### Fit Multiple Models
Fit LDA models for k = 2 to 15 topics using Gibbs sampling:
```r
lda5 <- LDA(d2, k = 5, method = "Gibbs", control = list(seed = 1234))
```

### Evaluate Perplexity
```r
perplexity(lda5, newdata = d2)
```

### Determine Optimal Number of Topics
Using `ldatuning::FindTopicsNumber()`:
```r
result <- FindTopicsNumber(
  dtm,
  topics = seq(2, 15, 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1234)
)
FindTopicsNumber_plot(result)
```

---

## 🎨 Visualization & Labeling

### Top Terms per Topic
```r
library(tidytext)
library(ggplot2)

terms <- tidy(lda9, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10)
```

### Plot
Each facet displays top 10 terms per topic with their β-values.
```r
ggplot(terms, aes(beta, term, fill = factor(topic))) +
  geom_col() + facet_wrap(~ topic)
```

---

## 🔗 Topic Relationship Analysis

### Topic Similarity Heatmap
Compute cosine similarity between topics:
```r
similarity_matrix <- text2vec::sim2(posterior(k)$terms, method = "cosine")
corrplot(similarity_matrix, method = "color")
```

### Topic Network Graph
```r
g <- igraph::graph_from_adjacency_matrix(similarity_matrix, mode = "undirected", weighted = TRUE)
plot(g)
```

### Intertopic Distance Map (MDS)
```r
mds <- cmdscale(1 - similarity_matrix, k = 2)
ggplot(...)
```

---

## 📊 Document–Topic Exploration

Extract most representative documents per topic to interpret thematic meaning.
```r
doc_topics <- posterior(lda9)$topics
top_docs[[topic]] <- order(doc_topics[, topic], decreasing = TRUE)[1:3]
```

---

## 📚 Bibliometric Analysis
Launch **Biblioshiny** for interactive bibliometric exploration:
```r
library(bibliometrix)
biblioshiny()
```

---

## ⚙️ Installation
Install required R packages:
```r
install.packages(c(
  'readtext','quanteda','textstem','tm','topicmodels','ldatuning',
  'tidytext','ggplot2','text2vec','igraph','corrplot','bibliometrix'
))
```

---

## 💾 Outputs
| File | Description |
|------|--------------|
| `topics.csv` | Top words per topic |
| `Top_10_Terms_Per_Topic.csv` | β-values for top terms |
| `griffith2004.csv` | Topic evaluation metrics |
| `topic_network.png` | Network visualization of topic similarity |
| `mds_intertopic_map.png` | Intertopic distance visualization |

---

## 🔍 Notes & Recommendations
- Validate topic interpretability using top terms **and** representative documents.
- Use `dfm_trim()` to reduce sparsity and speed up modeling.
- Save models via `saveRDS()` for reproducibility.
- Ensure consistent random seeds (`seed = 1234`).

---

## 📄 License
Released under the **MIT License**.

---

## ✍️ Citation
If you use this workflow, please cite:
> Sahoo, T. (2025). *Topic Modeling & Bibliometric Analysis Pipeline in R.* GitHub Repository.

---

### 🧩 Future Extensions
- Add coherence scoring (via `textmineR` or `ldatuning`).
- Build a Shiny dashboard for interactive topic inspection.
- Integrate dynamic topic modeling (DTM) for temporal trend analysis.

---

**Author:** Tushar Sahoo  
**Language:** R (≥ 4.2)  
**Last Updated:** October 2025

