
############################################

######Load the packages and import data##################################

#############################################
require(readtext)
require(quanteda)
require(textstem)
require(dplyr)
require(tm)

x=readtext(file.choose())

x

names(x)
x$data <- paste(x$Title, x$Abstract,x$`Author Keywords`)

#x$data <- paste(scopus_data$Title, scopus_data$Abstract,scopus_data$`Author Keywords`)
xx=tolower(x$data)
xx
#============Package: TEXTSTEM============
#nstall.packages("textstem")
library(textstem)

xx=lemmatize_strings(xx)

xx


#============Package: TM============
xx=tm::removeNumbers(xx)
xx=tm::removePunctuation(xx)
xx=tm::removeWords(xx,stopwords('en'))
xx=stripWhitespace(xx)
xx
#============Package: QUANTEDA============
c=corpus(xx)
summary(c)

c=lemmatize_strings(c)
t <- quanteda::tokens(
  c,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE,
  remove_url = TRUE,
  remove_separators = TRUE
)
t=tokens_remove(t, stopwords('english'))

t <- tokens_keep(t, min_nchar = 2)
t

extra <- c("use", "study", "research", "analysis", "model", "strategy", "user", 
           "much", "can", "result", "post", "base", "find", "good", "platform", 
           "impact", "influence", "provide", "approach", "paper", "author", "examine", 
           "increase", "method", "also", "role", "propose", "analyze", "value", "make", 
           "identify", "high", "effect", "show", "relationship",  "factor", "performance", 
           "offer", "new", "text", "generate", "trend", "focus", "understand", "enhance", "relate",
           "article", "process", "significant", "word", "interaction", "time", "application", "various", 
           "implication", "purpose", "aim", "explore", "improve", "need", "food", "challenge", "numb", 
           "effective", "methodology", "campaign", "suggest", "review", "investigate", 
           "include", "however", "emerge", "collect", "key", "limit", "comparison", 
           "analytics", "affect", "create", "development", "level", "practice", "algorithm", 
           "framework", "right", "human", "effectiveness", "future", "context", "perspective",
           "positive", "help", "publish", "deep", "large", "conduct", "one", "sample",
           "potential", "different", "literature", "appeal", "two", "significantly", 
           "far", "test", "bank", "among", "survey", "compare", "theory", "important", 
           "lead", "across", "change", "become", "reveal", "highlight", "term", "enable",
           "current", "practical", "field", "topic", "achieve", "contribute", "reserve", 
           "utilize", "concern", "type", "regression", "indicate", "individual",
           "tourist", "manager", "researcher", "year", "concept", "within", "via", 
           "source", "generation", "first", "ltd", "form", "way", "relevant",
           "towards", "positively", "will", "real", "mouth", "smes", "mine",
           "keyword", "may", "three", "predict", "dynamic", "long", "scale", 
           "little", "apply", "discuss", "dataset", "travel", "gap", "work",
           "characteristic", "restaurant", "questionnaire", "specific", "benefit",
           "address", "sector", "comprehensive", "support", "main", "opinion", "tourism",
           "reach", "rate", "score", "assess", "moderate", "structural", "equation", 
           "exist", "sentence", "problem", "association", "importance", "analyse", "theoretical",
           "associate", "guide", "area", "recent", "advance", "take", "evaluate", "student", "self",
           "video", "task", "set", "start", "consider", "whether", "outcome", "accuracy", "low",
           "select", "order", "direct", "covid", "quantitative", "mediate", "state", "know", 
           "health", "environmental", "luxury", "component", "global", "introduce", "mean",
           "leverage", "five", "several", "particularly", "limitation", "publication", "
           inform", "preference", "involve", "elsevier", "account", "negative", "cigarette",
           "congruence", "empirical", "foster", "total", "sem", "success", "valuable", 
           "professional", "page", "cluster", "get", "obtain", "hypothesis", "emerald", 
           "metric", "play", "efficiency", "variable", "determine", "small", "due", 
           "combine", "graph", "grow", "science", "detection", "available", "specifically", 
           "amount", "era", "vs", "mobile", "classify", "university", "question", "tiktok", 
           "conceptual", "evolve", "shape", "addition", "classification", "educational", "draw", 
           "many", "photo", "ability", "copyright", "effort", "allow", "aspect", "statistical", 
           "ethical", "range", "underscore", "line", "website", "emphasize", "segmentation", "world",
           "power", "regard", "additionally", "organizational", "english", "proposition", "issue", "retailer", 
           "hashtag", "gamble", "engine", "previous", "category", "smma", "effectively", "case", "cost", "augment", 
           "extract", "springer", "moreover", "stakeholder", "mix", "increasingly", "objective", "capability", "furthermore",
           "extend", "direction", "implement", "give", "smmas", "perform", "short", "prediction", "click", "neural", 
           "successful", "confirm", "orient", "risk", "open", "critical", "conversion", "part", "prompt", "great", "link",
           "ensure", "participant", "remain", "sequential", "conclusion", "overall", "significance", "combination", 
           "multi", "index", "waste", "buy", "revenue", "saudi", "evidence", "bb", "email", "connection", "domain", 
           "orientation", "stimulus", "relation", "establish", "reduce", "multiple", "subject", "maintain", "gpt",
           "pandemic", "demand", "accord", "conclude", "still", "contribution", "attract", "co", "bibliometric", "uk", 
           "contain", "identification", "contextual", "position", "view", "public", "lack", "carry", "indicator", 
           "experimental", "warmth", "site", "маркетингу", "celebrity", "vaping", "bc", "china", "unique", "age", 
           "cross", "outperform", "cognitive", "match", "serve", "seek", "incorporate", "pre", "train", "gain", 
           "computer", "vision", "reflect", "modern", "sport", "corporate", "creation", "although", "volume", 
           "qualitative", "interview", "six", "convenience", "tailor", "journal", "four", "analytic", "pl",
           "non", "therefore", "rise", "interpret", "partial", "square", "day", "assist")



t=tokens_remove(t,extra)
t
d1=dfm(t)

topfeatures(d1,200)



#=====================================

# gsub function = Find & Replace
# Preprocessing: Replace words using a pattern
t1 <- gsub("\\b(social|medium)\\b", "social_media", t, perl = TRUE)
t1 <- gsub("\\b(artificial|intelligence)\\b", "artificial_intelligence", t1, perl = TRUE)
t1 <- gsub("\\b(ad|advatise)\\b", "advatise", t1, perl = TRUE)
t1 <- gsub("\\b(big|data)\\b", "bigdata", t1, perl = TRUE)
t1 <- gsub("\\b(machine|learn)\\b", "machine_learning", t1, perl = TRUE)
t1 <- gsub("\\be\\b", "electronic", t1, perl = TRUE)
t1 <- gsub("\\bai\\b", "artificial_intelligence", t1, perl = TRUE)
t1 <- gsub("\\bconsum\\b", "consumer", t1, perl = TRUE)
t1 <- gsub("\\bdigit\\b", "digital", t1, perl = TRUE)
t1 <- gsub("\\b(consumer|customer)\\b", "consumer", t1, perl = TRUE)
t1 <- gsub("\\bml\\b", "machine_learning", t1, perl = TRUE)

### Remove unwanted words ######
t1=quanteda::tokens(t1)
t1
d1=dfm(t1)
topfeatures(d1,150)

t1=quanteda::tokens(t1)
# Create the document-feature matrix
d1=dfm(t1)
d1
# Display top features
top_feat <- topfeatures(d1, 500)
top_feat
write.csv(top_feat, "remove.csv")

y<-featnames(d1)

y
write.csv(y, "final words to remove.csv")

### Topic Modeling ####

d2 <- convert(d1, to = "topicmodels")


library(topicmodels)
lda2= LDA(d2,k = 2, method = "Gibbs", control = list(seed = 1234))
lda3= LDA(d2,k = 3, method = "Gibbs", control = list(seed = 1234))
lda4= LDA(d2,k = 4, method = "Gibbs", control = list(seed = 1234))
lda5= LDA(d2,k = 5, method = "Gibbs", control = list(seed = 1234))
lda6= LDA(d2,k = 6, method = "Gibbs", control = list(seed = 1234))
lda7= LDA(d2,k = 7, method = "Gibbs", control = list(seed = 1234))
lda8= LDA(d2,k = 8, method = "Gibbs", control = list(seed = 1234))
lda9= LDA(d2,k = 9,method = "Gibbs", control = list(seed = 1234))
lda10= LDA(d2,k = 10, method = "Gibbs", control = list(seed = 1234))
lda11= LDA(d2,k = 11, method = "Gibbs", control = list(seed = 1234))
lda12= LDA(d2,k = 12, method = "Gibbs", control = list(seed = 1234))
lda13= LDA(d2,k = 13, method = "Gibbs", control = list(seed = 1234))
lda14= LDA(d2,k = 14, method = "Gibbs", control = list(seed = 1234))
lda15= LDA(d2,k = 15, method = "Gibbs", control = list(seed = 1234))


perplexity(lda2,newdata = d2)
perplexity(lda3, newdata = d2)
perplexity(lda4, newdata = d2)
perplexity(lda5, newdata = d2)
perplexity(lda6, newdata = d2)
perplexity(lda7, newdata = d2)
perplexity(lda8, newdata = d2)
perplexity(lda9, newdata = d2)
perplexity(lda10, newdata = d2)
perplexity(lda11, newdata = d2)
perplexity(lda12, newdata = d2)
perplexity(lda13, newdata = d2)
perplexity(lda14,newdata = d2)
perplexity(lda15,newdata = d2)


ldatop=data.frame(perplexity(lda2,newdata = d2),
                  perplexity(lda3, newdata = d2),
                  perplexity(lda4,newdata = d2),
                  perplexity(lda5,newdata = d2),
                  perplexity(lda6,newdata = d2),
                  perplexity(lda7,newdata = d2),
                  perplexity(lda8,newdata = d2),
                  perplexity(lda9,newdata = d2),
                  perplexity(lda10,newdata = d2),
                  perplexity(lda11,newdata = d2),
                  perplexity(lda12,newdata = d2),
                  perplexity(lda13,newdata = d2),
                  perplexity(lda14,newdata = d2),
                  perplexity(lda15,newdata = d2))

t(ldatop)
ldatop
min(ldatop)
plot(2:15, t(ldatop),
     type = 'b', lwd = 3, col = 1,
     las = 1,
     xlab = "Number of Topics",
     ylab = "Perplexity Score")


#####
#  Multiple way to find topics 
####
library(ldatuning)
library(topicmodels)
dtm <- convert(d1, to = "topicmodels")

result <- FindTopicsNumber(
  dtm,
  topics = seq(2, 15, by = 1),  # Try k = 2 to 10
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",  # Faster; alternative is "VEM"
  control = list(seed = 1234),
  mc.cores = 1L,     # Use >1 if you want faster processing (parallel)
  verbose = TRUE)
result
write.csv(result, "griffth2004.csv")
FindTopicsNumber_plot(result)
#############################################################
#


################################################

#### lda topic #####

#######################################
dtm <- convert(d1, to = "topicmodels")

k =LDA(dtm, k = 11, method = "Gibbs", control = list(seed = 1234))
k

c<-get_terms(k,10)
c
write.csv(c, "topics.csv")

########################################
#=======================================================
#===========================================================
#======= Visualization of Unique Words in each topic========
#=============================================================
#================================================================
# 

# Load required libraries
library(tidytext)
library(dplyr)
library(ggplot2)

# 1. Extract beta values (term-topic probabilities)
top_terms <- tidy(k, matrix = "beta")

# 2. Verify that the model contains exactly 11 topics
n_topics <- length(unique(top_terms$topic))

if (n_topics != 11) {
  stop(paste(
    "Expected 11 topics, but the model contains",
    n_topics, "topics. Please check your LDA model."
  ))
}

# 3. Select the top 10 terms for each topic
top_terms <- top_terms %>%
  group_by(topic) %>%
  slice_max(
    order_by = beta,
    n = 10,
    with_ties = FALSE
  ) %>%
  ungroup()

# 4. Assign final topic names in topic-number order
topic_labels <- c(
  "Social Media Trust",          # Topic 1
  "Market Analytics",            # Topic 2
  "AI Insights",                 # Topic 3
  "Marketing Communication",     # Topic 4
  "Brand Advertising",            # Topic 5
  "Personalized Engagement",     # Topic 6
  "Social Intelligence",         # Topic 7
  "AI-Enabled Marketing",        # Topic 8
  "Digital Relationships",       # Topic 9
  "AI Marketing Intelligence",  # Topic 10
  "Purchase Decision Behaviour"  # Topic 11
)

# 5. Map labels to the corresponding topic numbers
top_terms <- top_terms %>%
  mutate(
    topic_label = topic_labels[match(topic, sort(unique(topic)))]
  )

# 6. Define 11 distinct colours
topic_colours <- c(
  "#1F77B4",
  "#FF7F0E",
  "#2CA02C",
  "#D62728",
  "#9467BD",
  "#8C564B",
  "#E377C2",
  "#7F7F7F",
  "#BCBD22",
  "#17BECF",
  "#393B79"
)

names(topic_colours) <- as.character(sort(unique(top_terms$topic)))

# 7. Create the plot
p <- top_terms %>%
  mutate(
    term = tidytext::reorder_within(term, beta, topic_label),
    topic = factor(topic, levels = sort(unique(topic)))
  ) %>%
  ggplot(aes(x = beta, y = term, fill = topic)) +
  
  geom_col(
    width = 0.72,
    show.legend = FALSE
  ) +
  
  # Display beta values beside each bar
  geom_text(
    aes(label = sprintf("%.3f", beta)),
    hjust = -0.15,
    size = 3,
    colour = "black"
  ) +
  
  # Arrange 11 topics in a 3-column grid
  facet_wrap(
    ~ topic_label,
    scales = "free",
    ncol = 3
  ) +
  
  tidytext::scale_y_reordered() +
  
  scale_fill_manual(values = topic_colours) +
  
  # Allow space for beta-value labels
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.30))
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    axis.text.y = element_text(
      size = 9,
      face = "bold",
      colour = "black"
    ),
    axis.text.x = element_text(size = 9),
    axis.title = element_text(
      size = 11,
      face = "bold"
    ),
    strip.text = element_text(
      size = 11,
      face = "bold",
      colour = "black"
    ),
    panel.spacing = grid::unit(1, "lines"),
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5
    ),
    plot.margin = margin(12, 20, 12, 12)
  ) +
  
  labs(
    x = "Beta (Term Probability)",
    y = "Top Terms",
    title = "Top 10 Terms per Topic in the LDA Model"
  )

# 8. Display the plot
print(p)

# 9. Export the top terms and beta values to CSV
write.csv(
  top_terms,
  "Top_10_Terms_Per_Topic.csv",
  row.names = FALSE
)

# 10. Save the figure in high resolution
ggsave(
  filename = "Top_10_Terms_Per_Topic.png",
  plot = p,
  width = 15,
  height = 17,
  dpi = 300,
  bg = "white"
)

# 11. Save a PDF version for publication
ggsave(
  filename = "Top_10_Terms_Per_Topic.pdf",
  plot = p,
  width = 15,
  height = 17,
  device = "pdf",
  bg = "white"
)

#=========================================================
##########################################################
#####################Topic Correlation ############################
##########################################################
#==================================================
# ============================================================
# TOPIC SIMILARITY ANALYSIS (11-TOPIC LDA MODEL)
# ============================================================

# Load necessary libraries
library(topicmodels)
library(text2vec)
library(igraph)
library(corrplot)

# Step 1: Extract the topic-term probability matrix
# Rows = topics; columns = terms
topic_terms <- posterior(k)$terms

# Check the number of topics
n_topics <- nrow(topic_terms)

if (n_topics != 11) {
  stop(paste(
    "Expected 11 topics, but found",
    n_topics,
    "topics. Please check your LDA model."
  ))
}

# Step 2: Compute cosine similarity between topics
similarity_matrix <- text2vec::sim2(
  x = topic_terms,
  method = "cosine",
  norm = "l2"
)

# Convert similarity result to a standard matrix
similarity_matrix <- as.matrix(similarity_matrix)

# Step 3: Define the final topic labels
topic_labels <- c(
  "Social Media Trust",          # Topic 1
  "Market Analytics",            # Topic 2
  "AI Insights",                 # Topic 3
  "Marketing Communication",     # Topic 4
  "Brand Advertising",           # Topic 5
  "Personalized Engagement",     # Topic 6
  "Social Intelligence",         # Topic 7
  "AI-Enabled Marketing",        # Topic 8
  "Digital Relationships",       # Topic 9
  "AI Marketing Intelligence",   # Topic 10
  "Purchase Decision Behaviour"  # Topic 11
)

# Step 4: Assign topic labels to matrix rows and columns
rownames(similarity_matrix) <- topic_labels
colnames(similarity_matrix) <- topic_labels

# Step 5: Visualize cosine similarity using corrplot
corrplot(
  similarity_matrix,
  method = "color",
  type = "upper",
  order = "original",
  tl.col = "black",
  tl.cex = 0.8,
  tl.srt = 45,
  tl.font = 2,
  addCoef.col = "black",
  number.cex = 0.65,
  number.digits = 2,
  col = colorRampPalette(
    c("#2166AC", "white", "#B2182B")
  )(200),
  cl.pos = "r",
  title = "Topic Similarity (Cosine)",
  mar = c(0, 0, 3, 0)
)

# Step 6: Export similarity matrix to CSV
write.csv(
  similarity_matrix,
  "Topic_Similarity_Cosine_11_Topics.csv",
  row.names = TRUE
)
####################################
#==========================================
############### Topic Network Graph ##########################

#=====================================================
# ============================================================
# PART 1: TOPIC NETWORK GRAPH (11 TOPICS)
# ============================================================

library(igraph)
library(RColorBrewer)
library(topicmodels)

# Ensure similarity_matrix is the 11 x 11 cosine similarity matrix
if (!all(dim(similarity_matrix) == c(11, 11))) {
  stop("similarity_matrix must be an 11 x 11 matrix.")
}

# Final topic labels in model topic-number order
topic_labels <- c(
  "Social Media Trust",
  "Market Analytics",
  "AI Insights",
  "Marketing Communication",
  "Brand Advertising",
  "Personalized Engagement",
  "Social Intelligence",
  "AI-Enabled Marketing",
  "Digital Relationships",
  "AI Marketing Intelligence",
  "Purchase Decision Behaviour"
)

# Assign topic labels to the similarity matrix
rownames(similarity_matrix) <- topic_labels
colnames(similarity_matrix) <- topic_labels

# Filter weak links
similarity_network <- similarity_matrix
similarity_network[similarity_network < 0.10] <- 0

# Remove self-connections
diag(similarity_network) <- 0

# Build undirected weighted graph
g <- graph_from_adjacency_matrix(
  similarity_network,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

# Assign vertex labels
V(g)$label <- topic_labels

# Assign 11 distinct colours
topic_colors <- colorRampPalette(
  brewer.pal(12, "Set3")
)(11)

V(g)$color <- topic_colors

# Configure node size and appearance
V(g)$size <- 30
V(g)$label.color <- "black"
V(g)$label.cex <- 0.85
V(g)$label.font <- 2
V(g)$frame.color <- "grey40"

# Configure edge width based on similarity
E(g)$width <- 1 + E(g)$weight * 6
E(g)$color <- "grey65"

# Plot network graph
set.seed(123)

plot(
  g,
  layout = layout_with_fr(g),
  vertex.size = V(g)$size,
  vertex.color = V(g)$color,
  vertex.label = V(g)$label,
  vertex.label.color = V(g)$label.color,
  vertex.label.cex = V(g)$label.cex,
  vertex.label.font = V(g)$label.font,
  vertex.label.dist = 0,
  edge.width = E(g)$width,
  edge.color = E(g)$color,
  main = "Topic Network Graph (Cosine Similarity)"
)

# Export graph
png(
  "Topic_Network_Graph_11_Topics.png",
  width = 2400,
  height = 2000,
  res = 300
)

set.seed(123)

plot(
  g,
  layout = layout_with_fr(g),
  vertex.size = V(g)$size,
  vertex.color = V(g)$color,
  vertex.label = V(g)$label,
  vertex.label.color = "black",
  vertex.label.cex = 0.85,
  vertex.label.font = 2,
  edge.width = E(g)$width,
  edge.color = E(g)$color,
  main = "Topic Network Graph (Cosine Similarity)"
)

dev.off()


# ============================================================
# PART 2: IDENTIFY REPRESENTATIVE DOCUMENTS FOR EACH TOPIC
# ============================================================

# Extract document-topic probabilities
# k must be your fitted 11-topic LDA model
doc_topics <- posterior(k)$topics

# Check the number of topics
if (ncol(doc_topics) != 11) {
  stop("The fitted LDA model must contain exactly 11 topics.")
}

# Check document text length
# x$data must contain the original document text in model order
if (length(x$data) != nrow(doc_topics)) {
  stop("The number of documents in x$data does not match the LDA model.")
}

# Extract the top 3 representative documents per topic
top_docs <- vector("list", length = 11)

for (topic in seq_len(ncol(doc_topics))) {
  
  # Rank documents by topic probability
  sorted_docs <- order(
    doc_topics[, topic],
    decreasing = TRUE
  )
  
  # Select up to 3 documents
  selected_docs <- head(sorted_docs, 3)
  
  # Store document indices, probabilities, and text
  top_docs[[topic]] <- data.frame(
    Topic_Number = topic,
    Topic_Name = topic_labels[topic],
    Document = selected_docs,
    Probability = doc_topics[selected_docs, topic],
    Text = as.character(x$data[selected_docs]),
    stringsAsFactors = FALSE
  )
}

# Name list elements
names(top_docs) <- paste(
  "Topic", seq_along(topic_labels),
  topic_labels,
  sep = ": "
)

# Print representative documents
top_docs

# Combine all documents into one table
top_docs_table <- do.call(rbind, top_docs)

# Export results
write.csv(
  top_docs_table,
  "Top_3_Representative_Documents_11_Topics.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


############################################

###########MDS######################

##################################

###########################################################
# ============================================================
# INTERTOPIC DISTANCE MAP (MDS) — 11-TOPIC LDA MODEL
# ============================================================

# Install packages if required
# install.packages(c("topicmodels", "text2vec",
#                    "ggplot2", "ggrepel", "viridis"))

# Load required libraries
library(topicmodels)
library(text2vec)
library(ggplot2)
library(ggrepel)
library(viridis)

# Step 1: Extract topic-term probability matrix
# Rows = topics; columns = terms
topic_term_matrix <- posterior(k)$terms

# Step 2: Verify that the model contains 11 topics
if (nrow(topic_term_matrix) != 11) {
  stop("The fitted LDA model must contain exactly 11 topics.")
}

# Step 3: Calculate pairwise cosine similarity
topic_similarity <- as.matrix(
  text2vec::sim2(
    x = topic_term_matrix,
    method = "cosine",
    norm = "l2"
  )
)

# Step 4: Convert similarity into distance
topic_distance <- 1 - topic_similarity

# Ensure symmetry and remove small numerical discrepancies
topic_distance <- (topic_distance + t(topic_distance)) / 2
diag(topic_distance) <- 0

# Step 5: Apply multidimensional scaling
set.seed(123)

mds <- cmdscale(
  as.dist(topic_distance),
  k = 2,
  eig = TRUE
)

# Step 6: Define final topic labels
topic_labels <- c(
  "Social Media Trust",          # Topic 1
  "Market Analytics",            # Topic 2
  "AI Insights",                 # Topic 3
  "Marketing Communication",     # Topic 4
  "Brand Advertising",           # Topic 5
  "Personalized Engagement",     # Topic 6
  "Social Intelligence",         # Topic 7
  "AI-Enabled Marketing",        # Topic 8
  "Digital Relationships",       # Topic 9
  "AI Marketing Intelligence",   # Topic 10
  "Purchase Decision Behaviour"  # Topic 11
)

# Step 7: Prepare plotting dataframe
intertopic_df <- data.frame(
  Dim1 = mds$points[, 1],
  Dim2 = mds$points[, 2],
  Topic = factor(seq_len(11)),
  Label = topic_labels
)

# Step 8: Generate 11 distinct colours
topic_colors <- viridis(11, option = "D", end = 0.95)

# Step 9: Create the intertopic distance map
p <- ggplot(
  intertopic_df,
  aes(x = Dim1, y = Dim2, color = Topic)
) +
  geom_point(
    size = 7,
    alpha = 0.9
  ) +
  geom_text_repel(
    aes(label = Label),
    size = 4.2,
    fontface = "bold",
    color = "black",
    box.padding = 0.7,
    point.padding = 0.5,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  scale_color_manual(values = topic_colors) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(
      size = 17,
      face = "bold",
      hjust = 0.5
    ),
    axis.title = element_text(
      size = 12,
      face = "bold"
    ),
    axis.text = element_text(
      size = 10,
      color = "black"
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 25, 15, 25)
  ) +
  labs(
    title = "Intertopic Distance Map (MDS)",
    subtitle = "Cosine distance between 11 LDA topics",
    x = "Dimension 1",
    y = "Dimension 2"
  )

# Step 10: Display plot
print(p)

# Step 11: Export high-resolution PNG
ggsave(
  filename = "Intertopic_Distance_Map_11_Topics.png",
  plot = p,
  width = 14,
  height = 10,
  dpi = 300,
  bg = "white"
)

# Step 12: Export PDF for publication
ggsave(
  filename = "Intertopic_Distance_Map_11_Topics.pdf",
  plot = p,
  width = 14,
  height = 10,
  device = "pdf",
  bg = "white"
)

# Step 13: Export MDS coordinates
write.csv(
  intertopic_df,
  "Intertopic_Distance_MDS_Coordinates.csv",
  row.names = FALSE
)



##########################################################################

###########################################################################
###################################################################

library(bibliometrix)

biblioshiny()




# Install once if required
# install.packages(c("DiagrammeR", "DiagrammeRsvg", "rsvg"))

library(DiagrammeR)

diagram_code <- "
digraph Framework {

  graph [
    rankdir = TB,
    bgcolor = white,
    pad = 0.3,
    nodesep = 0.35,
    ranksep = 0.55,
    splines = ortho,
    labelloc = t,
    label = \"AI IN SOCIAL MEDIA MARKETING\",
    fontsize = 24,
    fontname = Helvetica
  ]

  node [
    shape = box,
    style = \"rounded,filled\",
    fontname = Helvetica,
    fontsize = 17,
    margin = \"0.22,0.13\",
    width = 3.8,
    height = 0.55,
    penwidth = 1.8,
    color = \"#64748B\",
    fontcolor = \"#172033\"
  ]

  edge [
    color = \"#475569\",
    penwidth = 2,
    arrowsize = 0.9
  ]

  # STAGE 1

  subgraph cluster_technology {
    label = \"1. TECHNOLOGY AND INTELLIGENCE\"
    fontsize = 17
    fontcolor = \"#1D4ED8\"
    color = \"#93C5FD\"
    style = \"rounded,filled\"
    fillcolor = \"#EFF6FF\"

    AI [label = \"AI-Driven Insights\", fillcolor = \"#DBEAFE\"]
    SI [label = \"Social Intelligence\", fillcolor = \"#DBEAFE\"]
    DT [label = \"Digital Transformation\", fillcolor = \"#BFDBFE\"]
    TN [label = \"Digital Trust Networks\", fillcolor = \"#E0E7FF\"]

    AI -> SI -> DT -> TN [style = invis]
  }

  # STAGE 2

  subgraph cluster_consumer {
    label = \"2. CONSUMER-BRAND INTERACTION\"
    fontsize = 17
    fontcolor = \"#9A3412\"
    color = \"#FDBA74\"
    style = \"rounded,filled\"
    fillcolor = \"#FFF7ED\"

    MC [label = \"Marketing Communications\", fillcolor = \"#FFEDD5\"]
    BE [label = \"Brand Engagement\", fillcolor = \"#FFEDD5\"]

    MC -> BE
  }

  # STAGE 3

  subgraph cluster_market {
    label = \"3. MARKET AND COMMERCE\"
    fontsize = 17
    fontcolor = \"#166534\"
    color = \"#86EFAC\"
    style = \"rounded,filled\"
    fillcolor = \"#F0FDF4\"

    KI [label = \"Knowledge & Innovation\", fillcolor = \"#DCFCE7\"]
    MD [label = \"Market Dynamics\", fillcolor = \"#DCFCE7\"]
    DC [label = \"Digital Commerce\", fillcolor = \"#BBF7D0\"]

    KI -> MD -> DC
  }

  # MAIN VERTICAL FLOW

  DT -> MC
  BE -> KI

  # THEORETICAL FOUNDATIONS

  subgraph cluster_theory {
    label = \"THEORETICAL FOUNDATIONS\"
    fontsize = 17
    fontcolor = \"#334155\"
    color = \"#CBD5E1\"
    style = \"rounded,filled\"
    fillcolor = \"#F8FAFC\"

    DOI [label = \"Diffusion of Innovation (DOI)\",
         fontsize = 14, fillcolor = \"#E2E8F0\"]

    TOE [label = \"Technology-Organization-Environment (TOE)\",
         fontsize = 14, fillcolor = \"#E2E8F0\"]

    RBV [label = \"Resource-Based View (RBV)\",
         fontsize = 14, fillcolor = \"#E2E8F0\"]

    { rank = same; DOI; TOE; RBV }
  }

  DC -> TOE [style = invis]
}
"

# Render diagram
framework_plot <- grViz(diagram_code)
framework_plot

# Export SVG and high-resolution PNG
library(DiagrammeRsvg)
library(rsvg)

svg_code <- export_svg(framework_plot)

writeLines(
  svg_code,
  "Vertical_AI_Social_Media_Framework.svg"
)

rsvg_png(
  charToRaw(svg_code),
  file = "Vertical_AI_Social_Media_Framework.png",
  width = 3000,
  height = 4500
)

# Output folder
getwd()








