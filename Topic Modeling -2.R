
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
t=quanteda::tokens(c,
                   remove_punct = T,
                   remove_symbols = T,
                   remove_numbers = T,
                   remove_url = T,
                   remove_separators = T)
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

k =LDA(dtm, k = 9, method = "Gibbs", control = list(seed = 1234))
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

# Load necessary libraries
library(tidytext)
library(dplyr)
library(ggplot2)
library(forcats)


# Extract the beta values (per-term-per-topic probabilities)
top_terms <- tidy(k, matrix = "beta")

# Get the top 10 terms per topic
top_terms <- top_terms %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() 
# Assign custom topic labels
topic_labels <- c(
  "Brand Engagement",
  "Market Dynamics",
  "Marketing Communications",
  "Knowledge & Innovation",
  "Social Intelligence",
  "AI-Driven Insights",
  "Digital Commerce",
  "Digital Trust Networks",
  "Digital Transformation"
)
    
# Map labels to topics
top_terms <- top_terms %>%
  mutate(topic_label = topic_labels[topic])

top_terms %>%
  mutate(term = reorder_within(term, beta, topic_label)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(width = 0.7, show.legend = FALSE) +  # wider bars
  geom_text(aes(label = round(beta, 3)), 
            hjust = -0.1, size = 3.5, color = "black") +  # value labels
  facet_wrap(~ topic_label, scales = "free", ncol = 3) +  # spread across grid
  scale_y_reordered() +
  scale_fill_manual(values = c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
  "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22"
)) +  # better contrasting colors
  theme_minimal(base_size = 16) + 
  theme(
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 16, face = "bold"),
    plot.margin = margin(10, 30, 10, 10)  # extra space for labels
  ) +
  labs(
    x = "Beta (Term Importance)",
    y = "Terms",
    title = "Top 10 Terms per Topic in LDA Model",
     ) +
  coord_cartesian(xlim = c(0, max(top_terms$beta) + 0.01))  # adjust space for labels


write.csv(top_terms, "Top_10_Terms_Per_Topic.csv", row.names = FALSE)





#=========================================================
##########################################################
#####################Topic Correlation ############################
##########################################################
#==================================================


# Load necessary libraries
library(text2vec)
library(igraph)
library(corrplot)

# Step 1: Compute topic-term matrix and cosine similarity
topic_terms <- posterior(k)$terms  # Rows = topics, Columns = terms
similarity_matrix <- sim2(topic_terms, method = "cosine")  # Cosine similarity

# Step 2: Define topic labels (short and descriptive)
topic_labels <- c(
  "Brand Engagement",
"Market Dynamics",
  "Marketing Communications",
   "Knowledge & Innovation",
   "Social Intelligence",
   "AI-Driven Insights",
  "Digital Commerce",
  "Digital Trust Networks",
  "Digital Transformation"
)
# Assign labels to similarity matrix
rownames(similarity_matrix) <- topic_labels
colnames(similarity_matrix) <- topic_labels

# Step 3: Visualize cosine similarity using corrplot (optional heatmap)
corrplot(similarity_matrix,
         method = "color",
         tl.col = "black",
         tl.cex = 1.2,
         tl.srt = 45,
         tl.font = 2,
         addCoef.col = "black",
         number.cex = 0.9,
         title = "Topic Similarity (Cosine)",
         mar = c(0, 0, 2, 0))


####################################
#==========================================
############### Topic Network Graph ##########################

#=====================================================


library(igraph)
library(RColorBrewer)

# Filter weak links
similarity_matrix[similarity_matrix < 0.1] <- 0  

# Build graph
g <- graph_from_adjacency_matrix(similarity_matrix, 
                                 mode = "undirected", 
                                 weighted = TRUE, 
                                 diag = FALSE)

# Assign labels
V(g)$label <- topic_labels  

# Assign unique colors (Set3 palette gives distinct bright colors)
topic_colors <- brewer.pal(n = length(topic_labels), name = "Set3")
V(g)$color <- topic_colors  

# Increase circle size
V(g)$size <- 20  

# Plot graph with improved readability
plot(g,
     layout = layout_with_fr,
     vertex.size = V(g)$size,
     vertex.color = V(g)$color,
     vertex.label.color = "black",
     vertex.label.cex = 1.5,      # large readable text
     vertex.label.dist = 2,       # pushes labels further from nodes
     edge.width = E(g)$weight * 5,
     main = "Topic Network Graph (Cosine Similarity)")





##################################
#####To find out the thought behind the topic #######
###########################

# Extract document-topic probabilities (gamma matrix)
doc_topics <- posterior(lda9)$topics  # Matrix: documents x topics

# Get top 3 documents per topic
top_docs <- list()
for (topic in 1:ncol(doc_topics)) {
  # Get documents sorted by topic probability (descending)
  sorted_docs <- order(doc_topics[, topic], decreasing = TRUE)
  # Store top 3 document indices and their content
  top_docs[[topic]] <- data.frame(
    Document = sorted_docs[1:3],
    Probability = doc_topics[sorted_docs[1:3], topic],
    Text = x$data[sorted_docs[1:3]]
  )
}

# Name topics according to your labels
topic_labels <- c(
  "AI Marketing",
  "Digital Promotion",
  "User Insight",
  "Technology Adoption",
  "Consumer Decision-Making",
  "Service Personalization",
  "Influencer Strategy",
  "Online Value Creation",
  "Customer Loyalty Programs"
)

names(top_docs) <- paste0("Topic ", 1:9, ": ", topic_labels)

# Print results
top_docs



############################################

###########MDS######################

##################################

###########################################################

# Install and load required packages
# install.packages(c("FactoMineR", "factoextra", "ggrepel", "RColorBrewer", "viridis"))
library(topicmodels)
library(text2vec)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(viridis)  # For additional color palettes

# Step 1: Extract topic-term matrix (beta)
topic_term_matrix <- posterior(k)$terms  # topics x terms

# Step 2: Compute pairwise cosine similarity between topics
topic_similarity <- sim2(topic_term_matrix, method = "cosine")

# Step 3: Convert similarity to distance
topic_distance <- 1 - topic_similarity

# Step 4: Apply MDS to project into 2D
mds <- cmdscale(topic_distance, k = 2)  # k = dimensions

# Step 5: Create dataframe for plotting
topic_labels <- c(
  "Brand Engagement",
  "Market Dynamics",
  "Marketing Communications",
  "Knowledge & Innovation",
  "Social Intelligence",
  "AI-Driven Insights",
  "Digital Commerce",
  "Digital Trust Networks",
  "Digital Transformation"
)

intertopic_df <- data.frame(
  Dim1 = mds[, 1],
  Dim2 = mds[, 2],
  Topic = factor(1:nrow(mds)),
  Label = topic_labels
)

# Step 6: Create a custom color palette with 9 distinct colors
# Option 1: Use a palette from the viridis package
topic_colors <- viridis(9)

# Option 2: Create a custom palette manually
# topic_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
#                  "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22")

# Step 7: Visualize with custom colors and no legend
ggplot(intertopic_df, aes(x = Dim1, y = Dim2, color = Topic)) +
  geom_point(size = 10, alpha = 0.9) +  # larger circles
  geom_text_repel(aes(label = Label, color = Topic),  
                  size = 6, fontface = "bold",  # larger text
                  box.padding = 0.6,
                  point.padding = 0.4,
                  max.overlaps = Inf,
                  show.legend = FALSE,
                  color = "black") +  # Set label text color to black
  scale_color_manual(values = topic_colors) +  # Use custom colors
  theme_minimal(base_size = 16) +  # slightly larger base font
  theme(legend.position = "none") +  
  labs(title = "Intertopic Distance Map (MDS)",
       x = "Dimension 1", y = "Dimension 2")

ggplot(intertopic_df, aes(x = Dim1, y = Dim2, color = Topic)) +
  geom_point(size = 10, alpha = 0.9) +  # larger circles
  geom_text_repel(aes(label = Label, color = Topic),  
                  size = 6, fontface = "bold",  # larger text
                  box.padding = 0.6,
                  point.padding = 0.4,
                  max.overlaps = Inf,
                  show.legend = FALSE,
                  color = "black") +  # Set label text color to black
  scale_color_manual(values = topic_colors) +  # Use custom colors
  theme_minimal(base_size = 16) +  # slightly larger base font
  theme(legend.position = "none") +  
  labs(title = "Intertopic Distance Map (MDS)",
       x = "Dimension 1", y = "Dimension 2") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title




##########################################################################

###########################################################################
###################################################################

library(bibliometrix)

biblioshiny()
