# 1.1 Packages loading and directory set

engineon <- function() {
  library("base")
  library("BBmisc")
  library("dplyr")
  library("ggforce")
  library("ggplot2")
  library("ggraph")
  library("ggthemes")
  library("igraph")
  library("network")
  library("ggalt")
  library("styler")
  library("purrr")
  library("qgraph")
  library("readr")
  library("stats")
  library("tibble")
  library("tidyr")
  library("tidyselect")
  library("tidyverse")
  library("wesanderson")
  library("ggrepel")
  library("readxl")
  library("readr")
  library("stats")
}

engineon()

theme <- theme_clean()

setwd("~/Google Drive/3. Archives/2017.9. - University/SoBigData Thesis/Report/R/Elaborazione dati")

# 1.2 Range01

range01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


# 2.1 Dataset load and first row factorization
dataset <- read.csv("dataset.csv", sep = ";")
dataset[, 1] <- as.character(dataset[, 1])

dataset$CollProjCoNorm <-
  BBmisc::normalize(dataset$CollaborationProjectCoordinators)
dataset$CollProjManNorm <-
  BBmisc::normalize(dataset$CollaborationProjectManager)
dataset$CollIndWPLNrom <-
  BBmisc::normalize(dataset$CollaborationIndividualWPLeaders)
dataset$CollTaskLNorm <-
  BBmisc::normalize(dataset$Individual.Task.Leaders)

dataset$IntConvNorm <-
  BBmisc::normalize(dataset$InternalConversation)
dataset$ExtConvNorm <-
  BBmisc::normalize(dataset$ExternalConversation)
dataset$CodNorm <- BBmisc::normalize(dataset$Codification)
dataset$SolNorm <- BBmisc::normalize(dataset$SolutionCreation)
dataset$RoutNorm <- BBmisc::normalize(dataset$Routines)

# 2.2 Competences load
competences <- read_excel(
  "Analisi Competenze.xlsx",
  col_types = c("text", "numeric", "numeric"),
  skip = 1
)

# 2.3 Dataset and competences join
dataset_join <-
  inner_join(dataset, competences, by = c("Sigla" = "Istituzione"))

# 3. Language categories creation and normalization
dataset_join <- dataset_join %>%
  mutate(
    language = case_when(
      Max == Min &
        Max <= median(Max, na.rm = TRUE) ~ "One Common Language",
      Max == Min &
        Max > median(Max, na.rm = TRUE) ~ "One Rare Language",
      Max != Min &
        Max > median(Max, na.rm = TRUE) ~ "Multilingual - Rare",
      Max != Min &
        Max <= median(Max, na.rm = TRUE) ~ "Multilingual - Common"
    )
  )

dataset_join$MaxNorm <- BBmisc::normalize(dataset_join$Max)
dataset_join$MinNorm <- BBmisc::normalize(dataset_join$Min)

levels(dataset_join$language) <-
  c(
    "Multilingual - Common",
    "One Common Language",
    "Multilingual - Rare",
    "One Rare Language"
  )

# 4.1 perceived network generation
# 4.1.1 Missing Matrix
missing <- matrix(1, nrow = 4, ncol = 31)
missingnames <- c("ENSL", "PSE", "USFD", "UvA")
rownames(missing) <- missingnames

# 4.1.2 Existing Matrix
initial <- c()
initial <- dataset[1]
initial <- as.data.frame(initial, stringsAsFactors = TRUE)
initial[2:32] <- dataset[23:53]
matrix <- as.matrix(initial[, 2:32])
row.names(matrix) <- initial[, 1]

# 4.1.3 Final Matrix
finalmatrix <- rbind(missing, matrix)
finalmatrix <-
  finalmatrix[order(row.names(finalmatrix)), order(colnames(finalmatrix))]

# 4.1.4 Network Generation
networkp <-
  graph_from_adjacency_matrix(
    finalmatrix,
    weighted = TRUE,
    diag = FALSE,
    mode = c("directed")
  )

# 4.1.5Graph Strength (also nomralized)
graph_strength_p <- graph.strength(networkp)
graph_strength_perceived <- as.data.frame(graph_strength_p)
graph_strength_perceived$graph_strength_p <-
  range01(graph_strength_perceived$graph_strength_p) # normalization

# 4.2.1 Edgelist upload
edgelist <- read.csv("edgelist.csv", header = TRUE, sep = ";")

# 4.2.2 Graph From Data Frame
networkr <-
  graph_from_data_frame(edgelist[edgelist$weight > 0, ], directed = FALSE)

# 4.2.3 Graph Strength (also normalized)
graph_strength_r <- graph.strength(networkr)
graph_strength_real <- as.data.frame(graph_strength_r)
graph_strength_real$graph_strength_r <-
  range01(graph_strength_real$graph_strength_r) # normalization

# 4.3. Graph Strength Union
graph_strength_real$Sigla <- row.names(graph_strength_real)
graph_strength_perceived$Sigla <-
  row.names(graph_strength_perceived)
graph_strength <-
  full_join(graph_strength_real, graph_strength_perceived, by = "Sigla")

# 4.3.1 UAQ correction
graph_strength[31, 1] <- 0

# 4.4 Dataset and Graph Strength Union
dataset_join <-
  merge(graph_strength, dataset_join, by = "Sigla", all = TRUE)


# 4.5.1 Perceived centrality visualization [Chart 1]
ggplot(
  graph_strength_perceived,
  aes(x = graph_strength_p, y = reorder(
    row.names(graph_strength_perceived), graph_strength_p
  ))
) +
  geom_col() +
  theme_minimal()

# 4.5.2 Real Centrality visualization [Chart 2]
ggplot(graph_strength_real, aes(x = graph_strength_r, y = reorder(
  row.names(graph_strength_real), graph_strength_r
))) +
  geom_col() +
  theme_minimal()

# 4.5.3 Matrix visualization [Chart 3]
ggplot(dataset_join, aes(x = graph_strength_r, y = graph_strength_p)) +
  geom_jitter() +
  theme_minimal() +
  # geom_label_repel(aes(label = Sigla)) +
  geom_vline(
    xintercept = median(dataset_join$graph_strength_r),
    alpha = 0.3
  ) +
  geom_hline(
    yintercept = median(dataset_join$graph_strength_p),
    alpha = 0.3
  ) + xlab("Formal Centrality") + ylab("Perceived Centrality")

# 4.6 Bcentrality creation and factorization with levels
dataset_join <- dataset_join %>%
  mutate(
    Bcategory =
      case_when(
        dataset_join$graph_strength_p > median(dataset_join$graph_strength_p) &
          dataset_join$graph_strength_r > median(dataset_join$graph_strength_r) ~ "4",
        dataset_join$graph_strength_p <= median(dataset_join$graph_strength_p) &
          dataset_join$graph_strength_r > median(dataset_join$graph_strength_r) ~ "2",
        dataset_join$graph_strength_p > median(dataset_join$graph_strength_p) &
          dataset_join$graph_strength_r <= median(dataset_join$graph_strength_r) ~ "3",
        dataset_join$graph_strength_p <= median(dataset_join$graph_strength_p) &
          dataset_join$graph_strength_r <= median(dataset_join$graph_strength_r) ~ "1"
      )
  )

dataset_join$Bcategory <-
  factor(dataset_join$Bcategory,
    levels = c("1", "2", "3", "4")
  )

# 5.0 Languages visualization

ggplot(dataset_join, aes(x = Max, y = Min)) +
  geom_jitter() +
  geom_abline(slope = 1) +
  geom_label_repel(label = Sigla) +
  ylab("Knowledge Management Process Stimulated, normalized") +
  ggtitle("Nonaka's Knowledge Management Process, per Typology of Institution") +
  labs(fill = "Typlogy of Institution") +
  geom_hline(yintercept = 0, col = "black") +
  theme(strip.background = element_rect(fill = "black")) +
  theme(strip.text.x = element_text(size = 10))

# 6. Nonaka Separato

# 6.1 Preparazione Dataset
NonakaSep <- dataset[68:72]
NonakaSep <- NonakaSep[, -2]
NonakaSep$activity <- dataset$activity
NonakaSep$activity <- as.factor(NonakaSep$activity)
NonakaSep <- gather(NonakaSep, key, value, -activity)
NonakaSep <- NonakaSep %>%
  drop_na() %>%
  group_by(key, activity) %>%
  summarise(val = mean(value))

# 6.2 Sostituzione etichette
variable_names <- list(
  "CodNorm" = "Codification - Externalization",
  "IntConvNorm" = "Conversations - Socialization",
  "RoutNorm" = "Routines Creation - Internalization",
  "SolNorm" = "Solution Creation - Combination"
)

variable_labeller <- function(variable, value) {
  return(variable_names[value])
}

# 6.3 Visualizzazione
ggplot(NonakaSep, aes(x = activity, y = val, fill = activity)) +
  geom_col(size = .7, width = 0.6) +
  facet_wrap(vars(key),
    labeller = variable_labeller,
    nrow = 2,
    ncol = 2
  ) +
  scale_x_discrete(labels = "") +
  theme_light() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  ylab("Knowledge Management Process Stimulated, normalized") +
  ggtitle("Nonaka's Knowledge Management Process, per Typology of Institution") +
  labs(fill = "Typlogy of Institution") +
  geom_hline(yintercept = 0, col = "black") +
  theme(strip.background = element_rect(
    fill =
      "black"
  )) +
  theme(strip.text.x = element_text(size = 10))  +
  scale_fill_brewer(
    palette =
      "PuRd"
  )

# 7.0 Nonaka Intero

# 7.1 Preparazione database
NonakaInt <- dataset %>%
  select(IntConvNorm:RoutNorm)
NonakaInt <- NonakaInt[, -2]
NonakaInt <- gather(NonakaInt, key, value)
NonakaInt$key <- as.factor(NonakaInt$key)
NonakaInt <- NonakaInt %>%
  group_by(key) %>%
  summarize(mean = mean(value, na.rm = TRUE))

# 7.2 Visualizzazione
ggplot(NonakaInt, aes(x = key, y = mean)) +
  geom_col(width = 0.9) +
  theme_light() +
  ggtitle("Nonaka's Knowledge Management Process, Aggregated") +
  ylab("Knowledge Management Process stimulated, aggregated") +
  xlab("Knowledge Management Process") +
  geom_hline(yintercept = 0, col = "black") 

# 8.0 collaborationSep divided

# 8.1 Dataset Preparation
collaborationSep <- dataset %>%
  select(CollProjCoNorm:CollTaskLNorm)
collaborationSep$activity <- as.factor(dataset$activity)
collaborationSep <- gather(collaborationSep, key, value, -activity)
collaborationSep$key <- as.factor(collaborationSep$key)
collaborationSep <- collaborationSep %>%
  group_by(key, activity) %>%
  summarise(x = mean(value, na.rm = FALSE))

levels(collaborationSep$key) <-
  c(
    "Individual WP leader",
    "Project Coordinator",
    "Project Manager",
    "Individual Task Leader"
  )

# 8.2 Visualization
ggplot(collaborationSep, aes(x = activity, y = x, fill = activity)) +
  geom_col(width = 0.7) +
  facet_wrap(. ~ key, ncol = 2, nrow = 2) +
  ylab("Average Likelihood of Frequent collaborationSep") +
  xlab("") +
  ggtitle("collaborationSep with WP leaders, Project Coordinators and Project Managers") +
  labs(size = "Percentage of Human\n Resources Deployed") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(fill = "Typlogy of Institution") +
  geom_hline(yintercept = 0, col = "black")

# 9,0 Collaboration cumulative

# 9.1 Dataset preparation
collaborationCum <- select(dataset, CollProjCoNorm:CollTaskLNorm)
collaborationCum <- gather(collaborationCum, key, value)
collaborationCum$key <- as.factor(collaborationCum$key)
collaborationCum <- collaborationCum %>%
  group_by(key) %>%
  summarise(x = mean(value, na.rm = TRUE))

# 9.2 Visualization
ggplot(collaborationCum, aes(x = key, y = x)) +
  geom_col(width = 0.5, ) +
  theme_clean() +
  ggtitle("Average collaborationSep with WP leaders, Project Coordinators and Project Managers") +
  geom_hline(yintercept = 0, col = "black")

# 10. Languages

# 10.1 Scatterplots
ggplot(dataset_join, aes(x = Max, y = Min)) +
  theme +
  geom_jitter(size = 2, alpha = 0.8) +
  geom_abline(slope = 1, alpha = 0.3) +
  geom_vline(
    xintercept = mean(dataset_join$Max, na.rm = TRUE),
    alpha = 0.3
  ) +
  scale_color_brewer(palette = "Dark2")

ggplot(dataset_join, aes(x = Max, y = Min, col = language)) +
  theme +
  geom_jitter(size = 2, alpha = 0.8) +
  geom_abline(slope = 1, alpha = 0.3) +
  geom_vline(
    xintercept = mean(dataset_join$Max, na.rm = TRUE),
    alpha = 0.3
  ) +
  scale_color_brewer(palette = "Dark2")

# 10.2.1 Extension plot
ggplot(dataset_join, aes(
  x = Min,
  xend = Max,
  y = reorder(Sigla, Max),
  shape = language
)) +
  geom_dumbbell(
    color = "#e3e2e1",
    colour_x = "#5b8124",
    size = 3,
    colour_xend = "#0e668b"
  ) +
  labs(
    x = "Rarità Massima e Minima delle competenze possedute dalla istituzione", y =
      "Istitution"
  ) +
  theme +
  geom_vline(xintercept = mean(dataset_join$Max), alpha = 0.3)

# 11 Composite Results

# 11.1 Nonaka and Centrality

# 11.1.1 Nonaka and Centrality Separato

# 11.1.1.1 Dataset preparation
NonCenSep <- dataset_join[70:74]
NonCenSep <- NonCenSep[, -2]
NonCenSep$Bcategory <- dataset_join$Bcategory

NonCenSep <- NonCenSep %>%
  gather(key, value, -Bcategory) %>%
  drop_na() %>%
  group_by(Bcategory, key) %>%
  summarize(val = mean(value, na.rm = TRUE))

# 11.1.1.2 Visualization
ggplot(NonCenSep, aes(x = Bcategory, y = val, fill = Bcategory)) +
  geom_col(size = .7, width = 0.6) +
  facet_wrap(vars(key),
    labeller = variable_labeller,
    nrow = 2,
    ncol = 2
  ) +
  scale_x_discrete(labels = "") +
  theme_light() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  ylab("Knowledge Management Process Stimulated, normalized") +
  ggtitle("Nonaka's Knowledge Management Process, per Typology of Institution") +
  labs(fill = "Typlogy of Institution") +
  geom_hline(yintercept = 0, col = "black") +
  theme(strip.background = element_rect(
    fill =
      "black"
  )) +
  theme(strip.text.x = element_text(size = 12)) +
  scale_fill_brewer(
    palette =
      "Blues"
  )

# 11.1.2 Nonaka and Centrality Cumulativo

# 11.1.2.1 Dataset preparation
datasetNonakaSComp <- NonCenSep %>%
  group_by(Bcategory) %>%
  summarize(mean = mean(val, na.rm = TRUE))

# 11.1.2.2 Visualization
ggplot(datasetNonakaSComp, aes(x = Bcategory, y = mean)) +
  geom_col(fill = "#003366") +
  theme_light() +
  ylab("Knowledge Management Process Stimulated, from 1 to 5") +
  ggtitle("Nonaka's Knowledge Management Process") +
  labs(fill = "Typlogy of Institution") +
  geom_hline(yintercept = 0, col = "black") +
  theme(strip.background = element_rect(
    fill =
      "black"
  ))

# 11.2 Languages and Centrality

# 11.2.1 Database creation

LanCen <- dataset_join %>%
  select(language, Bcategory) %>%
  drop_na() %>%
  group_by(language, Bcategory) %>%
  summarize(number = n())

# 11.2.2 Plots

ggplot(LanCen, aes(y = number, x = language, fill = Bcategory)) +
  geom_col(position = "dodge") +
  theme +
  scale_fill_brewer(
    palette =
      "Oranges"
  ) + ylab ("Number of Node") + xlab("Language category") + labs(fill="Centrality \ncategory")

ggplot(LanCen, aes(y = number, x = language, fill = Bcategory)) +
  geom_col(position = "fill") +
  theme +
  scale_fill_brewer(
    palette =
      "Oranges"
  )+ ylab ("Relative nubmber of Node") + xlab("Language category") + labs(fill="Centrality \ncategory") 

# 11.3 Linguaggi e Nonaka

# 11.3.1 Database creation

NonLan <- dataset_join %>%
  select(language, CodNorm, IntConvNorm, SolNorm, RoutNorm) %>%
  drop_na() %>%
  gather(key, value, -language, ) %>%
  group_by(language, key) %>%
  summarize(value = mean(value, na.rm = TRUE))

# 11.3.2 Plot

ggplot(NonLan, aes(x = language, y = value, fill = language)) +
  geom_col(size = .7, width = 0.6, ) +
  facet_wrap(vars(key),
    labeller = variable_labeller,
    nrow = 2,
    ncol = 2
  ) +
  theme_clean() +
  ylab("Distance from the average") +
  xlab("") +
  ggtitle("Knowlede Process created, by language category") +
  geom_hline(yintercept = 0, col = "black") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  scale_fill_brewer(
    palette =
      "Accent"
  )
