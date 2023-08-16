#########################################
# script to create figures summarizing output of the BERTopic model analyzing discourses on sustainable agriculture in German agricultural magazines and newspapers as well as scientific literature
# main author: Mariana Madruga de Brito, mariana.brito@ufz.de
# contact: Bartosz Bartkowski, bartosz.bartkowski@ufz.de 
#########################################

library(here)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(tidyr)

### RUN THIS CODE BEFORE THE TOPIC AND CLUSTER TREND ANALYSIS
data <- read.table(here("data_R.csv"), sep=",", header=TRUE, fill=TRUE)

magazines <- subset(data, data$corpus == "ag_magazines")
newspapers <- subset(data, data$corpus == "newspaper")
articles <- subset(data, data$corpus == "scientific_articles")

# Calculate the sum of Count for each year
year_sums_magazines <- magazines %>%
  group_by(year) %>%
  summarize(year_sum = sum(n))

year_sums_newspapers <- newspapers %>%
  group_by(year) %>%
  summarize(year_sum = sum(n))
#year_sums_newspapers <- rbind(year_sums_newspapers, data.frame(year = 1997, year_sum = 0))

year_sums_articles <- articles %>%
  group_by(year) %>%
  summarize(year_sum = sum(n))

#add 0 to missing years
# Create a sequence of years 
all_years_magazines <- 2000:2022

all_years_newspapers <- 1996:2021

all_years_articles <- 1992:2021

#########################################################################
##PLOTS BY TOPIC
# Expand the dataframe with missing years for each Label
magazines <- magazines %>%
  complete(Label, year = all_years_magazines, fill = list(n = 0)) %>%
  select(-Cluster,-corpus)

newspapers <- newspapers %>%
  complete(Label, year = all_years_newspapers, fill = list(n = 0)) %>%
  select(-Cluster,-corpus)

articles <- articles %>%
  complete(Label, year = all_years_articles, fill = list(n = 0)) %>%
  select(-Cluster,-corpus)

## NORMALIZE BY YEAR AND TOPIC
# Merge the year sums back into the original dataframe
magazines <- magazines %>%
  left_join(year_sums_magazines, by = "year") %>%
  mutate(percent = n / year_sum)

newspapers <- newspapers %>%
  left_join(year_sums_newspapers, by = "year") %>%
  mutate(percent = n / year_sum)
newspapers$percent[is.na(newspapers$percent)] <- 0

articles <- articles %>%
  left_join(year_sums_articles, by = "year") %>%
  mutate(percent = n / year_sum)

# Create scatterplot with facets for each topic

a <-ggplot(magazines, aes(x = year, y = percent)) +
  geom_point() +
  geom_smooth(method = lm, color="#BC6C25") +
  stat_poly_eq(use_label(c("R2","P")))  +
  scale_y_continuous(limits = c(0, NA)) +
  facet_wrap(~ Label, ncol = 3) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Magazines - topics")

ggsave("SM_magazines_topics.png", plot = a, width = 10, height = 8, units = "in")

b <- ggplot(newspapers, aes(x = year, y = percent)) +
  geom_point() +
  geom_smooth(method = lm, color="#BC6C25") +
  stat_poly_eq(use_label(c("R2","P"))) +
  scale_y_continuous(limits = c(0, NA)) +
  facet_wrap(~ Label, ncol = 4) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Newspapers - topics")

ggsave("SM_newspapers_topics.png", plot = b, width = 10, height = 12, units = "in")


c <- ggplot(articles, aes(x = year, y = percent)) +
  geom_point() +
  geom_smooth(method = lm, color="#BC6C25") +
  stat_poly_eq(use_label(c("R2","P"))) +
  facet_wrap(~ Label, ncol = 5) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Scientific articles - topics")

ggsave("SM_articles_topics.png", plot = c, width = 10, height = 12, units = "in")

######################################
## PLOTS BY CLUSTER (re-run initial code!!!)

# Expand the dataframe with missing years for each cLUSTER
magazines_cluster <- magazines %>%
  complete(Cluster, year = all_years_magazines, fill = list(n = 0))  %>%
  select(-Label,-corpus)

newspapers_cluster <- newspapers %>%
  complete(Cluster, year = all_years_newspapers, fill = list(n = 0)) %>%
  select(-Label,-corpus)

articles_cluster <- articles %>%
  complete(Cluster, year = all_years_articles, fill = list(n = 0)) %>%
  select(-Label,-corpus)

#aggregate by cluster

magazines_cluster <- magazines_cluster %>%
  group_by(Cluster, year) %>%
  summarise(total_n = sum(n))

newspapers_cluster <- newspapers_cluster %>%
  group_by(Cluster, year) %>%
  summarise(total_n = sum(n))

articles_cluster <- articles_cluster %>%
  group_by(Cluster, year) %>%
  summarise(total_n = sum(n))

# Merge the year sums back into the original dataframe
magazines_cluster <- magazines_cluster %>%
  left_join(year_sums_magazines, by = "year") %>%
  mutate(percent = total_n / year_sum)

newspapers_cluster <- newspapers_cluster %>%
  left_join(year_sums_newspapers, by = "year") %>%
  mutate(percent = total_n / year_sum)

articles_cluster <- articles_cluster %>%
  left_join(year_sums_articles, by = "year") %>%
  mutate(percent = total_n / year_sum)

# Create scatterplot with facets for each cluster
d <- ggplot(magazines_cluster, aes(x = year, y = percent)) +
  geom_point() +
  geom_smooth(method = lm, color="#BC6C25") +
  stat_poly_eq(use_label(c("R2","P"))) +
  facet_wrap(~ Cluster, ncol = 3) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Magazines - Clusters")

ggsave("SM_magazines_clusters.png", plot = d, width = 10, height = 8, units = "in")


e <- ggplot(newspapers_cluster, aes(x = year, y = percent)) +
  geom_point() +
  geom_smooth(method = lm, color="#BC6C25") +
  stat_poly_eq(use_label(c("R2","P"))) +
  scale_y_continuous(limits = c(0, NA)) +
  facet_wrap(~ Cluster, ncol = 4) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Newspapers - Clusters")

ggsave("SM_newspaper_clusters.png", plot = e, width = 10, height = 8, units = "in")


f <- ggplot(articles_cluster, aes(x = year, y = percent)) +
  geom_point() +
  geom_smooth(method = lm, color="#BC6C25") +
  stat_poly_eq(use_label(c("R2","P"))) +
  facet_wrap(~ Cluster, ncol = 4) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "Scientific articles - Clusters")

ggsave("SM_articles_clusters.png", plot = f, width = 10, height = 8, units = "in")

#### CALCULATE DIVERSITY
library(vegan)
library(gridExtra)

#for this we need the data without the 0 values, so I imported the data again

data <- read.table(here("data_R.csv"), sep=",", header=TRUE, fill=TRUE)

magazines <- subset(data, data$corpus == "ag_magazines")
newspapers <- subset(data, data$corpus == "newspaper")
articles <- subset(data, data$corpus == "scientific_articles")

#aggregate by cluster

magazines_cluster <- magazines %>%
  group_by(Cluster, year) %>%
  summarise(total_n = sum(n))

newspapers_cluster <- newspapers %>%
  group_by(Cluster, year) %>%
  summarise(total_n = sum(n))

articles_cluster <- articles %>%
  group_by(Cluster, year) %>%
  summarise(total_n = sum(n))

#transform cluster data into numeric
magazines_cluster$Cluster_n <- as.numeric(factor(magazines_cluster$Cluster))
newspapers_cluster$Cluster_n <- as.numeric(factor(newspapers_cluster$Cluster))
articles_cluster$Cluster_n <- as.numeric(factor(articles_cluster$Cluster))

#calculate the diversity
diversity_magazines_cluster <- aggregate(Cluster_n ~ year, data = magazines_cluster, FUN = function(x) {
  diversity(x, index = "shannon")
})

diversity_newspapers_cluster <- aggregate(Cluster_n ~ year, data = newspapers_cluster, FUN = function(x) {
  diversity(x, index = "shannon")
})

diversity_articles_cluster <- aggregate(Cluster_n ~ year, data = articles_cluster, FUN = function(x) {
  diversity(x, index = "shannon")
})

#plot results
g <- ggplot(diversity_magazines_cluster, aes(x = year, y = Cluster_n)) +
  geom_point() +
  geom_smooth(method = lm, color="#BC6C25") +
  stat_poly_eq(use_label(c("R2","P"))) +
  scale_y_continuous(limits = c(0, 3)) +
  labs(x = "Year", y = "Shannon Diversity Index", title = "Diversity of Magazine clusters over years\n ")

ggsave("SM_diversity_magazine_clusters.png", plot = g, width = 6, height = 6, units = "in")

h <- ggplot(diversity_newspapers_cluster, aes(x = year, y = Cluster_n)) +
  geom_point() +
  geom_smooth(method = lm, color="#BC6C25") +
  stat_poly_eq(use_label(c("R2","P"))) +
  scale_y_continuous(limits = c(0, 3)) +
  labs(x = "Year", y = "Shannon Diversity Index", title = "Diversity of Newspapers clusters over years\n ")

ggsave("SM_diversity_newspaper_clusters.png", plot = h, width = 6, height = 6, units = "in")

i <- ggplot(diversity_articles_cluster, aes(x = year, y = Cluster_n)) +
  geom_point() +
  geom_smooth(method = lm, color="#BC6C25") +
  stat_poly_eq(use_label(c("R2","P"))) +
  scale_y_continuous(limits = c(0, 3)) +
  labs(x = "Year", y = "Shannon Diversity Index", title = "Diversity of Scientific articles clusters\nover years")

ggsave("SM_diversity_articles_clusters.png", plot = i, width = 6, height = 6, units = "in")

diversity_plots <- grid.arrange(g, h, i, ncol = 3)
ggsave("SM_diversity_plots_b.png", plot = diversity_plots, width = 12, height = 5, units = "in")

