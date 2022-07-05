#--- PACKAGES ---

library('igraph')
library('data.table')
install.packages("tidyverse")
install.packages('tibble')
library('dplyr')
library(tidyr)
library(stringr)
install.packages('splitstackshape')
library(splitstackshape) 

# --- DATA READING ---

# data load
dt_cikm <- fread("assignment2/cmd_cleaning/papers_CIKM.csv",
                    header = FALSE)
dt_kdd <- fread("assignment2/cmd_cleaning/papers_KKD.csv",
                    header = FALSE)
dt_icwsm <- fread("assignment2/cmd_cleaning/papers_ICWSM.csv",
                    header = FALSE)
dt_www <- fread("assignment2/cmd_cleaning/papers_WWW.csv",
                    header = FALSE)
dt_ieee <- fread("assignment2/cmd_cleaning/papers_IEEE.csv",
                    header = FALSE)
dt_ttl <- do.call("rbind", list(dt_cikm, dt_kdd, dt_icwsm, dt_www, dt_ieee))
colnames(dt_ttl) <- c("year", "paper", "conference", "authors")
View(dt_ttl)

# filtering data
yrs <- (max(dt_ttl$year)-5)
dt_final <- filter(dt_ttl, year >= yrs)

# script for creating csv for undirect co-authorships graphs
dt_authors <- cSplit(dt_final, "authors", ",")
dt_authors1 <- dt_authors[, c(4:33, 1)]

# loop to itterate through all combinations
authors_final <- data.frame(year = character(), author1 = character(), author2 = character())
i <- 1
while (i < nrow(dt_authors1)) {
    dt_authors1
    for (a in seq(ncol(dt_authors1))) {
        aa <- dt_authors1[i, ..a]
        if (is.na(aa)) {
            break
        }
        for (b in seq(ncol(dt_authors1))) {
            bb <- dt_authors1[i, ..b]
            if (is.na(bb)) {
                break
            }
            if (a!=b) {
                year = dt_authors1[i, "year"]
                authors_final[nrow(authors_final)+1,] <- c(year,aa,bb)
            }
        }
    }
i <- i + 1
print(i)
}

#write.csv(authors_final, "./authors_final.csv", row.names = FALSE)
authors_final = read.csv("authors_final.csv")

# to exclude unneccassary rows from produced by loop
`%!in%` <- Negate(`%in%`)
lst <- list(2015, 2016, 2017, 2018, 2019, 2020)
authors_final1 <- filter(authors_final, author1 %!in% lst)
authors_final1 <- filter(authors_final1, author2 %!in% lst)

# loop for creating the csv files for each year
yrs_to_filter <- list(2016, 2017, 2018, 2019, 2020)
for (y in yrs_to_filter) {
    df <- authors_final1[authors_final1$year == y, ]
    labels <- apply(df[, c("author1", "author2")], 1, sort)
    df$id <- as.numeric(factor(apply(labels, 2,
                        function(x) paste(x, collapse = ""))))
    final_df <- as.data.frame(table(df$id))
    colnames(final_df) <- c("id", "Frequency")
    final_df$id <- as.numeric(final_df$id)
    mapping <- unique(df[, c(2:4)])
    mapping$id <- as.numeric(mapping$id)
    final_df2 <- left_join(final_df, mapping, by = "id")
    final_df3 <- final_df2[!duplicated(final_df2$id), ]
    final_df3 <- final_df3[,c(3, 4, 2)]
    colnames(final_df3) <- c("from", "to", "weight")
    path <- paste("authors_", y, ".csv", sep = "")
    write.csv(final_df3, path, row.names = FALSE)
}

#--- 2 Average degree over time ---
#Your next task is to create plots that visualize the 5-year evolution of different
#metrics for the graph. More specifically, you will create plots for:

• Number of vertices
• Number of edges
• Diameter of the graph
• Average degree (simple, not weighted)

csv16 <- read.csv('assignment2/csv/authors_2016.csv')
csv17 <- read.csv('assignment2/csv/authors_2017.csv')
csv18 <- read.csv('assignment2/csv/authors_2018.csv')
csv19 <- read.csv('assignment2/csv/authors_2019.csv')
csv20 <- read.csv('assignment2/csv/authors_2020.csv')

# graphs creation
library("igraph")
g16 <- graph_from_data_frame(csv16, directed = FALSE)
g17 <- graph_from_data_frame(csv17, directed = FALSE)
g18 <- graph_from_data_frame(csv18, directed = FALSE)
g19 <- graph_from_data_frame(csv19, directed = FALSE)
g20 <- graph_from_data_frame(csv20, directed = FALSE)

# number of vertices
vrx_16 <- gorder(g16)
vrx_17 <- gorder(g17)
vrx_18 <- gorder(g18)
vrx_19 <- gorder(g19)
vrx_20 <- gorder(g20)

# number of edges
edg_16 <- gsize(g16)
edg_17 <- gsize(g17)
edg_18 <- gsize(g18)
edg_19 <- gsize(g19)
edg_20 <- gsize(g20)

# diameters of the graphs
diam16 <- diameter(g16, directed = FALSE)
diam17 <- diameter(g17, directed = FALSE)
diam18 <- diameter(g18, directed = FALSE)
diam19 <- diameter(g19, directed = FALSE)
diam20 <- diameter(g20, directed = FALSE)

# average graphs degrees
avg_dgr16 <- mean(degree(g16))
avg_dgr17 <- mean(degree(g17))
avg_dgr18 <- mean(degree(g18))
avg_dgr19 <- mean(degree(g19))
avg_dgr20 <- mean(degree(g20))

viz_vertices <- cbind(vrx_16, vrx_17, vrx_18, vrx_19, vrx_20)
colnames(viz_vertices) <- c(2016, 2017, 2018, 2019, 2020)
viz_edges <- cbind(edg_16, edg_17, edg_18, edg_19, edg_20)
colnames(viz_edges) <- c(2016, 2017, 2018, 2019, 2020)
viz_diam <- cbind(diam16, diam17, diam18, diam19, diam20)
colnames(viz_diam) <- c(2016, 2017, 2018, 2019, 2020)
viz_degree <- cbind(avg_dgr16, avg_dgr17, avg_dgr18, avg_dgr19, avg_dgr20)
colnames(viz_degree) <- c(2016, 2017, 2018, 2019, 2020)

# visualizations
par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
barplot(viz_vertices, col = 'slateblue4', main = 'Vertices')
barplot(viz_edges, col = 'violetred4', main = "Edges")
barplot(viz_diam, col = 'springgreen4', main = "Diameters")
barplot(viz_degree, col = 'steelblue4', main = "Avg Degree")

#What do you notice for each of the 5 above metrics? Are there significant fluctuations during these five years?

# 3 Important nodes

Next, you will write to code to create and print data frames for the 5-year
evolution of the top-10 authors with regard to:
• Degree (simple, not weighted)
• PageRank
Again, provide short comments on your findings. Do you notice variations on
the top-10 lists for the different years?