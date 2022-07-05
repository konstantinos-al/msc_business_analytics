# --- PACKAGES ---

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
