# --- PACKAGES ---

#install.packages("igraph")
library('igraph')
#install.packages("scales")
library("scales")

#--- 1 DATASET LOAD ---

df = read.csv("http://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-all-edges.csv", header = TRUE)
head(df)

# columns to use
columns = c('Source','Target','weight')
df = df[columns]
head(df,10)
View(df)

# checking for Null
View(as.data.frame(table(df$Source)))
View(as.data.frame(table(df$Target)))

# dropping rows with Nan either in source or target columns
df2 = df[!(df$Source=="Nan" | df$Target=="Nan"),]
nrow(df)
nrow(df2) #removing 8 rows in total

# graph creation
g = graph_from_data_frame(df2, directed = TRUE)

#--- 2 NETWORK PROPERTIES ---

# number of vertices
gorder(g)

# number of edges
gsize(g)

# diameter of the graph
diameter(g, directed = TRUE)

# number of triangles
sum(count_triangles(g, vids=V(g)))

# the top-10 characters of the network as far as their degree is concerned
deg = degree(g, mode = 'all')
deg_ranked = sort(rank(deg), decreasing = TRUE)
head(deg_ranked, 10)

# the top-10 characters of the network as far as their weighted degree is concerned
deg_weighted = strength(g, mode = "all", weights = df$weight)
deg_ranked_weighted = sort(rank(deg_weighted), decreasing = TRUE)
head(deg_ranked_weighted, 10)

deg_ranked_weighted$[1:10]

#--- 3 SUBGRAPH ---

# plotting the graph
plot(g, vertex.label = NA, vertex.color = rgb(0.7,0.2,0.3,0.7), 
    vertex.frame.color='black', vertex.size = 3, 
    edge.arrow.width = 0.5, edge.arrow.size = 0.4 
    )

# subgraph
in_degree = degree(g, mode='all')
to_keep = in_degree >= 10
sg <- induced_subgraph(g, to_keep, impl='copy_and_delete')
plot(sg, vertex.label=NA, vertex.color = rgb(0.7,0.2,0.3,0.7), 
    vertex.frame.color='black', vertex.size = 10, 
    edge.arrow.width = 0.5, edge.arrow.size = 0.4 
    )

# density of initial graph
edge_density(g)

# density of sub-graph
edge_density(sg)

# --- CENTRALITY ---

# closeness centrality
g_closeness = closeness(g, mode='all')
closeness_centrality = sort(rank(g_closeness), decreasing = TRUE)
head(closeness_centrality, 15)

# betweeness centrality
g_betweenness = betweenness(g, mode='all')
betweeness_centrality = sort(rank(g_betweenness), decreasing = TRUE)
head(betweeness_centrality, 15)

# --- PAGRE RANK ---

# page rank algortithm
pgr = page_rank(g, algo='arpack')

# to use in visualisation // rescaled for beautification
mtrx = as.matrix(pgr$vector)
mtrx_rscl = rescale(mtrx, to=c(3, 25))

# creating the plot
plot(g, vertex.label = NA, vertex.color = rgb(0.7,0.2,0.3,0.7), 
    vertex.frame.color='black', vertex.size = mtrx_rscl, 
    edge.arrow.width = 0.5, edge.arrow.size = 0.4 
    )
