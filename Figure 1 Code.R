library(igraph)


# node information
nodes <- data.frame(
  name   = c("Upin", "Ipin", "Tok Dalang", "Mei Mei", "Rajoo"),
  Race   = c("Malay", "Malay", "Malay", "Chinese", "Indian"),
  Gender = c("Male", "Male", "Male", "Female", "Male")
)


# directed and weighted edges
edges <- data.frame(
  from = c("Upin", "Ipin", "Mei Mei", "Mei Mei"),
  to   = c("Ipin", "Upin", "Ipin", "Rajoo"),
  Weight = c(1, 2, 2, 1)
)

series_net_directed <- graph_from_data_frame(
  edges,
  directed = TRUE,
  vertices = nodes
)


# node colours according to race and node shapes according to gender
race_colour <- c(
  Malay = "tan1",
  Chinese = "lightblue",
  Indian = "plum"
)

gender_shape <- c(
  Male = "circle",
  Female = "square"
)

# plot
# coordinates for the nodes
coords <- matrix(
  c(
    -1.0,  1.0, # Upin
    0.2,  1.0,  # Ipin
    -1.4, -0.2, # Tok Dalang
    1.4,  0.2,  # Mei Mei
    0.2, -1.0   # Rajoo
  ),
  byrow = TRUE,
  ncol = 2
)

plot(
  series_net_directed,
  layout = coords,
  vertex.size = 28,
  vertex.color = race_colour[V(series_net_directed)$Race],
  vertex.shape = gender_shape[V(series_net_directed)$Gender],
  vertex.frame.color = "black",
  vertex.label = V(series_net_directed)$name,
  vertex.label.cex = 0.9,
  vertex.label.color = "black",
  edge.color = "black",
  edge.width = E(series_net_directed)$Weight * 2,   # thickness reflects weight
  edge.arrow.size = 0.75,
  edge.curved = 0.4,
  edge.label = NA                                    
)


# legend
# race
legend(x = -1.7, y = 1.15,
       legend = names(race_colour),
       col = race_colour,
       pch = 16,
       pt.cex = 1.7,
       cex = 1,
       bty = "n",
       title = "Race")

# gender
legend(x = -1.7, y = 0.58,
       legend = c("Male", "Female"),
       pch = c(16, 15),
       col = "black",
       pt.cex = 1.7,
       cex = 1,
       bty = "n",
       title = "Gender")
