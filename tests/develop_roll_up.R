graph_tree <- graph_SBI

#graph_tree <- create_SBI_tree(code_SBI = c('C'))
level_to <- 1
name_attribute <- "qty_companies"
plot_graydon_graph(graph_tree,
                   vertex.size = 3,
                   edge.arrow.size = 0,
                   vertex.label = "",
                   main = "Initial")

graph_rolled <- roll_up_hierarchy_by_level(graph_tree, level_to, name_attribute)

plot_graydon_graph(graph_rolled,
                   vertex.size = 3,
                   edge.arrow.size = 0,
                   #vertex.label = "",
                   main = "Removed empty")
