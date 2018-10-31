# Process market data
source("load_market_nl.R")
tbl_market <- prep_data_market_nl(FALSE)

library(igraph)

# Function definitions ----
# Function for creating a graph for all company hierarchies ----
create_graph_company_hierarchies <- function(tbl_market) {
  
  vec_sbi_holdings <- c("64", "642", "6420")
  
  # Create parent child relations between companies
  tbl_parent_child <- tbl_market %>% 
    filter(!is.na(id_mothercompany)) %>% 
    select(id_graydon, id_mothercompany)
  
  # All unique companies having parent-child relations
  tbl_node <- unique(
    rbind(
      tbl_parent_child %>% 
        select(id_graydon),
      tbl_parent_child %>%
        select(id_graydon = id_mothercompany)
    )
  )
  
  # Add node attributes
  tbl_node %<>%
    left_join(tbl_market, by = "id_graydon") %>% 
    mutate(is_holding = code_sbi %in% vec_sbi_holdings) %>% 
    mutate(qty_employees_cum = number_employees,
           code_sbi_holding = code_sbi) %>% 
    select(id_graydon,
           code_sbi,
           code_sbi_holding,
           is_holding,
           is_stopped,
           score_growth,
           qty_employees = number_employees,
           qty_employees_cum
           )
  
  # Create a graph for all companies 
  graph_company_hierarchies <- graph_from_data_frame(d = tbl_parent_child, 
                                                     vertices = tbl_node, 
                                                     directed = TRUE)
  return(graph_company_hierarchies)
}

# Function for plotting a company hierarchy holding_sbi ----
plot_company_hierarchy_qty_employees <- function(graph_hierarchy, title = "", graph_layout = NULL, show_id = FALSE){
  
  if(is.null(graph_layout)){
    graph_layout <- layout.fruchterman.reingold(graph_hierarchy)
  }
  
  # Determine vertice label
  V(graph_hierarchy)$label <- paste0(#V(graph_hierarchy)$name, "\n",
                                     V(graph_hierarchy)$qty_employees, "\\",
                                     V(graph_hierarchy)$qty_employees_cum)
  V(graph_hierarchy)$color <- ifelse(V(graph_hierarchy)$is_holding, col_graydon[2], col_graydon[4])
  V(graph_hierarchy)$label.family <- "Roboto"
  
  plot(graph_hierarchy, 
       palette = col_graydon, 
       main = title, 
       layout = graph_layout,
       vertex.label.color = col_graydon[8],
       vertex.label.cex = .7,
       vertex.size = 12,
       vertex.frame.color="white",
       edge.arrow.size = 0.5,
       arrow.width = 0.5) 
}

plot_company_hierarchy_qty_employees <- function(graph_hierarchy, title = "", graph_layout = NULL, show_id = FALSE){
  
  if(is.null(graph_layout)){
    graph_layout <- layout.fruchterman.reingold(graph_hierarchy)
  }
  
  # Determine vertice label
  V(graph_hierarchy)$label <- paste0(#V(graph_hierarchy)$name, "\n",
    V(graph_hierarchy)$qty_employees, "\\",
    V(graph_hierarchy)$qty_employees_cum)
  V(graph_hierarchy)$color <- ifelse(V(graph_hierarchy)$is_holding, col_graydon[2], col_graydon[4])
  V(graph_hierarchy)$label.family <- "Roboto"
  
  plot(graph_hierarchy, 
       palette = col_graydon, 
       main = title, 
       layout = graph_layout,
       vertex.label.color = col_graydon[8],
       vertex.label.cex = .7,
       vertex.size = 12,
       vertex.frame.color="white",
       edge.arrow.size = 0.5,
       arrow.width = 0.5) 
}

# Function for plotting a company hierarchy ----
plot_company_hierarchy <- function(graph_hierarchy, title = "", graph_layout = NULL){
  
  if(is.null(graph_layout)){
    graph_layout <- layout.fruchterman.reingold(graph_hierarchy)
  }
  
  # Determine vertice label
  V(graph_hierarchy)$label <- paste0(V(graph_hierarchy)$name)
  V(graph_hierarchy)$color <- ifelse(V(graph_hierarchy)$is_holding, col_graydon[2], col_graydon[4])
  V(graph_hierarchy)$label.family <- "Roboto"
  
  plot(graph_hierarchy, 
       palette = col_graydon, 
       main = title, 
       layout = graph_layout,
       vertex.label.color = col_graydon[8],
       vertex.label.cex = .7,
       vertex.size = 12,
       vertex.frame.color="white",
       edge.arrow.size = 0.5,
       arrow.width = 0.5) 
}

# Function for plotting a company hierarchy qty_employees ----
plot_company_hierarchy_holdings <- function(graph_hierarchy, title = "", graph_layout = NULL, show_id = FALSE){
  
  if(is.null(graph_layout)){
    graph_layout <- layout.fruchterman.reingold(graph_hierarchy)
  }
  
  # Determine vertice label
  V(graph_hierarchy)$label <- paste0(#V(graph_hierarchy)$name, "\n",
                                     V(graph_hierarchy)$code_sbi, "\\",
                                     V(graph_hierarchy)$code_sbi_holding)
  V(graph_hierarchy)$color <- ifelse(V(graph_hierarchy)$is_holding, col_graydon[2], col_graydon[4])
  V(graph_hierarchy)$label.family <- "Roboto"
  
  plot(graph_hierarchy, 
       palette = col_graydon, 
       main = title, 
       layout = graph_layout,
       vertex.label.color = col_graydon[8],
       vertex.label.cex = .7,
       vertex.size = 12,
       vertex.frame.color="white",
       edge.arrow.size = 0.5,
       arrow.width = 0.5) 
}

# Process a whole list of company graphs ----
process_company_hierarchy <- function(list_company_hierarchies) {

  # Count number of companies in a company networks ----
  list_number_of_companies <- vector('integer', length(list_company_hierarchies)) # Create empty list 
  i<-1 
  
  for(company_hierarchy in list_company_hierarchies){
    
    list_number_of_companies[i] <- length(V(company_hierarchy))
    list_company_hierarchies[i] <- recalcuate_company_hierarchy(company_hierarchy)
    i <- i + 1
  }
  
  return(list_company_hierarchies) 
}

# Calculating values throughout a company network ----
recalcuate_company_hierarchy <- function(company_hierarchy){
  
  # Determine the root vertice
  vertex_root <- get_root_vertex(company_hierarchy)
  
  # Distances between root and nodes in the hierarchy
  V(company_hierarchy)$dist_to_root <- distances(company_hierarchy,
                                                 v = V(company_hierarchy),
                                                 to = vertex_root)
  
  # Determine the calculation order based on ascending distance from root
  id_graydon_inward <- V(company_hierarchy)$name[sort(V(company_hierarchy)$dist_to_root,
                                                      decreasing = TRUE,
                                                      index.return = TRUE)$ix]
  
  # Iterate through each company in the network
  for(id_graydon in id_graydon_inward) {
    
    # The company vertice
    vertex_company <- V(company_hierarchy)[id_graydon]
    
    # Create a small subnetwork of the company and it's child companies
    ego_graph <- make_ego_graph(graph = company_hierarchy, 
                                order = 1,
                                nodes = vertex_company,
                                mode = "in")[[1]]
    
    # Calculate values
    V(company_hierarchy)[id_graydon]$qty_employees_cum <- determine_qty_employees(ego_graph)
    V(company_hierarchy)[id_graydon]$code_sbi_holding <- determine_holding_sbi(ego_graph)
  }

  return(company_hierarchy)
}

# Get the root node of a tree ----
get_root_vertex <- function(tree_graph){
  
  # Find root node
  idx_root <- which(sapply(sapply(V(tree_graph),
                                  function(x) neighbors(tree_graph, x, mode="out")),
                           length) == 0)
  vertex_root <- V(tree_graph)[idx_root]
  rm(idx_root)
  
  return(vertex_root)
}

# Determine the number of employees ----
determine_qty_employees <- function(ego_graph){
  
  #is_holding <- V(ego_graph)$code_sbi %in% c("64", "642", "6420")
  qty_employees_cum <- V(ego_graph)$qty_employees_cum
  
  qty_employees <- sum( qty_employees_cum, na.rm = TRUE ) 

  # Sum quantities of all nodes that are not holdings
  #qty_employees <- sum( qty_employees_cum[!is_holding], na.rm = TRUE )
  # Add all 
   
  
  # Add egographs root node quantity if it is a holding
  #qty_employees <- ifelse(is_holding[1], qty_employees + qty_employees_cum[1], qty_employees)
  
  return(qty_employees)
}

# Determine holding SBI replacement
determine_holding_sbi <- function(ego_graph){
  
  vec_sbi_holdings <- c("64", "642", "6420")
  
  # The default new SBI code is the same as the current
  code_sbi_new <- get_root_vertex(ego_graph)$code_sbi
  
  # Determine wether the node is a holding
  is_holding <- get_root_vertex(ego_graph)$code_sbi %in% vec_sbi_holdings
  
  # If the company is not a holding the new SBI code is the same as the original
  if(is_holding){

    sbi_children <- V(ego_graph)[ nei(V(ego_graph), "in") ]$code_sbi_holding # Getting SBI codes of 'children'
    sbi_children <- sbi_children[!sbi_children %in% vec_sbi_holdings]        # Remove holding SBI codes for children
    sbi_children <- sbi_children[!is.na(sbi_children)]                       # Remove empty SBI codes for children
    
    if(length(sbi_children) > 0) {

      code_sbi_2 <- str_sub(sbi_children, 1, 2)                         # Shorten SBI code to first 2 digits
      freq_sbi_2 <- table(code_sbi_2)                                   # Count SBI code 2-digit occurence
      code_sbi_new <- names(freq_sbi_2)[which.max(freq_sbi_2)][1]       # Get first of maximum values
    }
  }
  
  return(code_sbi_new)
}






# Rubble code ----
# table(list_number_of_companies, useNA = "ifany")
# 
# list_number_of_companies[list_number_of_companies == 487]
# 
# match( 13, list_number_of_companies)
# 
# plot_company_hierarchy(list_company_hierarchies[[255]])
# 
# # Second method and comparison
# tic("Method 2")
# list_number_of_companies2 <- sapply(sapply(list_company_hierarchies, V), 
#                                     length)
# toc()
