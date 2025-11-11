####一、 Pathway1_down network###############################################################
library(igraph)
library(ggraph)
library(graphlayouts)
library(ggplot2)
library(dplyr)
library(openxlsx)


node_data <- read.table('Pathway1_down_nodes.tsv', header = TRUE, check.names = FALSE, comment.char = "")
colnames(node_data)[1] <- 'node1'


degree_data <- read.table('Pathway1_down_degrees.tsv', header = TRUE, check.names = FALSE, comment.char = "")
colnames(degree_data)[1] <- 'node1'

# top30
top30_nodes <- degree_data %>% 
   arrange(desc(node_degree)) %>% 
   head(30) %>% 
   pull(node1)

# top30 edges
top30_edges <- node_data %>% 
   filter(node1 %in% top30_nodes & node2 %in% top30_nodes)


top30_degree_data <- degree_data %>% 
   filter(node1 %in% top30_nodes)
write.csv(top30_degree_data, "Pathway1_down_top30_degree_data.csv", row.names = FALSE)


graph <- graph_from_data_frame(top30_edges, vertices = top30_degree_data, directed = FALSE)

pdf("Pathway1_down_network.pdf", width = 5, height = 4)
ggraph(graph, layout = "stress") +
   geom_edge_arc(
      color = "#8B9DBA", 
      alpha = 0.6,         
      linewidth = 0.25,    
      curvature = 0.2
   ) +
   geom_node_point(
      aes(fill = node_degree, size = node_degree), 
      color = "white", 
      shape = 21, 
      stroke = 1.2
   ) +
   geom_node_text(
      aes(label = name), 
      color = "#333333",
      size = 8/2.85,  
      family = "sans",
      fontface = "bold",
      show.legend = FALSE,
      repel = TRUE,
      box.padding = 0.5,
      point.padding = 0.5
   ) +
   scale_edge_width(guide = "none") +
   scale_edge_alpha(guide = "none") +
   scale_fill_gradientn(
      colors = c("#dcedc8", "#80cbc4", "#00796b"),
      name = "Node Degree",
      guide = guide_colorbar(
         barwidth = 0.3,  
         barheight = 8,   
         title.position = "top",
         title.vjust = 0.8,
         ticks.colour = "gray30",
         frame.colour = "gray50"
      )
   ) +
   scale_size_continuous(guide = "none") +
   theme_graph(base_family = "sans", base_size = 8) +  
   theme(
      plot.background = element_rect(fill = "#F8F9FA", color = NA),
      panel.background = element_rect(fill = "#F8F9FA", color = NA),
      legend.position = "right",
      legend.box = "vertical",
      legend.title = element_text(size = 8, face = "bold", angle = 0), 
      legend.text = element_text(size = 8),                            
      legend.key.size = unit(0.3, "lines"),                            
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      plot.margin = unit(c(0.5, 0.8, 0.5, 0.5), "cm"),
      panel.spacing = unit(0, "lines")
   )
dev.off()
