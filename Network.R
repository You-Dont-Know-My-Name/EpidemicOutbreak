library(igraph);

setwd("E:\\Managing epidemic outbreak\\task6")

generation_BA <- function(n){
  graph <- graph.empty(directed=FALSE)
  data <- get_population()
  n_now = 0
  while(n_now < n){
    graph <- add_node(graph, data)
    n_now = n_now + 1
    cat("vertex = ", n_now, "\n")
  }
  #plot(graph)
  #cat(as_edgelist(graph, names = TRUE))
  #print(as_adjacency_matrix(graph))
  #print(gsize(graph))
  #print(get.edge.ids(graph, 1))
  #print(sum(as_adjacency_matrix(graph)))
  
  
  #cat("\ndegrees\n")
  for(i in 1:vcount(graph)){
    #cat("i = ", i, "age = ", V(graph)$age[i], "gender = ", V(graph)$gender[i], " k = ", sum(as_adjacency_matrix(graph)[i,]), "\n")
    V(graph)$k[i] = sum(as_adjacency_matrix(graph)[i,])
  }
  return(graph)
}


compleet_graph <- function(n){
  E <- combn(1:n,2);
  graph <- graph(E, directed=FALSE)
  graph <- set_info(graph)
  return(graph)
}


EG_graph <- function(n, amount_of_edges){
  graph <- erdos.renyi.game(n, amount_of_edges, type = "gnm", directed = FALSE, loops = FALSE)
  graph <- set_info(graph)
  return(graph)
}


BA_graph <- function(n, amount_of_adges){
  graph <- barabasi.game(n, power = 1, m=amount_of_adges, directed=F)
  graph <-set_info(graph)
  return(graph)
}

SW_graph <- function(n, amount_of_adges, prob_reconnection){
  graph <- sample_smallworld(1, n, amount_of_adges, prob_reconnection, loops = FALSE, multiple = FALSE)
  graph <-set_info(graph)
  return(graph)
}


set_info <- function(graph){
  data <- get_population()
  for(i in 1:vcount(graph)){
    graph <- set_node_info(graph, i, data)
    #cat("i = ", i, "age = ", V(graph)$age[i], "gender = ", V(graph)$gender[i], " k = ", sum(as_adjacency_matrix(graph)[i,]), "\n")
    #V(graph)$k[i] = sum(as_adjacency_matrix(graph)[i,])
  }
  return(graph)
}


get_population <- function(){
  data_men <- read.csv(file ="dataset_ukraine/men_total_data.csv")
  data_women <- read.csv(file ="dataset_ukraine/women_total_data.csv")
  #print(data_men)
  
  total <- 0
  for(i in 1:nrow(data_men)){
    total = total + data_men$total[i]
    total = total + data_women$total[i]
  }
  
  data <-data.frame(age_group = c(), gender = c(), probability = c())
  
  for(i in 1:nrow(data_men)){
    data <- rbind(data, list(age_group = data_men$person_age_group[i], gender = "men", probability = data_men$total[i]/total))
    data <- rbind(data, list(age_group = data_women$person_age_group[i], gender = "women", probability = data_women$total[i]/total))
  }
  data <- cumulative_data_prob(data)
  #print(data)
  return(data)
}


cumulative_data_prob <-function(data){
  data$cumulative <- c(data$probability[1])
  for(i in 2:nrow(data)){
    data$cumulative[i] = data$cumulative[i-1] + data$probability[i]
  }
  return(data)
}


set_node_info <- function(graph, index, data){
  p = runif(1)
  for(i in 1:nrow(data)){
    if(p < data$cumulative[i]){
      #cat(index, '\n')
      V(graph)$id = index
      V(graph)$age[index] <- data$age_group[i]
      V(graph)$gender[index] <- data$gender[i]
      V(graph)$time[index] <- -1
      V(graph)$incubation_period[index] <- Inf
      V(graph)$time_of_contagion[index] <- Inf
      V(graph)$treatment_time[index] <- Inf
      V(graph)$time_of_death[index] <- Inf
      V(graph)$contagion[index] <- 0
      V(graph)$activity[index] = rand_person_activity()
      V(graph)$color[index] = "#ffffff"
      #print(vertex_attr(graph, index = V(graph)[indexx]) <- list(age = data$age_group[i], gender = data$gender[i], state = c(1,2,3)))
      graph <- set.vertex.attribute(graph, 'state', index = index, list(list('susceptible' = 1, 'infected_type1' = 0, 'infected_type2' = 0, 'immunized_type1' = 0, 'immunized_type2' = 0, 'dead_v1' = 0, 'dead_v2' = 0)))
      break
    }
  }
  return(graph)
}


# random value, acording to normal distrinution from 0 to 2
rand_person_activity <- function(){
  activity = 0
  while(activity <= 0 | 2 <= activity){
    activity = rnorm(1, 1, 1/3)
    #print(activity)
  }
  return(activity)
}


get_minimal_amount_of_neighbors <- function(age_group, gender){
  #cat("age group: ", age_group, " gender ", gender, "\n")
  if(age_group == "0-9"){
    return(5)
  }
  
  if(age_group == "10-19"){
    return(6)
  }
  
  if(age_group == "20-29"){
    return(7)
  }
  
  if(age_group == "30-39"){
    return(7)
  }
  
  if(age_group == "40-49"){
    return(7)
  }
  
  if(age_group == "50-59"){
    return(6)
  }
  
  if(age_group == "60-69"){
    return(5)
  }
  
  if(age_group == "70-79"){
    return(4)
  }
  
  if(age_group == "80-89"){
    return(3)
  }
  
  if(age_group == "90+"){
    return(2)
  }
  
}


add_node <- function(graph, data){
  graph <- add_vertices(graph, 1)
  graph <- set_node_info(graph, vcount(graph), data)
  m = get_minimal_amount_of_neighbors(age_group = V(graph)$age[vcount(graph)], gender = V(graph)$gender[vcount(graph)])
  amount_vertex <-vcount(graph)
  if(m >= amount_vertex-1 & amount_vertex != 1){
    for(i in 1:(amount_vertex-1)){
      graph <- add_edges(graph, c(i, amount_vertex))
    }
  }
  else if(m < vcount(graph)-1){
    nodes = get_nodes(graph, m)
    for(i in 1:length(nodes)){
      graph <- add_edges(graph, c(nodes[i], amount_vertex))
    }
  }
  return(graph)
}


get_nodes <- function(graph, m){
  nodes = c()
  list_prob <- get_list_prob(graph)
  for(i in 1:m){
    cumulative_prob <- get_cumulative_prob(list_prob)
    p = runif(1)
    for(j in 1:length(cumulative_prob)){
      #cat("j prob = ", j, " ", cumulative_prob[j], "\n")
      if(p < cumulative_prob[j]){
        nodes[i] <- j
        list_prob <- del_prob(list_prob, j)
        break
      }
    }
  }
  return(nodes)
}


del_prob <- function(list_prob, index){
  for(i in 1:length(list_prob)){
    list_prob[i] = list_prob[i] / (1 - list_prob[index])
  }
  list_prob[index] = 0
  return(list_prob)
}


get_list_prob <- function(graph){
  list_prob = c();
  ek = sum(as_adjacency_matrix(graph))
  for(i in 1:(vcount(graph)-1)){
    ki = sum(as_adjacency_matrix(graph)[i,])
    list_prob[i] <- ki/ek
  }
  return(list_prob)
}


get_cumulative_prob <- function(list_prob){
  cumulative_prob = c()
  for(i in 1:length(list_prob)){
    cumulative_prob[i] = 0
    for(j in 1:i){
      cumulative_prob[i] = cumulative_prob[i] + list_prob[j]
    }
  }
  return(cumulative_prob)
}


get_network_info <-function(vertices_info, data){
  #print(vertices_info)
  population_data <- get_population()
  data = data.frame(age_group = population_data$age_group, gender=population_data$gender, amount=0,
                    k_mean = 0, k_median = 0, 
                    k_max = 0, k_min = 0, k_sd = 0)
  
  amount <- aggregate(vertices_info$k, by=list(vertices_info$age, vertices_info$gender), FUN=length)
  k_mean <- aggregate(vertices_info$k, by=list(vertices_info$age, vertices_info$gender), FUN=mean)
  k_median <- aggregate(vertices_info$k, by=list(vertices_info$age, vertices_info$gender), FUN=median)
  k_max <- aggregate(vertices_info$k, by=list(vertices_info$age, vertices_info$gender), FUN=max)
  k_min <- aggregate(vertices_info$k, by=list(vertices_info$age, vertices_info$gender), FUN=min)
  k_sd <- aggregate(vertices_info$k, by=list(vertices_info$age, vertices_info$gender), FUN=sd)
  
  for(i in 1:nrow(data)){
    data$amount[i] <- ifelse(length(subset(amount, Group.1 == data$age_group[i] & Group.2 == data$gender[i])$x) != 0, 
                             subset(amount, Group.1 == data$age_group[i] & Group.2 == data$gender[i])$x, 0)
    
    data$k_mean[i] <- ifelse(length(subset(k_mean, Group.1 == data$age_group[i] & Group.2 == data$gender[i])$x) != 0, 
                             subset(k_mean, Group.1 == data$age_group[i] & Group.2 == data$gender[i])$x, 0)
    
    data$k_median[i] <- ifelse(length(subset(k_median, Group.1 == data$age_group[i] & Group.2 == data$gender[i])$x) != 0, 
                               subset(k_median, Group.1 == data$age_group[i] & Group.2 == data$gender[i])$x, 0)
    
    data$k_max[i] <- ifelse(length(subset(k_max, Group.1 == data$age_group[i] & Group.2 == data$gender[i])$x) != 0, 
                            subset(k_max, Group.1 == data$age_group[i] & Group.2 == data$gender[i])$x, 0)
    
    data$k_min[i] <- ifelse(length(subset(k_min, Group.1 == data$age_group[i] & Group.2 == data$gender[i])$x) != 0, 
                            subset(k_min, Group.1 == data$age_group[i] & Group.2 == data$gender[i])$x, 0)
    
    data$k_sd[i] <- ifelse(length(subset(k_sd, Group.1 == data$age_group[i] & Group.2 == data$gender[i])$x) != 0, 
                           subset(k_sd, Group.1 == data$age_group[i] & Group.2 == data$gender[i])$x, 0)
  }
  
  data[nrow(data) + 1, ] = c(age_group = "total", gender = "total", amount=nrow(vertices_info), k_mean = mean(vertices_info$k), 
                             k_median = median(vertices_info$k), k_max = max(vertices_info$k), k_min = min(vertices_info$k), 
                             sd = sd(vertices_info$k))
  for(i in 1:nrow(data)-1){
    
  }
  print(data)
}


amount_of_gender <- function(graph){
  men = 0
  women = 0
  for(i in 1:vcount(graph)){
    if(V(graph)$gender[i] == "men"){
      men = men + 1
    }
    else {
      women = women + 1
    }
  }
  #cat ("men = ", men, " women = ", women)
}


sample_cluster <- function(nv = 200, clustering_coef = 0.1, thres = 0.01) {
  cat("cc = ", clustering_coef, " t = ", thres, "\n")
  g <- sna::rguman(1, nv, mut = clustering_coef, asym = 0, null = 1 - clustering_coef) %>% 
    graph_from_adjacency_matrix(mode = "undirected")
  
  print(transitivity(g))
  
  while (!(transitivity(g) >= clustering_coef-thres & transitivity(g) <= clustering_coef+thres)) {
    g <- sna::rguman(1, nv, mut = clustering_coef, asym = 0, null = 1 - clustering_coef) %>% 
      graph_from_adjacency_matrix(mode = "undirected")
    print(transitivity(g))
  }
  return(g)
}

generation_clustering_model <- function(nv = 200, clustering_coef = 0.1){
  graph <- sample_cluster(nv, clustering_coef)
  data <- get_population()
  
  for(i in 1:vcount(graph)){
    #print(i)
    graph <- set_node_info(graph, i, data)
    #cat("i = ", i, "age = ", V(graph)$age[i], "gender = ", V(graph)$gender[i], " k = ", sum(as_adjacency_matrix(graph)[i,]), "\n")
    V(graph)$k[i] = sum(as_adjacency_matrix(graph)[i,])
    #V(graph)$k[i] = 0
  }
  return(graph)
}









#time = as.numeric(Sys.time())
#graph <- generation_BA(200)
#print(V(graph)$state[1][[1]][['susceptible']])
#print(graph)
#plot(graph, vertex.color="#FEE0D2", layout=layout.circle)
#community <- walktrap.community(graph)
#plot(community, graph,  layout=layout.fruchterman.reingold)
#get_network_info(subset(as_data_frame(graph, 'both')$vertices, select = c(age, gender, k)))
#amount_of_gender(graph)
#compleet_graph(5)
#print(as.numeric(Sys.time()) - time)
#print(as_data_frame(graph, 'both')$vertices)

#g <- barabasi.game(10, m = 3, directed = FALSE, )
#community <- walktrap.community(g)
#plot(community, g,  layout=layout.fruchterman.reingold, edge.curved=0.2)
#degree_distribution(g)

#graph <- compleet_graph(1000)
#print(get_network_info(subset(as_data_frame(graph, 'both')$vertices, select = c(age, gender, k))))
