setwd("E:\\Managing epidemic outbreak\\task6")
#install.packages("igraph")
suppressPackageStartupMessages(library(igraph))
source("Network.R")


t_incubation = 0
t_contagion = 10
t_treatment = 10
t_death = 14


CTMC <- function(G, q = 0.01, Time = 0, type = 1, death_rate_type = "same", network_type = "compleet_same")
{
  
  colors = c("#31A354", "#DE2D26")
  
  L <- actions(G, Time)
  G <- L$Graph
  emp.data_actions <- data.frame(
    "time" = c(),
    "action" = c(),
    "age_interval" = c(),
    "gender" = c()
  )
  emp.data <- rbind(emp.data_actions,L$action)
  
  #cat("q = ", q, "Time = ", Time, "s = ", s, "\n")
  
  virys_type = ""
  imunized = ""
  if (type == 1){
    virys_type = 'infected_type1'
    imunized = 'immunized_type1'
  }
  else if(type == 2){
    virys_type = 'infected_type2'
    imunized = 'immunized_type2'
  }
  else return(Inf)
  qi = get_concentration(G, q, type, network_type)
  qii = sum(qi)
  #cat ("number of infected = ", amount_of_infected(G), type, "\n")
  
  if (qii > 0)
  {
    # Compute exponential distribution, rate qii
    ### !!! ?????????????????????? ?????????????????? ??????????, ???????????? ???????? ???? ?????? ?????????????????? ??????????????. !!! ???????????????? ?? ????????????????
    # Update graph, select event proportional to rate
    sumprob = 0;
    u <- runif(1);
    for (j in 1:vcount(G))
    {
      previous_sumprob = sumprob;
      if (V(G)$state[j][[1]]$susceptible == 1 && qi[j] != 0 && V(G)$state[j][[1]][[imunized]] == 0)
      {sumprob = sumprob + qi[j]/qii;}
      if (previous_sumprob < u && u <= sumprob){
        V(G)$state[j][[1]][[virys_type]] = 1; # vertex j gets infected
        V(G)$state[j][[1]][['susceptible']] = 0;
        V(G)$time[j] = Time;
        V(G)$color[j] = colors[type];
        #V(G)$incubation_period[j] <- Time + t_incubation
        #V(G)$time_of_contagion[j] <- Time + t_incubation + t_contagion
        V(G)$treatment_time[j] <- Time + t_incubation + t_treatment
        if(runif(1) <= get_prob_die(V(G)$age[j], V(G)$gender[j], death_rate_type)){
          V(G)$time_of_death[j] <- Time + t_incubation + t_death
          V(G)$treatment_time[j] <- Inf
        }
        emp.data <- rbind(emp.data, save_action(G, j, "infected", as.double(Time), type))
        v = 0
      }
    }
    qii = sum(get_concentration(G, q, type, network_type))
    if(qii > 0){
      DeltaT <- rexp(1,qii) 
    }
    else
      DeltaT <- Inf;
    
    return(list(first=G, second=DeltaT, third = emp.data))
  }
  else
    DeltaT <- Inf; # Infinity; Signals that absorbing state is reached
  
  return(list(first=G, second=DeltaT, third = emp.data))
}


get_concentration <- function(G, qq = 0.01, type = 1, network_type = "compleet_same"){
  virys_type = ""
  imunized = ""
  if (type == 1){
    virys_type = 'infected_type1'
    imunized = 'immunized_type1'
  }
  else if(type == 2){
    virys_type = 'infected_type2'
    imunized = 'immunized_type2'
  }
  else return(Inf)
  
  qi <- rep(0.0, vcount(G));
  qii = 0;
  qqi = 0
  alive <- FALSE   
  if(network_type == "compleet_same"){
    for (j in 1:vcount(G))
    {
      if(alive == FALSE){
        if (V(G)$state[j][[1]]$susceptible == 1 & V(G)$state[j][[1]][[imunized]] == 0)
        {
          # count incoming flights from infected neighbors;
          v=0;
          for (nb in neighbors(G,V(G)[j],mode=c("in")) ){
            if (V(G)$state[nb][[1]][[virys_type]] == 1 ) v = v + 1 #V(G)$activity[j] * V(G)$activity[nb];
          }
          qqi = qq * v; # compute rate q_ij
          qi[j] = qqi
        }
        if (qqi > 0){
          alive <- TRUE;  # Process is still alive
        }
      }
      else{
        if(V(G)$state[j][[1]]$susceptible == 1 & V(G)$state[j][[1]][[imunized]] == 0){
          qi[j] = qqi
        }
      }
    }
  }
  else{
    #ÏÎÄÓÌÀÒÈ ßÊ ÏÎÌ²ÍßÒÈ Ì²ÑÖßÌÈ (ÇÀÏÓÑÊÀÒÈ ÖÈÊË ÄËß ²ÍÔ²ÊÎÂÀÍÈÕ ÂÓÇË²Â ² ØÓÊÀÒÈ ÉÌÎÂ²ÐÍ²ÑÒÜ ÇÀÐÀÇÈÒÈ ÑÓÑ²Ä²Â)
    for (j in 1:vcount(G))
    {
      if (V(G)$state[j][[1]]$susceptible == 1 & V(G)$state[j][[1]][[imunized]] == 0)
      {
        # count incoming flights from infected neighbors;
        v=0;
        for (nb in neighbors(G,V(G)[j],mode=c("in")) ){
          if (V(G)$state[nb][[1]][[virys_type]] == 1 ) v <- v + 1#V(G)$activity[j] * V(G)$activity[nb];
        }
        qi[j] <- qq * v; # compute rate q_ij
        if (qi[j] > 0) alive <- TRUE;  # Process is still alive
      }
    }
  }
  qii = sum(qi)
  
  return(qi)
}


get_time_to_next_infication <- function(G, q = 0.01, type = 1){
  virys_type = paste("infected_type", toString(type), sep = "")
  imunized = paste("immunized_type", toString(type), sep = "")
  qi <- rep(0.0, vcount(G));
  qii = 0;
  for (j in 1:vcount(G))
  {
    if (V(G)$state[j][[1]]$susceptible == 1 & V(G)$state[j][[1]][[imunized]] == 0)
    {
      # count incoming flights from infected neighbors;
      v=0;
      for (nb in neighbors(G,V(G)[j],mode=c("in")) ) 
        if (V(G)$state[nb][[1]][[virys_type]] == 1 ) v <- v + V(G)$activity[j] * V(G)$activity[nb];
        qi[j] <- q * v; # compute rate q_ij
    }
  }
  qii = sum(qi)  # Rate of leaving the current state i
  #cat("qii = ", qii, "\n")
  if(qii == 0){
    return(Inf)
  }
  return(rexp(1,qii))
}


actions <- function(Graph,Time){
  L <- take_actions(Graph, Time, 1)
  Graph <- L$Graph
  emp.actions <- L$action
  
  L <- take_actions(Graph, Time, 2)
  
  return(list(Graph = L$Graph, action = rbind(emp.actions, L$action)))
}


#ïåðåðîáèòè
take_actions <- function(Graph,Time, type){
  colors = c("#74C476", "#FB6A4A", "#DEEBF7","#E5F5E0","#FEE0D2")
  emp.action <- NULL
  infected_type = paste("infected_type", toString(type), sep = "")
  immunized_type = paste("immunized_type", toString(type), sep = "")
  dead_type = paste("dead_v", toString(type), sep = "")
  
  
  for(i in 1:vcount(Graph)){
    if (V(Graph)$state[i][[1]][[infected_type]] == 1 & V(Graph)$time_of_death[i] <= Time){
      #V(Graph)$contagion[i] = 0
      V(Graph)$state[i][[1]][[dead_type]] = 1
      V(Graph)$state[i][[1]][[infected_type]] = 0
      V(Graph)$state[i][[1]][['susceptible']] = 0
      
      Graph <- delete_edges(Graph,incident(Graph, i))
      emp.action <- save_action(Graph, i, "dead", Time, type)
      V(Graph)$color[i] = colors[3+type]
    }
    
    if (V(Graph)$state[i][[1]][[infected_type]] == 1 & V(Graph)$treatment_time[i] <= Time){
      V(Graph)$state[i][[1]][[infected_type]] = 0
      V(Graph)$state[i][[1]][['susceptible']] = 1
      V(Graph)$state[i][[1]][[immunized_type]] = 1
      V(Graph)$contagion[i] = 0
      
      #wrond!!! for one virus edges are not deleting
      # if(V(Graph)$state[i][[1]][["immunized_type1"]] == 1 & V(Graph)$state[i][[1]][["immunized_type2"]] == 1){
      #   V(Graph)$color[i] = colors[3] 
      #   Graph <- delete_edges(Graph,incident(Graph, i))
      # }
      if(V(Graph)$state[i][[1]][["immunized_type1"]] == 1){
        V(Graph)$color[i] = colors[3]
        Graph <- delete_edges(Graph,incident(Graph, i))
        V(Graph)$state[i][[1]][['susceptible']] = 0
      }
      else{
        V(Graph)$color[i] = colors[type]
      }
      
      #ïåðåðîáèòè
      #Graph <- delete_edges(Graph,incident(Graph, i))
      emp.action <- save_action(Graph, i, "treatment", Time, type)
    }
    
    # if (V(Graph)$state[i][[infected_type]] == 1 & V(Graph)$incubation_period[i] <= Time){
    #   V(Graph)$contagion[i] = 1
    #   V(Graph)$incubation_period[i] = Inf
    #   emp.action <- save_action(Graph, i, "end incubation", Time)
    # }
    # 
    # if (V(Graph)$state[i] == 1 & V(Graph)$time_of_contagion[i] <= Time){
    #   V(Graph)$contagion[i] = 0
    #   V(Graph)$time_of_contagion[i] = Inf
    #   emp.action <- save_action(Graph, i, "end contagion", Time)
    # }
  }
  return(list(Graph = Graph, action = emp.action))
}


save_action <- function(G, j, action, Time, virus_type){
  emp.data_actions <- data.frame(
    "time" = c(as.double(Time * 1.0)), 
    "action" = c(action),
    "age_interval" = c(V(G)$age[j]),
    "gender" = c(V(G)$gender[j]),
    "virus_type" = virus_type
  )
  return(emp.data_actions)
}


get_prob_die <- function(age, gender, death_rate_type = "same"){
  avg_death_rate = 0.01820518643617639 + 0.2
  if(death_rate_type == "same"){
    return(avg_death_rate)
  }
  if(gender == "M"){
    data <- read.csv(file ="dataset_ukraine/men_total_data.csv")
  }
  else{
    data <- read.csv(file ="dataset_ukraine/women_total_data.csv")
  }
  d = data[data["person_age_group"] == age]
  names(d) <- names(data)
  options(digits=8)
  return(as.double(d["p_death_if_infected"]) + 0.2)
}


amount_of_infected <- function(G, type = 1){
  infected_type = paste("infected_type", toString(type), sep = "")
  amount = 0
  for(i in 1:vcount(G)){
    if (V(G)$state[i][[1]][[infected_type]] == 1){
      amount = amount + 1
    }
  }
  return(amount)
}


amount_of_cured <- function(G, type = 1){
  immunized_type = paste("immunized_type", toString(type), sep = "")
  amount = 0
  for(i in 1:vcount(G)){
    if (V(G)$state[i][[1]][[immunized_type]] == 1){
      amount = amount + 1
    }
  }
  return(amount)
}

amount_of_dead <- function(G, type = 1){
  dead_type = paste("dead_v", toString(type), sep = "")
  amount = 0
  for(i in 1:vcount(G)){
    if (V(G)$state[i][[1]][[dead_type]] == 1){
      amount = amount + 1
    }
  }
  return(amount)
}


amount_of_contagions <- function(G, type = 1){
  amount = 0
  for(i in 1:vcount(G)){
    if (V(G)$contagion[i] == 1){
      amount = amount + 1
    }
  }
  return(amount)
}


amount_of_susceptible <- function(G, type = 1){
  immunized_type = paste("immunized_type", toString(type), sep = "")
  amount = 0
  for(i in 1:vcount(G)){
    if (V(G)$state[i][[1]]$susceptible == 1 & V(G)$state[i][[1]][[immunized_type]] == 0){
      amount = amount + 1
    }
  }
  return(amount)
}


critically_infected <- function(G, type = 1, death_rate_type = "same"){
  infected_type = paste("infected_type", toString(type), sep = "")
  amount = 0
  for(i in 1:vcount(G)){
    if (V(G)$state[i][[1]][[infected_type]] == 1){
      amount = amount + get_prob_die(V(G)$age[i], V(G)$gender[i], death_rate_type)
    }
  }
  return(amount)
}


infication <-function(G, Time, type = 1, n = 1, death_rate_type = "same"){
  colors = c("#31A354", "#DE2D26")
  #cat("virus_type = ", type, "\n")
  infected = c()
  i = 1
  
  virys_type = ""
  imunized = ""
  if (type == 1){
    virys_type = 'infected_type1'
    imunized = 'immunized_type1'
  }
  else if(type == 2){
    virys_type = 'infected_type2'
    imunized = 'immunized_type2'
  }
  
  
  while(i <= n){
    r = 0
    while(TRUE){
      r = as.integer(runif (1, 1, vcount(G)))
      if(V(G)$state[r][[1]][['susceptible']] == 1 & V(G)$state[r][[1]][[imunized]] == 0){
        break
      }
    }
    #cat("infication person ", r)
    
    infected[i] = r
    V(G)$state[r][[1]][[virys_type]] = 1; # vertex i gets infected
    V(G)$state[r][[1]][['susceptible']] = 0;
    V(G)$time[r] = Time;
    V(G)$color[r] = colors[type];
    V(G)$incubation_period[r] <- Time + t_incubation
    V(G)$time_of_contagion[r] <- Time + t_incubation + t_contagion
    V(G)$treatment_time[r] <- Time + t_incubation + t_treatment
    V(G)$contagion[r] = 1
    if(runif(1) <= get_prob_die(V(G)$age[r], V(G)$gender[r], death_rate_type)){
      V(G)$time_of_death[r] <- Time + t_incubation + t_death
    }
    i = i+1
    
  }
  return(G)
}


create_graph <- function(size, type, network_param_one = NULL, network_param_two = NULL){
  if(type == "compleet" | type == "compleet_same"){
    return(compleet_graph(size))
  }
  if(type == "barabasi"){
    return(BA_graph(size, amount_of_adges = network_param_one))
  }
  if(type == "smallworld"){
    return(SW_graph(size, amount_of_adges = network_param_one, prob_reconnection = network_param_two))
  }
  if(type == "erdos_renyi"){
    return(EG_graph(size, amount_of_edges = network_param_one))
  }
  
  return(NULL)
}


#DeltaT_infication - vector
new_time <- function(DeltaT_infication, DeltaT_death, time_step, dt){
  m = min(min(DeltaT_infication), time_step, dt)
  return(list(t=m, dti=DeltaT_infication - m, ts=time_step - m))
}


simulation <- function(size, number_of_infected, virus_types, infection_rates, dt, t, 
                       network_type = "compleet", file_name = "1", folder = "output_data", 
                       death_rate_type = "same", network_param_one = NULL, network_param_two = NULL){
  cat("virus types : ", virus_types, "\n")
  #cat(infection_rates)
  
  Graph <- create_graph(size, network_type, network_param_one, network_param_two)
  Time <- 0
  
  DeltaT_death <- 7 # when?
  time_step <- 0
  
  DeltaT_infication = c()
  for(type in virus_types){
    cat("type = ", type, "\n")
    Graph <- infication(Graph, Time, type, number_of_infected, death_rate_type)
    DeltaT_infication[type] = get_time_to_next_infication(Graph, infection_rates[type], type)
    cat("di = ", DeltaT_infication[type], "\n")
  }
  #cat("DeltaT_infication = ", DeltaT_infication,"\n")
  #readline()
  
  emp.data <- data.frame(
    "time" = c(), 
    "infected" = c(),
    #"cured" = c(),
    "dead" = c(),
    #"contagious" = c(),
    "critically_infected" = c(),
    "susceptible" = c(),
    "virus_type" = c()
  )
  
  emp.data_actions <- data.frame(
    "time" = c(),
    "action" = c(),
    "age_interval" = c(),
    "gender" = c()
  )
  
  #cat("size = ", object.size(Graph), "\n")
  
  while (Time < t) {
    #plot(Graph,layout=layout.circle,vertex.label=V(Graph)$names)
    
    #plot(Graph,vertex.label=V(Graph)$names,  layout=layout.kamada.kawai)
    
    times <- new_time(DeltaT_infication, DeltaT_death, time_step, dt)
    Time <- Time + times$t
    DeltaT_infication <- times$dti
    time_step <- times$ts
    #cat("now : ", Time, "\n")
    
    for(i in virus_types){
      #cat(i)
      if(DeltaT_infication[i] == 0){
        #cat("infication virus type ", i, "\n")
        L <-CTMC(Graph, infection_rates[i], Time - (Time %% dt) + DeltaT_infication[i], type = i, death_rate_type = death_rate_type, network_type = network_type); # simulate next event, lambda=0.001
        Graph <- L$first;
        DeltaT_infication[i] = L$second;
        emp.data_actions <- rbind(emp.data_actions,L$third)
        #cat("DeltaT_infication for virus type ", i, "  = ", DeltaT_infication[i],"\n")
      }
    }
    
    if(time_step == 0){
      L <- actions(Graph, Time)
      Graph <- L$Graph
      emp.data_actions <- rbind(emp.data_actions,L$action)
      for(i in virus_types){
        emp.new_data <- data.frame(
          time = c(Time),
          infected = c(amount_of_infected(Graph, type = i)),
          #cured = c(amount_of_cured(Graph, type = i)),
          dead = c(amount_of_dead(Graph, type = i)),
          #contagious = c(amount_of_contagions(Graph, type = i)),
          critically_infected = c(critically_infected(Graph, type = i, death_rate_type = death_rate_type)),
          susceptible = c(amount_of_susceptible(Graph, type = i)),
          virus_type = c(i)
        )
        emp.data <- rbind(emp.data,emp.new_data)
      }
      time_step <- dt
    }
  }
  #print(emp.data)
  write.csv(emp.data, paste(paste(paste(folder,"data", sep = "/"),file_name, sep = "_"), "csv", sep = "."))
  write.csv(emp.data_actions, paste(paste(paste(folder,"data_actions", sep = "/"),file_name, sep = "_"), "csv", sep = "."))
  #write.csv(vcount(Graph), file=paste(folder,"size.txt", sep = "/"))
  
  #gc(verbose = getOption("verbose"))
  
  #write(line,file="myfile.txt",append=TRUE)
}

args <- commandArgs(trailingOnly = TRUE)
cat("args : ", args, "\n")
size = as.integer(args[1])
start_infected = as.integer(args[2])
#treatment_time = as.integer(args[3])
time_max = as.integer(args[3])
options(digits=10)
time_step = as.double(args[5])
file_name = as.integer(args[6])
if(!dir.exists(args[7])){
  dir.create(args[7]) 
}
#dir.create(paste(args[7], file_name, sep = "/"))
folder = args[7]
network_type = args[8]
#setwd(toString(args[7]))

#ïåðåðîáèòè íà âåêòîð
infection_rates = as.double(unlist(strsplit(args[4]," ")))
virus_types = as.integer(unlist(strsplit(args[9]," ")))
death_rate_type = args[10]
network_param_one = args[11]
network_param_two = as.double(args[12])


# !!! infection rate must depend on population
#simulation <- function(size, number_of_infected, c(virus_types), c(infection_rates), dt, t, network_type = "compleet", file_name = "1", folder = "output_data")
print(Sys.time())
if (!file.exists(paste(paste(paste(folder,"data", sep = "/"),file_name, sep = "_"), "csv", sep = "."))){
  simulation (size, start_infected, virus_types, infection_rates, time_step, time_max, 
              network_type, file_name, folder, death_rate_type, network_param_one, network_param_two)
}
#simulation(size = 100, number_of_infected = 1, virus_types = c(1), infection_rates = c(0.5), dt=1, t=100, network_type = "erdos_renyi", file_name = "1", folder = "output_data", death_rate_type = "same", network_param_one = 200, network_param_two = NULL)
# 
# gg <- create_graph(100, "erdos_renyi", network_param_one = 300)
# plot(gg)
# V(gg)$degree <- degree(gg)
# sum(V(gg)$degree==0)
#degree(gg)

#q = 0.01
#start.time <- Sys.time()
#simulation(50, 1, c(1), c(q), 1, 100)
#cat("\n---------------------\n\ntime : ", Sys.time() - start.time)
#cat(args[1])



while(runif(1) != 1){

}
print("hello")