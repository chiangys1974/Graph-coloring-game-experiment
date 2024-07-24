library("ggplot2")

#-----------------
# Edge list data
# Be sure to save the edge list data as "tab-delimited" text
# Be careful of the correct file names of the data below

Edge_Dyna=unlist(scan("C:/Temp/Edge_Data_Dyna.txt",what = list(""))) # get edge data (by actor)
Edge_Stat=unlist(scan("C:/Temp/Edge_Data_Stat.txt",what = list(""))) # get edge data (by actor)

Behavior_Dyna=read.table("C:/Temp/Color_Game_Dyna.txt",header=TRUE)
Behavior_Stat=read.table("C:/Temp/Color_Game_Stat.txt",header=TRUE)
Player_ID_Dyna = Behavior_Dyna$session_code_n*100+Behavior_Dyna$id_in_group
Player_ID_Stat = Behavior_Stat$session_code_n*100+Behavior_Stat$id_in_group

#-------------------------------------------------------------------------------
## Sort out and analyze players' scores
# Loop over 15 rounds (the maximum number of rounds implemented)
# options(warn=2) in case of encountering "warnings", this will break the loop so as to check errors
#-------------------------------------------------------------------------------
S = NULL
for (r in 1:15){ 
  for (player in unique(Player_ID_Dyna)){ # either Dyna or Stat will work, as each participant took part in both treatments in the experiment
    session = floor(player/100)
    # Dynamic
    if(length(which(Behavior_Dyna$round_number==r & Player_ID_Dyna==player))>0){
      scoreDyna = Behavior_Dyna[which(Behavior_Dyna$round_number==r & Player_ID_Dyna==player), 5]
      actionDyna = Behavior_Dyna[which(Behavior_Dyna$round_number==r & Player_ID_Dyna==player), 6]
    }else {
      scoreDyna = 100
      actionDyna = 99 # missing values
    } # perfect score
    # Static
    if(length(which(Behavior_Stat$round_number==r & Player_ID_Stat==player))>0){
      scoreStat = Behavior_Stat[which(Behavior_Stat$round_number==r & Player_ID_Stat==player), 5]
      actionStat = Behavior_Stat[which(Behavior_Stat$round_number==r & Player_ID_Stat==player), 6]
    }else {
      scoreStat = 100
      actionStat = 99 # missing values
      } # perfect score
    
    S = rbind(S,c(r, scoreDyna, scoreStat, player,session, actionDyna, actionStat))
  }
}

# check percentage of perfect scores in a session before time limit (r=15)
max_round = 15
session_score = NULL
for (s in unique(S[,5])){# 5th column in S is session ID
  for (r in 2:max_round){
    session_score = rbind(session_score ,c(r,s,mean(S[which(S[,1]==r & S[,5]==s),3]))) # the 3rd column in S is scoreStat
  }}
# Percentage of all completion session_wise ...
length(unique(session_score[which(session_score[,1]<15 & session_score[,3]==100),1]))/length(unique(S[,5]))



### Figue 1: Plotting the distribution of scores over rounds
SS = NULL
#static networks
for (i in 2:max_round){# loop over rounds from round 2
  s_mean_static = mean(S[which(S[,1]==i),3]) # the 3rd column in S is scoreStat
  n_size = length(which(S[,1]==i)) # the 1st column in S is round number
  s_lower_static = s_mean_static-qt(0.975,df=n_size-1)*(sd(S[which(S[,1]==i),3])/sqrt(n_size))
  s_upper_static = s_mean_static+qt(0.975,df=n_size-1)*(sd(S[which(S[,1]==i),3])/sqrt(n_size))
  SS = rbind(SS, c(i, s_mean_static, s_lower_static, s_upper_static))
}
#dynamic networks
for (i in 2:15){
  s_mean_dynamic = mean(S[which(S[,1]==i),2]) # the 2nd column in S is scoreDyna
  n_size = length(which(S[,1]==i))
  s_lower_dynamic = s_mean_dynamic-qt(0.975,df=n_size-1)*(sd(S[which(S[,1]==i),2])/sqrt(n_size))
  s_upper_dynamic = s_mean_dynamic+qt(0.975,df=n_size-1)*(sd(S[which(S[,1]==i),2])/sqrt(n_size))
  SS = rbind(SS, c(i, s_mean_dynamic, s_lower_dynamic, s_upper_dynamic))
}

### --- Here, we add the simulation data from the random agent-based model 
###     programmed at the end of this code page
SS=rbind(SS,naive_model)
## ----------------------------------------------------------------------

SS=data.frame(SS,c(rep("Static Network",14),rep("Dynamic Network",14), rep("Random Simulation",14))) # round 2,3,...15
names(SS)[1]<-"Round"
names(SS)[2]<-"Score"
names(SS)[3]<-"CI_lower"
names(SS)[4]<-"CI_upper"
names(SS)[5]<-"treatment"

# CI plot using ribbon plotting
library(magrittr)
SS %>% 
  ggplot(aes(Round, Score, fill = treatment))+
  theme(axis.line = element_line(color="gray"),
        axis.title=element_text(size=16,face="bold"))+
  geom_line(color="black")+
  geom_ribbon(aes(ymin = CI_lower,
                  ymax = CI_upper,
                  group=treatment),
              alpha=0.5) +
  scale_fill_manual(values=c("steelblue4","violetred4","wheat4"))


### Two-way repeated measures ANOVA
# Re-arrange the data: 
TRM_ANOVA = cbind(rep(S[,1],2),c(S[,2],S[,3]),c(rep(1,nrow(S)),rep(2,nrow(S))),
                  rep(S[,4],2))
TRM_ANOVA = data.frame(TRM_ANOVA)
names(TRM_ANOVA)[1]<-"Round"
names(TRM_ANOVA)[2]<-"Scores"
names(TRM_ANOVA)[3]<-"Treatment"
names(TRM_ANOVA)[4]<-"SubID"

attach(TRM_ANOVA)
model.aov <- aov(Scores ~ 
                   Round * Treatment + 
                   Error(SubID/(Round * Treatment)))
summary(model.aov)


#------------------------------------------------------------------
# Figure 2 ->
# Stacked barplot to show the temporal distribution of action types
#------------------------------------------------------------------
Stacked_data=NULL
for (r in 1:max_round){
  ccolor=length(which(S[,1]==r & S[,7]==1)) # S[,7]==1 indicates dynamic network treatment in S containing the action of changing color = 1 (see codebook.xls)
  #cneighbor=length(which(S[,1]==r & S[,6]==5)) 
  sput=length(which(S[,1]==r & S[,7]==3))
  total = ccolor+sput
  Stacked_data=rbind(Stacked_data,c(r,floor(100*ccolor/total)))
  #Stacked_data=rbind(Stacked_data,c(r,floor(100*cneighbor/total)))
  Stacked_data=rbind(Stacked_data,c(r,100-floor(100*ccolor/total)))
}
Stacked_data=data.frame(Stacked_data)
Stacked_data=cbind(Stacked_data,rep(c("Change_Color","Stay_Put"),max_round))
names(Stacked_data) = c("Round","Percentage","Action_Type")
ggplot() + 
  geom_bar(aes(y = Percentage, x = Round, fill = Action_Type), data = Stacked_data,
           stat="identity")+
  scale_fill_manual(values=c('burlywood4', 'gray45'))


#----------------------------------------------------------------
# Round-by-round analysis on 'dynamic networks' treatment 
#----------------------------------------------------------------
## Round by round analysis on 'dynamic networks' treatment
library(sna)
library(network)
# Set off data folders
Behavior_ = Behavior_Dyna
Edge_ = Edge_Dyna
Data=NULL
Sessions = unique(Behavior_$session_code_n) # we use Dynamic Network treatment data as a handy source; same as Static Network treatment
Net_Level_Data = NULL
Add_neighbor_list = NULL
Delete_neighbor_list = NULL

for (s in Sessions){
  session_data = Behavior_[which(Behavior_$session_code_n == s),]
  session_size = max(session_data $ id_in_group)
  max_round_ =  max(session_data $ round_number) # max_round_ is session dependent
  who_a_hundred = which(session_data[which(session_data $ round_number == max_round_),5]==100)# check players' scores
  if (length(who_a_hundred) == session_size){ # if everyone scores 100
    max_round_ = max_round_-1 # in fact, the max_round_ number should be the previous round
  }
  
  for (r in 1:max_round_){
    subdata = Behavior_[which(Behavior_$session_code_n == s & Behavior_$round_number == r),]
    
    # normal cases -------------------------------------------
    if (r < max_round_){
      
      adjacency_m = matrix(rep(0,session_size^2),nrow = session_size,  ncol = session_size)
      adjacency_round_start = matrix(rep(0,session_size^2),nrow = session_size,  ncol = session_size)
      
      for (player in 1: session_size){
        
        # Round_Start Neighbors
        index = which(Behavior_$session_code_n == s &
                        Behavior_$round_number == r &
                        Behavior_$id_in_group == player) # this index applies to the grand data (Behavioral data)
        neighbors_start = strtoi(unlist(strsplit(Edge_[index],","))) # transform edge list data from string to numeric
        
        # Round_End Neighbors
        index = which(Behavior_$session_code_n == s &
                        Behavior_$round_number == r+1 & # KEY: we draw on database to extract list of neighbors reported at the beginning of the round t+1
                        Behavior_$id_in_group == player) 
        neighbors = strtoi(unlist(strsplit(Edge_[index],",")))
        
        adjacency_m[player, neighbors]=1
        adjacency_round_start[player, neighbors_start]=1
        
      }
      
      ##--- Processing network level attributes:
      node_degree = rowSums(adjacency_round_start)
      degree_inequality = sd(node_degree)/(session_size-1)
      library(igraph)
      Alpha = fit_power_law(node_degree)$alpha # Power-law scale
      detach(package:igraph)
      net=network(adjacency_round_start, matrix.type="adjacency")
      clustering_mean=mean(gtrans(net))
      closeness_mean = mean(closeness(net))
      
      Net_Level_Data = rbind(Net_Level_Data,c(s, r, Alpha, clustering_mean, closeness_mean,degree_inequality))
      
      
    } # if < max_round_
    
    
    # Special case: Last round
    if (r == max_round_){    
      
      adjacency_round_start = matrix(rep(0,session_size^2),nrow = session_size,  ncol = session_size)
      
      for (agent in 1:session_size){   
        index = which(Behavior_$session_code_n == s &
                        Behavior_$round_number == r & # KEY: this is the beginning of the round
                        Behavior_$id_in_group == agent)
        extant_neighbors = strtoi(unlist(strsplit(Edge_[index],",")))
        adjacency_round_start[agent, extant_neighbors]=1
      } # agent
      
      adjacency_m = adjacency_round_start
      
      for (agent in 1:session_size){
        #First, delete a neighbor
        deleted_neighbor = subdata$delete_player_n[which(subdata$id_in_group == agent)]
        if (deleted_neighbor!=996){
          adjacency_m[agent, deleted_neighbor] = 0
          adjacency_m[deleted_neighbor, agent] = 0
        }
        
        #Then, add a new neighbor
        added_neighbor = subdata$add_player_n[which(subdata$id_in_group == player)]
        if (added_neighbor!=996){
          adjacency_m[agent, added_neighbor] = 1
          adjacency_m[added_neighbor, agent] = 1
        }
        
      }# for agent
      
      ## Processing network level attributes: 'BEGINNING' of last round
      node_degree = rowSums(adjacency_round_start)
      degree_inequality = sd(node_degree)/(session_size-1)
      library(igraph)
      Alpha = fit_power_law(node_degree)$alpha
      detach(package:igraph)
      net=network(adjacency_round_start, matrix.type="adjacency")
      clustering_mean=mean(gtrans(net))
      closeness_mean = mean(closeness(net))
      
      Net_Level_Data = rbind(Net_Level_Data,c(s, r, Alpha, clustering_mean, closeness_mean,degree_inequality))
      
      ## Processing network level attributes: 'END' of last round
      node_degree = rowSums(adjacency_m)
      degree_inequality = sd(node_degree)/(session_size-1)
      library(igraph)
      Alpha = fit_power_law(node_degree)$alpha
      detach(package:igraph)
      net=network(adjacency_m,matrix.type="adjacency")
      clustering_mean=mean(gtrans(net))
      closeness_mean = mean(closeness(net))
      
      Net_Level_Data = rbind(Net_Level_Data,c(s, r+1, Alpha, clustering_mean, closeness_mean,degree_inequality))
      
    } # if r = max_round_
    
    
    ## Track player's actions and scores
    for (player in 1:session_size){
      neighbors = which(adjacency_round_start[player, ] == 1)
      non_neighbors = which(adjacency_round_start[player, ] == 0)
      non_neighbors_ = non_neighbors[which(non_neighbors!=player)]
      action = subdata$actionType_n[which(subdata$id_in_group == player)]
      
      # if change neighbors
      if (action==5){ # 5 is coded as changing neighbor in the dataset
        ## whom to delete?
        for (g in neighbors){ 
        ## ----- track neighbors' records
          connectedness_neighbor = sum(adjacency_round_start[g,])
          score_neighbor = subdata$score[which(subdata$id_in_group == g)]
          action_history_neighbor = Behavior_[which(Behavior_$session_code_n == s 
                                                    & Behavior_$round_number < r 
                                                    & Behavior_$id_in_group == g),]
          index = which(subdata$id_in_group == player) # row index of the focal player
          if (g == subdata$delete_player_n[index]){
            deleted = 1
          }else{deleted = 0}
          
          # check neighbors' past history: 1-> changing color and 5-> changing neighbor, as were coded in the data set
          change_color_percent=length(which(action_history_neighbor$actionType_n==1))/(length(action_history_neighbor$actionType_n)+10^-5) # adding a trivial value to make the nominator non-zero
          change_neighbor_percent=length(which(action_history_neighbor$actionType_n==5))/(length(action_history_neighbor$actionType_n)+10^-5)
          
          
          Delete_neighbor_list = rbind(Delete_neighbor_list,c(s,r,g,deleted,
                                                              connectedness_neighbor,
                                                              score_neighbor,
                                                              change_color_percent,
                                                              change_neighbor_percent
                                                              ))
        }
      
      ## whom to link to?
      for (h in non_neighbors){ 
        ## ----- track neighbors' records
        connectedness_non_neighbor = sum(adjacency_round_start[h,])
        score_non_neighbor = subdata$score[which(subdata$id_in_group == h)]
        action_history_non_neighbor = Behavior_[which(Behavior_$session_code_n == s 
                                                  & Behavior_$round_number < r 
                                                  & Behavior_$id_in_group == h),]
        index = which(subdata$id_in_group == player) # row index of the focal player
        if (h == subdata$add_player_n[index]){
          added = 1
        }else{added = 0}
        
        # check non_neighbors' past history
        change_color_percent=length(which(action_history_non_neighbor$actionType_n==1))/(length(action_history_non_neighbor$actionType_n)+10^-5)
        change_neighbor_percent=length(which(action_history_non_neighbor$actionType_n==5))/(length(action_history_non_neighbor$actionType_n)+10^-5)
        
        Add_neighbor_list = rbind(Add_neighbor_list,c(s,r,h,added,
                                                            connectedness_non_neighbor,
                                                            score_non_neighbor,
                                                      change_color_percent,
                                                      change_neighbor_percent
                                                      
        ))
      }
      }
      
      # compute players' scores
      if (action <=1){ # 1-> changing color coded in the data set
        player_color = subdata$new_color_n[which(subdata$id_in_group == player)]
      }else{player_color = subdata$color_n[which(subdata$id_in_group == player)]}
      neighbor_colors = NULL
      for (k in neighbors){
        if (subdata$actionType_n[which(subdata$id_in_group == k)]==1){
          nc = subdata$new_color_n[subdata$id_in_group == k] # neighbors who changed color in the current round
        }else{nc = subdata$color_n[subdata$id_in_group == k]}
        
        neighbor_colors = c(neighbor_colors , nc)
      }
      
      player_score = round(100*(length(which(neighbor_colors != player_color))/ length(neighbors)))
      # this is the score that a player earned in the current round, which would be displayed at the beginning of next round in the experiment
      
      player_prior_score =  subdata$score[which(subdata$id_in_group == player)]
      
      Data = rbind(Data,c(s,r,player,action, player_score, player_prior_score, length(neighbors)))
      
    } # for player
    
  } # looping over rounds
  
} # looping over sessions

#-----------------------------------------------------
# Figure 3: evolution of network topology over rounds
#-----------------------------------------------------
# First, we deal with extreme values (Inf)
Net_Level_Data[which(Net_Level_Data=='Inf')]=NA
NetT = NULL
for (k in 3:5){ # three network attributes: alpha, clustering, path-distance, k is the column index in the Net_Level_Data
for (i in unique(Net_Level_Data[,2])){ # 2 -> 2nd column in the data is round number i 
  mean_static = mean(Net_Level_Data[which(Net_Level_Data[,2]==i),k],na.rm=TRUE) # Alpha value
  n_size = length(which(Net_Level_Data[,2]==i))
  if (n_size==1){
    SDD = 0
    QT=0
  }else{
    SDD = sd(Net_Level_Data[which(Net_Level_Data[,2]==i),k],na.rm=TRUE)
    QT=qt(0.975,df=n_size-1)}
  lower_static = mean_static-QT*(SDD/sqrt(n_size))
  upper_static = mean_static+QT*(SDD/sqrt(n_size))
  NetT  = rbind(NetT , c(i, mean_static, lower_static, upper_static))
}}

NetT=data.frame(NetT,c(rep("Alpha Value",length(unique(Net_Level_Data[,2]))),
                       rep("Clustering Coefficient",length(unique(Net_Level_Data[,2]))), 
                       rep("Closeness Centrality",length(unique(Net_Level_Data[,2]))))) # round 1,2,...16
names(NetT)[1]<-"Round"
names(NetT)[2]<-"Mean_Value"
names(NetT)[3]<-"CI_lower"
names(NetT)[4]<-"CI_upper"
names(NetT)[5]<-"Network_Attribute"

# CI plot using ribbon plotting
# panel (a): clustering coefficients and closeness centrality
NetT1 = NetT[17:48,] # we separate the plot into two sub-plots
library(magrittr)

# 1st plot: clustering and closeness centrality
plot_left= ggplot(NetT1,aes(Round, Mean_Value, fill = Network_Attribute))+
  theme(axis.line = element_line(color="gray"),
        axis.title=element_text(size=14,face="bold"))+
  labs(y="Mean Values")+
  geom_line(color="black")+
  geom_ribbon(aes(ymin = CI_lower,
                  ymax = CI_upper,
                  group=Network_Attribute),
              alpha=0.5) +
  scale_fill_manual(values=c("wheat4","violetred4"))

# 2nd plot: power-law distribution fit (alpha values)
NetT2 = NetT[1:16,]
plot_right=ggplot(NetT2,aes(Round, Mean_Value))+ 
  theme(axis.line = element_line(color="gray"),
        axis.title=element_text(size=14,face="bold"))+
  labs(y="Alpha Values")+
  geom_line(color="darkblue")+
  geom_ribbon(aes(ymin = CI_lower,
                  ymax = CI_upper),
              alpha=0.5,
              fill="skyblue3") 

# Combine two subplots
library("ggpubr")
ggarrange(plot_left,plot_right,
          labels = c("a", "b"),
          ncol = 2, nrow = 1)




#---------------------------------------------------------------
#Running multinominal model on participants' choices of actions -
#---------------------------------------------------------------
data_mul=cbind(as.factor(Data[,4]),Data[,6],Data[,7],Data[,1]*100+Data[,3])
# see 'Data' above for the column indices for the corresponding variables
data_mul = data.frame(data_mul)
names(data_mul) = c("Action_Type","Score","Connectedness","Player_ID")

library(nnet)
model = multinom(as.factor(Action_Type)~Score+Connectedness,reference="1")

##--------------------------------------------------------------------------------------------
# Because conventional clustering standard error Cl function does not apply to 'nnet' object,
# here we refer to https://stackoverflow.com/questions/73136246/clustered-standard-errors-stars-and-summary-statistics-in-modelsummary-for-mul
# for a revision of the function (see below)
# function to calculate clustered standard errors
mlogit.clust <- function(model,data,variable) {
  beta <- c(t(coef(model)))
  vcov <- vcov(model)
  k <- length(beta)
  n <- nrow(data)
  max_lev <- length(model$lev)
  xmat <- model.matrix(model)
  # u is deviance residuals times model.matrix
  u <- lapply(2:max_lev, function(x)
    residuals(model, type = "response")[, x] * xmat)
  u <- do.call(cbind, u)
  m <- dim(table(data[,variable]))
  u.clust <- matrix(NA, nrow = m, ncol = k)
  fc <- factor(data[,variable])
  for (i in 1:k) {
    u.clust[, i] <- tapply(u[, i], fc, sum)
  }
  cl.vcov <- vcov %*% ((m / (m - 1)) * t(u.clust) %*% (u.clust)) %*% vcov
  return(cl.vcov = cl.vcov)
}
#-------------------------------------------------------------------------------------------------
b <- c(t(coef(model))) # regression coefficients
var <- mlogit.clust(model,data_mul,"Player_ID") # clustered variances
se <- sqrt(diag(var)) # clustered standard errors
p <- (1-pnorm(abs(b/se))) * 2 # computing p values



# Run logit model to check what kinds of players are deleted or added
# See 'Delete_neighbor_list' and 'Add_neighbor_list' above for the column indices for the corresponding variables
model_delete = glm(Delete_neighbor_list[,4]~Delete_neighbor_list[,5]+Delete_neighbor_list[,6]+
                     Delete_neighbor_list[,7]+Delete_neighbor_list[,8],family = binomial)
model_add = glm(Add_neighbor_list[,4]~Add_neighbor_list[,5]+Add_neighbor_list[,6]+
                  Add_neighbor_list[,7]+Add_neighbor_list[,8],family = binomial)

# clustering standared errors of the regression coefficients
### Clustering regression coefficients
cl   <- function(model, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- length(model$coefficients)
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(model),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
  coeftest(model, vcovCL) }

ID_add=Add_neighbor_list[,1]*1000+Add_neighbor_list[,3]
ID_delete=Delete_neighbor_list[,1]*1000+Delete_neighbor_list[,3]
# Multiply 1000 to create a unique ID for each player: Add_neighbor_list[,1] is session ID 

#-------------------------------------------------------------------------------
# typology of actors with respect to the triad of choices
#-------------------------------------------------------------------------------
Players_Type = NULL
for (s in Sessions){
  # see 'Data' above for the column indices for the corresponding variables below
  players_set = unique(Data[which(Data[,1]==s),3])
  for (p in players_set){
    index = which(Data[,1]==s & Data[,3]==p)
    total = length(index)
    choose_person = length(which(Data[index, 4] >=5 )) / total
    choose_color = length(which(Data[index, 4] <=2)) / total
    choose_idle = length(which(Data[index, 4] >=3 & Data[index, 4] <=4)) / total
    
    performance = mean(Data[index,5])
    Players_Type = rbind(Players_Type, c(s, p, performance,
                                         choose_person, choose_color, choose_idle,
                                         total))
  }
  
}

#---------------------------------------------------------------------------
# Table 2 
# Running Tobit regression for the effect of action type on game performance  
#---------------------------------------------------------------------------
library(AER)
model = tobit(Players_Type[,3]~Players_Type[,4]+Players_Type[,5],left=0,right=100)

#-----------------------------------------------------------------------
# Ternary contour plot for players' action profiles against performance-
#-----------------------------------------------------------------------
library(ggtern)
Change_Neighbor = Players_Type[,4]
Change_Color = Players_Type[,5]
Idle = Players_Type[,6]
Performance = Players_Type[,3]

# Testing the strategies in regression
summary(lm(Performance~Change_Neighbor+Idle))

# Plotting ternary presentations
  # point plot
t_data = data.frame(Change_Neighbor,Change_Color,Idle, Performance)

ggtern(data = t_data, aes(x=Change_Color,y=Change_Neighbor,z=Idle)) +
  geom_point(size=3, aes(color=Performance)) +
  theme_bw() 
+
  scale_color_gradient2(low = "green", mid = "yellow", high = "red")

  # interpolating plot
ggtern(t_data,aes(Change_Color,Change_Neighbor,Idle,value=Performance)) +
  stat_interpolate_tern(geom="polygon",
                        formula=value~x+y,
                        method=loess,n=100,
                        breaks=seq(0,100,by=1),
                        aes(fill=..level..),expand=1) +
  geom_point()



#-------------------------------------------------------------------------------
# Simulate random choices of updating neighbors in the Dynamic Network treatment
#-------------------------------------------------------------------------------
library('igraph')
Sessions = unique(Behavior_Dyna$session_code_n)
Max_Round = 15
Color_Num = 4
Num_Links = 6
Score_over_round = NULL
Break_condition=0 # for controlling break condition

for (trials in 1:100){
for (s in Sessions){
  session_data = Behavior_Stat[which(Behavior_Stat$session_code_n == s),]
  session_size = max(session_data $ id_in_group)
  color_choices = rep(1,session_size) # randomly choose color #1 as the initial condition
  scores = rep(0,session_size) # initial scores
  # initial network
  g=make_lattice(
    length = session_size,
    dim = 1,
    nei = Num_Links/2, # 6 neighbors as a total
    directed = FALSE,
    mutual = FALSE,
    circular = TRUE
  )
  M1=as.matrix(as_adjacency_matrix(g))
  
  for (r in 1:Max_Round){
    old_scores = scores
    Score_over_round = rbind(Score_over_round,cbind(rep(r,session_size),scores))
    old_net = M1
    for (agent in 1:session_size){
      # random choice of changing color, changing neighbor or doing nothing
      action = sample(seq(1,3),1) # three options: 1 = change neighbor; 2 = change color; 3 = no change
      if (action == 1){# change neighbor
        linked_ = which(M1[agent,]==1)
        unlinked_ = which(M1[agent,]==0)
        unlinked_ = unlinked_[which(unlinked_!=agent)]
        
        if (length(linked_)>1){
          cut = sample(linked_,1)
        }else{cut = linked_}
        
        if (length(unlinked_)>1){
          new_neighbor = sample(unlinked_,1) # random choice of new neighbor
        }else{new_neighbor = unlinked_}
        
        M1[agent,cut] = 0
        M1[cut,agent] = 0
        
        M1[agent,new_neighbor] = 1
        M1[new_neighbor,agent] = 1
        
        
      } else if (action == 2){ # change color
        remaining_colors = which(seq(1,Color_Num)!= color_choices[agent])
        color_choices[agent] = sample(remaining_colors,1)
      }
      
      else {}
      
      if(sum(rowSums(M1)) != Num_Links*session_size){ # this only breaks the current agent
        Break_condition=1 # set the condition so that break can continuously be applied to the next loop 'round'
        break 
      }else{Break_condition=0}
      
    }# agent
    
    if (Break_condition==1){ # this will break the round looping 
      break
    }
    
    # update scores
    scores=NULL
    for (agent in 1:session_size){
      neighbors = which(M1[agent,]==1)
      if (length(neighbors)>0){
        neighbor_colors = color_choices[neighbors]
        agent_color = color_choices[agent]
        agent_score = 100*(length(which(neighbor_colors != agent_color))/ length(neighbors))
      }else{agent_score = old_scores[agent]}
      scores=c(scores,agent_score)
    }
    
  } # round
  
} # session
}# trials

# Prepare data for plotting Figure 1
naive_model = NULL
#static networks
for (i in 2:max_round){
  s_mean_static = mean(Score_over_round[which(Score_over_round[,1]==i),2])
  n_size = length(which(Score_over_round[,1]==i))
  s_lower_static = s_mean_static-qt(0.975,df=n_size-1)*(sd(Score_over_round[which(Score_over_round[,1]==i),2])/sqrt(n_size))
  s_upper_static = s_mean_static+qt(0.975,df=n_size-1)*(sd(Score_over_round[which(Score_over_round[,1]==i),2])/sqrt(n_size))
  naive_model  = rbind(naive_model , c(i, s_mean_static, s_lower_static, s_upper_static))
}
