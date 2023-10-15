
library('data.table')
library('dplyr')
library('tidyr')
library('ggplot2')
library('scales') # visualisation
library('grid') # visualisation
library('gridExtra') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation
library('ggridges') #visualisation
library('forcats') # factor manipulation

# Multiplot Function ------------------------------------------------------

# Define multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}    

setwd("~/Desktop/UBCO-courses/Data501-data analytics/Final_Project/data")
pubg<- fread('train_V2.csv',header = T)

Ids <- pubg[,c('Id','groupId','matchId')]
summary(Ids)
glimpse(Ids)

Match_Stats <- pubg[,c('matchDuration','matchType','numGroups')]
summary(Match_Stats)
glimpse(Match_Stats)

Kill_Counts <- pubg[,c('DBNOs','kills','assists','killStreaks','headshotKills','roadKills','teamKills')]
Kill_Counts_f <- list(DBNOs = pubg[DBNOs>0,'DBNOs'],
                      kills = pubg[kills>0,'kills'],
                      assists = pubg[assists>0,'assists'],
                      killStreaks = pubg[killStreaks>0,'killStreaks'],
                      headshotKills = pubg[headshotKills>0,'headshotKills'],
                      roadKills = pubg[roadKills>0,'roadKills'],
                      teamKills = pubg[teamKills>0,'teamKills'])

sapply(Kill_Counts_f,function(x)summary(x))
glimpse(Kill_Counts)
# sapply(Kill_Counts_f,function(x)glimpse(x))

In_Game_Stats <- pubg[,c('boosts','heals','revives','vehicleDestroys','weaponsAcquired')]
In_Game_Stats_f <- list(boosts = pubg[boosts>0,'boosts'],
                       heals = pubg[heals>0,'heals'],
                       revives = pubg[revives>0,'revives'],
                       vehicleDestroys = pubg[vehicleDestroys>0,'vehicleDestroys'],
                       weaponsAcquired = pubg[weaponsAcquired>0,'weaponsAcquired'])

sapply(In_Game_Stats_f,function(x)summary(x))
glimpse(In_Game_Stats)
# sapply(In_Game_Stats_f,function(x)glimpse(x))

Distances <- pubg[,c('longestKill','swimDistance','rideDistance','walkDistance')]
Distances_f <- list(longestKill = pubg[longestKill>1,'longestKill'],
                   swimDistance = pubg[swimDistance>1,'swimDistance'],
                   rideDistance = pubg[rideDistance>1,'rideDistance'],
                   walkDistance = pubg[walkDistance>1,'walkDistance'])
sapply(Distances_f,function(x)summary(x))
summary(Distances)
glimpse(Distances)

Points <- pubg[,c('damageDealt','killPoints','winPoints','rankPoints' )]
Points_f <- list(damageDealt = pubg[damageDealt>0, 'damageDealt'],
                killPoints = pubg[killPoints>0,'killPoints'],
                winPoints = pubg[winPoints>0,'winPoints'],
                rankPoints = pubg[rankPoints>0,'rankPoints'])
sapply(Points_f,function(x)summary(x))
summary(Points)
glimpse(Points)

Performance <- pubg[,c('killPlace','maxPlace','winPlacePerc')]
summary(Performance)
glimpse(Performance)

# missing values ----------------------------------------------------------


# sum(is.na(pubg))
# subset(pubg,rowSums(is.na(pubg)) > 0)
pubg<-pubg[!rowSums(is.na(pubg)) > 0,]
sum(is.na(pubg))


# Individual features visualization ---------------------------------------


# Match Stats Group -------------------------------------------------------


pubg<- pubg %>%
  mutate(matchType = factor(matchType))

pubg <- pubg %>%
    filter(matchType %in% c('squad-fpp','duo-fpp','solo-fpp','squad','duo','solo'))

p1 <- pubg %>%  
    ggplot(aes(matchDuration)) +
    geom_density(fill = "red", bw = 1) +
    geom_vline(xintercept = 1600, linetype = 2)

pubg <- pubg %>%
  mutate(map = cut(matchDuration, 
                   breaks= c(-Inf,1600,Inf) ,
                   labels= c('map_s','map_b'))) %>%
  mutate(map = factor(map)) 

p5 <- pubg %>%
  ggplot(aes(map, fill = map)) +
  geom_bar() + 
  ggtitle('New Variable')

p2 <- pubg %>%
    ggplot(aes(matchType, fill = matchType)) +
    geom_bar(col = 'mediumvioletred' ,fill = 'mediumaquamarine' ) +
    theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size= 8, angle=45))

p3 <- pubg %>%  
    ggplot(aes(numGroups)) +
    geom_density(fill = "green", bw = 1)

p4 <- pubg %>%  
    ggplot(aes(numGroups,fill = matchType )) +
    geom_density( bw = 1, alpha = .5) +
    ggtitle('numGroups - mathcType')


layout <- matrix(c(1,1,1,1,2,2,2,3,3,4,4,5,5,5),2,7,byrow = TRUE)
multiplot(p1,p5,p2,p3,p4, layout = layout)

 



p1 <- pubg %>%  
    mutate(DBNOs = replace(DBNOs, DBNOs>quantile(DBNOs,.99),'6+')) %>%
    ggplot(aes(DBNOs, fill = DBNOs)) +
    geom_bar() +
    theme(legend.position = 'none') 

p2 <- pubg %>%  
    mutate(kills = replace(kills, kills>quantile(kills,.99),'8+')) %>%
    ggplot(aes(kills, fill = kills)) +
    geom_bar() +
    theme(legend.position = 'none') 

p3 <- pubg %>%  
    mutate(assists = replace(assists, assists>quantile(assists,.99),'4+')) %>%
    ggplot(aes(assists, fill = assists)) +
    geom_bar() +
    theme(legend.position = 'none') 

p4 <- pubg %>%  
    mutate(killStreaks = replace(killStreaks, killStreaks>quantile(killStreaks,.99),'4+')) %>%
    ggplot(aes(killStreaks, fill = killStreaks)) +
    geom_bar() +
    theme(legend.position = 'none') 

p5 <- pubg %>%  
    mutate(headshotKills = replace(headshotKills, headshotKills>quantile(headshotKills,.99),'4+')) %>%
    ggplot(aes(headshotKills, fill = headshotKills)) +
    geom_bar() +
    theme(legend.position = 'none') 

p6 <- pubg %>%  
    mutate(roadKills = replace(roadKills, roadKills>quantile(roadKills,.99),'1+')) %>%
    ggplot(aes(roadKills, fill = roadKills)) +
    geom_bar() +
    theme(legend.position = 'none') 

p7 <- pubg %>%  
    mutate(teamKills = replace(teamKills, teamKills>quantile(teamKills,.99),'2+')) %>%
    ggplot(aes(teamKills, fill = teamKills)) +
    geom_bar() +
    theme(legend.position = 'none') 

layout <- matrix(c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7),2,12,byrow=TRUE)
multiplot(p1, p2, p3, p5, p4,p6,p7, layout=layout)


pubg %>%
    group_by(kills==0) %>%
    summarise(percentage = n()/nrow(pubg)*100)

pubg %>%
    group_by(DBNOs==0) %>%
    summarise(percentage = n()/nrow(pubg)*100)


p1 <- pubg %>%  
    mutate(boosts = replace(boosts, boosts>quantile(boosts,.99),'8+')) %>%
    ggplot(aes(boosts, fill = boosts)) +
    geom_bar() +
    theme(legend.position = 'none') 

p2 <- pubg %>%  
    mutate(heals = replace(heals, heals> quantile(heals,.97),'9+')) %>%
    mutate(heals = factor(heals)) %>%
    ggplot(aes(heals, fill = heals)) +
    geom_bar() +
    theme(legend.position = 'none') 

p3 <- pubg %>% 
    mutate(revives = replace(revives, revives>quantile(revives,.99),'3+')) %>%
    ggplot(aes(revives, fill = revives)) +
    geom_bar() +
    theme(legend.position = 'none')

p4 <- pubg %>%  
    mutate(vehicleDestroys = replace(vehicleDestroys, vehicleDestroys>quantile(vehicleDestroys,.99),'1+')) %>%
    ggplot(aes(vehicleDestroys, fill = vehicleDestroys)) +
    geom_bar() +
    theme(legend.position = 'none') 


p5 <- pubg %>%  
    mutate(weaponsAcquired = replace(weaponsAcquired, weaponsAcquired>quantile(weaponsAcquired,.96),'9+')) %>%
    mutate(weaponsAcquired = factor(weaponsAcquired)) %>%
    ggplot(aes(weaponsAcquired, fill = weaponsAcquired)) +
    geom_bar() +
    geom_vline(xintercept = median(pubg$weaponsAcquired), linetype = 2) +
    theme(legend.position = 'none') 

p6 <- pubg %>%  
    filter(winPlacePerc > 0.7) %>%
    mutate(boosts = replace(boosts, boosts>quantile(boosts,.99),'up')) %>%
    ggplot(aes(boosts, fill = boosts)) +
    geom_bar() +
    theme(legend.position = 'none') +
    ggtitle('players survived \ntill the last 30%')




layout <- matrix(c(1,2,3,4,5,6),2,3,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, layout=layout)

pubg %>%
    group_by(boosts==0) %>%
    summarise(percentage = n()/nrow(pubg)*100)

pubg %>%
    group_by(vehicleDestroys==0) %>%
    summarise(percentage = n()/nrow(pubg)*100)

lk <- pubg %>%
        filter(longestKill<quantile(longestKill,.99)& longestKill >0) %>%
        summarise(median = median(longestKill))
p1 <- pubg %>% 
    filter(longestKill<quantile(longestKill,.99) & longestKill >0)%>%
    ggplot(aes(longestKill)) +
    geom_histogram(bins = 30, fill = "red", alpha = 0.7) +
    geom_vline(xintercept = lk$median,linetype = 2) 

swim <- pubg %>%
    filter(swimDistance<quantile(swimDistance,.99)& swimDistance >0) %>%
    summarise(median = median(swimDistance)) 
p2 <- pubg %>%
    filter(swimDistance<quantile(swimDistance,.99)& swimDistance >0) %>%
    ggplot(aes(swimDistance)) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
    geom_vline(xintercept = swim$median ,linetype = 2)

ride <- pubg %>%
    filter(rideDistance<quantile(rideDistance,.99)& rideDistance >0) %>%
    summarise(median = median(rideDistance)) 
p3 <- pubg %>%
    filter(rideDistance<quantile(rideDistance,.99) & rideDistance>0) %>%
    ggplot(aes(rideDistance)) +
    geom_histogram(bins = 30, fill = "darkgreen", alpha = 0.7) +
    geom_vline(xintercept = ride$median, linetype = 2)

walk <- pubg %>%
    filter(walkDistance<quantile(walkDistance,.99)) %>%
    summarise(median = median(walkDistance)) 
p4 <- pubg %>%  
    filter(walkDistance<quantile(walkDistance,.99)) %>%
    ggplot(aes(walkDistance)) +
    geom_histogram(bins = 30, fill = "orange", alpha = 0.7) +
    geom_vline(xintercept = walk$median,linetype = 2)

p5<- pubg %>%
    summarise(ride_no = length(which(rideDistance == 0)),
              ride_yes = length(which(rideDistance > 0)),
             swim_no = length(which(swimDistance == 0)),
              swim_yes = length(which(swimDistance > 0))) %>%
    gather(type, count, ride_no, ride_yes, swim_no, swim_yes) %>%
    ggplot(aes(type ,count)) +
    geom_bar(stat = 'identity' , aes(fill = type)) +
    theme(legend.position = 'none') +
    ggtitle('players swim/ride\nor not')

layout <- matrix(c(1,1,2,2,3,3,4,4,4,5,5,5),2,6,byrow=TRUE)
multiplot(p1, p2, p3, p4,p5, layout=layout)

type <- c('LongestKill','swimDistance','rideDistance','walkDistance')
median <- c(lk$median,swim$median, ride$median,walk$median)
rbind(type,median)

pubg %>%
    group_by(longestKill >27) %>%
    summarise(percentage = n()/nrow(pubg)*100)

pubg %>%
    group_by(walkDistance > 700) %>%
    summarise(percentage = n()/nrow(pubg)*100)

damage <- pubg %>%
        filter(damageDealt<quantile(damageDealt,.99)) %>%
        summarise(median = median(damageDealt))
p1 <- pubg %>% 
    filter(damageDealt<quantile(damageDealt,.99))%>%
    ggplot(aes(damageDealt)) +
    geom_histogram(bins = 30, fill = "red", alpha = 0.7) +
    geom_vline(xintercept = damage$median,linetype = 2) 


killp <- pubg %>%
        filter(killPoints<quantile(killPoints,.99) & killPoints > 900 ) %>%
        summarise(median = median(killPoints))
p2 <- pubg %>% 
    filter(killPoints<quantile(killPoints,.99) & killPoints > 900)%>%
    ggplot(aes(killPoints)) +
    geom_histogram(bins = 30, fill = "darkgreen", alpha = 0.7) +
    geom_vline(xintercept = killp$median,linetype = 2) 



winp <- pubg %>%
        filter(winPoints<quantile(winPoints,.99) & winPoints > 1250 ) %>%
        summarise(median = median(winPoints))
p3 <- pubg %>% 
    filter(winPoints<quantile(winPoints,.99) & winPoints > 1250) %>%
    ggplot(aes(winPoints)) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
    geom_vline(xintercept = winp$median,linetype = 2) +
    scale_y_sqrt()



rankp <- pubg %>%
        filter(rankPoints<quantile(rankPoints,.99) & rankPoints > 1000 ) %>%
        summarise(median = median(rankPoints))
p4 <- pubg %>% 
    filter(rankPoints<quantile(rankPoints,.99) & rankPoints > 1000)%>%
    ggplot(aes(rankPoints)) +
    geom_histogram(bins = 30, fill = "orange", alpha = 0.7) +
    geom_vline(xintercept = rankp$median,linetype = 2) +
    scale_y_sqrt()



layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)

type <- c('damageDealt' ,'killPoints' ,'winPoints' ,'rankPoints')
median <- c(damage$median , killp$median,winp$median,rankp$median)
rbind(type,median)

pubg %>%
    group_by(damageDealt == 0 ) %>%
    summarise(percentage = n()/nrow(pubg)*100)


# pubg %>%
#     group_by(killPoints == 0 ) %>%
#     summarise(percentage = n()/nrow(pubg)*100)

# pubg %>%
#     group_by(killPoints > 900 ) %>%
#     summarise(percentage = n()/nrow(pubg)*100)

# pubg %>%
#     group_by(winPoints == 0 ) %>%
#     summarise(percentage = n()/nrow(pubg)*100)

# pubg %>%
#     group_by(winPoints > 1250) %>%
#     summarise(percentage = n()/nrow(pubg)*100)

# pubg %>%
#     group_by(rankPoints <1 ) %>%
#     summarise(percentage = n()/nrow(pubg)*100)

# pubg %>%
#     group_by(rankPoints > 1000) %>%
#     summarise(percentage = n()/nrow(pubg)*100)

p1 <- pubg %>% 
    ggplot(aes(killPlace),fill = killPlace) +
    geom_vline(xintercept = median(pubg$killPlace), linetype = 2) +
    geom_histogram(bins = 50 ,fill = 'mediumaquamarine', alpha = 0.7 ) +
    theme(legend.position = 'none') 
    


p2 <- pubg %>%
    ggplot(aes(maxPlace,fill = matchType)) +
    geom_histogram(bins = 50 , alpha = 0.7 )

p4 <- pubg %>%
    ggplot(aes(numGroups,fill = matchType)) +
    geom_histogram(bins = 50 , alpha = 0.7 )


p3 <- pubg %>%  
    ggplot(aes(winPlacePerc)) +
    geom_vline(xintercept = median(pubg$winPlacePerc), linetype = 2) +
    geom_histogram(bins = 20, fill = "red", alpha = 0.7) +
    theme(legend.position = 'none') 


layout <- matrix(c(1,1,2,2,2,2,3,3,3,4,4,4),2,6,byrow=TRUE)
multiplot(p1,p3,p2,p4, layout=layout)

type <- c('killPlace' ,'winPlacePerc')
median <- c(median(pubg$killPlace),median(pubg$winPlacePerc))
rbind(type,median)

InGameStats <- pubg[,c('boosts','heals','revives','vehicleDestroys','weaponsAcquired')]
p1 <- InGameStats %>%
  gather(boosts,heals,revives,vehicleDestroys,weaponsAcquired ,key = 'Statstype',value = 'count') %>%
  mutate(Statstype = fct_relevel(factor(Statstype),"heals", after = Inf)) %>%
  ggplot(aes(count, fill = Statstype)) +
  geom_density(position = "stack",bw = .1) +
  geom_vline(xintercept = c(3,5), linetype = 2) +
  scale_x_log10(lim = c(1,100)) +
  scale_y_sqrt() +
  labs(x = "Counts", fill = "Stats type") +
  ggtitle('general player\'s performance')


pubg1 <- pubg %>%
  filter(winPlacePerc == 1)
InGameStats1 <- pubg1[,c('boosts','heals','revives','vehicleDestroys','weaponsAcquired')]
p2 <- InGameStats1 %>%
  gather(boosts,heals,revives,vehicleDestroys,weaponsAcquired ,key = 'Statstype',value = 'count') %>%
  mutate(Statstype = fct_relevel(factor(Statstype),"heals", after = Inf)) %>%
  ggplot(aes(count, fill = Statstype)) +
  geom_density(position = "stack",bw = .1) +
  geom_vline(xintercept = c(3,5), linetype = 2) +
  scale_x_log10(lim = c(1,100)) +
  scale_y_sqrt() +
  labs(x = "Counts", fill = "Stats type") +
  ggtitle('winner\'s performance')


pubg2 <- pubg %>%
  filter(winPlacePerc == 0)
InGameStats2 <- pubg2[,c('boosts','heals','revives','vehicleDestroys','weaponsAcquired')]
p3 <- InGameStats2 %>%
  gather(boosts,heals,revives,vehicleDestroys,weaponsAcquired ,key = 'Statstype',value = 'count') %>%
  mutate(Statstype = fct_relevel(factor(Statstype),"heals", after = Inf)) %>%
  ggplot(aes(count, fill = Statstype)) +
  geom_density(position = "stack",bw = .1) +
  geom_vline(xintercept = c(3,5), linetype = 2) +
  scale_x_log10(lim = c(1,100)) +
  scale_y_sqrt() +
  labs(x = "Counts", fill = "Stats type") +
  ggtitle('winPlacePerc = 0')

layout <- matrix(c(1,2,3),3,1,byrow = TRUE)
multiplot(p3,p1,p2, layout = layout)


p1 <- pubg %>%
    filter(boosts <20) %>%
    group_by(boosts) %>%
    summarise(mean = mean(winPlacePerc),
              median = median(winPlacePerc)) %>%
    gather(type, winPlacePerc, mean, median) %>%
    ggplot(aes(boosts, winPlacePerc, colour = type)) +
    geom_line() +
    geom_point() 

p2 <- pubg %>%
    filter(heals< 20) %>%
    group_by(heals) %>%
    summarise(mean = mean(winPlacePerc),
              median = median(winPlacePerc)) %>%
    gather(type, winPlacePerc, mean, median) %>%
    ggplot(aes(heals, winPlacePerc, colour = type )) +
    geom_line() +
    geom_point() 

p3 <- pubg %>%
    filter(revives< 10) %>%
    group_by(revives) %>%
    summarise(mean = mean(winPlacePerc),
              median = median(winPlacePerc)) %>%
    gather(type, winPlacePerc, mean, median) %>%
    ggplot(aes(revives, winPlacePerc, colour = type )) +
    geom_line() +
    geom_point() 

p4 <- pubg %>%
    filter(vehicleDestroys< 10) %>%
    group_by(vehicleDestroys) %>%
    summarise(mean = mean(winPlacePerc),
              median = median(winPlacePerc)) %>%
    gather(type, winPlacePerc, mean, median) %>%
    ggplot(aes(vehicleDestroys, winPlacePerc, colour = type )) +
    geom_line() +
    geom_point() 

p5 <- pubg %>%
    filter(weaponsAcquired <20) %>%
    group_by(weaponsAcquired) %>%
    summarise(mean = mean(winPlacePerc),
              median = median(winPlacePerc)) %>%
    gather(type, winPlacePerc, mean, median) %>%
    ggplot(aes(weaponsAcquired, winPlacePerc, colour = type )) +
    geom_line() +
    geom_point() 

layout = matrix(c(1,1,2,2,3,3,4,4,5,5,5,5),3,4,byrow = TRUE)
multiplot(p1,p2,p3,p4,p5, layout = layout)


p1 <- pubg %>%
    filter(DBNOs < 20) %>%
    group_by(DBNOs) %>%
    summarise(mean = mean(winPlacePerc),
              median = median(winPlacePerc)) %>%
    gather(type, winPlacePerc, mean, median) %>%
    ggplot(aes(DBNOs, winPlacePerc, colour = type )) +
    geom_line() +
    geom_point() 

p2 <- pubg %>%
    filter(kills< 20) %>%
    group_by(kills) %>%
    summarise(mean = mean(winPlacePerc),
              median = median(winPlacePerc)) %>%
    gather(type, winPlacePerc, mean, median) %>%
    ggplot(aes(kills, winPlacePerc, colour = type )) +
    geom_line() +
    geom_point() 

p3 <- pubg %>%
    filter(assists< 10) %>%
    group_by(assists) %>%
    summarise(mean = mean(winPlacePerc),
              median = median(winPlacePerc)) %>%
    gather(type, winPlacePerc, mean, median) %>%
    ggplot(aes(assists, winPlacePerc, colour = type )) +
    geom_line() +
    geom_point() 

p4 <- pubg %>%
    filter(killStreaks< 10) %>%
    group_by(killStreaks) %>%
    summarise(mean = mean(winPlacePerc),
              median = median(winPlacePerc)) %>%
    gather(type, winPlacePerc, mean, median) %>%
    ggplot(aes(killStreaks, winPlacePerc, colour = type )) +
    geom_line() +
    geom_point() 

p5 <- pubg %>%
    filter(headshotKills< 15) %>%
    group_by(headshotKills) %>%
    summarise(mean = mean(winPlacePerc),
              median = median(winPlacePerc)) %>%
    gather(type, winPlacePerc, mean, median) %>%
    ggplot(aes(headshotKills, winPlacePerc, colour = type )) +
    geom_line() +
    geom_point() 

p6 <- pubg %>%
    filter(roadKills< 10) %>%
    group_by(roadKills) %>%
    summarise(mean = mean(winPlacePerc),
              median = median(winPlacePerc)) %>%
    gather(type, winPlacePerc, mean, median) %>%
    ggplot(aes(roadKills, winPlacePerc, colour = type )) +
    geom_line() +
    geom_point() 



layout = matrix(c(1,2,3,4,5,6),3,2,byrow = TRUE)
multiplot(p1,p2,p3,p4,p5,p6, layout = layout)
