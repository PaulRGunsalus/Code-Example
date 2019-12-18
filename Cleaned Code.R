
##Data downloaded from http://www.seanlahman.com/baseball-archive/statistics/

setwd("~ set own file path here")

library(tidyverse)
library(lubridate)
library(plotly)
## Read in desired data files
appearances<-read.csv("Appearances.csv")
batting<-read.csv("Batting.csv")
salaries<-read.csv("Salaries.csv")
people<-read.csv("People.csv")
#Create Number of Teams per Year
teams<-read.csv("Teams.csv")
num.teams<-teams %>% 
  group_by(yearID) %>% 
  summarise(Teams.Number=n())
##Assign Players a position for each season
player.pos<-appearances %>% 
  group_by(playerID, yearID) %>% 
  gather(pos, G, G_p:G_rf, G_dh) %>% 
  filter(G==max(G)) %>% 
  select(yearID:G_all, pos, G) %>%
  mutate(pos = substring(as.character(pos), 3, 4)) %>%
  arrange(yearID, teamID) %>% 
  select(yearID,teamID, playerID, pos)

## Assigned Position based on which they played most in a career
career.app<-appearances %>% 
  group_by(playerID) %>% 
  summarize(G_all=sum(G_all),
            GS=sum(GS),
            G_p=sum(G_p),
            G_c=sum(G_c),
            G_1b=sum(G_1b),
            G_2b=sum(G_2b),
            G_3b=sum(G_3b),
            G_ss=sum(G_ss),
            G_lf=sum(G_lf),
            G_cf=sum(G_cf),
            G_rf=sum(G_rf),
            G_dh=sum(G_dh))

career.pos<-career.app %>% 
  group_by(playerID) %>% 
  gather(pos, G, G_p:G_rf, G_dh) %>% 
  filter(G==max(G)) %>% 
  select(playerID:G_all, pos, G) %>%
  mutate(pos = substring(as.character(pos), 3, 4)) %>%
  arrange(playerID)


## Assign names, Rookie Years, and Final Year
## and selected variables of interest
names<-people %>% 
  mutate(full.name=paste(nameFirst, nameLast, sep = " "),
         Rookie.Year=str_sub(debut,1,4),
         Final.Year=str_sub(finalGame,1,4)) %>% 
  select(playerID, full.name, Rookie.Year, Final.Year)

## Get CPI (Consumer Price Index) to Adjust Salaries for inflation
monthly_cpi <-read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
             skip = 53, header = TRUE)
## Stored cpi for later usages
write.csv(monthly_cpi, "cpi.csv")
## Reread in the monthly cpis
monthly_cpi<-read.csv("cpi.csv")

## Created a year variable
monthly_cpi$cpi_year <- year(monthly_cpi$DATE)

# Found yearly cpi
yearly_cpi <- monthly_cpi %>% 
  group_by(cpi_year) %>% 
  summarize(cpi = mean(VALUE))
yearly_cpi$adj_factor <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == 2016]

sal.cpi<-left_join(salaries, yearly_cpi, by=c("yearID"="cpi_year"))

## Get adjusted salaries to 2016
salaries.adj<-sal.cpi %>% 
  mutate(adj2016=salary/adj_factor) %>% 
  select(-cpi)

## Find Career Earnings
career.earnings<-salaries.adj %>% 
  dplyr::group_by(playerID) %>% 
  summarise(Car.Earnings=sum(salary),
            Car.Earnings.Adj=sum(adj2016))

## Find Season Stats for Each Player

season.player<- batting %>% 
  group_by(playerID, yearID) %>% 
  summarise(total_H  = sum(H,na.rm=TRUE),
            total_G  = sum(G,na.rm=TRUE),
            total_AB = sum(AB,na.rm=TRUE),
            total_R = sum(R,na.rm=TRUE),
            total_X2B =sum(X2B,na.rm = TRUE),
            total_X3B = sum(X3B, na.rm=TRUE),
            total_HR = sum(HR,na.rm=TRUE),
            total_RBI=sum(RBI,na.rm=TRUE),
            total_SB=sum(SB,na.rm=TRUE),
            total_CS=sum(CS,na.rm = TRUE),
            total_BB=sum(BB, na.rm = TRUE),
            total_SO=sum(SO, na.rm = TRUE),
            total_IBB= sum(IBB, na.rm = TRUE),
            total_HBP = sum(HBP, na.rm = TRUE),
            total_SH = sum(SH, na.rm = TRUE),
            total_SF = sum(SF, na.rm = TRUE),
            total_GIDP =sum(GIDP, na.rm = TRUE),
            Bavg= total_H/total_AB) %>% 
  mutate(total_X1B = total_H-(total_HR+total_X2B+total_X3B)) %>% 
  mutate(total_bases= total_X1B+(2*total_X2B)+(3*total_X3B)+(4*total_HR)+
           total_SB+total_BB+total_IBB+total_HBP,
         Steal.Pct=total_SB/(total_SB+total_CS))

## MLB Totals for each year
MLB.totals<- batting %>% 
  group_by(yearID) %>% 
  summarise(total_H  = sum(H,na.rm=TRUE),
            total_G  = sum(G,na.rm=TRUE),
            total_AB = sum(AB,na.rm=TRUE),
            total_R = sum(R,na.rm=TRUE),
            total_X2B =sum(X2B,na.rm = TRUE),
            total_X3B = sum(X3B, na.rm=TRUE),
            total_HR = sum(HR,na.rm=TRUE),
            total_RBI=sum(RBI,na.rm=TRUE),
            total_SB=sum(SB,na.rm=TRUE),
            total_CS=sum(CS,na.rm = TRUE),
            total_BB=sum(BB, na.rm = TRUE),
            total_SO=sum(SO, na.rm = TRUE),
            total_IBB= sum(IBB, na.rm = TRUE),
            total_HBP = sum(HBP, na.rm = TRUE),
            total_SH = sum(SH, na.rm = TRUE),
            total_SF = sum(SF, na.rm = TRUE),
            total_GIDP =sum(GIDP, na.rm = TRUE),
            Bavg= total_H/total_AB) %>% 
  mutate(total_X1B = total_H-(total_HR+total_X2B+total_X3B)) %>% 
  mutate(total_bases= total_X1B+(2*total_X2B)+(3*total_X3B)+(4*total_HR)+
           total_SB+total_BB+total_IBB+total_HBP,
         Steal.Pct=total_SB/(total_SB+total_CS))
num.players<-batting %>%
  group_by(yearID) %>% 
  summarise(Total.Players=n())

MLB.totals<-cbind(MLB.totals,num.teams[,2], num.players[,2])
MLB.totals<-as.data.frame(MLB.totals)

## Join Season Stats, Names and Positions
season.stats<-inner_join(season.player,career.pos, by="playerID")
season.stats2<-left_join(as.data.frame(season.stats), names, by="playerID")

## Join Salaries to Season Stats
salaries.seasons<-inner_join(season.stats2,
                             salaries.adj, by=c("playerID", "yearID")) %>% 
  mutate(DPB=adj2016/total_bases) %>% 
  arrange(yearID)

## Make Career Stats for each player
career.player<- batting %>% 
  group_by(playerID) %>% 
  summarise(total_H  = sum(H,na.rm=TRUE),
            total_G  = sum(G,na.rm=TRUE),
            total_AB = sum(AB,na.rm=TRUE),
            total_R = sum(R,na.rm=TRUE),
            total_X2B =sum(X2B,na.rm = TRUE),
            total_X3B = sum(X3B, na.rm=TRUE),
            total_HR = sum(HR,na.rm=TRUE),
            total_RBI=sum(RBI,na.rm=TRUE),
            total_SB=sum(SB,na.rm=TRUE),
            total_CS=sum(CS,na.rm = TRUE),
            total_BB=sum(BB, na.rm = TRUE),
            total_SO=sum(SO, na.rm = TRUE),
            total_IBB= sum(IBB, na.rm = TRUE),
            total_HBP = sum(HBP, na.rm = TRUE),
            total_SH = sum(SH, na.rm = TRUE),
            total_SF = sum(SF, na.rm = TRUE),
            total_GIDP =sum(GIDP, na.rm = TRUE),
            Bavg= total_H/total_AB) %>% 
  mutate(total_X1B = total_H-(total_HR+total_X2B+total_X3B)) %>% 
  mutate(total_bases= total_X1B+(2*total_X2B)+(3*total_X3B)+(4*total_HR)+
           total_SB+total_BB+total_IBB+total_HBP,
         Steal.Pct=total_SB/(total_SB+total_CS))

##  Join Career Stats, Names and Positions
career.stats<-inner_join(as.data.frame(career.player),career.pos, by="playerID")
career.stats2<-left_join(career.stats, names, by="playerID") 
salaries.career<-inner_join(career.stats2, career.earnings, by="playerID") %>% 
  mutate(Car.DPB=Car.Earnings.Adj/total_bases)

##Filtered selected careers of players to focus on
selected.careers<-salaries.career %>% 
  filter(pos!="p" & total_G>500  & total_AB>500) %>% 
  filter(Rookie.Year>=1985 & Final.Year <=2016)

##Then found their individual seasons where they had at least 100 Atbats
selected.players<-salaries.seasons %>% 
  dplyr::filter(playerID %in% selected.careers$playerID & total_AB >100)
## Joined data with a specific career metric to indentify each 
## individual player by other than their name
temp<-selected.careers %>% 
  select(playerID,Car.DPB)
selected.players2<-inner_join(selected.players,temp, by="playerID")

## Median value for set metric for added graph
Median<-median(selected.careers$Car.DPB)

## Here ends the data cleaning

save(season.stats2, career.stats2, salaries.seasons,
     salaries.career,selected.careers, selected.players2,
     MLB.totals,Median, file="Baseball.rdata")


## This added files for the shiny app specifically
## stat choices for MLB totals
mlb.stat.choices<-c("Hits"="total_H",
                    "Games"="total_G",
                    "At Bats"="total_AB",
                    "Batting Average"="Bavg",
                    "Runs"="total_R",
                    "Singles"="total_X1B",
                    "Doubles"="total_X2B",
                    "Triples"="total_X3B",
                    "Home Runs"="total_HR",
                    "RBI (Runs Batted In)"="total_RBI",
                    "Stolen Bases"="total_SB",
                    "Caught Stealing"="total_CS",
                    "Steal Percentage"="Steal.Pct",
                    "Walks"="total_BB",
                    "Intentional Walks"="total_IBB",
                    "Hit by Pitch"="total_HBP",
                    "Sacrifice Hits"="total_SH",
                    "Sacrifice Flies"="total_SF",
                    "Double Plays"="total_GIDP",
                    "Strike Outs"="total_SO",
                    "Bases Produced"="total_bases",
                    "Teams"="Teams.Number",
                    "Players"="Total.Players")
#Stat choices for careers
stat.choices<-c("Hits"="total_H",
                "Games"="total_G",
                "At Bats"="total_AB",
                "Batting Average"="Bavg",
                "Runs"="total_R",
                "Singles"="total_X1B",
                "Doubles"="total_X2B",
                "Triples"="total_X3B",
                "Home Runs"="total_HR",
                "RBI (Runs Batted In)"="total_RBI",
                "Stolen Bases"="total_SB",
                "Caught Stealing"="total_CS",
                "Steal Percentage"="Steal.Pct",
                "Walks"="total_BB",
                "Intentional Walks"="total_IBB",
                "Hit by Pitch"="total_HBP",
                "Sacrifice Hits"="total_SH",
                "Sacrifice Flies"="total_SF",
                "Double Plays"="total_GIDP",
                "Strike Outs"="total_SO",
                "Bases Produced"="total_bases",
                "Career Earnings"="Car.Earnings",
                "Career Earnings Adjusted"="Car.Earnings.Adj",
                "Career Dollars Per Base"="Car.DPB")
#Stored Y-variables
yvars<-c("total_H",
         "total_G",
         "total_AB",
         "Bavg",
         "total_R",
         "total_X1B",
         "total_X2B",
         "total_X3B",
         "total_HR",
         "total_RBI",
         "total_SB",
         "total_CS",
         "Steal.Pct",
         "total_BB",
         "total_IBB",
         "total_HBP",
         "total_SH",
         "total_SF",
         "total_GIDP",
         "total_SO",
         "total_bases",
         "Car.Earnings",
         "Car.Earnings.Adj",
         "Car.DPB",
         "Teams.Number",
         "Total.Players",
         "salary",
         "adj2016",
         "DPB")

#named the variables for custom label associations
names(yvars)<-c("Hits",
                "Games",
                "At Bats",
                "Batting Average",
                "Runs",
                "Singles",
                "Doubles",
                "Triples",
                "Home Runs",
                "RBI (Runs Batted In)",
                "Stolen Bases",
                "Caught Stealing",
                "Steal Percentage",
                "Walks",
                "Intentional Walks",
                "Hit by Pitch",
                "Sacrifice Hits",
                "Sacrifice Flies",
                "Double Plays",
                "Strike Outs",
                "Bases Produced",
                "Career Earnings",
                "Career Earnings Adjusted",
                "Career Dollars Per Base",
                "Teams",
                "Players",
                "Salary",
                "Adjusted Salary",
                "Dollars Per Base")

## Saved all the data
save(mlb.stat.choices,stat.choices,yvars,file="Shiny.rdata")
load("baseball.rdata")






