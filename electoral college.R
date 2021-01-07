library(tidyverse)
library(lubridate)
library(usmap)
library(gridExtra)
library(dplyr)
library(rgdal)
plot_election_results = function(electoral = F, vote_type = "total", region = c(.east_north_central,.east_south_central,.midwest_region,.mid_atlantic,.mountain,.new_england,.northeast_region,.north_central_region,.pacific,.south_atlantic,.south_region)) {
  
  presidential <- read.csv("~/Downloads/USElection2020-NYT-Results-master/data/latest/presidential.csv",na=c('','NA','NULL','PrivacySuppressed'))
  electoralvotes <- read_csv("~/Downloads/electoralvotes.csv")
  remove(electoralvotes)
  attach(presidential)
  
  if (vote_type == "total") {
    presidential_data = select(presidential,state,votes,results_trumpd, results_bidenj)
    presidential_data_grouped = group_by(presidential_data, state)
    #total votes by state
    totalvotes = summarise(presidential_data_grouped, sum(votes))
    #trump votes
    trumpvotes = summarise(presidential_data_grouped, sum(results_trumpd))
    #biden votes
    bidenvotes = summarise(presidential_data_grouped, sum(results_bidenj))
    #add these together
    voting_data = mutate(totalvotes, TRUMP = trumpvotes$`sum(results_trumpd)`/totalvotes$`sum(votes)`, BIDEN = bidenvotes$`sum(results_bidenj)`/totalvotes$`sum(votes)`)
    voting_data_final = mutate(voting_data,WINNER = ifelse(voting_data$TRUMP > voting_data$BIDEN, 1, 0))
    voting_data_final = mutate(voting_data_final, state = statepop$full)
    #electoral count
    voting_data_final_calc = mutate(voting_data_final, numberofvotes = electoralvotes$`number of votes`)
    trumpelectoralcount = sum(voting_data_final_calc$WINNER*voting_data_final_calc$numberofvotes)
    bidenelectoralcount = sum(voting_data_final_calc$numberofvotes) - trumpelectoralcount
    
  } else if(vote_type == "absentee") {
      presidential_data = select(presidential, state, absentee_votes, results_absentee_trumpd, results_absentee_bidenj)
      presidential_data_grouped = group_by(presidential_data, state)
      #total absentee votes by state
      totalvotesabs = summarise(presidential_data_grouped, sum(absentee_votes))
      #trump absentee votes
      trumpvotesabs = summarise(presidential_data_grouped, sum(results_absentee_trumpd))
      #biden absentee votes
      bidenvotesabs = summarise(presidential_data_grouped, sum(results_absentee_bidenj))
      #add these together
      voting_data_abs = mutate(totalvotesabs, TRUMPABS = trumpvotesabs$`sum(results_absentee_trumpd)`/totalvotesabs$`sum(absentee_votes)`, BIDENABS=bidenvotesabs$`sum(results_absentee_bidenj)`/totalvotesabs$`sum(absentee_votes)`)
      voting_data_abs = mutate(voting_data_abs,state = statepop$full, WINNERABS = ifelse(TRUMPABS>BIDENABS,1,0))
      #electoral count
      voting_data_final_calc_a = mutate(voting_data_abs, numberofvotes = electoralvotes$`number of votes`)
      trumpelectoralcount_a = sum(voting_data_final_calc_a$WINNERABS*voting_data_final_calc_a$numberofvotes)
      bidenelectoralcount_a = sum(voting_data_final_calc_a$numberofvotes) - trumpelectoralcount_a
    
  } else if(vote_type == "in-person") {
      presidential_data = mutate(presidential, IPTRUMP = presidential$results_trumpd-presidential$results_absentee_trumpd, IPBIDEN = presidential$results_bidenj-presidential$results_absentee_bidenj,TOTAL = IPTRUMP+IPBIDEN)
      presidential_data_ip = select(presidential_data,IPTRUMP,IPBIDEN,TOTAL,state)
      presidential_data_ip_G = group_by(presidential_data_ip, state)
      #total IP
      totalip = summarise(presidential_data_ip_G, sum(TOTAL))
      #total TRUMP
      trumptotalip = summarise(presidential_data_ip_G, sum(IPTRUMP))
      #total BIDEN
      bidentotalip = summarise(presidential_data_ip_G, sum(IPBIDEN))
      #add together
      voting_data_ip = mutate(totalip,TRUMPIP = trumptotalip$`sum(IPTRUMP)`/totalip$`sum(TOTAL)`,BIDENIP = bidentotalip$`sum(IPBIDEN)`/totalip$`sum(TOTAL)`)
      voting_data_ip = mutate(voting_data_ip, state = statepop$full, WINNERIP = ifelse(TRUMPIP>BIDENIP,1,0))
      #electoral count
      voting_data_final_calc_ip = mutate(voting_data_ip, numberofvotes = electoralvotes$`number of votes`)
      trumpelectoralcount_ip = sum(voting_data_final_calc_ip$WINNERIP*voting_data_final_calc_ip$numberofvotes)
      bidenelectoralcount_ip = sum(voting_data_final_calc_ip$numberofvotes) - trumpelectoralcount_ip
  } else{
    print("Invalid vote_type input try again")
  }
  
  #plot
  state_centers = usmap_transform((tibble(state.center$x, state.center$y, state.name)))
  electoralvotes = electoralvotes[-9,]
  state_centers = mutate(state_centers, number_of_votes = electoralvotes$`number of votes`)
  electoralvotes = mutate(electoralvotes, center_long = state_centers$state.center.x.1,center_lat = state_centers$state.center.y.1)
  electoralvotes[2,3] = -1203560
  electoralvotes[2,4] = -1837070
  electoralvotes[11,3] = -450000
  electoralvotes[11,4] = -2130070
  
  if (electoral == T & vote_type == "total") {
    which_region = c(region)
    plot_usmap(data=voting_data_final, values = "WINNER", regions = "states", include = region)+
      scale_fill_gradient(low = "blue", high = "red", labels = c("Biden","Trump"), name = "Candidate", limits = c(0,1), breaks = c(0,1),guide = "legend") +
      theme(legend.position = "right") +
      ggtitle(paste("Biden:",bidenelectoralcount, "Electoral Votes\nTrump:", trumpelectoralcount,"Electoral Votes")) +
      geom_text(data = electoralvotes,aes(x = center_long,y = center_lat,label = `number of votes`))
  } else if (electoral == T & vote_type == "absentee"){
    which_region = c(region)
    plot_usmap(data=voting_data_abs, values = "WINNERABS",regions = "states", include = region)+
      scale_fill_gradient(low = "blue", high = "red", labels = c("Biden", "Trump"), name = "Candidate", limits=c(0,1), breaks=c(0,1), guide = "legend") +
      theme(legend.position = "right") +
      ggtitle(paste("Biden:",bidenelectoralcount_a,"Electoral Votes\nTrump:",trumpelectoralcount_a,"Electoral Votes"))  +
      geom_text(data = electoralvotes,aes(x = center_long, y = center_lat, label = `number of votes`))
  } else if (electoral == T & vote_type == "in-person"){
    which_region = c(region)
    plot_usmap(data=voting_data_ip, values = "WINNERIP", regions = "states", include = region)+
      scale_fill_gradient(low = "blue", high = "red", labels = c("Biden","Trump"), name="Candidate", limits = c(0,1), breaks = c(0,1), guide = "legend") +
      theme(legend.position = "right") +
      ggtitle(paste("Biden:",bidenelectoralcount_ip,"Electoral Votes\nTrump:",trumpelectoralcount_ip,"Electoral Votes"))  +
      geom_text(data = electoralvotes, aes(x = center_long, y = center_lat, label = `number of votes`))
  } else if (electoral == F & vote_type == "total") {
    which_region = c(region)
    plot_usmap(data=voting_data_final, values = "TRUMP", regions = "states",include=region) +
      scale_fill_gradient(low = "blue", high = "red",name = "Percent for Trump", label = scales::comma) +
      ggtitle(paste("Total Popular Vote")) +
      theme(legend.position = "right") 
  } else if(electoral == F & vote_type == "absentee") {
    which_region = c(region)
    plot_usmap(data=voting_data_abs, values = "TRUMPABS", regions = "states",include = region) +
      scale_fill_gradient(low = "blue", high = "red", name = "Percent for Trump",label=scales::comma) +
      ggtitle(paste("Total Absentee Popular Vote")) +
      theme(legend.position = "right") 
  } else if(electoral == F & vote_type == "in-person"){
    which_region = c(region)
    plot_usmap(data=voting_data_ip,values = "TRUMPIP", regions = "states",include = region)+
      scale_fill_gradient(low = "blue", high = "red", name = "Percent for Trump", label = scales::comma) +
      ggtitle(paste("Total In-Person Popular Vote")) +
      theme(legend.position = "right") 
  } else {
    print("Invalid electoral input, try again")
  }
}
#Test Cases
plot_election_results(electoral=T,vote_type="total",region=c("GA","FL","TX"))
plot_election_results(electoral=F,vote_type="in-person",region=.west_region)
plot_election_results(electoral=T,vote_type="total")
plot_election_results(electoral=F,vote_type="absentee",region=.east_south_central)
plot_election_results(electoral=T,vote_type="total")
plot_election_results(electoral=T,vote_type="in-person")
