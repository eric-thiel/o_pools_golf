library(dplyr)
library(googlesheets4)



gs4_auth(email = "eric.thiel96@gmail.com")
Teams = read_sheet("https://docs.google.com/spreadsheets/d/1h4AGAiF6xzrsEztIG3SytM3f_wjNgBl0n_yto-6dIM0/edit#gid=190038856",
                       "Teams")

Odds = read_sheet("https://docs.google.com/spreadsheets/d/1h4AGAiF6xzrsEztIG3SytM3f_wjNgBl0n_yto-6dIM0/edit#gid=190038856",
                   "odds")




board = Teams

board = left_join(board, Odds, by = c("Golfer_1"= "Player"))
board = board %>% rename("makecut_1" = "made_cut_odds", "score_1"="score")

board = left_join(board, Odds, by = c("Golfer_2"= "Player"))
board = board %>% rename("makecut_2" = "made_cut_odds", "score_2"="score")

board = left_join(board, Odds, by = c("Golfer_3"= "Player"))
board = board %>% rename("makecut_3" = "made_cut_odds", "score_3"="score")

board = left_join(board, Odds, by = c("Golfer_4"= "Player"))
board = board %>% rename("makecut_4" = "made_cut_odds", "score_4"="score")

board = left_join(board, Odds, by = c("Golfer_5"= "Player"))
board = board %>% rename("makecut_5" = "made_cut_odds", "score_5"="score")

board = left_join(board, Odds, by = c("Golfer_6"= "Player"))
board = board %>% rename("makecut_6" = "made_cut_odds", "score_6"="score")

board = left_join(board, Odds, by = c("Golfer_7"= "Player"))
board = board %>% rename("makecut_7" = "made_cut_odds", "score_7"="score")

board = left_join(board, Odds, by = c("Golfer_8"= "Player"))
board = board %>% rename("makecut_8" = "made_cut_odds", "score_8"="score")

board$makecut_1 = as.numeric(board$makecut_1)
board$makecut_2 = as.numeric(board$makecut_2)
board$makecut_3 = as.numeric(board$makecut_3)
board$makecut_4 = as.numeric(board$makecut_4)
board$makecut_5 = as.numeric(board$makecut_5)
board$makecut_6 = as.numeric(board$makecut_6)
board$makecut_7 = as.numeric(board$makecut_7)
board$makecut_8 = as.numeric(board$makecut_8)









board$all_8 = board$makecut_1 * board$makecut_2 * board$makecut_3 * board$makecut_4 * board$makecut_5 * board$makecut_6 * 
  board$makecut_7 * board$makecut_8





start_sim = Odds

start_sim$made_cut = ifelse(runif(nrow(start_sim),0,1) > start_sim$made_cut_odds,0,1)


simmed = Teams

simmed = left_join(simmed, start_sim[c("made_cut","Player")], by = c("Golfer_1"= "Player"))
simmed = simmed %>% rename("made_cut_1"="made_cut")

simmed = left_join(simmed, start_sim[c("made_cut","Player")], by = c("Golfer_2"= "Player"))
simmed = simmed %>% rename("made_cut_2"="made_cut")

simmed = left_join(simmed, start_sim[c("made_cut","Player")], by = c("Golfer_3"= "Player"))
simmed = simmed %>% rename("made_cut_3"="made_cut")

simmed = left_join(simmed, start_sim[c("made_cut","Player")], by = c("Golfer_4"= "Player"))
simmed = simmed %>% rename("made_cut_4"="made_cut")

simmed = left_join(simmed, start_sim[c("made_cut","Player")], by = c("Golfer_5"= "Player"))
simmed = simmed %>% rename("made_cut_5"="made_cut")

simmed = left_join(simmed, start_sim[c("made_cut","Player")], by = c("Golfer_6"= "Player"))
simmed = simmed %>% rename("made_cut_6"="made_cut")

simmed = left_join(simmed, start_sim[c("made_cut","Player")], by = c("Golfer_7"= "Player"))
simmed = simmed %>% rename("made_cut_7"="made_cut")

simmed = left_join(simmed, start_sim[c("made_cut","Player")], by = c("Golfer_8"= "Player"))
simmed = simmed %>% rename("made_cut_8"="made_cut")

simmed$num_golfers_made_cut = simmed$made_cut_1 + simmed$made_cut_2 + simmed$made_cut_3 + simmed$made_cut_4 + 
  simmed$made_cut_5 + simmed$made_cut_6 + simmed$made_cut_7 + simmed$made_cut_8 



simmed = simmed %>% select(Team, num_golfers_made_cut)
simmed$trial = 1


results = simmed[0,]

i = 1

run_sim <- function(nsims){
  i = 1
  repeat {
  
  start_sim = Odds
  
  start_sim$made_cut = ifelse(runif(nrow(start_sim),0,1) > start_sim$made_cut_odds,0,1)
  
  
  simmed = Teams
  
  simmed = left_join(simmed, start_sim[c("made_cut","Player")], by = c("Golfer_1"= "Player"))
  simmed = simmed %>% rename("made_cut_1"="made_cut")
  
  simmed = left_join(simmed, start_sim[c("made_cut","Player")], by = c("Golfer_2"= "Player"))
  simmed = simmed %>% rename("made_cut_2"="made_cut")
  
  simmed = left_join(simmed, start_sim[c("made_cut","Player")], by = c("Golfer_3"= "Player"))
  simmed = simmed %>% rename("made_cut_3"="made_cut")
  
  simmed = left_join(simmed, start_sim[c("made_cut","Player")], by = c("Golfer_4"= "Player"))
  simmed = simmed %>% rename("made_cut_4"="made_cut")
  
  simmed = left_join(simmed, start_sim[c("made_cut","Player")], by = c("Golfer_5"= "Player"))
  simmed = simmed %>% rename("made_cut_5"="made_cut")
  
  simmed = left_join(simmed, start_sim[c("made_cut","Player")], by = c("Golfer_6"= "Player"))
  simmed = simmed %>% rename("made_cut_6"="made_cut")
  
  simmed = left_join(simmed, start_sim[c("made_cut","Player")], by = c("Golfer_7"= "Player"))
  simmed = simmed %>% rename("made_cut_7"="made_cut")
  
  simmed = left_join(simmed, start_sim[c("made_cut","Player")], by = c("Golfer_8"= "Player"))
  simmed = simmed %>% rename("made_cut_8"="made_cut")
  
  simmed$num_golfers_made_cut = simmed$made_cut_1 + simmed$made_cut_2 + simmed$made_cut_3 + simmed$made_cut_4 + 
    simmed$made_cut_5 + simmed$made_cut_6 + simmed$made_cut_7 + simmed$made_cut_8 
  
  
  
  simmed = simmed %>% select(Team, num_golfers_made_cut)
  simmed$trial = i
  
  results = rbind(results, simmed)
  print(i)
  i = i +1
  if(i >= nsims){
    break
  }
  }
  return(results)
}


nsims = 10000
g = run_sim(10000)

working = g

working$none_through = ifelse(working$num_golfers_made_cut==0,1,0)
working$one_through = ifelse(working$num_golfers_made_cut==1,1,0)
working$two_through = ifelse(working$num_golfers_made_cut==2,1,0)
working$three_through = ifelse(working$num_golfers_made_cut==3,1,0)
working$four_through = ifelse(working$num_golfers_made_cut==4,1,0)
working$five_through = ifelse(working$num_golfers_made_cut==5,1,0)
working$six_through = ifelse(working$num_golfers_made_cut==6,1,0)
working$seven_through = ifelse(working$num_golfers_made_cut==7,1,0)
working$eight_through = ifelse(working$num_golfers_made_cut==8,1,0)



summary = working %>% group_by(Team)%>%
  summarise(none_through = sum(none_through), one_through = sum(one_through), two_through = sum(two_through),
            three_through = sum(three_through), four_through = sum(four_through), five_through = sum(five_through),
            six_through = sum(six_through), seven_through = sum(seven_through), eight_through = sum(eight_through))

summary$none_through = summary$none_through / nsims
summary$one_through = summary$one_through / nsims
summary$two_through = summary$two_through / nsims
summary$three_through = summary$three_through / nsims
summary$four_through = summary$four_through / nsims
summary$five_through = summary$five_through / nsims
summary$six_through = summary$six_through / nsims
summary$seven_through = summary$seven_through / nsims
summary$eight_through = summary$eight_through / nsims


summary$at_least_6_through = summary$eight_through + summary$seven_through + summary$six_through



