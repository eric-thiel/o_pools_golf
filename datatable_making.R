library(reactable)
library(sparkline)

options(reactable.theme = reactableTheme(
    color = "hsl(233, 0%, 0%)",
    backgroundColor = "hsl(291, 0%, 81%)",
    borderColor = "hsl(233, 9%, 22%)",
    stripedColor = "hsl(233, 12%, 22%)",
    highlightColor = "hsl(233, 12%, 24%)",
    inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    ))

summary_1 = summary %>% select(Team, none_through)
summary_2 = summary %>% select(Team, one_through)
summary_3 = summary %>% select(Team, two_through)
summary_4 = summary %>% select(Team, three_through)
summary_5 = summary %>% select(Team, four_through)
summary_6 = summary %>% select(Team, five_through)
summary_7 = summary %>% select(Team, six_through)
summary_8 = summary %>% select(Team, seven_through)
summary_9 = summary %>% select(Team, eight_through)

summary_1 = summary_1 %>% rename(exi = none_through)
summary_2 = summary_2 %>% rename(exi = one_through)
summary_3 = summary_3 %>% rename(exi = two_through)
summary_4 = summary_4 %>% rename(exi = three_through)
summary_5 = summary_5 %>% rename(exi = four_through)
summary_6 = summary_6 %>% rename(exi = five_through)
summary_7 = summary_7 %>% rename(exi = six_through)
summary_8 = summary_8 %>% rename(exi = seven_through)
summary_9 = summary_9 %>% rename(exi = eight_through)


summarissss = rbind(summary_1, summary_2)
summarissss = rbind(summarissss, summary_3)
summarissss = rbind(summarissss, summary_4)
summarissss = rbind(summarissss, summary_5)
summarissss = rbind(summarissss, summary_6)
summarissss = rbind(summarissss, summary_7)
summarissss = rbind(summarissss, summary_8)
summarissss = rbind(summarissss, summary_9)

summarissss = left_join(summarissss, board[c("Score","Team")], by = c("Team"="Team"))
summarissss = left_join(summarissss, summary[c("at_least_6_through","Team")], by = c("Team"="Team"))
data <- summarissss %>%
  group_by(Team) %>%
  summarise(Score = first(Score), `6+ Golfers on Weekend` = first(at_least_6_through), outcomeDist = list(exi))


reactable(data, columns = list(
  outcomeDist = colDef(cell = function(values) {
    sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = 0.45,width = 500, height = 20)
  }),
  Team = colDef(minWidth =100, align = "center"),
  Score = colDef(minWidth =100, align = "center"),
  `6+ Golfers on Weekend` = colDef(minWidth =100, align = "center")
  
  
), fullWidth = TRUE, highlight = TRUE
)





