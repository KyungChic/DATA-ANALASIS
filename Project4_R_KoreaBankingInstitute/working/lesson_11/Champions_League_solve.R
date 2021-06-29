## 데이터 통합과 병합 응용
## 제공 데이터 : 16시즌 경기 기록 상세, 15 시즌 득점, 2016 발롱도르 수상 후보, 2016 유로 입상 국가

# 1) 데이터 불러오기 및 탐색
str(CL_16)
CL_16

# 2) 2016 발롱도르 수상 후보의 16 시즌 득점 기록
Award

player_sum_16 <- aggregate(Goals~Player, data=CL_16, sum)

merge(x=player_sum_16, y=Award, by="Player", all.y=TRUE)


# 3) 2016 유로 대회에서 입상한 국가에 속한 선수들의 16 시즌 득점 기록
Euro

National_Sum_16 <- aggregate(Goals~Nationality, data=CL_16, sum)

merge(x=National_Sum_16, y=Euro, by='Nationality' ,all.y=TRUE)


# 4) 15, 16 시즌에 모두 골을 기록한 선수

merge(x=CL_15, y=player_sum_16, by='Player')
