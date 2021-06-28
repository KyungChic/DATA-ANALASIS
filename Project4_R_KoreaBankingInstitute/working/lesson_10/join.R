## 데이터 병합

# 1) 데이터 생성(국적정보, 성별 정보)
nat<- data.frame(id=c(1,2,3),nationality=c("한국","프랑스","일본"))
gen<-data.frame(id=c(2,3,4),gender=c("남성","여성","남성"))

nat
gen


# 2) 내부 병합 :공통되는 값을 기준으로 대응되는 데이터값만으로 병합
merge(x=nat,y=gen,by='id')

# 3) 부분외부병합 (국적정보기준) : 대응되는 데이터만 유지하되, 겹치지 않는 데이터 값은 하나만.
merge(x=nat,y=gen,by='id',all.x=TRUE)


# 4) 부분외부병합 (성별정보기준) 
merge(x=nat,y=gen,by='id',all.y=TRUE)

# 5) 전체외부병합 
merge(x=nat,y=gen,by='id',all=TRUE)

