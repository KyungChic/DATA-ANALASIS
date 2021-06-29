## R 기초 프로그래밍

# 1) x의 값이 3보다 큰 경우 "more than 3"이라는 텍스트를 반환하는 조건을 생성

x<-0

if (x > 3){
  print("more than 3")
}

# 2) y의 값이 0보다 큰 경우 "more than 0"라는 텍스트를 반환하고
# 그렇지 않은 경우 "less than 0"라는 텍스트를 반환하는 조건문 생성

y<-0

if(y>0){
  print("more than 0")
} else{
  print("less than 0")
}


# 3) 1~5 값을 포함하고 있는 z 벡터의 값을 순차적으로 출력해주는 
# 반복문 생성

z <- c(1:5)

for(i in z){
  print(i)
}

# 4) 1~5 값을 포함하고 있는 z 벡터의 제곱값을 반환해주는 사용자 정의 함수 생성

square<-function(x){
  x^2
}

for(i in z){
  print(square(i))
}













