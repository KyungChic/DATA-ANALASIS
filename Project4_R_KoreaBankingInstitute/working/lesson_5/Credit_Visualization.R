# 부정고객 점수 시각화


# 패키지 불러오기
library(ggplot2)

# 주거래은행에 따른 부정 사용의심점수 시각화
ggplot(result,aes(x=score,fill=bank))+
  geom_histogram(binwidth=0.6,alpha=0.5,position="identity")+
  ggtitle("주거래은행에 따른 고객의심 부정사용 의심 점수")+
  theme_bw()
