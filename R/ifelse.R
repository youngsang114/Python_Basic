library(dplyr)
library(ggplot2)

# 극단치
View(mpg)

# 극단치를 확인
boxplot(mpg$cty)
# 극단치를 수치로 표현
boxplot(mpg$cty)$stats


mpg = ggplot2::mpg

# 이상치는 26초과이거나 9미만인 경우
# 이상치를 결측치로 변환
# 결측치의 개수를 확인
ifelse(mpg$cty > 26| mpg$cty < 9, NA, mpg$cty) # 결과가 나오지 않은건 = 저장되지않았다
ifelse(mpg$cty > 26| mpg$cty < 9, NA, mpg$cty) -> mpg$cty
table(is.na(mpg$cty))

# dplyr 패키지를 이용하여 결측치를 제거하고 제조사별(manufacturer)로 그룹화
# 도심연비(cty)의 평균
# 도심연비가 좋은 상위 5개의 제조사를 확인
mpg %>% filter(!is.na(cty)) %>% group_by(manufacturer) %>%
  summarise(cty_mean = mean(cty) %>% arrange(desc(cty_mean)) %>% head(5)

mpg = ggplot2::mpg # mpg를 초기화

# total 파생변수 생성
# total은 (cty + hwy) / 2

total = (mpg$cty + mpg$hwy) / 2
mpg$total = total # case 1 mpg$total에 total이라는 벡터를 집어넣겠다 : total -> mpg$total
data.frame(mpg, total) # case 2
cbind(mpg, total) # case 3

mpg %>% mutate(total =(cty+hwy)/2) -> mpg # case4

# test 파생변수 생성
# total이 30이상이면 'A' 
# 20이상이고 30미만이면 'B'
# 20 미만이면 'C'
# ifelse(조건식, 참인경우의 값, ifelse(조건식2, 참인경우, 거짓인경우))

# 30미만인건 첫번째 ifelse에서 걸러짐
ifelse(mpg$total >= 30, 'A', ifelse(mpg$total >= 20, 'B', 'C')) -> mpg$test

qplot(mpg$test)
# $사용할때는 뒤에 컬럼이 오고 ex) mpg$total (total은 컬럼)
# ::사용할때는 뒤에 데이터프레임 ex) ggplot2::mpg (mpg는 데이터프레임)

# midwest 데이터
midwest = ggplot2::midwest
View(midwest)

# 컬럼의 이름을 변경
# rename(데이터프레임명, 새컬럼의 이름 = 변경이 될 컬럼의 이름)
# popasian 컬럼을 asian, poptotal 컬럼을 total 변경
# ratio 파생변수 생성 -> 전체 인구수 대비 아시아의 인구 수(백분율)
# group 파생변수 생성 -> ratio 평균보다 ratio의 값이 크면 'large' 아니면 'small'
# 컬럼의 이름은 ''안넣어도 되고 문자열은 '' 넣어야함

rename(midwest, asian = popasian, total = poptotal) -> midwest
midwest

midwest %>% mutate(ratio = asian/total*100) -> midwest

midwest$group = ifelse(midwest$ratio > mean(midwest$ratio), 'large', 'small') 
qplot(midwest$group)
table(midwest$group)

