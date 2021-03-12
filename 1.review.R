#--------------------------------------------------------------------
# chapter 1 ~ 4. tutorials
#--------------------------------------------------------------------

## handle string
a = c('ass','bb')
print(paste(a, collapse = ','))
print(paste(a, collapse = ' '))
print(paste(a, collapse = ''))


## use library - ggplot2
library(ggplot2)

x <- c('a','a','b','c')
qplot(x)

qplot(data = mpg, x = hwy)
qplot(data = mpg, x = cty)
qplot(data = mpg, x = drv, y = hwy)
qplot(data = mpg, x = drv, y = hwy, geom = "line")
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot")
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot", color = drv)


## dataframe
eng = c(5, 10, 30, 20)
math = c(23, 20, 14, 23)
class = c(1,1,2,2)

df_midterm = data.frame(eng, math, class); df_midterm

mean(df_midterm$eng)


## use external data
install.packages("xlsx")
library(xlsx)   # xlsx 파일 쓰기
library(readxl) # xlsx 파일 읽기


# 엑셀 읽기
setwd("C:/Users/user/study/do_it_R")
df_exam = read_excel("excel_exam.xlsx")
df_exam

df_exam = read_excel("excel_exam_novar.xlsx", col_names = F)
df_exam

df_exam = read_excel("excel_exam_sheet.xlsx", sheet = 3)
df_exam

# 엑셀 쓰기
write.xlsx(df, "filename.xlsx", sheetName = "sheet1",
           col.names = T, row.names = T, append = F)
# 같은 파일 다른 씨트에 쓰기
write.xlsx(new_df, "filename.xlsx", sheetName = "sheet2",
           col.names = T, row.names = T, append = T)
# 이게 더 빠름 (xlsx2)
write.xlsx2(df, "filename.xlsx", sheetName = "sheet1",
           col.names = T, row.names = T, append = F)


# csv 파일 읽기
read.csv(header = T, stringsAsFactors = F)

# csv로 저장
write.csv(df, file = "filename.csv")

# RData 파일 활용
save(df, file = "filename.rda")  # rda 확장자로 저장
load("filename.rda") # rda 파일을 불러오고 저장했을 때의 변수 df를 생성


#--------------------------------------------------------------------
# chapter 5. data analysis basic
#--------------------------------------------------------------------
df_sample = read.csv("https://raw.githubusercontent.com/youngwoos/Doit_R/master/Data/csv_exam.csv")
head(df_sample)
View(df_sample)          # 뷰어로 깔끔하게 보기
dim(df_sample)           # 차원확인
str(df_sample)           # 데이터 속성 확인
summary(df_sample)       # 요약 통계량


## 변수명 바꾸기
library(dplyr)
# 더블콜론 :: 를 사용하면 패키지 속 데이터, 함수 지정가능
mpg = as.data.frame(ggplot2::mpg)
mpg_copy = mpg 
mpg_copy = rename(mpg_copy, city = cty, highway = hwy)
print(colnames(mpg_copy)); print(colnames(mpg))

## 파생변수 생성
# 도시 + 고속도로 통합 연비 => total 변수
mpg_copy$total = (mpg_copy$city + mpg_copy$highway)/2; mpg_copy
hist(mpg_copy$total, breaks=seq(10,40,2), col="RED", border="white",prob = T)
lines(density(mpg_copy$total))
summary(mpg_copy$total)
# 고연비 평가 변수
mpg_copy$test <- ifelse(mpg_copy$total >= 20, "pass", "fail")
head(mpg_copy, 25)
table(mpg_copy$test)
library(ggplot2)
qplot(mpg_copy$test)
# 연비 등급 평가 변수 - 중첩조건문
mpg_copy$grade <- ifelse(mpg_copy$total >= 30, "A", 
                         ifelse(mpg_copy$total >= 20, 'B', 'C'))
head(mpg_copy, 25)
table(mpg_copy$grade)
qplot(mpg_copy$grade)

### ch5. Exercise
midwest = as.data.frame(ggplot2::midwest)
?midwest

# description
str(midwest)
summary(midwest)
View(midwest)
dim(midwest)

# add feature
midwest_c = midwest
midwest_c = rename(midwest_c, total = poptotal, asian = popasian)
midwest_c$asian_prop = midwest_c$asian / midwest_c$total
hist(midwest_c$asian_prop, breaks = 20)

boxplot(midwest_c$asian_prop ~ midwest_c$state)
?boxplot

m_asian_p = mean(midwest_c$asian_prop)
midwest_c$asian_many = ifelse(midwest_c$asian_prop >= m_asian_p, "large", "small")
table(midwest_c$asian_many)
qplot(midwest_c$asian_many)
