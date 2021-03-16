#------------------------------------------- setting
setwd("C:/Users/user/study/R_BasicSkills/sample_data")

install.packages("foreign")

library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

#------------------------------------------- data loading
raw_welfare = read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                        to.data.frame = T)

welfare = raw_welfare

# head(welfare)
# View(welfare)
dim(welfare) # row : 16664, col : 957
# str(welfare)
# summary(welfare)

welfare = rename(welfare, 
                 sex = h10_g3,
                 birth = h10_g4,
                 marriage = h10_g10,
                 religion = h10_g11,
                 income = p1002_8aq1,
                 code_job = h10_eco9,
                 code_region = h10_reg7)

#------------------------------------------------------------------ 
# analysis 
#------------------------------------------------------------------ 
## 1. sex & income
class(welfare$sex)
table(welfare$sex)

class(welfare$income)
summary(welfare$income)

# change coding
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

# missing value handling
# income의 정상값 범위는 1 ~ 9998
# 소득변수에 대한 분석을 위해서는 결측값을 모두 제거해야함
welfare$income = ifelse(welfare$income == 0, NA, welfare$income)
sum(is.na(welfare$income))


# relation analysis
sex_mean_income = welfare %>% group_by(sex) %>% 
  summarise(mean_income = mean(income, na.rm = T))

sex_mean_income
ggplot(data = sex_mean_income, aes(x = sex, y = mean_income)) + geom_col()
ggplot(data = welfare, aes(x = income, fill = sex)) + geom_histogram(alpha = 0.5) + xlim(0, 1500)

welfare %>% group_by(sex) %>% summarise(mean = mean(income, na.rm = T), 
                                        med = median(income, na.rm = T), 
                                        sd = sd(income, na.rm = T),
                                        iqr = IQR(income, na.rm = T),
                                        Q1 = quantile(income, 0.25, na.rm = T),
                                        Q3 = quantile(income, 0.75, na.rm = T))

ggplot(data = welfare, aes(x = sex, y = income)) + geom_boxplot()

#------------------------------------------------------------------ 
## 2. age & income
class(welfare$birth)
qplot(welfare$birth)
summary(welfare$birth)
table(is.na(welfare$birth))

# create age variable
welfare$age = 2015 - welfare$birth + 1
qplot(welfare$age, binwidth = 3)
summary(welfare$age)

# relation analysis
ggplot(data = welfare, aes(x = age, y = income)) + geom_point()

age_income_stat = welfare %>% 
  group_by(age) %>% 
  summarise(median_income = median(income, na.rm = T))
age_income_stat

age_income_stat2 = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(median_income = median(income),
            mean_income = mean(income))
age_income_stat2

ggplot(data = age_income_stat2, aes(x = age, y = median_income)) + geom_line()
ggplot(data = age_income_stat2, aes(x = age, y = mean_income)) + geom_line()
ggplot(data = age_income_stat2, aes(x = age, y = mean_income, col = sex)) + geom_line()

table(welfare$age)

# binning - 나이를 범주형으로 이산화
welfare$age_int = ifelse(welfare$age < 20, 1, 
                         ifelse(welfare$age < 30, 2,
                                ifelse(welfare$age < 40, 3,
                                       ifelse(welfare$age < 50, 4,
                                              ifelse(welfare$age < 60, 5,
                                                     ifelse(welfare$age < 70, 6,
                                                            ifelse(welfare$age < 80, 7, 8)))))))
table(welfare$age_int)

welfare$age_int <- as.factor(welfare$age_int)
welfare_income = welfare %>% filter(!is.na(income))
ggplot(data = welfare_income, aes(x = age_int, y = income,  fill = sex)) + geom_boxplot()
# 거의 모든 구간에서 남자 소득이 높음. 차이가 4 ~50대에서 가장 큼

ggplot(data = welfare_income, aes(x = age_int, y = income,  fill = sex)) + geom_col(position = "dodge")

#------------------------------------------------------------------ 
## 3. job & income
class(welfare$code_job)
table(welfare$code_job)

list_job = read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)
dim(list_job)

# code로된 변수를 쉽게 인식하기위한 join
welfare = left_join(welfare, list_job, id = "code_job")

# 직업별 평균소득
job_income = welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))

top10 = job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)

ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) +
  geom_col() +
  coord_flip()

bot10 = job_income %>% 
  arrange(mean_income) %>% 
  head(10)

ggplot(data = bot10, aes(x = reorder(job, -mean_income), y = mean_income)) +
  geom_col() +
  coord_flip() +
  ylim(0, 850)

#------------------------------------------------------------------ 
## 4. job & sex
# 남녀 상위빈도 직업 
job_male_10 = welfare %>% 
  filter(sex == "male" & !is.na(job)) %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

job_male_10

job_female_10 = welfare %>% 
  filter(sex == "female" & !is.na(job)) %>% 
  group_by(job) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

job_female_10

install.packages("gridExtra")
library(gridExtra)
p1 = ggplot(data = job_male_10, aes(x = reorder(job, n), y = n)) + 
  geom_col() + 
  coord_flip()
p2 = ggplot(data = job_female_10, aes(x = reorder(job, n), y = n)) + 
  geom_col() + 
  coord_flip() 

grid.arrange(p1, p2, ncol = 2)

#------------------------------------------------------------------ 
## 4. religion & divorce

table(welfare$religion)
table(welfare$marriage)

# change coding
welfare$religion = ifelse(welfare$religion == 1, "yes", "no")
table(welfare$religion)

# create divorce variable
welfare$group_marriage = ifelse(welfare$marriage == 1, "marriage",
                                ifelse(welfare$marriage == 3, "divorce",NA))
qplot(welfare$group_marriage)

# refine
religion_divorce = welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion ,group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group * 100, 1))

religion_divorce

# divorce 
divorce = religion_divorce %>% 
  filter(group_marriage == "divorce") %>% 
  select(religion, pct)

ggplot(data = divorce, aes(x = religion, y = pct, fill = religion)) +
  geom_col()

# statistical inference
testing_set = welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  select(religion, group_marriage)

g1 = testing_set %>% filter(religion == "yes") %>% select(group_marriage)
g2 = testing_set %>% filter(religion == "no") %>% select(group_marriage)

g1 = ifelse(g1$group_marriage == "marriage", 1, 0)
g2 = ifelse(g2$group_marriage == "marriage", 1, 0)

# 1, 0 자료들에 대해 평균차이검정 하는게 맞나??
t.test(g1, g2)

print(mean(g1) / sd(g1))
print(mean(g2) / sd(g2))
print(length(g1));print(length(g2))

install.packages("STAT")
library(STAT)

# 것보다 비율 검정을 따로 하는게 더 정확하지 않겠음?
prop.test(c(sum(g1), sum(g2)), c(length(g1), length(g2)))


#------------------------------------------------------------------ 
## 5. region & age
class(welfare$code_region)
table(welfare$code_region)

list_region = data.frame(code_region = c(1:7),
                         region = c("서울",
                                    "수도권(인천/경기)",
                                    "부산/경남/울산",
                                    "대구/경북",
                                    "대전/충남",
                                    "강원/충북",
                                    "광주/전남/전북/제주도"))

welfare = left_join(welfare, list_region, id = "code_region")

meanage_for_region = welfare %>% 
  group_by(region) %>% 
  summarise(mean_age = mean(age))

ggplot(data = meanage_for_region, aes(x = reorder(region, mean_age),
                                      y = mean_age,
                                      fill = region)) + 
  geom_col() +
  coord_flip()


welfare = welfare %>% mutate(ageg = ifelse(age < 30, "young",
                                 ifelse(age < 60, "middle","old")))

# 지역별 연령대 범주 소속 관측총합
region_ageg = welfare %>% 
  group_by(region, ageg) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n / tot_group * 100, 2))

region_ageg

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip()

# 노년층비율 오름차순으로 정렬
order_ageg = region_ageg %>% 
  filter(ageg == "old") %>% 
  arrange(pct)

order = order_ageg$region

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  # coord_flip() +
  scale_x_discrete(limits = order)

# young - middle - old 순으로 출력하고자 함
region_ageg$ageg = factor(region_ageg$ageg,
                          level = c("old", "middle",'young'))

class(region_ageg$ageg)
levels(region_ageg$ageg)

# 좀 더 깔끔한 그래프
ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(limits = order)
