install.packages("ggiraphExtra")
library(ggiraphExtra)

#---------------------------------------- data loading
# - 지도시각화를 위한 데이터준비
# - 위치데이터 (위도, 경도, 레이블)
# - 지도에 시각화할 특성값 데이터 (본 예에서는 지역별 범죄율)

# 미국 주별 범죄 데이터
str(USArrests)

library(tibble)
crime = rownames_to_column(USArrests, var = "state")
crime$state = tolower(crime$state)
head(crime)

# 미국 주별 위/경도 데이터
install.packages("maps")
library(maps)
library(ggplot2)

states_map = map_data("state")
str(states_map)
dim(states_map)

# ---------------------------------------- make choropleth map
install.packages("mapproj")
library(mapproj)

ggChoropleth(data = crime,                # 지도에 표현할 메인 데이터셋
             aes(fill = Murder,           # 지역별로 색을 입힐 수치형 변수
                 map_id = state),         # 지역을 구분할 범주형 변수
             map = states_map,            # 위/경도(long, lat), 지역(region)변수 포함 데이터셋
             interactive = F)                                   

# ---------------------------------------- 한국지도 시각화
install.packages('stringi')
install.packages('devtools')
devtools::install_github("cardiomoon/kormaps2014")

library(kormaps2014)

# 데이터 준비
# korpop1 = changeCode(korpop1)             # 인구데이터
str(korpop1)
# kormap1 = changeCode(kormap1)             # 지리데이터
str(kormap1)

library(dplyr)
korpop1 = rename(korpop1, pop = 총인구_명, name = 행정구역별_읍면동)

kormap1$long = as.numeric(kormap1$long)
kormap1$lat = as.numeric(kormap1$lat)

# 시각화
ggChoropleth(data = korpop1, aes(fill = pop, 
                                 map_id = code, 
                                 tooltip = name),
             map = kormap1, interactive = T)


ggChoropleth(data = tbc, aes(fill = NewPts, 
                                 map_id = code, 
                                 tooltip = name),
             map = kormap1, interactive = T) 


# ---------------------------------------- 필요한 것
# - 지역 레이블, 위도, 경도을 포함한 지리데이터셋
# - 지역 레이블, 특성값을 포함한 데이터셋