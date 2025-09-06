#라이브러리 불러오기
library(dplyr) ; library(ggplot2) ; library(leaflet)


#*[데이터 불러오기]
getwd()
trans_stops <- read.csv("./데이터/국토교통부_전국 버스정류장 위치정보_20241028.csv", fileEncoding = "euc-kr")
head(trans_stops)


#*[데이터 전처리]
sum(is.na(trans_stops))   # 데이터프레임 전체 NA 개수
trans_stops_clean <- na.omit(trans_stops)
head(trans_stops_clean)

#도시별 버스정류장 총수
city_counts <- trans_stops_clean %>%
  group_by(도시명) %>%
  summarise(bus_stop_count = n()) %>%
  arrange(desc(bus_stop_count))

#도시별 버스정류장 총수_TOP10
city_counts_10 <- trans_stops_clean %>%
  group_by(도시명) %>%
  summarise(bus_stop_count = n()) %>%
  arrange(desc(bus_stop_count)) %>%
  head(10)

#서울 버스정류장
seoul <- trans_stops_clean %>%
  filter(도시명 == '서울특별시')
head(seoul)


#*[시각화]
#도시별 버스정류장 총수_TOP10
ggplot(city_counts_10, aes(x = reorder(도시명, -bus_stop_count), y = bus_stop_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "도시명", y = "정류장 수", title = "도시별 버스정류장 개수") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#[서울 버스정류장 지도화]
head(seoul[, c("위도", "경도")]) #위도(latitude)와 경도(longitude) 컬럼 이름이 정확한지 확인

#지도 만들기
leaflet(data = seoul) %>%
  addTiles() %>%  # 기본 OpenStreetMap 타일
  addMarkers(lng = ~경도, lat = ~위도, popup = ~정류장명)

leaflet(data = seoul) %>%
  addTiles() %>%
  addMarkers(lng = ~경도, lat = ~위도, popup = ~정류장명,
             clusterOptions = markerClusterOptions())

#지역별 인구수
pop_counts <- population %>%
  group_by(시도명) %>%
  summarise(total_pop = sum(계, na.rm = TRUE)) %>%
  arrange(desc(total_pop))
head(pop_counts)

###
지역별 인구 수 데이터 불러오기
인구 대비 정류장 밀도 분석 (회귀분석? 상관분석?)
교통 취약 지역(인구는 많지만 정류장이 적은 지역) 찾기

