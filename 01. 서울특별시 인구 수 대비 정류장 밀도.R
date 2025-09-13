#[인구 대비 정류장 밀도 분석]

#*[라이브러리 불러오기]
library(dplyr) ; library(ggplot2) ; library(leaflet) ; library(sf)

#*[데이터 불러오기]
getwd()
trans_stops <- read.csv("./데이터/국토교통부_전국 버스정류장 위치정보_20241028.csv", fileEncoding = "euc-kr")
population <- read.csv("./데이터/행정안전부_지역별(행정동) 성별 연령별 주민등록 인구수_20250731.csv", fileEncoding = "euc-kr")
head(trans_stops, 5)
head(population, 1)

#*[데이터 전처리]
sum(is.na(trans_stops))   # 데이터프레임 전체 NA 개수

#서울 버스정류장
trans_stops_clean <- na.omit(trans_stops)
seoul_bus_stop <- trans_stops_clean %>%
  filter(도시명 == '서울특별시')
head(seoul_bus_stop)

install.packages("readr")
library(readr)
write_csv(seoul_bus_stop, "./데이터/seoul_bus_stop.csv")


seoul_bus_rv <- read.csv("./데이터/서울버스정류장_리버스지오코딩.csv", fileEncoding = "utf-8")
seoul_bus_rv <- seoul_bus_rv %>%
  filter(도시명 == "서울특별시") %>%
  select(X_ROAD_AD)

library(stringr)

# 정규식: '서울', '서울시', '서울특별시' 이후의 첫 토큰(공백/콤마/괄호 전까지)을 캡처
m <- str_match(seoul_bus_rv$X_ROAD_AD, "서울(?:특별시|시)?\\s*([^\\s,\\(\\)]+)")

# 그룹(1)이 우리가 원하는 행정구
seoul_bus_rv$행정구 <- m[,2]

seoul_bus_rv <- seoul_bus_rv %>%
  filter(!is.na(행정구)) %>%
  count(행정구, name = "정류장수") %>%
  arrange(desc(정류장수))
head(seoul_bus_rv) #***************************************

#서울 인구수
seoul_pop <- population %>%
  filter(시도명 == '서울특별시') %>%
  group_by(시군구명) %>%
  summarise(total_pop = sum(계, na.rm = TRUE)) %>%
  arrange(desc(total_pop))
head(seoul_pop) #**************************************





seoul_data <- seoul_bus_rv %>%
  left_join(seoul_pop, by = c("행정구" = "시군구명"))

head(seoul_data)

#*[시각화]

#[서울 버스정류장 지도화]
head(seoul_bus_stop[, c("위도", "경도")]) #위도(latitude)와 경도(longitude) 컬럼 이름이 정확한지 확인

#지도 만들기
leaflet(data = seoul_bus_stop) %>%
  addTiles() %>%  # 기본 OpenStreetMap 타일
  addMarkers(lng = ~경도, lat = ~위도, popup = ~정류장명)

leaflet(data = seoul_bus_stop) %>%
  addTiles() %>%
  addMarkers(lng = ~경도, lat = ~위도, popup = ~정류장명,
             clusterOptions = markerClusterOptions())
#───────────────────────────────────────────────────────────────────
#인구 대비 정류장 밀도 수 분석







#───────────────────────────────────────────────────────────────────
# 서울시 구 경계 불러오기
gu_shp <- st_read("./sigun_grid/seoul.shp")  # 예: 서울시 구 shapefile


# 정류장 데이터를 sf 객체로 변환
bus_sf <- st_as_sf(seoul, coords = c("경도", "위도"), crs = 4326)

#구별 정류장 수 계산
bus_counts <- bus_sf %>%
  group_by(시군구명) %>%
  summarise(bus_stations = n(), .groups = "drop")

#인구 대비 정류장 밀도 계산
density_data <- pop_counts %>%
  left_join(bus_counts, by = "시군구명") %>%
  mutate(pop_per_station = total_pop / bus_stations) %>%
  arrange(desc(pop_per_station))
