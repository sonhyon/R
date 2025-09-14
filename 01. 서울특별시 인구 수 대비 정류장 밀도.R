#[인구 대비 정류장 밀도 분석]

#*[라이브러리 불러오기]
library(dplyr) ; library(ggplot2) ; library(leaflet) ; library(sf) ; library(readr) ; library(stringr)

#*[데이터 불러오기]
getwd()
trans_stops <- read.csv("./데이터/국토교통부_전국 버스정류장 위치정보_20241028.csv", fileEncoding = "euc-kr")
population <- read.csv("./데이터/행정안전부_지역별(행정동) 성별 연령별 주민등록 인구수_20250731.csv", fileEncoding = "euc-kr")
head(trans_stops, 5)
head(population, 1)

#*[데이터 전처리]

#[서울 버스정류장]
sum(is.na(trans_stops))   # 데이터프레임 전체 NA 개수
trans_stops_clean <- na.omit(trans_stops)

seoul_bus_stop <- trans_stops_clean %>%
  filter(도시명 == '서울특별시')
head(seoul_bus_stop)
#write_csv(seoul_bus_stop, "./데이터/seoul_bus_stop.csv")

#리버스 지오코딩 불러오기
seoul_bus_rv <- read.csv("./데이터/서울버스정류장_리버스지오코딩.csv", fileEncoding = "utf-8")
seoul_bus_rv <- seoul_bus_rv %>%
  filter(도시명 == "서울특별시") %>%
  select(X_ROAD_AD)

# 정규식: '서울', '서울시', '서울특별시' 이후의 첫 토큰(공백/콤마/괄호 전까지)을 캡처
m <- str_match(seoul_bus_rv$X_ROAD_AD, "서울(?:특별시|시)?\\s*([^\\s,\\(\\)]+)")
seoul_bus_rv$행정구 <- m[,2]

seoul_bus_rv <- seoul_bus_rv %>%
  filter(!is.na(행정구)) %>%
  count(행정구, name = "정류장수") %>%
  arrange(desc(정류장수))
head(seoul_bus_rv)


#[서울 인구수]
seoul_pop <- population %>%
  filter(시도명 == '서울특별시') %>%
  group_by(시군구명) %>%
  summarise(total_pop = sum(계, na.rm = TRUE)) %>%
  arrange(desc(total_pop))
head(seoul_pop)

#인구수 대비 버스정류장 밀도 데이터프레임 구하기
seoul_data <- seoul_bus_rv %>%
  left_join(seoul_pop, by = c("행정구" = "시군구명"))

seoul_data <- seoul_data %>%
  mutate(`정류장 밀도` = total_pop / 정류장수) %>%
  arrange(desc(`정류장 밀도`))

head(seoul_data)



#*[시각화]

# 행정구 순서를 정류장 밀도 기준 내림차순으로 설정
seoul_data$행정구 <- factor(seoul_data$행정구, levels = seoul_data$행정구)

ggplot(seoul_data, aes(x = 행정구, y = `정류장 밀도`, fill = 행정구)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(`정류장 밀도`, 1)), vjust = -0.5, size = 3) + # 막대 위에 값 표시
  labs(title = "서울시 행정구별 버스 정류장 밀도 (내림차순)",
       x = "행정구",
       y = "정류장 밀도") +
  theme_minimal() +
  theme(legend.position = "none")
