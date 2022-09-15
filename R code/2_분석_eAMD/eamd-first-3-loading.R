# 2018.7.1 이후 exudative AMD 진단과 함께 anti-VEGF (Lucentis, Eylea)      발생환자들 data까지 있다고 가정.
# 필요한 column : PERSON_ID, DRUG_EXPOSURE_START_DATE


# TODO : data name 변경
# 투약 순번 만들기
loading_data <- data %>%
  group_by(PERSON_ID) %>%
  arrange(PERSON_ID, DRUG_EXPOSURE_START_DATE) %>%
  mutate(PERSON_DRUG_ASD_NUMBER = row_number()) %>%
  select(PERSON_ID, DRUG_EXPOSURE_START_DATE, PERSON_DRUG_ASD_NUMBER)

loading_data$DRUG_EXPOSURE_START_DATE = as.Date(loading_data$DRUG_EXPOSURE_START_DATE)

# 저장 해야 되는 n / total
total_n <- length(unique(loading_data$PERSON_ID))

# 최소 첫 3로딩(3번째 주사)를 가지고 있는 PERSON_ID
has_3loading <- loading_data %>%
  filter(PERSON_DRUG_ASD_NUMBER ==3) %>%
  select(PERSON_ID)

# 저장해야 되는 n
total_3loading <- nrow(has_3loading)


# diff 계산 
loading_data <- loading_data %>%
  filter(PERSON_ID %in% has_3loading$PERSON_ID) %>%
  filter(PERSON_DRUG_ASD_NUMBER == 3 | PERSON_DRUG_ASD_NUMBER == 1) %>%
  group_by(PERSON_ID) %>%
  mutate(THIRD_DATE = max(DRUG_EXPOSURE_START_DATE)) %>%
  mutate(FIRST_DATE = min(DRUG_EXPOSURE_START_DATE)) %>%
  mutate(DIFF = THIRD_DATE - FIRST_DATE) %>%
  select(PERSON_ID, FIRST_DATE, THIRD_DATE, DIFF) %>%
  distinct()


diff_data <- loading_data %>%
  mutate(is_60 = ifelse(60 <= DIFF, 1, 0)) %>%
  mutate(is_80 = ifelse(80 <= DIFF, 1, 0)) %>%
  mutate(is_100 = ifelse(100 <= DIFF, 1, 0)) %>%
  mutate(is_120 = ifelse(120 <= DIFF, 1, 0))

## 코로나 시점 전 2018-07-01 ~ 2019-12-31
before_covid <- diff_data %>%
  filter(THIRD_DATE < "2020-01-01")

## 코로나 시점 후 2020-01-01 ~ 2022-04-30
after_covid <- diff_data %>%
  filter("2019-12-31" < THIRD_DATE)

# 저장해야 되는 n들
before_complete_60 <- length(which(before_covid$is_60==1))
before_complete_80 <- length(which(before_covid$is_80==1))
before_complete_100 <- length(which(before_covid$is_100==1))
before_complete_120 <- length(which(before_covid$is_120==1))


after_complete_60 <- length(which(after_covid$is_60==1))
after_complete_80 <- length(which(after_covid$is_80==1))
after_complete_100 <- length(which(after_covid$is_100==1))
after_complete_120 <- length(which(after_covid$is_120==1))

n_table <- data.frame(total_n, total_3loading, 
    before_complete_60, before_complete_80, before_complete_100, before_complete_120,
    after_complete_60, after_complete_80, after_complete_100, after_complete_120)


# csv 저장
write.csv(n_table, file="./result_eamd/n_table")








































































