# ARIMA - vitrectomy

#------------------ LIBRARY ---------------------

library(devtools)
library(SqlREnder)
library(DatabaseConnector)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)

#------------------ DataBase Connection -------------
# TODO : DB정보입력 해야함.
dbms <- "oracle"
server <- ""
user <- ""
password <- "" 
port <- ""
pathToDriver <- "C:/"

connectionDetails <- createConnectionDetails(dbms=dbms,
                                             server=server,
                                             user=user,
                                             password=password,
                                             port=port,
                                             pathToDriver = pathToDriver)
conn <- connect(connectionDetails)

#------------------ Def Functions -------------------

###--------------- makeCohort 
### 함수인자(순서와 타입 지켜야함)들을 받아 sql문을 통해 조건에 맞는 데이터를 return하는 함수
# cdm 스키마(필수)  - 문자열 ex) "cdm_2020_view
# voca 스키마(필수) - 문자열 ex)"cdm_voca"
# ancestor id(필수) - 문자열 ex) "4294683, 4314406, 4246506, 433973"
# start_date(선택)  - 문자열 ex) "2018-01-01"
makeCohort <- function(cdm, voca, ancestor_ids, start_date="") {
  # 기본 sql문 토대
  # from 뒤 띄어쓰기 필수
  sql_1 <- "select person_id, procedure_concept_id, procedure_date, provider_id from " 
  sql_2 <- "procedure_occurrence where procedure_concept_id in (selecet descendant_concept_id from "
  sql_3 <- "concept_ancestor where ancestor_concept_id in ("
  # 인자 setting
  cdm <- paste0(cdm, '.')  # sql문에는 .이 들어가야함  
  voca <- paste0(voca, '.')
  ancestor_ids <- ancestor_ids
  if (start_date=="") {
    return (
      querySql(conn, paste0(sql_1, cdm, sql_2, voca, sql_3, ancestor_ids, '));'))
    )
  }
  # start date 있는 경우
  start_date <- paste0("TO_DATE('", start_date, "', 'YYYY-MM-DD')") # sql문에 맞는 형태로 변환
  sql_4 <- "and procedure_date >"
    return (
      querySql(conn, paste0(sql_1, cdm, sql_2, voca, sql_3, ancestor_ids, ')', sql_4, start_date, ');'))
  )
}

###--------------- checkData
### data를 인자로 받아 row의 개수와 사람 수를 return하는 함수
### return value에서 data_person, data_row로 접근
checkData <- function(data) {
  return(
    c(data_person=length(unique(data$PERSON_ID)), data_row= nrow(data))
  )
}

# selectFirst
### data를 인자로 받아 첫 병원 방문 row만 select해 data를 return하는 함수
selectFirst <- function() {
  return(
    data %>% group_by (PERSON_ID) %>%
      arrange(PRPCEDURE_DATE) %>%
      filter(row_number()==1)
  )
}

###--------------- unitDataSum 
### 함수인자(순서와 타입 지켜야함)들을 받아 unit 별 합계 column을 포함해 data를 return하는 함수
# 1. data(필수)
# 2. unit(필수) : 문자열 "week" or "month"
unitDataSum <- function(data, unit) {
  data["UNIT_DATE"] = floor_date(data$PROCEDURE_DATE, unit=unit)
  data <- data %>%
    group_by(UNIT_DATE) %>%
    summarise(UNIT_SUM = n())
  data <- data %>%
    arrange(UNIT_DATE)
  return(
    data
  )
}

###---------------  makeTimeSeries
### 함수인자(순서와 타입 지켜야함)들을 받아 time-series data를 return하는 함수
# 1. data(필수)
# 2. start_year(필수) : 숫자 2018
# 3. start_month(필수) : 숫자 1 (1월)
# 4. unit(필수) : 문자열 "week" or "month"
makeTimeSeries <- function(data, start_year, start_month, unit) {
  if (unit=="week") {
    frequency <- 52
  } else if (unit == "month") {
    frequency <- 12
  }
  ts_data <- ts(data=data($UNIT_SUM, frequency=frequency, start=c(start_year, start_month))
  return(
    ts_data
  )
}

###--------------- TimeSeries Plot 
# 인자로 time-series를 받아 decomposed plot과 acf, pacf plot 을 현재경로에 plots라는 pdf파일로 저장하는 함수
makePlots <- function(data) {
  # 저장 시작 / 저장 종료는 함수를 부르고나서 실행하게됨.
  getwd()
  pdf("./plots/pdf")
  # s.window -> seasonal component 추출 & 차분 하지 않음.
  decomposed_plot <- data %>% stl(s.window="periodic") %>% autoplot()
  # 1차 차분하여 residual, acf, pacf plot 
  diff_plot <- data %>% stl(s.window = "periodic") %>% seasadj %>% diff %>% ggtsdisplay()
  # autoplot은 바로 plot이 나타나지 않아 return 필요 & ggtsdisplay는 바로 plot을 띄우면서 저장
  # 두 개 중 하나만 사용하고 싶었지만 안됌.
  return(
    decomposed_plot
  )
}

###--------------- autoArima
# data를 인자로 받아 auto.arima를 결과를 return
autoArima <- function(data) {
  return(
    auto.arima(data)
  )
}

###--------------- fitArima
# data와 autoArima에서 생성된 추천 파라미터 값(p,d,q)을 인자로 받아 arima를 fitting.
# fitting 된 arima 모형 결과 plot를 현재경로 arima_plots라는 pdf파일로 저장하는 함수
# autoArima에서 추천 모형은 최대2개로 파악되어짐.
fitArima <- function(data, parameters) {
  if (length(parameters)<1) {
    return()
  }
  # plot 저장 시작
  getwd() 2
  pdf("./ arima_plots/pdf")
  fit_data <- data %>% stl(s.window = "periodic") %>% seasadj %>% diff
  order <- c(parameters["p"], parameters["d"], parameters["q"])
  first_fit <- Arima(fit_data, order = order)
  if (parameters["P"]) {
    order2 <- c(parameters["P"], parameters["D"], parameters["Q"])
    second_fit <- Arima(fit_data, order = order2)
    return(
      c(checkresiduals(first_fit), checkresiduals(second_fit))
    )
  }
  return(
    c(checkresiduals(first_fit))
  )
}
# Funtion Defination section 종료

#------------------ 원내 CDM에서 사용한 Code -------------------

# 1. make Cohort
vitrectomy_data <- makeCohort("cdm_2020_view", "cdm_voca", "4294683, 4314406, 4246506, 4333973, 4170911", "2016-01-01")

# 2. check cohort data
check_data <- checkData(vitrectomy_data)
check_data["data_row"]    # row 수
check_data["data_person"] # 사람 수

# 3. 첫 방문(치료)
selected_data <- selectFirst(vitrectomy_data)
# check selected data
check_data <- checkData(selected_data)
check_data["data_row"]    # row 수
check_data["data_person"] # 사람 수

# 4. weekly sum 주별 합산 & Time-Series data
weekly_data <- unitDataSum(selected_data, "week")
ts_data <- makeTimeSeries(weekly_data, 2016, 1 "week")

# 5. Decomposed  & Residual & ACF, PACF Plots
plots = makePlots(ts_data)
plots
dev.off() # makePlots에서 실행한 plot 저장 종료

# 6. auto-arima & arima result Plot
auto_arima_fit <- autoArima(ts_data) # auto-arima
fitArima(ts_data, arimaorder(auto_arima_fit)) # auto-arima에서 나온 추천 parameter를 가지고 fitArima 호출
dev.off()  # fitArima에서 실행한 plot 저장 종료










































