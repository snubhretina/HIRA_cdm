
# --------------- Settings.R --------------------

#기본적으로 분석에 사용할 패키키들을 불러오고,
# DB 연결을 합니다.

# 1. LIBRARY
library(devtools)
library(SqlRender)
library(DatabaseConnector)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(ggrepel)
library(scales)
library(survival)

# 1-1. LIBRARY 없을 시 설치
# install.packages("devtools")
# install.packages("SqlRender")
# install.packages("DatabaseConnector")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("forecast")
# install.packages("ggrepel")
# install.packages("scales")
# install.packages("survival")



# 2. 심사평가원 DB 연결
# TODO : 데이터베이스 연결 파라미터 변수 5종 입력 
dbms <- ""
user <- ""
password <- ""
server <- ""
pathToDriver <- ""

connectionDetails <- createConnectionDetails(dbms=dbms,
                                             user=user,
                                             password=password,
                                             server=server,
                                             pathToDriver = pathToDriver)
conn <- connect(connectionDetails)

# 3. etc setting
options(scipen=100)
