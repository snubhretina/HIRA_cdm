# COX : COVID as time-varying covatiate

library(devtools)
library(SqlRender)
library(DatabaseConnector)
library(dplyr)
library(lubridate)

#parameter
cdm_schema <- "cdm_2020_view"
cdm_voca_schema <- "cdm_voca"
researcher_schema <- "G65829"
covid_observation_id <- "704996"
covid_condition_id <- "37311061, 439676, 4100065"

source("R code/1_Settings.R")
source("R code/2_Functions.R")

# data 만들기
# Covid data
covidObsSqlquery <- function(researcher, cdm, concept_ids){
  sql_1 <- "create table "
  sql_2 <- "covid_obs_data as select person_id, observation_concept_id as COVID_CONCEPT_ID, observation_date as COVID_DATE from "
  sql_3 <- "observation where observation_concept_id in ("
  researcher <- paste0(researcher, '.')
  cdm <- paste0(cdm, '.')
  concept_ids <- concept_ids
  return(
    paste0(sql_1, researcher, sql_2, cdm, sql_3, concept_ids, ');')
  )
}

covidCondSqlquery <- function(researcher, cdm, concept_ids){
  sql_1 <- "create table "
  sql_2 <- "covid_cond_data as select person_id, condition_concept_id as COVID_CONCEPT_ID, condition_start_date as COVID_DATE from "
  sql_3 <- "condition_occurrence where condition_concept_id in ("
  researcher <- paste0(researcher, '.')
  cdm <- paste0(cdm, '.')
  concept_ids <- concept_ids
  return(
    paste0(sql_1, researcher, sql_2, cdm, sql_3, concept_ids, ');')
  )
}

covidAllSqlquery <- function(researcher){
  sql_1 <- "create table "
  sql_2 <- "covid_data_all as select a.* from "
  sql_3 <- "covid_obs_data a union all select b.* from "
  sql_4 <- "covid_cond_data b;"
  researcher <- paste0(researcher, '.')
  return(
    paste0(sql_1, researcher, sql_2, researcher, sql_3, researcher, sql_4)
  )
}

{
  sqlquery_covid_obs <- covidObsSqlquery(researcher_schema, cdm_schema, covid_observation_id)
  sqlquery_covid_obs
  executeSql(conn, sqlquery_covid_obs) ## 에러가 난다면 아래 주석 풀어서 실행 후 다시 실행
  # executeSql(conn, paste0("drop table ", researcher_schema, ".covid_obs_data;"))
  
  sqlquery_covid_cond <- covidCondSqlquery(researcher_schema, cdm_schema, covid_condition_id)
  sqlquery_covid_cond
  executeSql(conn, sqlquery_covid_cond) ## 에러가 난다면 아래 주석 풀어서 실행 후 다시 실행
  # executeSql(conn, paste0("drop table ", researcher_schema, ".covid_cond_data;"))
  
  sqlquery_covid_all <- covidAllSqlquery(researcher_schema)
  sqlquery_covid_all
  executeSql(conn, sqlquery_covid_all) ## 에러가 난다면 아래 주석 풀어서 실행 후 다시 실행
  # executeSql(conn, paste0("drop table ", researcher_schema, ".covid_data_all;"))

  paste0("create table ", researcher_schema, ".covid_data_rownum as select a.*, row_number() over (partition by a.person_id order by a.covid_date) as p_row_num from ", researcher_schema, ".covid_data_all a;")
}


# 데이터 행. 사람 수 확인
check_covid_tb <- querySql(conn, paste0("select count(distinct person_id) as count_p, count(*) as count_n from ", researcher_schema, ".covid_data_all;"))
check_covid_tb




