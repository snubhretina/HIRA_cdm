# COX : COVID as time-varying covatiate

library(devtools)
library(SqlRender)
library(DatabaseConnector)
library(dplyr)
library(lubridate)
library(survival)

# parameter
cdm_schema <- ""
cdm_voca_schema <- ""
researcher_schema <- ""
covid_observation_id <- "704996"
covid_condition_id <- "37311061, 439676, 4100065"
rvo_concept_id <- "440392"

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

covidRownumSqlquery <- function(researcher){
  sql_1 <- "create table "
  sql_2 <- "covid_data_rownum as select a.*, row_number() over (partition by a.person_id order by a.covid_date) as p_row_num from "
  sql_3 <- "covid_data_all a;"
  researcher <- paste0(researcher, '.')
  return(
    paste0(sql_1, researcher, sql_2, researcher, sql_3)
  )
}

covidDataSqlquery <- function(researcher){
  sql_1 <- "create table "
  sql_2 <- "covid_data_todo as select * from "
  sql_3 <- "covid_data_rownum where p_row_num in (1);"
  researcher <- paste0(researcher, '.')
  return(
    paste0(sql_1, researcher, sql_2, researcher, sql_3)
  )
}

# RVO data
rvoCondSqlquery <- function(researcher, cdm, cdm_voca, concept_ids){
  sql_1 <- "create table "
  sql_2 <- "rvo_data as select person_id, condition_concept_id, condition_start_date from "
  sql_3 <- "condition_occurrence where condition_concept_id in (select descendant_concept_id from "
  sql_4 <- "concept_ancestor where ancestor_concept_id in ("
  researcher <- paste0(researcher, '.')
  cdm <- paste0(cdm, '.')
  cdm_voca <- paste0(cdm_voca, '.')
  concept_ids <- concept_ids
  return(
    paste0(sql_1, researcher, sql_2, cdm, sql_3, cdm_voca, sql_4, concept_ids, '));')
  )
}

rvoRownumSqlquery <- function(researcher){
  sql_1 <- "create table "
  sql_2 <- "rvo_data_rownum as select a.*, row_number() over (partition by a.person_id order by a.condition_start_date) as p_row_num from "
  sql_3 <- "rvo_data a;"
  researcher <- paste0(researcher, '.')
  return(
    paste0(sql_1, researcher, sql_2, researcher, sql_3)
  )
}

rvoDataSqlquery <- function(researcher){
  sql_1 <- "create table "
  sql_2 <- "rvo_data_todo as select * from "
  sql_3 <- "rvo_data_rownum where p_row_num in (1);"
  researcher <- paste0(researcher, '.')
  return(
    paste0(sql_1, researcher, sql_2, researcher, sql_3)
  )
}

# Washout
covidWashoutSqlquery <- function(researcher, washout_date){
  sql_1 <- "create table "
  sql_2 <- "covid_data_aft_washout as select a.* from "
  sql_3 <- "covid_data_todo a where a.person_id not in (select b.person_id from "
  sql_4 <- "covid_data_todo b where b.covid_date < "
  sql_5 <- ") and a.person_id not in (select c.person_id from "
  sql_6 <- "rvo_data_todo c where c.condition_start_date < "
  researcher <- paste0(researcher, '.')
  washout_date <- paste0("TO_DATE('", washout_date, "', 'YYYY-MM-DD')")
  return(
    paste0(sql_1, researcher, sql_2, researcher, sql_3, researcher, sql_4, washout_date, sql_5, researcher, sql_6, washout_date, ");")
  )
}

rvoWashoutSqlquery <- function(researcher, washout_date){
  sql_1 <- "create table "
  sql_2 <- "rvo_data_aft_washout as select a.* from "
  sql_3 <- "rvo_data_todo a where a.person_id not in (select b.person_id from "
  sql_4 <- "covid_data_todo b where b.covid_date < "
  sql_5 <- ") and a.person_id not in (select c.person_id from "
  sql_6 <- "rvo_data_todo c where c.condition_start_date < "
  researcher <- paste0(researcher, '.')
  washout_date <- paste0("TO_DATE('", washout_date, "', 'YYYY-MM-DD')")
  return(
    paste0(sql_1, researcher, sql_2, researcher, sql_3, researcher, sql_4, washout_date, sql_5, researcher, sql_6, washout_date, ");")
  )
}

# Cox Data todo
coxDataSqlquery <- function(researcher){
  sql_1 <- "create table "
  sql_2 <- "cox_data_todo as select a.*, b.condition_concept_id as rvo_id, b.condition_start_date as rvo_date from "
  sql_3 <- "covid_data_aft_washout a left join "
  sql_4 <- "rvo_data_aft_washout b on a.person_id = b.person_id;"
  researcher <- paste0(researcher, '.')
  return(
    paste0(sql_1, researcher, sql_2, researcher, sql_3, researcher, sql_4)
  )
}
coxDateSqlquery <- function(researcher, start_date, end_date){
  sql_1 <- "create table "
  sql_2 <- "cox_data_date as select a.*, "
  sql_3 <- " as start_date, case when a.rvo_date is NULL then "
  sql_4 <- " else a.rvo_date end as end_date, case when a.rvo_date is NULL then 0 else 1 end as outcome from "
  sql_5 <- "cox_data_todo a;"
  researcher <- paste0(researcher, '.')
  start_date <- paste0("TO_DATE('", start_date, "', 'YYYY-MM-DD')")
  end_date <- paste0("TO_DATE('", end_date, "', 'YYYY-MM-DD')")
  return(
    paste0(sql_1, researcher, sql_2, start_date, sql_3, end_date, sql_4, researcher, sql_5)
  )
}

# Simple Cox
coxSimpleSqlquery <- function(researcher){
  sql_1 <- "create table "
  sql_2 <- "simple_cox as select a.*, 0 as strt, (a.end_date - a.start_date) as stopp, (a.covid_date - a.start_date) as time_cov_start, (a.end_date - a.covid_date) as time_cov_stop from "
  sql_3 <- "cov_data_date a;"
  researcher <- paste0(researcher, '.')
  return(
    paste0(sql_1, researcher, sql_2, researcher, sql_3)
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
  
  sqlquery_covid_rownum <- covidRownumSqlquery(researcher_schema)
  sqlquery_covid_rownum
  executeSql(conn, sqlquery_covid_rownum) ## 에러가 난다면 아래 주석 풀어서 실행 후 다시 실행
  # executeSql(conn, paste0("drop table ", researcher_schema, ".covid_data_rownum;"))
  
  sqlquery_covid_todo <- covidDataSqlquery(researcher_schema)
  sqlquery_covid_todo
  executeSql(conn, sqlquery_covid_todo) ## 에러가 난다면 아래 주석 풀어서 실행 후 다시 실행
  # executeSql(conn, paste0("drop table ", researcher_schema, ".covid_data_todo;"))
  
  ############################################################################################
  
  sqlquery_rvo_cond <- rvoCondSqlquery(researcher_schema, cdm_schema, cdm_voca_schema, rvo_concept_id)
  sqlquery_rvo_cond
  executeSql(conn, sqlquery_rvo_cond) ## 에러가 난다면 아래 주석 풀어서 실행 후 다시 실행
  # executeSql(conn, paste0("drop table ", researcher_schema, ".rvo_data;"))
  
  sqlquery_rvo_rownum <- rvoRownumSqlquery(researcher_schema)
  sqlquery_rvo_rownum
  executeSql(conn, sqlquery_rvo_rownum) ## 에러가 난다면 아래 주석 풀어서 실행 후 다시 실행
  # executeSql(conn, paste0("drop table ", researcher_schema, ".rvo_data_rownum;"))
  
  sqlquery_rvo_todo <- rvoDataSqlquery(researcher_schema)
  sqlquery_rvo_todo
  executeSql(conn, sqlquery_rvo_todo) ## 에러가 난다면 아래 주석 풀어서 실행 후 다시 실행
  # executeSql(conn, paste0("drop table ", researcher_schema, ".rvo_data_todo;"))
  
  #############################################################################################
  
  sqlquery_covid_washout <- covidWashoutSqlquery(researcher_schema, "2018-01-01")
  sqlquery_covid_washout
  executeSql(conn, sqlquery_covid_washout) ## 에러가 난다면 아래 주석 풀어서 실행 후 다시 실행
  # executeSql(conn, paste0("drop table ", researcher_schema, ".covid_data_aft_washout;"))
  
  sqlquery_rvo_washout <- rvoWashoutSqlquery(researcher_schema, "2018-01-01")
  sqlquery_rvo_washout
  executeSql(conn, sqlquery_covid_washout) ## 에러가 난다면 아래 주석 풀어서 실행 후 다시 실행
  # executeSql(conn, paste0("drop table ", researcher_schema, ".covid_data_aft_washout;"))
  
  #############################################################################################
  
  sqlquery_cox_todo <- coxDataSqlquery(researcher_schema)
  sqlquery_cox_todo
  executeSql(conn, sqlquery_cox_todo) ## 에러가 난다면 아래 주석 풀어서 실행 후 다시 실행
  # executeSql(conn, paste0("drop table ", researcher_schema, ".cox_data_todo;"))
  
  sqlquery_cox_date <- coxDateSqlquery(researcher_schema, "2020-01-01", "2022-04-30")
  sqlquery_cox_date
  executeSql(conn, sqlquery_cox_date) ## 에러가 난다면 아래 주석 풀어서 실행 후 다시 실행
  # executeSql(conn, paste0("drop table ", researcher_schema, ".cox_data_date;"))
  
  sqlquery_simple_cox <- coxSimpleSqlquery(researcher_schema)
  sqlquery_simple_cox
  executeSql(conn, sqlquery_simple_cox) ## 에러가 난다면 아래 주석 풀어서 실행 후 다시 실행
  # executeSql(conn, paste0("drop table ", researcher_schema, ".cox_data_date;"))
}

#paste0("create table ", researcher_schema, ".covid_data_rownum as select a.*, row_number() over (partition by a.person_id order by a.covid_date) as p_row_num from ", researcher_schema, ".covid_data_all a;")



# 데이터 행. 사람 수 확인
check_covid_tb <- querySql(conn, paste0("select count(distinct person_id) as count_p, count(*) as count_n from ", researcher_schema, ".covid_data_all;"))
check_covid_tb

# Simple Cox Analysis
simple_cox_data <- querySql(conn, paste0("select * from ", researcher_schema, ".simple_cox;"))
fit1 <- survfit(Surv(STOPP, OUTCOME)~1, data = simple_cox_data)
summary(fit1)
summary_table_simple <- summary(fit1)

pdf("./simple_cox_km_plot.pdf")
plot(fit1, xlab = "time(days)", ylab = "Survival function", ylim = c(0.95,1))
title(main = "Keplan-Meier survival estimates")
dev.off()

survival_table_simple <- as.data.frame(summary_table_simple[c("time", "n.risk", "n.event", "n.censor", "surv", "cumhaz", "std.chaz", "lower", "upper")])
write.csv(survival_table_simple, "./simple_cox_survival_table.csv")

# COVID as Time varying Covariates
timevar_cox_data <- querySql(conn, paste0("select * from ", researcher_schema, ".timevar_cox_final;"))
fit2 <- coxph(Surv(time = STRT_FINAL, time2 = STOP_FINAL, event = EVENT_FINAL) ~ factor(OUTCOME), data = timevar_cox_data)
summary(fit2)
summary_table_timevar <- summary(fit2)

survival_table_timevar <- as.data.frame(summary_table_timevar[c("n", "nevent", "coefficients", "conf.int")])
write.csv(survival_table_timevar, "./simple_cox_survival_table.csv")


