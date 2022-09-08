# COX : COVID as time-varying covatiate

library(devtools)
library(SqlRender)
library(DatabaseConnector)
library(dplyr)
library(lubridate)
library(survival)

#parameter
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


