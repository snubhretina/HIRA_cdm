## TODO : schema를 설정해주세요.
cdm_schema <- ""
cdm_voca_schema <- ""
researcher_schema <- ""

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
  sql_3 <- "cox_data_date a;"
  researcher <- paste0(researcher, '.')
  return(
    paste0(sql_1, researcher, sql_2, researcher, sql_3)
  )
}

# Time-varying covariate Cox
coxTimeDupSqlquery <- function(researcher){
  sql_1 <- "create table "
  sql_2 <- "time_cox_dup as select * from "
  sql_3 <- "simple_cox where covid_date < end_date;"
  researcher <- paste0(researcher, '.')
  return(
    paste0(sql_1, researcher, sql_2, researcher, sql_3)
  )
}

coxTimeDataSqlquery <- function(researcher){
  sql_1 <- "create table "
  sql_2 <- "time_cox_dup_todo as select a.* from "
  sql_3 <- "simple_cox a union all select b.* from "
  sql_4 <- "time_cox_dup b;"
  researcher <- paste0(researcher, '.')
  return(
    paste0(sql_1, researcher, sql_2, researcher, sql_3, researcher, sql_4)
  )
}

coxTimeRowSqlquery <- function(researcher){
  sql_1 <- "create table "
  sql_2 <- "time_cox_dup_todo_rownum as select a.*, row_number() over (partition by a.person_id order by a.covid_date) as time_cov_num, case when a.person_id in (select b.person_id from "
  sql_3 <- "time_cox_dup_todo b) then 1 else 0 end as time_cov_yn from "
  sql_4 <- "time_cox_dup_todo a;"
  researcher <- paste0(researcher, '.')
  return(
    paste0(sql_1, researcher, sql_2, researcher, sql_3, researcher, sql_4)
  )
}

coxTimeFinSqlquery <- function(researcher){
  sql_1 <- "create table "
  sql_2 <- "time_cox_final as select a.*, case when a.time_cov_yn in (0,1) and a.time_cov_num in (1) then 0 when a.time_cov_yn in (1) and a.time_cov_num in (2) then a.time_cov_start else a.time_cov_start end as strt_final, "
  sql_3 <- "case when a.time_cov_yn in (1) and a.time_cov_num in (1) then a.time_cov_start when a.time_cov_yn in (1) and a.time_cov_num in (2) then a.stopp else a.stopp end as stop_final, "
  sql_4 <- "case when a.time_cov_yn in (1) and a.time_cov_num in (1) then 0 when a.time_cov_yn in (1) and a.time_cov_num in (2) then 1 else 1 end as event_final from "
  sql_5 <- "time_cox_dup_todo_rownum a;"
  researcher <- paste0(researcher, '.')
  return(
    paste0(sql_1, researcher, sql_2, sql_3, sql_4, researcher, sql_5)
  )
}

# delete DB
deleteSqlquery <- function(researcher, data){
  return(
    paste0("drop table ", researcher, ".", data, ";")
  )
}