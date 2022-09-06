# --------------- Funtions.R -------------
# 코호트를 만들고, 기초적인 data 처리 관련 함수들이 정의 되어 있습니다.

# TODO : cdm 스키마 명을 입력 해야 합니다.
## 1. cdm_schema - CDM 스키마
# @cdm_schema.condition_occurence 이런식으로 쓰입니다.

## 2. cdm_voca_schema - CDM vocabulary 스키마
# cdm_voca_schema에서 concept_ancestor와 ancestor_concept_id 컬럼을 코호트 조건으로 사용합니다.

cdm_schema <- ""     # 1. cdm_schema
cdm_voca_schema <- ""  # 2. cdm_voca_schema


### func 1. PROCEDURE_OCCURECE TABLE 
# ex) data <- procedureCohort("4283015, 42220065", "2020-01-01") or procedureCohort("4283015, 42220065")
procedureCohort <- function(ancestor_ids, start_date="") {
  # 기본 sql문 토대
  # from 뒤 띄어쓰기 필수
  sql_1 <- "select person_id, procedure_concept_id, procedure_date from " 
  sql_2 <- "procedure_occurrence where procedure_concept_id in (selecet descendant_concept_id from "
  sql_3 <- "concept_ancestor where ancestor_concept_id in ("
  # 인자 setting
  cdm_schema <- paste0(cdm, '.')  # sql문에는 .이 들어가야함  
  cdm_voca_schema <- paste0(voca, '.')
  ancestor_ids <- ancestor_ids
  # START_DATE가 입력 되어진 경우
  if (start_date!="") {
    sql_4 <- "and procedure_date >"
    start_date <- paste0("TO_DATE('", start_date, "', 'YYYY-MM-DD')") # sql문에 맞는 형태로 변환
    return (
      querySql(conn, paste0(sql_1, cdm_schema, sql_2, cdm_voca_schema, sql_3, ancestor_ids, ')', sql_4, start_date, ');'))
    )
  }
  # START_DATE가 없는 경우(if문 탈출)
  return (
    querySql(conn, paste0(sql_1, cdm_schema, sql_2, cdm_voca_schema, sql_3, ancestor_ids, '));'))
  )
}

### func 2. CONDITION_OCCURECE TABLE
# ex) data <- conditionCohort("4283015", "2020-01-01")or conditionCohort("4283015, 42220065")
conditionCohort <- function(ancestor_ids, start_date="") {
  # 기본 sql문 토대
  # from 뒤 띄어쓰기 필수
  sql_1 <- "select person_id, condition_concept_id, condition_start_date from " 
  sql_2 <- "condition_occurrence where condition_concept_id in (selecet descendant_concept_id from "
  sql_3 <- "concept_ancestor where ancestor_concept_id in ("
  # 인자 setting
  cdm_schema <- paste0(cdm, '.')  # sql문에는 .이 들어가야함  
  cdm_voca_schema <- paste0(voca, '.')
  ancestor_ids <- ancestor_ids
  # START_DATE가 입력 되어진 경우
  if (start_date!="") {
    sql_4 <- "and condition_start_date >"
    start_date <- paste0("TO_DATE('", start_date, "', 'YYYY-MM-DD')") # sql문에 맞는 형태로 변환
    return (
      querySql(conn, paste0(sql_1, cdm_schema, sql_2, cdm_voca_schema, sql_3, ancestor_ids, ')', sql_4, start_date, ');'))
    )
  }
  # START_DATE가 없는 경우(if문 탈출)
  return (
    querySql(conn, paste0(sql_1, cdm_schema, sql_2, cdm_voca_schema, sql_3, ancestor_ids, '));'))
  )
}


### func 3. DRUG_EXPOSURE TABLE
# ex) data <- drugCohort("4283015, "3567841", "2020-01-01")or drugCohort("3567841")

drugCohort <- function(ancestor_ids, start_date="") {
  # 기본 sql문 토대
  # from 뒤 띄어쓰기 필수
  sql_1 <- "select person_id, drug_concept_id, drug_exposure_date from " 
  sql_2 <- "drug_occurrence where drug_concept_id in (selecet descendant_concept_id from "
  sql_3 <- "concept_ancestor where ancestor_concept_id in ("
  # 인자 setting
  cdm_schema <- paste0(cdm, '.')  # sql문에는 .이 들어가야함  
  cdm_voca_schema <- paste0(voca, '.')
  ancestor_ids <- ancestor_ids
  # START_DATE가 입력 되어진 경우
  if (start_date!="") {
    sql_4 <- "and drug_exposure_start_date >"
    start_date <- paste0("TO_DATE('", start_date, "', 'YYYY-MM-DD')") # sql문에 맞는 형태로 변환
    return (
      querySql(conn, paste0(sql_1, cdm_schema, sql_2, cdm_voca_schema, sql_3, ancestor_ids, ')', sql_4, start_date, ');'))
    )
  }
  # START_DATE가 없는 경우(if문 탈출)
  return (
    querySql(conn, paste0(sql_1, cdm_schema, sql_2, cdm_voca_schema, sql_3, ancestor_ids, '));'))
  )
}

### func 4. checkData
# data를 인자로 받아 row의 개수와 사람 수를 return하는 함수
# ex) checked <- checkData(data) / checked["person"] , checked["row"]
checkData <- function(data) {
  return(
    c(person=length(unique(data$PERSON_ID)), row= nrow(data))
  )
}

### func 5. selectFirst
# data를 인자로 받아 첫 병원 방문 row만 select해 data를 return하는 함수
# ex) selecet_data <- selectFirst(data, "PROCEDURE_DATE") or selectFirst(data, "DRUG_EXPOSURE_START_DATE")
selectFirst <- function(data, column_name) {
  return(
    data %>% 
      group_by (PERSON_ID) %>%
      arrange(column_name) %>%
      filter(row_number()==1)
  )
}

























