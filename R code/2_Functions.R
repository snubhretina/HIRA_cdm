# --------------- Funtions.R -------------

# TODO : cdm 스키마 명을 입력 해야 합니다.
## 1. cdm_schema - CDM 스키마
# @cdm_schema.condition_occurence 이런식으로 쓰입니다.
## 2. cdm_voca_schema - CDM vocabulary 스키마
# cdm_voca_schema에서 concept_ancestor와 ancestor_concept_id 컬럼을 코호트 조건으로 사용합니다.

cdm_schema <- ""     # 1. cdm_schema
cdm_voca_schema <- ""  # 2. cdm_voca_schema


### FOR CONCEPT_ID HIERARCHY
# 모든 쿼리문에서 동일하게 사용하기 때문에 전역변수로 사용합니다.
sql_id1 <- "select a.descendant_concept_id, a.ancestor_concept_id, b.concept_name from "
sql_id2 <- "concept_ancestor a inner join "
sql_id3 <- "concept b on a.descendant_concept_id = b.concept_id where ancestor_concept_id in ("

### Cohort Definition : func 1 ~3
### func 1. PROCEDURE_OCCURECE TABLE 
# ex) data <- procedureCohort("4283015, 42220065", "2020-01-01") or procedureCohort("4283015, 42220065")
procedureCohort <- function(ancestor_ids, start_date="") {
  # 기본 sql문 토대
  # from 뒤 띄어쓰기 필수
  sql_1 <- "select person_id, procedure_concept_id, procedure_date from " 
  sql_2 <- "procedure_occurrence where procedure_concept_id in (select descendant_concept_id from "
  sql_3 <- "concept_ancestor where ancestor_concept_id in ("
  # 인자 setting
  cdm_schema <- paste0(cdm_schema, '.')  # sql문에는 .이 들어가야함  
  cdm_voca_schema <- paste0(cdm_voca_schema, '.')
  ancestor_ids <- ancestor_ids
  # START_DATE가 입력 되어진 경우
  if (start_date!="") {
    sql_4 <- "and procedure_date >"
    start_date <- paste0("TO_DATE('", start_date, "', 'YYYY-MM-DD')") # sql문에 맞는 형태로 변환
    return (
      c(
        data = querySql(conn, paste0(sql_1, cdm_schema, sql_2, cdm_voca_schema, sql_3, ancestor_ids, ')', sql_4, start_date, ');')),
        ids = querySql(conn, paste0(sql_id1, cdm_voca_schema, sql_id2, cdm_voca_schema, sql_id3, ancestor_ids, ');'))
      )
    )
  }
  # START_DATE가 없는 경우(if문 탈출)
  return (
    c(
      data = querySql(conn, paste0(sql_1, cdm_schema, sql_2, cdm_voca_schema, sql_3, ancestor_ids, '));')),
      ids = querySql(conn, paste0(sql_id1, cdm_voca_schema, sql_id2, cdm_voca_schema, sql_id3, ancestor_ids, ');'))
    )
  )
}

### func 2. CONDITION_OCCURECE TABLE
# ex) data <- conditionCohort("4283015", "2020-01-01")or conditionCohort("4283015, 42220065")
conditionCohort <- function(ancestor_ids, start_date="") {
  # 기본 sql문 토대
  # from 뒤 띄어쓰기 필수
  sql_1 <- "select person_id, condition_concept_id, condition_start_date from " 
  sql_2 <- "condition_occurrence where condition_concept_id in (select descendant_concept_id from "
  sql_3 <- "concept_ancestor where ancestor_concept_id in ("
  # 인자 setting
  cdm_schema <- paste0(cdm_schema, '.')  # sql문에는 .이 들어가야함  
  cdm_voca_schema <- paste0(cdm_voca_schema, '.')
  ancestor_ids <- ancestor_ids
  # START_DATE가 입력 되어진 경우
  if (start_date!="") {
    sql_4 <- "and condition_start_date >"
    start_date <- paste0("TO_DATE('", start_date, "', 'YYYY-MM-DD')") # sql문에 맞는 형태로 변환
    return (
      c(
        data = querySql(conn, paste0(sql_1, cdm_schema, sql_2, cdm_voca_schema, sql_3, ancestor_ids, ')', sql_4, start_date, ');')),
        ids = querySql(conn, paste0(sql_id1, cdm_voca_schema, sql_id2, cdm_voca_schema, sql_id3, ancestor_ids, ');'))
      )
    )
  }
  # START_DATE가 없는 경우(if문 탈출)
  return (
    c(
      data = querySql(conn, paste0(sql_1, cdm_schema, sql_2, cdm_voca_schema, sql_3, ancestor_ids, '));')),
      ids = querySql(conn, paste0(sql_id1, cdm_voca_schema, sql_id2, cdm_voca_schema, sql_id3, ancestor_ids, ');'))
    )
  )
}


### func 3. DRUG_EXPOSURE TABLE
# ex) data <- drugCohort("4283015, "3567841", "2020-01-01")or drugCohort("3567841")

drugCohort <- function(ancestor_ids, start_date="") {
  # 기본 sql문 토대
  # from 뒤 띄어쓰기 필수
  sql_1 <- "select person_id, drug_concept_id, drug_exposure_start_date from " 
  sql_2 <- "drug_exposure where drug_concept_id in (select descendant_concept_id from "
  sql_3 <- "concept_ancestor where ancestor_concept_id in ("
  # 인자 setting
  cdm_schema <- paste0(cdm_schema, '.')  # sql문에는 .이 들어가야함  
  cdm_voca_schema <- paste0(cdm_voca_schema, '.')
  ancestor_ids <- ancestor_ids
  # START_DATE가 입력 되어진 경우
  if (start_date!="") {
    sql_4 <- "and drug_exposure_start_date >"
    start_date <- paste0("TO_DATE('", start_date, "', 'YYYY-MM-DD')") # sql문에 맞는 형태로 변환
    return (
      c(
        data = querySql(conn, paste0(sql_1, cdm_schema, sql_2, cdm_voca_schema, sql_3, ancestor_ids, ')', sql_4, start_date, ');')),
        ids = querySql(conn, paste0(sql_id1, cdm_voca_schema, sql_id2, cdm_voca_schema, sql_id3, ancestor_ids, ');'))
      )
    )
  }
  # START_DATE가 없는 경우(if문 탈출)
  return (
    c(
      data = querySql(conn, paste0(sql_1, cdm_schema, sql_2, cdm_voca_schema, sql_3, ancestor_ids, '));')),
      ids = querySql(conn, paste0(sql_id1, cdm_voca_schema, sql_id2, cdm_voca_schema, sql_id3, ancestor_ids, ');'))
    )
  )
}

#### function 4~6
### makeData : function 1~3에서 만든 함수의 return 값에 data로 접근하여 실제적인 data table을 만들어 리턴
### makeIdTable : function 1~3에서 data를 가져올 때 사용한 컨셉ID들에 대한 위계와 올바르게 매핑되어있는지 확인하기 위해 concept name을 데이터 프레임으로 만들어 리턴 하는 함수 / raw data 는function 1~3의 return 그대로, data는 makeData의 리턴 값

## func 4. makeData_P && makeIdTable_P 
# Procedure table일때 사용
makeData_P <- function(data) {
  PERSON_ID = data$data.PERSON_ID
  PROCEDURE_CONCEPT_ID = data$data.PROCEDURE_CONCEPT_ID
  PROCEDURE_DATE =data$data.PROCEDURE_DATE
  data = data.frame(PERSON_ID, PROCEDURE_CONCEPT_ID, PROCEDURE_DATE)
  return(
    data
  )
}

makeIdTable_P <- function(raw_data, data) {
  DES_ID = raw_data$ids.DESCENDANT_CONCEPT_ID
  ANC_ID = raw_data$ids.ANCESTOR_CONCEPT_ID
  NAME = raw_data$ids.CONCEPT_NAME
  id_data = data.frame(DES_ID, ANC_ID, NAME)
  # data table에서 사용된 ids
  used_id <- data %>%
    group_by(PROCEDURE_CONCEPT_ID) %>%
    filter(row_number()==1) %>%
    select(PROCEDURE_CONCEPT_ID)
  # return할 data : 사용된 ids의 위계정보와 컨셉 name
  data <- id_data %>%
    filter(DES_ID %in% used_id$PROCEDURE_CONCEPT_ID)
  return(
    data
  )
}

## func 5. makeData_C && makeIdTable_C 
# Condition table일때 사용
makeData_C <- function(data) {
  PERSON_ID = data$data.PERSON_ID
  CONDITION_CONCEPT_ID = data$data.CONDITION_CONCEPT_ID
  CONDITION_START_DATE =data$data.CONDITION_START_DATE
  data = data.frame(PERSON_ID, CONDITION_CONCEPT_ID, CONDITION_START_DATE)
  return(
    data
  )
}

makeIdTable_C <- function(raw_data, data) {
  DES_ID = raw_data$ids.DESCENDANT_CONCEPT_ID
  ANC_ID = raw_data$ids.ANCESTOR_CONCEPT_ID
  NAME = raw_data$ids.CONCEPT_NAME
  id_data = data.frame(DES_ID, ANC_ID, NAME)
  # data table에서 사용된 ids
  used_id <- data %>%
    group_by(CONDITION_CONCEPT_ID) %>%
    filter(row_number()==1) %>%
    select(CONDITION_CONCEPT_ID)
  # return할 data : 사용된 ids의 위계정보와 컨셉 name
  data <- id_data %>%
    filter(DES_ID %in% used_id$CONDITION_CONCEPT_ID)
  return(
    data
  )
}

## func 6. makeData_D && makeIdTable_D 
# Drug Expodure table일때 사용
makeData_D <- function(data) {
  PERSON_ID = data$data.PERSON_ID
  DRUG_CONCEPT_ID = data$data.DRUG_CONCEPT_ID
  DRUG_EXPOSURE_START_DATE =data$data.DRUG_EXPOSURE_START_DATE
  data = data.frame(PERSON_ID, DRUG_CONCEPT_ID, DRUG_EXPOSURE_START_DATE)
  return(
    data
  )
}

makeIdTable_D <- function(raw_data, data) {
  DES_ID = raw_data$ids.DESCENDANT_CONCEPT_ID
  ANC_ID = raw_data$ids.ANCESTOR_CONCEPT_ID
  NAME = raw_data$ids.CONCEPT_NAME
  id_data = data.frame(DES_ID, ANC_ID, NAME)
  # data table에서 사용된 ids
  used_id <- data %>%
    group_by(DRUG_CONCEPT_ID) %>%
    filter(row_number()==1) %>%
    select(DRUG_CONCEPT_ID)
  # return할 data : 사용된 ids의 위계정보와 컨셉 name
  data <- id_data %>%
    filter(DES_ID %in% used_id$DRUG_CONCEPT_ID)
  return(
    data
  )
}

### func 7. checkData
# data를 인자로 받아 row의 개수와 사람 수를 return하는 함수
# ex) checked <- checkData(data) / checked["person"] , checked["row"]
checkData <- function(data) {
  return(
    c(person=length(unique(data$PERSON_ID)), row= nrow(data))
  )
}

### func 8. selectFirst
# data를 인자로 받아 첫 병원 방문 row만 select해 data를 return하는 함수
# ex) select_data <- selectFirst(data, "PROCEDURE_DATE") or selectFirst(data, "DRUG_EXPOSURE_START_DATE")
selectFirst <- function(data, column_name) {
  return(
    data %>% 
      group_by (PERSON_ID) %>%
      arrange(column_name) %>%
      filter(row_number()==1)
  )
}

### func 9. unit Count(합산)
unitCount <- function(data) {
  data <- data %>%
    group_by(UNIT_DATE) %>%
    summarise(UNIT_COUNT = n())
  data <- data %>%
    arrange(UNIT_DATE)
  return(
    data
  )
}

### func 10. Washout
# 특정 시점 기준 이전으로 발생한 진단명의 washout person_id만 뽑아오는 query
# ex) washoutIdQuery("4146103", "2018-06-30")
washoutIdQuery <- function(ancestor_ids, washout_end_date) {
  sql_1 <- "select distinct person_id from " 
  sql_2 <- "condition_occurrence where condition_concept_id in (select descendant_concept_id from "
  sql_3 <- "concept_ancestor where ancestor_concept_id in ("
  cdm_schema <- paste0(cdm_schema, '.')  
  cdm_voca_schema <- paste0(cdm_voca_schema, '.')
  ancestor_ids <- ancestor_ids
  sql_4 <- ")) and condition_start_date <="
  washout_end_date <- paste0("TO_DATE('", washout_end_date, "', 'YYYY-MM-DD');") 
  return (
    paste0(sql_1, cdm_schema, sql_2, cdm_voca_schema, sql_3, ancestor_ids, sql_4, washout_end_date)
  )
}























