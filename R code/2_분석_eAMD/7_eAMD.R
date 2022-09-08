# condition RD + Procedure Vitrectomy, buckling
source("R code/1_Settings.R")
source("R code/2_Functions.R")

cdm_schema <- ""     # 1. cdm_schema
cdm_voca_schema <- ""  # 2. cdm_voca_schema

# data 만들기
# TODO : START_DATE
eamd <- conditionCohort("4146103", "2018-07-01")
eamd_data <- makeData_C(eamd)
eamd_used_id <- makeIdTable_C(eamd, eamd_data)

# TODO : START_DATE
luncentis <- drugCohort("21605124, 41405555", "2018-07-01")
eylea <- drugCohort("40257019, 42923303", "2018-07-01")

lucentis_data <- makeData_D(luncentis)
eylea_data <- makeData_D(eylea)

lucentis_ID_data <- makeIdTable_D(lucentis, lucentis_data)
eylea_ID_data <- makeIdTable_D(eylea, eylea_data)

check_lucentis <- checkData(lucentis_data)
check_eylea <- checkData(eylea_data)

# Washout
washoutQuery <- function(ancestor_ids, washout_end_date) {
  sql_1 <- "select person_id, condition_concept_id, condition_start_date from " 
  sql_2 <- "condition_occurrence where condition_concept_id in (select descendant_concept_id from "
  sql_3 <- "concept_ancestor where ancestor_concept_id in ("
  cdm_schema <- paste0(cdm_schema, '.')  
  cdm_voca_schema <- paste0(cdm_voca_schema, '.')
  ancestor_ids <- ancestor_ids
  sql_4 <- "and condition_start_date <="
  washout_end_date <- paste0("TO_DATE('", washout_end_date, "', 'YYYY-MM-DD')") 
  return (
      #data = querySql(conn, paste0(sql_1, cdm_schema, sql_2, cdm_voca_schema, sql_3, ancestor_ids, '));'))
      paste0(sql_1, cdm_schema, sql_2, cdm_voca_schema, sql_3, ancestor_ids, sql_4, washout_end_date, ');')
  )
}

