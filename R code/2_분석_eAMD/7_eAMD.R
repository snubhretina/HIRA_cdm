# condition RD + Procedure Vitrectomy, buckling
source("R code/1_Settings.R")
source("R code/2_Functions.R")

cdm_schema <- "cdm_2020_view"     # 1. cdm_schema
cdm_voca_schema <- "cdm_voca"  # 2. cdm_voca_schema

# data 만들기
# TODO : START_DATE
eamd <- conditionCohort("4146103")
eamd_data <- makeData_C(eamd)
eamd_used_id <- makeIdTable_C(eamd, eamd_data)

# TODO : START_DATE
luncentis <- drugCohort("21605124, 41405555", "2018-07-01")
eylea <- drugCohort("40257019, 42923303", "2018-07-01")

lucentis_data <- makeData_D(luncentis)
eylea_data <- makeData_D(eylea)

lucentis_ID_data <- makeIdTable_D(lucentis, lucentis_data)
eylea_ID_data <- makeIdTable_D(eylea, eylea_data)
lucentis_ID_data["DRUG_CLASS"] = "Lucentis"
eylea_ID_data["DRUG_CLASS"] = "Eylea"

used_id7 <- rbind(lucentis_ID_data, eylea_ID_data)
write.csv(used_id7, file="./result_eamd/used_id")

used_lucentis_id <- lucentis_ID_data$DES_ID
used_eylea_id <- eylea_ID_data$DES_ID
used_drug_id <- c(used_lucentis_id, used_eylea_id)

check_lucentis <- checkData(lucentis_data)
check_eylea <- checkData(eylea_data)

drug_data <- rbind(lucentis_data, eylea_data)
check_drug <- checkData(drug_data)

# Washout
washout_id <- querySql(conn, washoutIdQuery("4146103", "2018-06-30"))
eamd_data_todo <- eamd_data %>% filter(!PERSON_ID %in% washout_id$PERSON_ID)

# Data todo
drug_data_todo <- drug_data %>% filter(PERSON_ID %in% eamd_data_todo$PERSON_ID)

# 전체 주사횟수
#연도별
drug_data_todo$year <- format(as.Date(drug_data_todo$DRUG_EXPOSURE_START_DATE), "%Y")

all <- drug_data_todo %>%
  group_by(year) %>%
  summarise(count_n = n_distinct(PERSON_ID), count_inj = n())

luce <- drug_data_todo %>%
  filter(DRUG_CONCEPT_ID %in% used_lucentis_id) %>%
  group_by(year) %>%
  summarise(count_n = n_distinct(PERSON_ID), count_inj = n())

eyle <- drug_data_todo %>%
  filter(DRUG_CONCEPT_ID %in% used_eylea_id) %>%
  group_by(year) %>%
  summarise(count_n = n_distinct(PERSON_ID), count_inj = n())

mergeAll <- merge(merge(
  all,
  rani, by = 'year', all = TRUE),
  afli, by = 'year', all = TRUE)

colnames(mergeAll) <- c("YEAR", 
                        "ALLDRUG_N", "ALLDRUG_INJ",
                        "RANI_N", "RANI_INJ", 
                        "AFLI_N", "AFLI_INJ")

write.csv(mergeAll, file = "./result_eamd/all_drug_count_by_year.csv")

#월별
drug_data_todo["UNIT_DATE"] = floor_date(drug_data_todo$DRUG_EXPOSURE_START_DATE, unit="month")

all_m <- drug_data_todo %>%
  group_by(UNIT_DATE) %>%
  summarise(count_n = n_distinct(PERSON_ID), count_inj = n())

luce_m <- drug_data_todo %>%
  filter(DRUG_CONCEPT_ID %in% used_lucentis_id) %>%
  group_by(UNIT_DATE) %>%
  summarise(count_n = n_distinct(PERSON_ID), count_inj = n())

eyle_m <- drug_data_todo %>%
  filter(DRUG_CONCEPT_ID %in% used_eylea_id) %>%
  group_by(UNIT_DATE) %>%
  summarise(count_n = n_distinct(PERSON_ID), count_inj = n())

mergeAll_m <- merge(merge(
  all_m,
  rani_m, by = 'UNIT_DATE', all = TRUE),
  afli_m, by = 'UNIT_DATE', all = TRUE)

colnames(mergeAll_m) <- c("MONTH", 
                        "ALLDRUG_N", "ALLDRUG_INJ",
                        "RANI_N", "RANI_INJ", 
                        "AFLI_N", "AFLI_INJ")

write.csv(mergeAll_m, file = "./result_eamd/all_drug_count_by_month.csv")

# Index Date(진단일) 기준 진단발생
# select first
drug_data_s <- selectFirst(drug_data_todo, "DRUG_EXPOSURE_START_DATE")

#연도별
drug_data_s$year <- format(as.Date(drug_data_s$DRUG_EXPOSURE_START_DATE), "%Y")

all_ind <- drug_data_s %>%
  group_by(year) %>%
  summarise(count_n = n_distinct(PERSON_ID), count_inj = n())

luce_ind <- drug_data_s %>%
  filter(DRUG_CONCEPT_ID %in% used_lucentis_id) %>%
  group_by(year) %>%
  summarise(count_n = n_distinct(PERSON_ID), count_inj = n())

eyle_ind <- drug_data_s %>%
  filter(DRUG_CONCEPT_ID %in% used_eylea_id) %>%
  group_by(year) %>%
  summarise(count_n = n_distinct(PERSON_ID), count_inj = n())

mergeAll_ind <- merge(merge(
  all_ind,
  rani_ind, by = 'year', all = TRUE),
  afli_ind, by = 'year', all = TRUE)

colnames(mergeAll_ind) <- c("YEAR", 
                        "ALLDRUG_N", "ALLDRUG_INJ",
                        "RANI_N", "RANI_INJ", 
                        "AFLI_N", "AFLI_INJ")

write.csv(mergeAll_ind, file = "./result_eamd/drug_count_by_year.csv")

#월별
drug_data_s["UNIT_DATE"] = floor_date(drug_data_s$DRUG_EXPOSURE_START_DATE, unit="month")

all_ind_m <- drug_data_s %>%
  group_by(UNIT_DATE) %>%
  summarise(count_n = n_distinct(PERSON_ID), count_inj = n())

luce_ind_m <- drug_data_s %>%
  filter(DRUG_CONCEPT_ID %in% used_lucentis_id) %>%
  group_by(UNIT_DATE) %>%
  summarise(count_n = n_distinct(PERSON_ID), count_inj = n())

eyle_ind_m <- drug_data_s %>%
  filter(DRUG_CONCEPT_ID %in% used_eylea_id) %>%
  group_by(UNIT_DATE) %>%
  summarise(count_n = n_distinct(PERSON_ID), count_inj = n())

mergeAll_ind_m <- merge(merge(
  all_ind_m,
  rani_ind_m, by = 'UNIT_DATE', all = TRUE),
  afli_ind_m, by = 'UNIT_DATE', all = TRUE)

colnames(mergeAll_ind_m) <- c("MONTH", 
                          "ALLDRUG_N", "ALLDRUG_INJ",
                          "RANI_N", "RANI_INJ", 
                          "AFLI_N", "AFLI_INJ")

write.csv(mergeAll_ind_m, file = "./result_eamd/drug_count_by_month.csv")