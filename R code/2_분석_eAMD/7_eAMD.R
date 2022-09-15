# exudative AMD
source("1_Settings.R")
source("2_Functions.R")

## TODO : 현재 작업 위치 적어주세요.
cdm_schema <- ""     # 1. cdm_schema
cdm_voca_schema <- ""  # 2. cdm_voca_schema

# data 만들기
eamd <- conditionCohort("4146103")
eamd_data <- makeData_C(eamd)
eamd_used_id <- makeIdTable_C(eamd, eamd_data)

# TODO : START_DATE
lucentis <- drugCohort("21605124, 41405555", "2018-07-01")
eylea <- drugCohort("40257019, 42923303", "2018-07-01")

lucentis_data <- makeData_D(luncentis)
eylea_data <- makeData_D(eylea)

lucentis_ID_data <- makeIdTable_D(lucentis, lucentis_data)
eylea_ID_data <- makeIdTable_D(eylea, eylea_data)
lucentis_ID_data["DRUG_CLASS"] = "Lucentis"
eylea_ID_data["DRUG_CLASS"] = "Eylea"

used_id7 <- rbind(lucentis_ID_data, eylea_ID_data)
write.csv(used_id7, file="./result_eamd/used_id.csv")

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

##################################################################################################################################
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
  luce, by = 'year', all = TRUE),
  eyle, by = 'year', all = TRUE)

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
  luce_m, by = 'UNIT_DATE', all = TRUE),
  eyle_m, by = 'UNIT_DATE', all = TRUE)

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
  luce_ind, by = 'year', all = TRUE),
  eyle_ind, by = 'year', all = TRUE)

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
  luce_ind_m, by = 'UNIT_DATE', all = TRUE),
  eyle_ind_m, by = 'UNIT_DATE', all = TRUE)

colnames(mergeAll_ind_m) <- c("MONTH", 
                          "ALLDRUG_N", "ALLDRUG_INJ",
                          "RANI_N", "RANI_INJ", 
                          "AFLI_N", "AFLI_INJ")

write.csv(mergeAll_ind_m, file = "./result_eamd/drug_count_by_month.csv")

##################################################################################################################################
# 1년째, 2년째 평균 주사횟수
yrall <- drug_data_todo %>% 
  group_by(PERSON_ID) %>% 
  arrange(DRUG_EXPOSURE_START_DATE) %>% mutate(PERSONAL_DRUG_ASD_NUMBER = row_number())

yrall_first <- yrall %>% filter(PERSONAL_DRUG_ASD_NUMBER==1) %>% select('PERSON_ID','DRUG_EXPOSURE_START_DATE','DRUG_CONCEPT_ID')
colnames(yrall_first) <- c("PERSON_ID", 'FIRST_DRUG_EXPOSURE_START_DATE', "FIRST_TREATMENT")
yrall <- merge(yrall, yrall_first , by='PERSON_ID')
yrall['dayDiff'] <- yrall['DRUG_EXPOSURE_START_DATE'] - yrall['FIRST_DRUG_EXPOSURE_START_DATE']
yrall['year_gp'] <- ifelse(yrall$dayDiff<=365, 1, ifelse(yrall$dayDiff<=730, 2, ifelse(yrall$dayDiff<=1095,3,9999)))


# create table 
table_mean <- matrix(ncol = 6, byrow = T)
table_mean <- data.frame(table_mean)
colnames(table_mean) <- c("Overall_mean","Overall_sd",
                      "Rani_Ini_mean","Rani_Ini_sd",
                      "Afli_Ini_mean","Afli_Ini_sd")

table_mean_sup <- matrix(ncol = 9, byrow = T)
table_mean_sup <- data.frame(table_mean_sup)
colnames(table_mean_sup) <- c("Overall_Tot_Count","Overall_ID_Count","Overall_noinjcnt",
                          "Rani_Tot_Count","Rani_ID_Count","Rani_noinjcnt",
                          "Afli_Tot_Count","Afli_ID_Count","Afli_noinjcnt")

# Mean
####
yr1_data <- yrall %>% filter(year_gp == 1)
yr1_counts <- data.frame(table(yr1_data$PERSON_ID))
table_mean[1,1] <- round(mean(yr1_counts$Freq),2)
table_mean[1,2] <- round(sd(yr1_counts$Freq),2)

table_mean_sup[1,1] <- length(yr1_data$PERSON_ID)
table_mean_sup[1,2] <- length(unique(yr1_data$PERSON_ID))
table_mean_sup[1,3] <- length(unique(yr1_data$PERSON_ID)) -  length(unique(yr1_data$PERSON_ID))
#####
yr2_data <- yrall %>% filter(year_gp == 2)
yr2_counts <- data.frame(table(yr2_data$PERSON_ID))
table_mean[2,1] <- round(mean(yr2_counts$Freq),2)
table_mean[2,2] <- round(sd(yr2_counts$Freq),2)

table_mean_sup[2,1] <- length(yr2_data$PERSON_ID)
table_mean_sup[2,2] <- length(unique(yr2_data$PERSON_ID))
table_mean_sup[2,3] <- length(unique(yr1_data$PERSON_ID)) - length(unique(yr2_data$PERSON_ID))
#####
yr3_data <- yrall %>% filter(year_gp == 3)
yr3_counts <- data.frame(table(yr3_data$PERSON_ID))
table_mean[3,1] <- round(mean(yr3_counts$Freq),2)
table_mean[3,2] <- round(sd(yr3_counts$Freq),2)

table_mean_sup[3,1] <- length(yr3_data$PERSON_ID)
table_mean_sup[3,2] <- length(unique(yr3_data$PERSON_ID))
table_mean_sup[3,3] <- length(unique(yr1_data$PERSON_ID)) - length(unique(yr3_data$PERSON_ID))
####
table_mean[4,1] <- round(mean(c(mean(yr1_counts$Freq),mean(yr2_counts$Freq),mean(yr3_counts$Freq))),2)
table_mean[4,2] <- round(sd(c(mean(yr1_counts$Freq),mean(yr2_counts$Freq),mean(yr3_counts$Freq))),2)




{
  ####
  yr1_data_rani_lst <- yr1_data %>% filter(DRUG_CONCEPT_ID %in% used_lucentis_id) 
  yr1_rani <- yr1_data %>% filter(PERSON_ID %in% yr1_data_rani_lst$PERSON_ID)
  yr1_rani_counts <- data.frame(table(yr1_rani$PERSON_ID))
  
  table_mean[1,3] <- round(mean(yr1_rani_counts$Freq),2)
  table_mean[1,4] <- round(sd(yr1_rani_counts$Freq),2)
  
  table_mean_sup[1,4] <- length(yr1_rani$PERSON_ID)
  table_mean_sup[1,5] <- length(unique(yr1_rani$PERSON_ID))
  table_mean_sup[1,6] <- length(unique(yr1_rani$PERSON_ID)) -  length(unique(yr1_rani$PERSON_ID))
  ####
  yr1_data_afli_lst <- yr1_data %>% filter(DRUG_CONCEPT_ID %in% used_eylea_id) 
  yr1_afli <- yr1_data %>% filter(PERSON_ID %in% yr1_data_afli_lst$PERSON_ID)
  yr1_afli_counts <- data.frame(table(yr1_afli$PERSON_ID))
  
  table_mean[1,5] <- round(mean(yr1_afli_counts$Freq),2)
  table_mean[1,6] <- round(sd(yr1_afli_counts$Freq),2)
  
  table_mean_sup[1,7] <- length(yr1_afli$PERSON_ID)
  table_mean_sup[1,8] <- length(unique(yr1_afli$PERSON_ID))
  table_mean_sup[1,9] <- length(unique(yr1_afli$PERSON_ID)) -  length(unique(yr1_afli$PERSON_ID))
}


{
  ####
  yr2_data_rani_lst <- yr2_data %>% filter(DRUG_CONCEPT_ID %in% used_lucentis_id) 
  yr2_rani <- yr2_data %>% filter(PERSON_ID %in% yr1_data_rani_lst$PERSON_ID)
  yr2_rani_counts <- data.frame(table(yr2_rani$PERSON_ID))
  
  table_mean[2,3] <- round(mean(yr2_rani_counts$Freq),2)
  table_mean[2,4] <- round(sd(yr2_rani_counts$Freq),2)
  
  table_mean_sup[2,4] <- length(yr2_rani$PERSON_ID)
  table_mean_sup[2,5] <- length(unique(yr2_rani$PERSON_ID))
  table_mean_sup[2,6] <- length(unique(yr1_rani$PERSON_ID)) -  length(unique(yr2_rani$PERSON_ID))
  ####
  yr2_data_afli_lst <- yr2_data %>% filter(DRUG_CONCEPT_ID %in% used_eylea_id) 
  yr2_afli <- yr2_data %>% filter(PERSON_ID %in% yr1_data_afli_lst$PERSON_ID)
  yr2_afli_counts <- data.frame(table(yr2_afli$PERSON_ID))
  
  table_mean[2,5] <- round(mean(yr2_afli_counts$Freq),2)
  table_mean[2,6] <- round(sd(yr2_afli_counts$Freq),2)
  
  table_mean_sup[2,7] <- length(yr2_afli$PERSON_ID)
  table_mean_sup[2,8] <- length(unique(yr2_afli$PERSON_ID))
  table_mean_sup[2,9] <- length(unique(yr1_afli$PERSON_ID)) -  length(unique(yr2_afli$PERSON_ID))
}



{
  ####
  yr3_data_rani_lst <- yr3_data %>% filter(DRUG_CONCEPT_ID %in% used_lucentis_id) 
  yr3_rani <- yr3_data %>% filter(PERSON_ID %in% yr1_data_rani_lst$PERSON_ID)
  yr3_rani_counts <- data.frame(table(yr3_rani$PERSON_ID))
  
  table_mean[3,3] <- round(mean(yr3_rani_counts$Freq),2)
  table_mean[3,4] <- round(sd(yr3_rani_counts$Freq),2)
  
  table_mean_sup[3,4] <- length(yr3_rani$PERSON_ID)
  table_mean_sup[3,5] <- length(unique(yr3_rani$PERSON_ID))
  table_mean_sup[3,6] <- length(unique(yr1_rani$PERSON_ID)) -  length(unique(yr3_rani$PERSON_ID))
  ####
  yr3_data_afli_lst <- yr3_data %>% filter(DRUG_CONCEPT_ID %in% used_eylea_id) 
  yr3_afli <- yr3_data %>% filter(PERSON_ID %in% yr1_data_afli_lst$PERSON_ID)
  yr3_afli_counts <- data.frame(table(yr3_afli$PERSON_ID))
  
  table_mean[3,5] <- round(mean(yr3_afli_counts$Freq),2)
  table_mean[3,6] <- round(sd(yr3_afli_counts$Freq),2)
  
  table_mean_sup[3,7] <- length(yr3_afli$PERSON_ID)
  table_mean_sup[3,8] <- length(unique(yr3_afli$PERSON_ID))
  table_mean_sup[3,9] <- length(unique(yr1_afli$PERSON_ID)) -  length(unique(yr3_afli$PERSON_ID))
}


{
  ####
  table_mean[4,3] <- round(mean(c(mean(yr1_rani_counts$Freq),mean(yr2_rani_counts$Freq),mean(yr3_rani_counts$Freq))),2)
  table_mean[4,4] <- round(sd(c(mean(yr1_rani_counts$Freq),mean(yr2_rani_counts$Freq),mean(yr3_rani_counts$Freq))),2)
  table_mean[4,5] <- round(mean(c(mean(yr1_afli_counts$Freq),mean(yr2_afli_counts$Freq),mean(yr3_afli_counts$Freq))),2)
  table_mean[4,6] <- round(sd(c(mean(yr1_afli_counts$Freq),mean(yr2_afli_counts$Freq),mean(yr3_afli_counts$Freq))),2)
}

write.csv(table_mean, file='./result_eamd/table_mean.csv')
write.csv(table_mean_sup, file='./result_eamd/table_mean_sup.csv')

## 코로나 시점 전 2018-07-01 ~ 2019-12-31
drug_before_covid <- yrall %>%
  filter(FIRST_DRUG_EXPOSURE_START_DATE < "2020-01-01")

## 코로나 시점 후 2020-01-01 ~ 2022-04-30
drug_after_covid <- yrall %>%
  filter("2019-12-31" < FIRST_DRUG_EXPOSURE_START_DATE)

# create table 
table_mean_bfcvd <- matrix(ncol = 6, byrow = T)
table_mean_bfcvd <- data.frame(table_mean_bfcvd)
colnames(table_mean_bfcvd) <- c("Overall_mean","Overall_sd",
                                "Rani_Ini_mean","Rani_Ini_sd",
                                "Afli_Ini_mean","Afli_Ini_sd")

table_mean_bfcvd_sup <- matrix(ncol = 9, byrow = T)
table_mean_bfcvd_sup <- data.frame(table_mean_bfcvd_sup)
colnames(table_mean_bfcvd_sup) <- c("Overall_Tot_Count","Overall_ID_Count","Overall_noinjcnt",
                                    "Rani_Tot_Count","Rani_ID_Count","Rani_noinjcnt",
                                    "Afli_Tot_Count","Afli_ID_Count","Afli_noinjcnt")

# Mean
####
yr1_data <- drug_before_covid %>% filter(year_gp == 1)
yr1_counts <- data.frame(table(yr1_data$PERSON_ID))
table_mean_bfcvd[1,1] <- round(mean(yr1_counts$Freq),2)
table_mean_bfcvd[1,2] <- round(sd(yr1_counts$Freq),2)

table_mean_bfcvd_sup[1,1] <- length(yr1_data$PERSON_ID)
table_mean_bfcvd_sup[1,2] <- length(unique(yr1_data$PERSON_ID))
table_mean_bfcvd_sup[1,3] <- length(unique(yr1_data$PERSON_ID)) -  length(unique(yr1_data$PERSON_ID))
#####
yr2_data <- drug_before_covid %>% filter(year_gp == 2)
yr2_counts <- data.frame(table(yr2_data$PERSON_ID))
table_mean_bfcvd[2,1] <- round(mean(yr2_counts$Freq),2)
table_mean_bfcvd[2,2] <- round(sd(yr2_counts$Freq),2)

table_mean_bfcvd_sup[2,1] <- length(yr2_data$PERSON_ID)
table_mean_bfcvd_sup[2,2] <- length(unique(yr2_data$PERSON_ID))
table_mean_bfcvd_sup[2,3] <- length(unique(yr1_data$PERSON_ID)) - length(unique(yr2_data$PERSON_ID))
#####
yr3_data <- drug_before_covid %>% filter(year_gp == 3)
yr3_counts <- data.frame(table(yr3_data$PERSON_ID))
table_mean_bfcvd[3,1] <- round(mean(yr3_counts$Freq),2)
table_mean_bfcvd[3,2] <- round(sd(yr3_counts$Freq),2)

table_mean_bfcvd_sup[3,1] <- length(yr3_data$PERSON_ID)
table_mean_bfcvd_sup[3,2] <- length(unique(yr3_data$PERSON_ID))
table_mean_bfcvd_sup[3,3] <- length(unique(yr1_data$PERSON_ID)) - length(unique(yr3_data$PERSON_ID))
####
table_mean_bfcvd[4,1] <- round(mean(c(mean(yr1_counts$Freq),mean(yr2_counts$Freq),mean(yr3_counts$Freq))),2)
table_mean_bfcvd[4,2] <- round(sd(c(mean(yr1_counts$Freq),mean(yr2_counts$Freq),mean(yr3_counts$Freq))),2)




{
  ####
  yr1_data_rani_lst <- yr1_data %>% filter(DRUG_CONCEPT_ID %in% used_lucentis_id) 
  yr1_rani <- yr1_data %>% filter(PERSON_ID %in% yr1_data_rani_lst$PERSON_ID)
  yr1_rani_counts <- data.frame(table(yr1_rani$PERSON_ID))
  
  table_mean_bfcvd[1,3] <- round(mean(yr1_rani_counts$Freq),2)
  table_mean_bfcvd[1,4] <- round(sd(yr1_rani_counts$Freq),2)
  
  table_mean_bfcvd_sup[1,4] <- length(yr1_rani$PERSON_ID)
  table_mean_bfcvd_sup[1,5] <- length(unique(yr1_rani$PERSON_ID))
  table_mean_bfcvd_sup[1,6] <- length(unique(yr1_rani$PERSON_ID)) -  length(unique(yr1_rani$PERSON_ID))
  ####
  yr1_data_afli_lst <- yr1_data %>% filter(DRUG_CONCEPT_ID %in% used_eylea_id) 
  yr1_afli <- yr1_data %>% filter(PERSON_ID %in% yr1_data_afli_lst$PERSON_ID)
  yr1_afli_counts <- data.frame(table(yr1_afli$PERSON_ID))
  
  table_mean_bfcvd[1,5] <- round(mean(yr1_afli_counts$Freq),2)
  table_mean_bfcvd[1,6] <- round(sd(yr1_afli_counts$Freq),2)
  
  table_mean_bfcvd_sup[1,7] <- length(yr1_afli$PERSON_ID)
  table_mean_bfcvd_sup[1,8] <- length(unique(yr1_afli$PERSON_ID))
  table_mean_bfcvd_sup[1,9] <- length(unique(yr1_afli$PERSON_ID)) -  length(unique(yr1_afli$PERSON_ID))
}


{
  ####
  yr2_data_rani_lst <- yr2_data %>% filter(DRUG_CONCEPT_ID %in% used_lucentis_id) 
  yr2_rani <- yr2_data %>% filter(PERSON_ID %in% yr1_data_rani_lst$PERSON_ID)
  yr2_rani_counts <- data.frame(table(yr2_rani$PERSON_ID))
  
  table_mean_bfcvd[2,3] <- round(mean(yr2_rani_counts$Freq),2)
  table_mean_bfcvd[2,4] <- round(sd(yr2_rani_counts$Freq),2)
  
  table_mean_bfcvd_sup[2,4] <- length(yr2_rani$PERSON_ID)
  table_mean_bfcvd_sup[2,5] <- length(unique(yr2_rani$PERSON_ID))
  table_mean_bfcvd_sup[2,6] <- length(unique(yr1_rani$PERSON_ID)) -  length(unique(yr2_rani$PERSON_ID))
  ####
  yr2_data_afli_lst <- yr2_data %>% filter(DRUG_CONCEPT_ID %in% used_eylea_id) 
  yr2_afli <- yr2_data %>% filter(PERSON_ID %in% yr1_data_afli_lst$PERSON_ID)
  yr2_afli_counts <- data.frame(table(yr2_afli$PERSON_ID))
  
  table_mean_bfcvd[2,5] <- round(mean(yr2_afli_counts$Freq),2)
  table_mean_bfcvd[2,6] <- round(sd(yr2_afli_counts$Freq),2)
  
  table_mean_bfcvd_sup[2,7] <- length(yr2_afli$PERSON_ID)
  table_mean_bfcvd_sup[2,8] <- length(unique(yr2_afli$PERSON_ID))
  table_mean_bfcvd_sup[2,9] <- length(unique(yr1_afli$PERSON_ID)) -  length(unique(yr2_afli$PERSON_ID))
}



{
  ####
  yr3_data_rani_lst <- yr3_data %>% filter(DRUG_CONCEPT_ID %in% used_lucentis_id) 
  yr3_rani <- yr3_data %>% filter(PERSON_ID %in% yr1_data_rani_lst$PERSON_ID)
  yr3_rani_counts <- data.frame(table(yr3_rani$PERSON_ID))
  
  table_mean_bfcvd[3,3] <- round(mean(yr3_rani_counts$Freq),2)
  table_mean_bfcvd[3,4] <- round(sd(yr3_rani_counts$Freq),2)
  
  table_mean_bfcvd_sup[3,4] <- length(yr3_rani$PERSON_ID)
  table_mean_bfcvd_sup[3,5] <- length(unique(yr3_rani$PERSON_ID))
  table_mean_bfcvd_sup[3,6] <- length(unique(yr1_rani$PERSON_ID)) -  length(unique(yr3_rani$PERSON_ID))
  ####
  yr3_data_afli_lst <- yr3_data %>% filter(DRUG_CONCEPT_ID %in% used_eylea_id) 
  yr3_afli <- yr3_data %>% filter(PERSON_ID %in% yr1_data_afli_lst$PERSON_ID)
  yr3_afli_counts <- data.frame(table(yr3_afli$PERSON_ID))
  
  table_mean_bfcvd[3,5] <- round(mean(yr3_afli_counts$Freq),2)
  table_mean_bfcvd[3,6] <- round(sd(yr3_afli_counts$Freq),2)
  
  table_mean_bfcvd_sup[3,7] <- length(yr3_afli$PERSON_ID)
  table_mean_bfcvd_sup[3,8] <- length(unique(yr3_afli$PERSON_ID))
  table_mean_bfcvd_sup[3,9] <- length(unique(yr1_afli$PERSON_ID)) -  length(unique(yr3_afli$PERSON_ID))
}


{
  ####
  table_mean_bfcvd[4,3] <- round(mean(c(mean(yr1_rani_counts$Freq),mean(yr2_rani_counts$Freq),mean(yr3_rani_counts$Freq))),2)
  table_mean_bfcvd[4,4] <- round(sd(c(mean(yr1_rani_counts$Freq),mean(yr2_rani_counts$Freq),mean(yr3_rani_counts$Freq))),2)
  table_mean_bfcvd[4,5] <- round(mean(c(mean(yr1_afli_counts$Freq),mean(yr2_afli_counts$Freq),mean(yr3_afli_counts$Freq))),2)
  table_mean_bfcvd[4,6] <- round(sd(c(mean(yr1_afli_counts$Freq),mean(yr2_afli_counts$Freq),mean(yr3_afli_counts$Freq))),2)
}

write.csv(table_mean_bfcvd, file='./result_eamd/table_mean_bfcvd.csv')
write.csv(table_mean_bfcvd_sup, file='./result_eamd/table_mean_bfcvd_sup.csv')

# create table 
table_mean_aftcvd <- matrix(ncol = 6, byrow = T)
table_mean_aftcvd <- data.frame(table_mean_aftcvd)
colnames(table_mean_aftcvd) <- c("Overall_mean","Overall_sd",
                                 "Rani_Ini_mean","Rani_Ini_sd",
                                 "Afli_Ini_mean","Afli_Ini_sd")

table_mean_aftcvd_sup <- matrix(ncol = 9, byrow = T)
table_mean_aftcvd_sup <- data.frame(table_mean_aftcvd_sup)
colnames(table_mean_aftcvd_sup) <- c("Overall_Tot_Count","Overall_ID_Count","Overall_noinjcnt",
                                     "Rani_Tot_Count","Rani_ID_Count","Rani_noinjcnt",
                                     "Afli_Tot_Count","Afli_ID_Count","Afli_noinjcnt")

# Mean
####
yr1_data <- drug_after_covid %>% filter(year_gp == 1)
yr1_counts <- data.frame(table(yr1_data$PERSON_ID))
table_mean_aftcvd[1,1] <- round(mean(yr1_counts$Freq),2)
table_mean_aftcvd[1,2] <- round(sd(yr1_counts$Freq),2)

table_mean_aftcvd_sup[1,1] <- length(yr1_data$PERSON_ID)
table_mean_aftcvd_sup[1,2] <- length(unique(yr1_data$PERSON_ID))
table_mean_aftcvd_sup[1,3] <- length(unique(yr1_data$PERSON_ID)) -  length(unique(yr1_data$PERSON_ID))
#####
yr2_data <- drug_after_covid %>% filter(year_gp == 2)
yr2_counts <- data.frame(table(yr2_data$PERSON_ID))
table_mean_aftcvd[2,1] <- round(mean(yr2_counts$Freq),2)
table_mean_aftcvd[2,2] <- round(sd(yr2_counts$Freq),2)

table_mean_aftcvd_sup[2,1] <- length(yr2_data$PERSON_ID)
table_mean_aftcvd_sup[2,2] <- length(unique(yr2_data$PERSON_ID))
table_mean_aftcvd_sup[2,3] <- length(unique(yr1_data$PERSON_ID)) - length(unique(yr2_data$PERSON_ID))
#####
yr3_data <- drug_after_covid %>% filter(year_gp == 3)
yr3_counts <- data.frame(table(yr3_data$PERSON_ID))
table_mean_aftcvd[3,1] <- round(mean(yr3_counts$Freq),2)
table_mean_aftcvd[3,2] <- round(sd(yr3_counts$Freq),2)

table_mean_aftcvd_sup[3,1] <- length(yr3_data$PERSON_ID)
table_mean_aftcvd_sup[3,2] <- length(unique(yr3_data$PERSON_ID))
table_mean_aftcvd_sup[3,3] <- length(unique(yr1_data$PERSON_ID)) - length(unique(yr3_data$PERSON_ID))
####
table_mean_aftcvd[4,1] <- round(mean(c(mean(yr1_counts$Freq),mean(yr2_counts$Freq),mean(yr3_counts$Freq))),2)
table_mean_aftcvd[4,2] <- round(sd(c(mean(yr1_counts$Freq),mean(yr2_counts$Freq),mean(yr3_counts$Freq))),2)




{
  ####
  yr1_data_rani_lst <- yr1_data %>% filter(DRUG_CONCEPT_ID %in% used_lucentis_id) 
  yr1_rani <- yr1_data %>% filter(PERSON_ID %in% yr1_data_rani_lst$PERSON_ID)
  yr1_rani_counts <- data.frame(table(yr1_rani$PERSON_ID))
  
  table_mean_aftcvd[1,3] <- round(mean(yr1_rani_counts$Freq),2)
  table_mean_aftcvd[1,4] <- round(sd(yr1_rani_counts$Freq),2)
  
  table_mean_aftcvd_sup[1,4] <- length(yr1_rani$PERSON_ID)
  table_mean_aftcvd_sup[1,5] <- length(unique(yr1_rani$PERSON_ID))
  table_mean_aftcvd_sup[1,6] <- length(unique(yr1_rani$PERSON_ID)) -  length(unique(yr1_rani$PERSON_ID))
  ####
  yr1_data_afli_lst <- yr1_data %>% filter(DRUG_CONCEPT_ID %in% used_eylea_id) 
  yr1_afli <- yr1_data %>% filter(PERSON_ID %in% yr1_data_afli_lst$PERSON_ID)
  yr1_afli_counts <- data.frame(table(yr1_afli$PERSON_ID))
  
  table_mean_aftcvd[1,5] <- round(mean(yr1_afli_counts$Freq),2)
  table_mean_aftcvd[1,6] <- round(sd(yr1_afli_counts$Freq),2)
  
  table_mean_aftcvd_sup[1,7] <- length(yr1_afli$PERSON_ID)
  table_mean_aftcvd_sup[1,8] <- length(unique(yr1_afli$PERSON_ID))
  table_mean_aftcvd_sup[1,9] <- length(unique(yr1_afli$PERSON_ID)) -  length(unique(yr1_afli$PERSON_ID))
}


{
  ####
  yr2_data_rani_lst <- yr2_data %>% filter(DRUG_CONCEPT_ID %in% used_lucentis_id) 
  yr2_rani <- yr2_data %>% filter(PERSON_ID %in% yr1_data_rani_lst$PERSON_ID)
  yr2_rani_counts <- data.frame(table(yr2_rani$PERSON_ID))
  
  table_mean_aftcvd[2,3] <- round(mean(yr2_rani_counts$Freq),2)
  table_mean_aftcvd[2,4] <- round(sd(yr2_rani_counts$Freq),2)
  
  table_mean_aftcvd_sup[2,4] <- length(yr2_rani$PERSON_ID)
  table_mean_aftcvd_sup[2,5] <- length(unique(yr2_rani$PERSON_ID))
  table_mean_aftcvd_sup[2,6] <- length(unique(yr1_rani$PERSON_ID)) -  length(unique(yr2_rani$PERSON_ID))
  ####
  yr2_data_afli_lst <- yr2_data %>% filter(DRUG_CONCEPT_ID %in% used_eylea_id) 
  yr2_afli <- yr2_data %>% filter(PERSON_ID %in% yr1_data_afli_lst$PERSON_ID)
  yr2_afli_counts <- data.frame(table(yr2_afli$PERSON_ID))
  
  table_mean_aftcvd[2,5] <- round(mean(yr2_afli_counts$Freq),2)
  table_mean_aftcvd[2,6] <- round(sd(yr2_afli_counts$Freq),2)
  
  table_mean_aftcvd_sup[2,7] <- length(yr2_afli$PERSON_ID)
  table_mean_aftcvd_sup[2,8] <- length(unique(yr2_afli$PERSON_ID))
  table_mean_aftcvd_sup[2,9] <- length(unique(yr1_afli$PERSON_ID)) -  length(unique(yr2_afli$PERSON_ID))
}



{
  ####
  yr3_data_rani_lst <- yr3_data %>% filter(DRUG_CONCEPT_ID %in% used_lucentis_id) 
  yr3_rani <- yr3_data %>% filter(PERSON_ID %in% yr1_data_rani_lst$PERSON_ID)
  yr3_rani_counts <- data.frame(table(yr3_rani$PERSON_ID))
  
  table_mean_aftcvd[3,3] <- round(mean(yr3_rani_counts$Freq),2)
  table_mean_aftcvd[3,4] <- round(sd(yr3_rani_counts$Freq),2)
  
  table_mean_aftcvd_sup[3,4] <- length(yr3_rani$PERSON_ID)
  table_mean_aftcvd_sup[3,5] <- length(unique(yr3_rani$PERSON_ID))
  table_mean_aftcvd_sup[3,6] <- length(unique(yr1_rani$PERSON_ID)) -  length(unique(yr3_rani$PERSON_ID))
  ####
  yr3_data_afli_lst <- yr3_data %>% filter(DRUG_CONCEPT_ID %in% used_eylea_id) 
  yr3_afli <- yr3_data %>% filter(PERSON_ID %in% yr1_data_afli_lst$PERSON_ID)
  yr3_afli_counts <- data.frame(table(yr3_afli$PERSON_ID))
  
  table_mean_aftcvd[3,5] <- round(mean(yr3_afli_counts$Freq),2)
  table_mean_aftcvd[3,6] <- round(sd(yr3_afli_counts$Freq),2)
  
  table_mean_aftcvd_sup[3,7] <- length(yr3_afli$PERSON_ID)
  table_mean_aftcvd_sup[3,8] <- length(unique(yr3_afli$PERSON_ID))
  table_mean_aftcvd_sup[3,9] <- length(unique(yr1_afli$PERSON_ID)) -  length(unique(yr3_afli$PERSON_ID))
}


{
  ####
  table_mean_aftcvd[4,3] <- round(mean(c(mean(yr1_rani_counts$Freq),mean(yr2_rani_counts$Freq),mean(yr3_rani_counts$Freq))),2)
  table_mean_aftcvd[4,4] <- round(sd(c(mean(yr1_rani_counts$Freq),mean(yr2_rani_counts$Freq),mean(yr3_rani_counts$Freq))),2)
  table_mean_aftcvd[4,5] <- round(mean(c(mean(yr1_afli_counts$Freq),mean(yr2_afli_counts$Freq),mean(yr3_afli_counts$Freq))),2)
  table_mean_aftcvd[4,6] <- round(sd(c(mean(yr1_afli_counts$Freq),mean(yr2_afli_counts$Freq),mean(yr3_afli_counts$Freq))),2)
}

write.csv(table_mean_aftcvd, file='./result_eamd/table_mean_aftcvd.csv')
write.csv(table_mean_aftcvd_sup, file='./result_eamd/table_mean_aftcvd_sup.csv')

##################################################################################################################################
# 2018.7.1 이후 exudative AMD 진단과 함께 anti-VEGF (Lucentis, Eylea)      발생환자들 data까지 있다고 가정.
# 필요한 column : PERSON_ID, DRUG_EXPOSURE_START_DATE

# 투약 순번 만들기
loading_data <- drug_data_todo %>%
  group_by(PERSON_ID) %>%
  arrange(PERSON_ID, DRUG_EXPOSURE_START_DATE) %>%
  mutate(PERSON_DRUG_ASD_NUMBER = row_number()) %>%
  select(PERSON_ID, DRUG_EXPOSURE_START_DATE, PERSON_DRUG_ASD_NUMBER)

loading_data$DRUG_EXPOSURE_START_DATE = as.Date(loading_data$DRUG_EXPOSURE_START_DATE)

# 저장 해야 되는 n / total
total_n <- length(unique(loading_data$PERSON_ID))

# 최소 첫 3로딩(3번째 주사)를 가지고 있는 PERSON_ID
has_3loading <- loading_data %>%
  filter(PERSON_DRUG_ASD_NUMBER ==3) %>%
  select(PERSON_ID)

# 저장해야 되는 n
total_3loading <- nrow(has_3loading)


# diff 계산 
loading_data <- loading_data %>%
  filter(PERSON_ID %in% has_3loading$PERSON_ID) %>%
  filter(PERSON_DRUG_ASD_NUMBER == 3 | PERSON_DRUG_ASD_NUMBER == 1) %>%
  group_by(PERSON_ID) %>%
  mutate(THIRD_DATE = max(DRUG_EXPOSURE_START_DATE)) %>%
  mutate(FIRST_DATE = min(DRUG_EXPOSURE_START_DATE)) %>%
  mutate(DIFF = THIRD_DATE - FIRST_DATE) %>%
  select(PERSON_ID, FIRST_DATE, THIRD_DATE, DIFF) %>%
  distinct()


diff_data <- loading_data %>%
  mutate(is_60 = ifelse(60 <= DIFF, 1, 0)) %>%
  mutate(is_80 = ifelse(80 <= DIFF, 1, 0)) %>%
  mutate(is_100 = ifelse(100 <= DIFF, 1, 0)) %>%
  mutate(is_120 = ifelse(120 <= DIFF, 1, 0))

## 코로나 시점 전 2018-07-01 ~ 2019-12-31
before_covid <- diff_data %>%
  filter(FIRST_DATE < "2020-01-01")

## 코로나 시점 후 2020-01-01 ~ 2022-04-30
after_covid <- diff_data %>%
  filter("2019-12-31" < FIRST_DATE)

# 저장해야 되는 n들
before_complete_60 <- length(which(before_covid$is_60==1))
before_complete_80 <- length(which(before_covid$is_80==1))
before_complete_100 <- length(which(before_covid$is_100==1))
before_complete_120 <- length(which(before_covid$is_120==1))


after_complete_60 <- length(which(after_covid$is_60==1))
after_complete_80 <- length(which(after_covid$is_80==1))
after_complete_100 <- length(which(after_covid$is_100==1))
after_complete_120 <- length(which(after_covid$is_120==1))

n_table <- data.frame(total_n, total_3loading, 
                      before_complete_60, before_complete_80, before_complete_100, before_complete_120,
                      after_complete_60, after_complete_80, after_complete_100, after_complete_120)


# csv 저장
write.csv(n_table, file="./result_eamd/n_table.csv")

##################################################################################################################################
# 병원 (안과) 방문 횟수, f/u 간격이 3개월 혹은 4개월 등 이상으로 벌어진 경우가 발생하는 비율
obserAll <- drug_data_todo %>%
  group_by(PERSON_ID) %>%
  arrange(PERSON_ID, DRUG_EXPOSURE_START_DATE) %>%
  mutate(PERSON_DRUG_ASD_NUMBER = row_number())

data <- obserAll %>%
  group_by(PERSON_ID) %>%
  mutate(MAX_COUNT = max(PERSON_DRUG_ASD_NUMBER))
data <- data %>% 
  ungroup(PERSON_ID) %>%
  mutate(INDEX = row_number())
data_f <- data %>%
  group_by(PERSON_ID) %>%
  mutate(MIN_INDEX = min(INDEX))
data_f$DRUG_EXPOSURE_START_DATE = as.Date(data_f$DRUG_EXPOSURE_START_DATE)
p_index <- subset(data_f, select = c(PERSON_ID, MIN_INDEX))

person_index <- distinct(p_index)
person_index <- person_index$MIN_INDEX

# create table 
table_fu <- matrix(ncol = 3, byrow = T)
table_fu <- data.frame(table_fu)
colnames(table_fu) <- c("All_N","Month3_N","Month4_N")

# All 90 days
person <- c()
count <- c()
diff_days <- 90

for (i in person_index) {
  loopCnt <- data_f[i,]$MAX_COUNT-2
  if (loopCnt <= 0) {
    break;
  }
  for (j in 0:loopCnt) {
    row <- data_f[i+j,]
    row2 <- data_f[i+j+1,]
    calculate <- as.integer(row2$DRUG_EXPOSURE_START_DATE - row$DRUG_EXPOSURE_START_DATE)
    if (calculate > diff_days) {
      person <- append(person, row$PERSON_ID)
      count <- append(count, 1)
    }
  }
}

dt2 <- data.frame(P=person, C=count)

final <- dt2 %>%
  group_by(P) %>%
  mutate(SUM_COUNT = sum(C))
final1 <- distinct(final %>% filter(SUM_COUNT>1))

table_fu[1,1] <- length(unique(data_f$PERSON_ID))
table_fu[1,2] <- length(unique(final1$P))

# All 120 days
person <- c()
count <- c()
diff_days <- 120

for (i in person_index) {
  loopCnt <- data_f[i,]$MAX_COUNT-2
  if (loopCnt <= 0) {
    break;
  }
  for (j in 0:loopCnt) {
    row <- data_f[i+j,]
    row2 <- data_f[i+j+1,]
    calculate <- as.integer(row2$DRUG_EXPOSURE_START_DATE - row$DRUG_EXPOSURE_START_DATE)
    if (calculate > diff_days) {
      person <- append(person, row$PERSON_ID)
      count <- append(count, 1)
    }
  }
}

dt2 <- data.frame(P=person, C=count)

final <- dt2 %>%
  group_by(P) %>%
  mutate(SUM_COUNT = sum(C))
final1 <- distinct(final %>% filter(SUM_COUNT>1))

table_fu[1,3] <- length(unique(final1$P))

## 코로나 시점 전후 구분
data_f <- data_f %>%
  group_by(PERSON_ID) %>%
  mutate(FIRST_DATE = min(DRUG_EXPOSURE_START_DATE))

## 코로나 시점 전 2018-07-01 ~ 2019-12-31
data_f_bfcvd <- data_f %>%
  filter(FIRST_DATE < "2020-01-01")

## 코로나 시점 후 2020-01-01 ~ 2022-04-30
data_f_aftcvd <- data_f %>%
  filter("2019-12-31" < FIRST_DATE)

## 코로나 시점 전
data_f_bfcvd$DRUG_EXPOSURE_START_DATE = as.Date(data_f_bfcvd$DRUG_EXPOSURE_START_DATE)
p_index <- subset(data_f_bfcvd, select = c(PERSON_ID, MIN_INDEX))

person_index <- distinct(p_index)
person_index <- person_index$MIN_INDEX

# All 90 days
person <- c()
count <- c()
diff_days <- 90

for (i in person_index) {
  loopCnt <- data_f_bfcvd[i,]$MAX_COUNT-2
  if (loopCnt <= 0) {
    break;
  }
  for (j in 0:loopCnt) {
    row <- data_f_bfcvd[i+j,]
    row2 <- data_f_bfcvd[i+j+1,]
    calculate <- as.integer(row2$DRUG_EXPOSURE_START_DATE - row$DRUG_EXPOSURE_START_DATE)
    if (calculate > diff_days) {
      person <- append(person, row$PERSON_ID)
      count <- append(count, 1)
    }
  }
}

dt2 <- data.frame(P=person, C=count)

final <- dt2 %>%
  group_by(P) %>%
  mutate(SUM_COUNT = sum(C))
final1 <- distinct(final %>% filter(SUM_COUNT>1))

table_fu[2,1] <- length(unique(data_f_bfcvd$PERSON_ID))
table_fu[2,2] <- length(unique(final1$P))

# All 120 days
person <- c()
count <- c()
diff_days <- 120

for (i in person_index) {
  loopCnt <- data_f_bfcvd[i,]$MAX_COUNT-2
  if (loopCnt <= 0) {
    break;
  }
  for (j in 0:loopCnt) {
    row <- data_f_bfcvd[i+j,]
    row2 <- data_f_bfcvd[i+j+1,]
    calculate <- as.integer(row2$DRUG_EXPOSURE_START_DATE - row$DRUG_EXPOSURE_START_DATE)
    if (calculate > diff_days) {
      person <- append(person, row$PERSON_ID)
      count <- append(count, 1)
    }
  }
}

dt2 <- data.frame(P=person, C=count)

final <- dt2 %>%
  group_by(P) %>%
  mutate(SUM_COUNT = sum(C))
final1 <- distinct(final %>% filter(SUM_COUNT>1))

table_fu[2,3] <- length(unique(final1$P))

## 코로나 시점 후
data_f_aftcvd$DRUG_EXPOSURE_START_DATE = as.Date(data_f_aftcvd$DRUG_EXPOSURE_START_DATE)
p_index <- subset(data_f_aftcvd, select = c(PERSON_ID, MIN_INDEX))

person_index <- distinct(p_index)
person_index <- person_index$MIN_INDEX

# All 90 days
person <- c()
count <- c()
diff_days <- 90

for (i in person_index) {
  loopCnt <- data_f_aftcvd[i,]$MAX_COUNT-2
  if (loopCnt <= 0) {
    break;
  }
  for (j in 0:loopCnt) {
    row <- data_f_aftcvd[i+j,]
    row2 <- data_f_aftcvd[i+j+1,]
    calculate <- as.integer(row2$DRUG_EXPOSURE_START_DATE - row$DRUG_EXPOSURE_START_DATE)
    if (calculate > diff_days) {
      person <- append(person, row$PERSON_ID)
      count <- append(count, 1)
    }
  }
}

dt2 <- data.frame(P=person, C=count)

final <- dt2 %>%
  group_by(P) %>%
  mutate(SUM_COUNT = sum(C))
final1 <- distinct(final %>% filter(SUM_COUNT>1))

table_fu[3,1] <- length(unique(data_f_aftcvd$PERSON_ID))
table_fu[3,2] <- length(unique(final1$P))

# All 120 days
person <- c()
count <- c()
diff_days <- 120

for (i in person_index) {
  loopCnt <- data_f_aftcvd[i,]$MAX_COUNT-2
  if (loopCnt <= 0) {
    break;
  }
  for (j in 0:loopCnt) {
    row <- data_f_aftcvd[i+j,]
    row2 <- data_f_aftcvd[i+j+1,]
    calculate <- as.integer(row2$DRUG_EXPOSURE_START_DATE - row$DRUG_EXPOSURE_START_DATE)
    if (calculate > diff_days) {
      person <- append(person, row$PERSON_ID)
      count <- append(count, 1)
    }
  }
}

dt2 <- data.frame(P=person, C=count)

final <- dt2 %>%
  group_by(P) %>%
  mutate(SUM_COUNT = sum(C))
final1 <- distinct(final %>% filter(SUM_COUNT>1))

table_fu[3,3] <- length(unique(final1$P))

write.csv(table_fu, "./result_eamd/table_fu.csv")