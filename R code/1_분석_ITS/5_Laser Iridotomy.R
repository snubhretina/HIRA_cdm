## TODO : 현재 작업 위치 적어주세요.
# (1_Settings.R를 포함하여 R 파일들이 존재하는 폴더명으로 작성해주세요)
path <- ""  # 현재 작업 위치
setwd(path)
source("1_Settings.R")
source("2_Functions.R")

# ---------------------------- Cohort -----------------------------
LI <- procedureCohort("4283015, 4220065", "2018-01-01")
only_LI_data <- makeData_P(LI)
li_used_id <- makeIdTable_P(LI, only_LI_data)
# 사용된 Concept ID 확인을 위해 저장합니다.
write.csv(li_used_id, file="./result_its/5_used_id.csv")

checked <- checkData(only_LI_data)
#checkData return 값 csv파일로 저장 ->  반출 예정
PERSON <- checked["person"]
ROW <- checked["row"]
checked_table <- data.frame(PERSON, ROW)

# ---------------------------- Analysis -----------------------------

# select First 첫 수술 뽑기
select_only_LI_data <- selectFirst(only_LI_data, "PROCEDURE_DATE")
checked_s <- checkData(select_only_LI_data)

# 반출할 csv에 컬럼추가
PERSON_S <- checked_s["person"]
ROW_S <- checked_s["row"]
checked_table[,"PERSON_S"] <- PERSON_S
checked_table[,"ROW_S"] <- ROW_S

# week로 date 내림
select_only_LI_data["UNIT_DATE"] = floor_date(select_only_LI_data$PROCEDURE_DATE, unit="week")

# 수술 별, 주 별 count
weekly_li_data <- select_only_LI_data %>%
  group_by(UNIT_DATE, PROCEDURE_CONCEPT_ID) %>%
  mutate(UNIT_COUNT = max(row_number())) %>%
  arrange(UNIT_DATE)
# count 마지막 row들만 뽑기.

weekly_li_data <- weekly_li_data %>%
  group_by(UNIT_DATE, PROCEDURE_CONCEPT_ID) %>%
  slice(n())

## PROCEDURE CLASS 생성
weekly_li_data <- subset(weekly_li_data, select = c(PROCEDURE_CONCEPT_ID, UNIT_DATE, UNIT_COUNT))
weekly_li_data <- weekly_li_data %>%
  mutate(PROCEDURE_CLASS = ifelse(PROCEDURE_CONCEPT_ID =="4283015", "Optical Iridectomy", "Iridectomy"))

# data분기
# 1. "Optical Iridectomy"
optical_data <- weekly_li_data %>%
  filter(PROCEDURE_CLASS=="Optical Iridectomy")
# 2. "Iridectomy"
iridectomy_data <- weekly_li_data %>%
  filter(PROCEDURE_CLASS=="Iridectomy")


# ------------------ PLOT ------------------------

# min, max date  - plot에 사용
min_date_li = min(weekly_li_data$UNIT_DATE)
max_date_li = max(weekly_li_data$UNIT_DATE)

# 반출할 csv에 컬럼추가
checked_table[,"MIN_DATE"] <- min_date_li
checked_table[,"MAX_DATE"] <- max_date_li
# csv 저장
write.csv(checked_table, file="./result_its/5_dataAndDate.csv")

# Date Break
date_breaks <- seq(as.Date(min_date_li), as.Date(max_date_li), by="6 month")

getwd()
pdf("./result_its/5_only_LI_plots.pdf", width = 25)
# group line
plot <- ggplot2::ggplot(data = weekly_li_data,
                        aes(x=UNIT_DATE, y=UNIT_COUNT, 
                            group = PROCEDURE_CLASS, 
                            colour = PROCEDURE_CLASS))+ geom_line(size=1)
plot + labs(title="Laser Iridectomy Weekly Count", x="date", y="weekly count") + scale_x_date( breaks = date_breaks, labels = date_format("%y-%m-%d"))
# optical
plot <- ggplot2::ggplot(data = optical_data,
                        aes(x=UNIT_DATE, y=UNIT_COUNT))+ geom_line(size=1, color="#F8766D")
plot + labs(title="Optical Iridectomy Weekly Count", x="date", y="weekly count") + scale_x_date( breaks = date_breaks, labels = date_format("%y-%m-%d"))
# general
plot <- ggplot2::ggplot(data = iridectomy_data,
                        aes(x=UNIT_DATE, y=UNIT_COUNT))+ geom_line(size=1, color="#36C5CA")
plot + labs(title="Optical Iridectomy Weekly Count", x="date", y="weekly count") + scale_x_date( breaks = date_breaks, labels = date_format("%y-%m-%d"))
dev.off()



