
## TODO : 현재 작업 위치 적어주세요.
# (1_Settings.R를 포함하여 R 파일들이 존재하는 폴더명으로 작성해주세요)
path <- ""  # 현재 작업 위치
setwd(path)
source("1_Settings.R")
source("2_Functions.R")

# ---------------------------- Cohort -----------------------------
# data 만들기 
vitrectomy <- procedureCohort("4294683", "2018-01-01")
only_vitrectomy_data <- makeData_P(vitrectomy)
vi_used_id <- makeIdTable_P(vitrectomy, only_vitrectomy_data)

# 사용된 Concept ID 확인을 위해 저장합니다.
write.csv(vi_used_id, file="./result_its/4_used_id.csv")

# checked 
checked <- checkData(only_vitrectomy_data)

#checkData return 값 csv파일로 저장 ->  반출 예정
PERSON <- checked["person"]
ROW <- checked["row"]
checked_table <- data.frame(PERSON, ROW)

# ---------------------------- Analysis -----------------------------

# select First 첫 수술 뽑기
select_only_vitrectomy_data <- selectFirst(only_vitrectomy_data, "PROCEDURE_DATE")
checked_s <- checkData(select_only_vitrectomy_data)

# 반출할 csv에 컬럼추가
PERSON_S <- checked_s["person"]
ROW_S <- checked_s["row"]
checked_table[,"PERSON_S"] <- PERSON_S
checked_table[,"ROW_S"] <- ROW_S

# weekly count(sum) 주 별 수술 건수 합산
select_only_vitrectomy_data["UNIT_DATE"] = floor_date(select_only_vitrectomy_data$PROCEDURE_DATE, unit="week")
weekly_only_vitrectomy_data <- unitCount(select_only_vitrectomy_data)
weekly_only_vitrectomy_data <- data.frame(weekly_only_vitrectomy_data)

# min, max date  - plot에 사용
min_date_ov = min(weekly_only_vitrectomy_data$UNIT_DATE)
max_date_ov = max(weekly_only_vitrectomy_data$UNIT_DATE)

# 반출할 csv에 컬럼추가
checked_table[,"MIN_DATE"] <- min_date_ov
checked_table[,"MAX_DATE"] <- max_date_ov
# csv 저장
write.csv(checked_table, file="./result_its/4_dataAndDate.csv")

# plot x축 범위 만들어 놓기
date_breaks <- seq(as.Date(min_date_ov), as.Date(max_date_ov), by="6 month")

getwd()
pdf("./result_its/4_only_vitrectomy_plot.pdf", width = 25)
plot <- ggplot2::ggplot(data = weekly_only_vitrectomy_data,
                        aes(x=UNIT_DATE, y=UNIT_COUNT)) + geom_line(size=1)
plot + labs(title="Vitrectomy Weekly Count", x="date", y="weekly count") + scale_x_date( breaks = date_breaks, labels = date_format("%y-%m-%d"))
dev.off()




