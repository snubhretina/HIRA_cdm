## TODO : 현재 작업 위치 적어주세요.
# (1_Settings.R를 포함하여 R 파일들이 존재하는 폴더명으로 작성해주세요)
path <- ""  # 현재 작업 위치
setwd(path)
source("1_Settings.R")
source("2_Functions.R")

# ---------------------------- Cohort -----------------------------
# data 만들기 
rd <- conditionCohort("4055484", "2018-01-01")
rd_data <- makeData_C(rd)
rd_used_id <- makeIdTable_C(rd, rd_data)

vi_buck <- procedureCohort("4294683, 4230961", "2018-01-01")
vi_buck_data <- makeData_P(vi_buck)
vd_used_id <- makeIdTable_P(vi_buck, vi_buck_data)

# 사용된 Concept ID 확인을 위해 저장합니다.
used_id3 <- rbind(rd_used_id, vd_used_id)
write.csv(used_id3, file="./result_its/3_used_id.csv")

check_rd <- checkData(rd_data)
check_vi <- checkData(vi_buck_data)

#checkData return 값 csv파일로 저장 ->  반출 예정
PERSON <- c(check_rd["person"], check_vi["person"])
ROW <- c(check_rd["row"], check_vi["row"])
checked_table <- data.frame(PERSON, ROW)


# ---------------------------- Analysis -----------------------------

# select first
rd_data_s <- selectFirst(rd_data, "CONDITION_START_DATE")
vi_buck_data_s <- selectFirst(vi_buck_data, "PROCEDURE_DATE")
check_rd_s <- checkData(rd_data_s)
check_vi_s <- checkData(vi_buck_data_s)

# 반출할 csv에 컬럼추가
PERSON_S <- c(check_rd_s["person"], check_vi_s["person"])
ROW_S <- c(check_rd_s["row"], check_vi_s["row"])
checked_table[,"PERSON_S"] <- PERSON_S
checked_table[,"ROW_S"] <- ROW_S

# weekly
rd_data_s["UNIT_DATE"] = floor_date(rd_data_s$CONDITION_START_DATE, unit="week")
vi_buck_data_s["UNIT_DATE"] = floor_date(vi_buck_data_s$PROCEDURE_DATE, unit="week")

rd_data_w <- unitCount(rd_data_s)
vi_buck_data_w <- unitCount(vi_buck_data_s)
rd_data_w <- rename(rd_data_w, "RD" = UNIT_COUNT)
vi_buck_data_w <- rename(vi_buck_data_w, "Vitrectomy or Buckling" = UNIT_COUNT)

# join 
join_data <- distinct(full_join(rd_data_w, vi_buck_data_w))
join_data[is.na(join_data)] <- 0
join_data["ALL_COUNT"] = join_data["RD"] + join_data["Vitrectomy or Buckling"]

# plot
min_date_rd = min(join_data$UNIT_DATE)
max_date_rd = max(join_data$UNIT_DATE)

# 반출할 csv에 컬럼추가
checked_table[,"MIN_DATE"] <- min_date_rd
checked_table[,"MAX_DATE"] <- max_date_rd
# csv 저장
write.csv(checked_table, file="./result_its/3_dataAndDate.csv")


# plot x축 범위 만들어 놓기
date_breaks <- seq(as.Date(min_date_rd), as.Date(max_date_rd), by="6 month")

getwd()
pdf("./result_its/3_RD_Vitrectomy_Buckling_plots.pdf", width = 25)
# all
plot <- ggplot2::ggplot(data = join_data, aes(x=UNIT_DATE, y=ALL_COUNT)) + geom_line(size=1)
plot + labs(title=" RD Vitrectomy or Buckling Weekly Count", x="date", y="weekly count") + scale_x_date( breaks = date_breaks, labels = date_format("%y-%m-%d"))
# RD
plot <- ggplot2::ggplot(data = join_data, aes(x=UNIT_DATE, y=RD)) + geom_line(size=1, color="36c5ca")
plot + labs(title=" RD Weekly Count", x="date", y="weekly count") + scale_x_date( breaks = date_breaks, labels = date_format("%y-%m-%d"))
dev.off()



