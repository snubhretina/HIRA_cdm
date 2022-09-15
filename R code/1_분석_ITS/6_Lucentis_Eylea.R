
## TODO : 현재 작업 위치 적어주세요.
# (1_Settings.R를 포함하여 R 파일들이 존재하는 폴더명으로 작성해주세요)
path <- ""  # 현재 작업 위치
setwd(path)
source("1_Settings.R")
source("2_Functions.R")

# ---------------------------- Cohort -----------------------------
luncentis <- drugCohort("21605124, 41405555", "2018-01-01")
eylea <- drugCohort("40257019, 42923303", "2018-01-01")

# data
lucentis_data <- makeData_D(luncentis)
eylea_data <- makeData_D(eylea)

lucentis_ID_data <- makeIdTable_D(lucentis, lucentis_data)
eylea_ID_data <- makeIdTable_D(eylea, eylea_data)
# 사용된 Concept ID 확인을 위해 저장합니다.
used_id6 <- rbind(lucentis_ID_data, eylea_ID_data)
write.csv(used_id6, file="./result_its/6_used_id.csv")

check_lucentis <- checkData(lucentis_data)
check_eylea <- checkData(eylea_data)

## checkData return 값 csv파일로 저장 ->  반출 예정
PERSON <- c(check_lucentis["person"], check_eylea["person"])
ROW <- c(check_lucentis["row"], check_eylea["row"])
checked_table <- data.frame(PERSON, ROW)

# Weekly
lucentis_data["UNIT_DATE"] <- floor_date(lucentis_data$DRUG_EXPOSURE_START_DATE, unit="week")
eylea_data["UNIT_DATE"] <- floor_date(eylea_data$DRUG_EXPOSURE_START_DATE, unit="week")

lucentis_data_w <- unitCount(lucentis_data)
eylea_data_w <- unitCount(eylea_data)
lucentis_data_w["DRUG_CLASS"] = "Lucentis"
eylea_data_w["DRUG_CLASS"] = "Eylea"

# Rbind - All Data
# 구분 되어진 data
all_data <- rbind(lucentis_data_w, eylea_data_w)
# 구분 안되어진 all count data
all_data1 <- all_data %>%
  group_by(UNIT_DATE) %>%
  mutate(ALL_COUNT = sum(UNIT_COUNT)) %>%
  select(UNIT_DATE, ALL_COUNT)

# plot 
min_date = min(all_data$UNIT_DATE)
max_date = max(all_data$UNIT_DATE)
# 반출할 csv에 컬럼추가
checked_table[,"MIN_DATE"] <- min_date
checked_table[,"MAX_DATE"] <- max_date
# csv 저장
write.csv(checked_table, file="./result_its/6_dataAndDate.csv")


# plot x축 범위
date_breaks <- seq(as.Date(min_date), as.Date(max_date), by="6 month")
# pdf 저장 시작
getwd()
pdf("./result_its/6_lucentis_eylea_plots.pdf", width = 25)
# all 구분 안되어짐 
plot <- ggplot2::ggplot(data = all_data1, aes(x=UNIT_DATE, y=ALL_COUNT)) + geom_line(size=1)
plot + labs(title="All Weekly Count", x="date", y="weekly count") + scale_x_date( breaks = date_breaks, labels = date_format("%y-%m-%d"))
# all 구분 되어짐
plot <- ggplot2::ggplot(data = all_data, aes(x=UNIT_DATE, y=UNIT_COUNT, group=DRUG_CLASS, colour = DRUG_CLASS)) + geom_line(size=1)
plot + labs(title="All Weekly Count", x="date", y="weekly count") + scale_x_date( breaks = date_breaks, labels = date_format("%y-%m-%d"))
# Lucentis
plot <- ggplot2::ggplot(data = lucentis_data_w, aes(x=UNIT_DATE, y=UNIT_COUNT)) + geom_line(size=1, color="#36C5CA")
plot + labs(title="Lucentis Weekly Count", x="date", y="weekly count") + scale_x_date( breaks = date_breaks, labels = date_format("%y-%m-%d"))

# Eylea
plot <- ggplot2::ggplot(data = eylea_data_w, aes(x=UNIT_DATE, y=UNIT_COUNT)) + geom_line(size=1, color="#F8766D")
plot + labs(title="Lucentis Weekly Count", x="date", y="weekly count") + scale_x_date( breaks = date_breaks, labels = date_format("%y-%m-%d"))

dev.off()













