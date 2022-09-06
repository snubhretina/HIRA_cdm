source("R code/1_Settings.R")
source("R code/2_Functions.R")

# get Data
# TODO start date
only_LI_data <- procedureCohort("4283015, 42220065", "2018-01-01")

checked <- checkData(only_LI_data)
checked["person"]
checked["row"]

# select First 첫 수술 뽑기
select_only_LI_data <- selectFirst(only_LI_data, "PROCEDURE_DATE")
checked_s <- checkData(select_only_LI_data)
checked_s["person"]
checked_s["row"]

# week로 date 내림
select_only_LI_data["UNOIT_DATE"] = floor_date(select_only_LI_data$PROCEDURE_DATE, unit="week")

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
max_date_li = min(weekly_li_data$UNIT_DATE)
min_date_li
max_date_li

# Date Break
date_breaks <- seq(as.Date(min_date_li), as.Date(max_date_li), by="6 month")

getwd()
pdf("./only_LI_plots/pdf")
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


















