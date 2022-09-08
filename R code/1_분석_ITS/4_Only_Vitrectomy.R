source("R code/1_Settings.R")
source("R code/2_Functions.R")


#------------------------- Analysis
# TODO : start_Date 지정
vitrectomy <- procedureCohort("4294683", "2018-01-01")
only_vitrectomy_data <- makeData_P(vitrectomy)
vi_used_id <- makeIdTable_P(vitrectomy, only_vitrectomy_data)

# checked 
checked <- checkData(only_vitrectomy_data)
checked["person"]
checked["row"]

# select First 첫 수술 뽑기
select_only_vitrectomy_data <- selectFirst(only_vitrectomy_data, "PROCEDURE_DATE")

checked_s <- checkData(select_only_vitrectomy_data)
checked_s["person"]
checked_s["row"]

# weekly count(sum) 주 별 수술 건수 합산
weekly_only_vitrectomy_data <- unitCount(select_only_vitrectomy_data)
weekly_only_vitrectomy_data <- data.frame(weekly_only_vitrectomy_data)

# min, max date  - plot에 사용
min_date_ov = min(weekly_only_vitrectomy_data$UNIT_DATE)
max_date_ov = min(weekly_only_vitrectomy_data$UNIT_DATE)
min_date_ov
max_date_ov

# plot x축 범위 만들어 놓기
date_breaks <- seq(as.Date(min_date_ov), as.Date(max_date_ov), by="6 month")

getwd()
pdf("./only_vitrectomy_plot/pdf")
plot <- ggplot2::ggplot(data = weekly_only_vitrectomy_data,
                        aes(x=UNIT_DATE, y=UNIT_SUM)) + geom_line(size=1)
plot + labs(title="Vitrectomy Weekly Count", x="date", y="weekly count") + scale_x_date( breaks = date_breaks, labels = date_format("%y-%m-%d"))
dev.off()











