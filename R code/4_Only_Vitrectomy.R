source("R code/1_Settings.R")
source("R code/2_Functions.R")

### ----------------------- Unit Date Sum Function
# unit(필수) : 문자열 "week" or "month"
# ex) weekly_data <- unitDataSum(data, "week)
unitDataSum <- function(data, unit) {
  data["UNIT_DATE"] = floor_date(data$PROCEDURE_DATE, unit=unit)
  data <- data %>%
    group_by(UNIT_DATE) %>%
    summarise(UNIT_SUM = n())
  data <- data %>%
    arrange(UNIT_DATE)
  return(
    data
  )
}

#------------------------- Analysis
# TODO : start_Date 지정
only_vitrectomy_data <- procedureCohort("4294683", "2018-01-01")

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
weekly_only_vitrectomy_data <- unitDataSum(select_only_vitrectomy_data, "week")
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











