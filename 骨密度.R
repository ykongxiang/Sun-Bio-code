library(readxl)
library(dplyr)
library(rlang)
library(lubridate)
library(tidyr)
library(data.table)
median_x <- function(x) {
    x <- x[x != 0 & !is.na(x)]
    median(x, na.rm = TRUE)
}

g_median <- function(dataframe1, c1, c2) {
    m_data <- dataframe1 %>%
        group_by(across(all_of(c1))) %>%
        summarise(across(all_of(c2), list(median = median_x), .names = "median_{.col}"))
    return(m_data)
}

man <- function(x){
    x <- x[x != 0 & !is.na(x)]
    mean(x, na.rm = TRUE)
}
g_m <- function(dataframe1, c1, c2) {
    m_data <- dataframe1 %>%
        group_by(across(c1)) %>%
        summarise(across(c2, list(mean = man), .names = "mean_{.col}"))
    return(m_data)
}

BMD2019_origin <- read_excel("Sun Biology analysis/骨密度数据（原始）/2019.xlsx")
BMD2020_origin <- read_excel("Sun Biology analysis/骨密度数据（原始）/2020.xlsx", 
                                  col_types = c("text", "numeric", "text", 
                                                "text", "text", "text", "numeric", 
                                                "numeric", "text", "text", "numeric", 
                                                "text", "numeric", "numeric", "text", 
                                                "text", "numeric", "numeric", "numeric"))
BMD2021_origin <- read_excel("Sun Biology analysis/骨密度数据（原始）/2021.xlsx")
BMD2022_origin <- read_excel("Sun Biology analysis/骨密度数据（原始）/2022.xlsx")
BMD2023_origin <- read_excel("Sun Biology analysis/骨密度数据（原始）/2023.xlsx")
fill_time <- function(data) {
    # 确保时间列是字符型
    data$时间 <- as.character(data$时间)
    
    # 使用fill函数向前填充时间
    data <- data %>% 
        arrange(row_number()) %>%  # 确保数据是按照时间顺序排列的
        mutate(时间 = if_else(is.na(时间), NA, 时间)) %>%  # 将缺失的时间标记为NA
        fill(时间)  # 向前填充时间
    
    return(data)
}

# 应用函数填充时间
BMD2020_origin <- fill_time(BMD2020_origin)
BMD2021_origin <- fill_time(BMD2021_origin)
writexl::write_xlsx(BMD2020_origin,"Sun Biology analysis/骨密度数据（原始）/2020.xlsx")
writexl::write_xlsx(BMD2021_origin,"Sun Biology analysis/骨密度数据（原始）/2021.xlsx")

BMD2020 <- read_excel("Sun Biology analysis/骨密度数据（原始）/2020.xlsx", 
                      col_types = c("text", "numeric", "text", 
                                    "text", "text", "text", "numeric", 
                                    "numeric", "text", "text", "numeric", 
                                    "text", "numeric", "numeric", "text", 
                                    "text", "numeric", "numeric", "numeric"))
BMD2021 <- read_excel("Sun Biology analysis/骨密度数据（原始）/2021.xlsx")
BMD2022 <- read_excel("Sun Biology analysis/骨密度数据（原始）/2022.xlsx")
BMD2023 <- read_excel("Sun Biology analysis/骨密度数据（原始）/2023.xlsx")

BMD2020$BMD <- BMD2020$...11
BMD2020 <- BMD2020[, c("时间", "年龄", "性别", "BMD")]
BMD2021$BMD <- BMD2021$...11
BMD2021 <- BMD2021[, c("时间", "年龄", "性别", "BMD")]
BMD2022$BMD <- BMD2022$阿达
colnames(BMD2022)[1] <- '时间'
BMD2022 <- BMD2022[, c("时间", "年龄", "性别", "BMD")]
BMD2023$BMD <- BMD2023$啊我打算
colnames(BMD2023)[1] <- '时间'
BMD2023 <- BMD2023[, c("时间", "年龄", "性别", "BMD")]

process_data <- function(data, year) {
    # 确保时间列是字符型
    data$时间 <- as.numeric(data$时间)
    data$时间 <- round(data$时间,2)
    data$时间 <- as.character(format(data$时间, nsmall = 2))
    # 将月.日的格式转化为月-日的格式
    data$时间 <- gsub("\\.", "-", data$时间)
    
    # 添加年份到时间列
    data$时间 <- paste0(year, "-", data$时间)
    
    # 转换时间列为日期格式
    data$时间 <- as.Date(data$时间)
    
    return(data)
}

BMD2020 <- process_data(BMD2020, "2020")
BMD2021 <- process_data(BMD2021, "2021")
BMD2022 <- process_data(BMD2022, "2022")
BMD2023 <- process_data(BMD2023, "2023")
BMDtotal <- rbind(BMD2020,BMD2021,BMD2022,BMD2023)
writexl::write_xlsx(BMDtotal,"/Volumes/Samsung_T5/Sun Biology analysis/骨密度数据（原始）/total.xlsx")
BMDtotal <- na.omit(BMDtotal)
writexl::write_xlsx(BMDtotal,"/Volumes/Samsung_T5/Sun Biology analysis/骨密度数据（原始）/total_NA.xlsx")
BMDtotal <- read_excel("骨密度数据（原始）/total_NA.xlsx")
BMDtotal$年龄 <- as.numeric(BMDtotal$年龄)
BMDtotal$old <- floor(BMDtotal$年龄 / 10)
BMDtotal$month <- month(BMDtotal$时间)
BMDtotal$month <- BMDtotal$month +year(BMDtotal$时间)*100
BMDtotal$week <- isoweek(BMDtotal$时间)
BMDtotal$week <- year(BMDtotal$时间)*100+BMDtotal$week
c3 <- c('week','性别')
c2 <- c('week')
c1 <- c('week','old','性别')
BMD_mean <- g_m(BMDtotal,c2,'BMD')
BMD_median <- g_median(BMDtotal,c2,'BMD')
BMD <- merge(BMD_mean,BMD_median)
writexl::write_xlsx(BMD,'骨密度数据（原始）/weekly_BMD.xlsx')
BMD <- read_excel('骨密度数据（原始）/weekly_BMD.xlsx')
SN_origin <- read.csv2(file = '骨密度数据（原始）/SN_d_tot_V2.0.csv',fill = T,header = F)
SN_origin$日期 <- as.Date(paste(SN_origin$V1,SN_origin$V2,SN_origin$V3,sep = '-'))
SN_origin$month <- month(SN_origin$日期)
SN_origin$month <- year(SN_origin$日期)*100+SN_origin$month
SN_origin$week <- isoweek(SN_origin$日期)
SN_origin$week <- year(SN_origin$日期)*100+SN_origin$week
SN <- data.frame(SN_origin$日期,SN_origin$week,SN_origin$V5,SN_origin$month)
colnames(SN) <- c('日期','week','sunspot','month')
SN <- SN[SN$week >184900,]
colnames(SN) <- c('time','week','SN','month')
SN <- SN[SN$week>=202032&SN$week<=202352,]
time_index <- 1:nrow(SN)
model <- lm(SN ~ time_index, data = SN)
SN$sunspot.detrended <- SN$SN - predict(model)
colnames(SN) <- c('time','week','sunspot','month','sunspot.detrend')
weekly_SN_1 <- g_m(SN,'week',c('sunspot.detrend','sunspot'))
weekly_SN_2 <- g_median(SN,'week',c('sunspot.detrend','sunspot'))
weekly_SN <- merge(weekly_SN_1 ,weekly_SN_2 ,by = 'week')
writexl::write_xlsx(weekly_SN,"骨密度数据（原始）/weekly_SN.xlsx")
weekly_SN <- read_xlsx("骨密度数据（原始）/weekly_SN.xlsx")
BMD_SN <- merge(BMD, weekly_SN[, c("week",'mean_sunspot','median_sunspot',"mean_sunspot.detrend",'median_sunspot.detrend')], by = "week", all.x = TRUE)
writexl::write_xlsx(BMD_SN,'骨密度数据（原始）/weekly_BMD_SN.xlsx')
BMD_SN <- read_excel('骨密度数据（原始）/weekly_BMD_SN.xlsx')

ccf <- ccf(BMD_SN$mean_sunspot.detrend,BMD_SN$mean_BMD)
plot(BMD_SN$mean_BMD)
plot(BMD_SN$mean_sunspot.detrend)
