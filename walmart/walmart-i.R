# loading libraries
    library(reshape)
library(reshape2)


# error metric
    WMAE <- function(a, b, w){
        return(sum(w*abs(a-b))/sum(w))
    }


# loading datasets
load("./Data/train.RData")
load("./Data/test.RData")
load("./Data/features.RData")
load("./Data/stores.RData")


# creating panel
test$Weekly_Sales <- 0

panel <- rbind(train, test)
panel$Date <- as.Date(panel$Date)

    panel <- merge(panel, stores, by = c("Store"))
    panel <- merge(panel, features, by = c("Store", "Date", "IsHoliday"))

    panel$Holiday <- 0
    panel$Holiday[panel$IsHoliday == "TRUE"] <- 1

    panel$Weight <- 1
    panel$Weight[panel$Holiday == 1] <- 5

    panel$year <- as.numeric(substr(as.character(panel$Date), 1, 4))
    panel$month <- as.numeric(substr(as.character(panel$Date), 6, 7))
panel$day <- as.numeric(substr(as.character(panel$Date), 9, 10))

    panel$weekofyear <- as.numeric(format(as.Date(panel$Date), "%W"))
panel$fourweekofyear <- ceiling(panel$weekofyear/4)

    panel$val_flag <- 0
    panel$val_flag[panel$Date >= as.Date("2011-11-01") & panel$Date <= as.Date("2012-07-31")] <- 1

    panel$test_flag <- 0
    panel$test_flag[panel$Date >= as.Date("2012-11-01")] <- 1

    panel$count <- 1


# creating historical averages
    ldf1 <- lapply(unique(panel$Date), function(k){
            temp <- subset(panel, Date < as.Date(k) & Date < as.Date("2012-11-01"))
            d <- NULL
            if (nrow(temp) > 0)
            {
            d <- dcast(temp,
                Store + Dept
                ~ count,
                median,
                value.var =
                "Weekly_Sales")
            names(d)[3]
            <-
            "hist_med_store_dept"
            d$Date
            <-
            k
            }
            gc()

            return(d)
    })

ha1 <- do.call(rbind, ldf1)
    panel <- merge(panel, ha1, all.x = T, by = c("Store", "Dept", "Date"))


    py <- subset(panel, select = c("Store", "Dept", "Date", "Weekly_Sales"))                     
    names(py)[4] <- "prev_year"
    py$Date <- py$Date + 7*52
    panel <- merge(panel, py, all.x = T, by = c("Store", "Dept", "Date"))

    ppy <- subset(panel, select = c("Store", "Dept", "Date", "Weekly_Sales"))                     
    names(ppy)[4] <- "prev_prev_year"
    ppy$Date <- ppy$Date + 7*104
    panel <- merge(panel, ppy, all.x = T, by = c("Store", "Dept", "Date"))


# adjusting easter
    panel_adj <- subset(panel, Date == as.Date("2010-04-02") | Date == as.Date("2010-03-26") | Date
            == as.Date("2010-04-09"), select = c("Store", "Dept", "Date", "Weekly_Sales"))
    names(panel_adj)[4] <- "adj_avg_es_10_11"
    panel_adj$Date <- panel_adj$Date + 7*55

    panel <- merge(panel, panel_adj, all.x = T, by = c("Store", "Dept", "Date"))

    panel_adj <- subset(panel, Date == as.Date("2011-04-22") | Date == as.Date("2011-04-15") | Date
            == as.Date("2011-04-29"), select = c("Store", "Dept", "Date", "Weekly_Sales"))
    names(panel_adj)[4] <- "adj_avg_es_11_12"
    panel_adj$Date <- panel_adj$Date + 7*50                                                  

    panel <- merge(panel, panel_adj, all.x = T, by = c("Store", "Dept", "Date"))

    panel_adj <- subset(panel, Date == as.Date("2012-04-06") | Date == as.Date("2012-03-30") | Date
            == as.Date("2012-04-13"), select = c("Store", "Dept", "Date", "Weekly_Sales"))
    names(panel_adj)[4] <- "adj_avg_es_12_13"
    panel_adj$Date <- panel_adj$Date + 7*51

    panel <- merge(panel, panel_adj, all.x = T, by = c("Store", "Dept", "Date"))


# prediction
    panel$pred_avg <- panel$prev_year

    panel$pred_avg[!is.na(panel$adj_avg_es_10_11)] <-
    panel$adj_avg_es_10_11[!is.na(panel$adj_avg_es_10_11)]
    panel$pred_avg[!is.na(panel$adj_avg_es_11_12)] <-
    panel$adj_avg_es_11_12[!is.na(panel$adj_avg_es_11_12)]
    panel$pred_avg[!is.na(panel$adj_avg_es_12_13)] <-
    panel$adj_avg_es_12_13[!is.na(panel$adj_avg_es_12_13)]

    panel$pred_avg[is.na(panel$pred_avg)] <- panel$hist_med_store_dept[is.na(panel$pred_avg)]
    panel$pred_avg[is.na(panel$pred_avg)] <- 1


# lifts
panel$ratio <- panel$Weekly_Sales / (panel$pred_avg)
    panel$ratio[panel$ratio > 1.05] <- 1.05
    panel$ratio[panel$ratio <= 0.5] <- 0.5

    panel_sub <- subset(panel, Date >= as.Date("2012-07-01") & Date < as.Date("2012-11-01"))

    d <- data.frame("Store" = NULL, "Dept" = NULL)
    y <- lapply(unique(panel_sub$Store), function(k){
            z <- lapply(unique(panel_sub$Dept), function(l){
                temp <- subset(panel_sub, Store == k & Dept == l)

                if (nrow(temp[which(temp$ratio > 1),]) > 12 | nrow(temp[which(temp$ratio < 1),]) >
                    12)
                {
                x <- data.frame("Store" = k, "Dept" = l)
                return(x)
                }
                else
                {
                return(NULL)
                }
                })

            s <- do.call(rbind, z)
            return(s)}) 

ff <- do.call(rbind, y)
    ff$flag <- 1

    panel_sub <- merge(panel_sub, ff, by = c("Store", "Dept")) 

    d <- dcast(panel_sub, Store + Dept ~ count, median, value.var = "ratio")
    names(d)[3] <- "lift"

    panel <- merge(panel, d, all.x = T, by = c("Store", "Dept"))

    panel$pred_avg[panel$month == 11 & panel$year == 2012 & !is.na(panel$lift)] <-
    panel$pred_avg[panel$month == 11 & panel$year == 2012 & !is.na(panel$lift)] *
    panel$lift[panel$month == 11 & panel$year == 2012 & !is.na(panel$lift)]
    panel$pred_avg[panel$month == 12 & panel$year == 2012 & !is.na(panel$lift)] <-
    panel$pred_avg[panel$month == 12 & panel$year == 2012 & !is.na(panel$lift)] *
    panel$lift[panel$month == 12 & panel$year == 2012 & !is.na(panel$lift)]
    panel$pred_avg[panel$month == 1 & panel$year == 2013 & !is.na(panel$lift)] <-
    panel$pred_avg[panel$month == 1 & panel$year == 2013 & !is.na(panel$lift)] *
    panel$lift[panel$month == 1 & panel$year == 2013 & !is.na(panel$lift)]
    panel$pred_avg[panel$month == 2 & panel$year == 2013 & !is.na(panel$lift)] <-
    panel$pred_avg[panel$month == 2 & panel$year == 2013 & !is.na(panel$lift)] *
    panel$lift[panel$month == 2 & panel$year == 2013 & !is.na(panel$lift)]
    panel$pred_avg[panel$month == 3 & panel$year == 2013 & !is.na(panel$lift)] <-
    panel$pred_avg[panel$month == 3 & panel$year == 2013 & !is.na(panel$lift)] * 0.99
    panel$pred_avg[panel$month == 4 & panel$year == 2013 & !is.na(panel$lift)] <-
    panel$pred_avg[panel$month == 4 & panel$year == 2013 & !is.na(panel$lift)] * 0.99
    panel$pred_avg[panel$month == 5 & panel$year == 2013 & !is.na(panel$lift)] <-
    panel$pred_avg[panel$month == 5 & panel$year == 2013 & !is.na(panel$lift)] * 0.99
    panel$pred_avg[panel$month == 6 & panel$year == 2013 & !is.na(panel$lift)] <-
    panel$pred_avg[panel$month == 6 & panel$year == 2013 & !is.na(panel$lift)] * 0.99
    panel$pred_avg[panel$month == 7 & panel$year == 2013 & !is.na(panel$lift)] <-
    panel$pred_avg[panel$month == 7 & panel$year == 2013 & !is.na(panel$lift)] * 0.99

    panel$pred_avg[panel$pred_avg < 1] <- 1

    pw <- subset(panel, select = c("Store", "Dept", "Date", "pred_avg"))                     
    names(pw)[4] <- "prev_week"
    pw$Date <- pw$Date + 7
    panel <- merge(panel, pw, all.x = T, by = c("Store", "Dept", "Date"))

    fw <- subset(panel, select = c("Store", "Dept", "Date", "pred_avg"))                     
    names(fw)[4] <- "future_week"
    fw$Date <- fw$Date - 7
    panel <- merge(panel, fw, all.x = T, by = c("Store", "Dept", "Date"))


    panel$pred_avg[panel$Date == as.Date("2012-12-14") & !is.na(panel$prev_week)] <- (0.5 *
            panel$pred_avg[panel$Date == as.Date("2012-12-14") & !is.na(panel$prev_week)] + 0.5 *
            panel$prev_week[panel$Date == as.Date("2012-12-14") & !is.na(panel$prev_week)]) * 0.98
    panel$pred_avg[panel$Date == as.Date("2012-12-21") & !is.na(panel$prev_week)] <- (0.5 *
            panel$pred_avg[panel$Date == as.Date("2012-12-21") & !is.na(panel$prev_week)] + 0.5 *
            panel$prev_week[panel$Date == as.Date("2012-12-21") & !is.na(panel$prev_week)]) * 0.98


# submission
    submit <- subset(panel_test, select = c("Store", "Dept", "Date", "pred_avg"))
    submit$Id <- paste(submit$Store, "_", submit$Dept, "_", submit$Date, sep = "")

    submit <- subset(submit, select = c("Id", "pred_avg"))
    names(submit)[2] <- "Weekly_Sales"

    submit$Weekly_Sales[submit$Weekly_Sales < 0] <- 0

    write.csv(submit, "./Submission/submit.csv", row.names = F)


