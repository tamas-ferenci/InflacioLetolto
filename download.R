library(data.table)

RawDataCountryWeights <- as.data.table(eurostat::get_eurostat("prc_hicp_cow"))
RawDataCountryWeights$year <- lubridate::year(RawDataCountryWeights$TIME_PERIOD)
RawDataCountryWeights <- rbindlist(lapply(
  1999:max(RawDataCountryWeights$year),
  function(yr)
    rbind(RawDataCountryWeights[year==yr&statinfo=="COWEU27_2020"&!geo%in%c("EU27_2020", "EA"),
                                .(geo, year, values)],
          RawDataCountryWeights[year==yr&statinfo=="COWEU28"&geo=="UK", .(geo, year, values)],
          RawDataCountryWeights[year==yr&statinfo=="COWEA"&geo!="EA",
                                .(geo, year, values = values*RawDataCountryWeights[
                                  year==yr&statinfo=="COWEU"&geo=="EA"]$values/1000)])))
names(RawDataCountryWeights)[names(RawDataCountryWeights)=="values"] <- "weight"
RawDataCountryWeights$geo <- countrycode::countrycode(RawDataCountryWeights$geo, "eurostat", "cldr.name.hu")
saveRDS(RawDataCountryWeights, "RawDataCountryWeights.rds")

RawData <- merge(as.data.table(eurostat::get_eurostat("prc_hicp_manr"))[, .(coicop, geo, TIME_PERIOD, annual = values)],
                 as.data.table(eurostat::get_eurostat("prc_hicp_mmor"))[, .(coicop, geo, TIME_PERIOD, monthly = values)],
                 by = c("coicop", "geo", "TIME_PERIOD"), all = TRUE)
RawData <- RawData[geo%in%c(eurostat::eu_countries$code, "UK")&TIME_PERIOD>="1999-01-01"]
RawData$year <- lubridate::year(RawData$TIME_PERIOD)
RawData$geo <- countrycode::countrycode(RawData$geo, "eurostat", "cldr.name.hu")
RawData$time <- RawData$TIME_PERIOD
RawData <- melt(RawData[, .(coicop, geo, time, annual, monthly)], id.vars = c("coicop", "geo", "time"))
RawData <- RawData[order(geo, time, variable, coicop)]
saveRDS(RawData, "RawDataInflation.rds")
