# library(foreign)
# ess <- read.dta("~/data/ess/integr_ed20_round6/ESS6e02.dta")
# save(ess, file="~/data/ess/integr_ed20_round6/ESS6e02.rda")

# Load Ess data
# liitetään maiden suomenkieliset nimet ja regiimien nimet
# library(RCurl)
# GHurl <- getURL("https://raw.githubusercontent.com/muuankarski/data/master/world/countries_continents.csv")
# dat <- read.csv(text = GHurl)
# dat <- dat[!duplicated(dat[c("cCode2")]),]
# ess <- merge(ess,dat,
#              by.x="cntry",
#              by.y="cCode2",
#              all.x=TRUE)
# save(ess, file="data/ess.rda")
# load(url("http://koti.kapsi.fi/~muuankarski/avoin-suomi/ess.rda"))

# load EU country shapefile from GISco
# download.file("http://epp.eurostat.ec.europa.eu/cache/GISCO/geodatafiles/CNTR_2010_20M_SH.zip",
#                          destfile="zipfile")
# unzip("zipfile")
# library(rgdal)
# # read into SpatialPolygonsDataFrame
# map <- readOGR(dsn = "./CNTR_2010_60M_SH/data", layer = "CNTR_RG_60M_2010")
# library(ggplot2)
# library(rgeos)
# map <- gBuffer(map, width=0, byid=TRUE)
# map$id <- rownames(map@data)
# map.points <- fortify(map, region = "id")
# map.df <- merge(map.points, map, by = "id")
# save(map.df, file="map.df.rda")
# load(url("http://koti.kapsi.fi/~muuankarski/avoin-suomi/map.df.rda"))