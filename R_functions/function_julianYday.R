
# Given a day of the year i.e. 123rd day of the year, return the equivalent julian date i.e. May 3


julian_yday <- function(yday) {

    yday_juliandate <- read.csv("R_functions/yday_juliandate.csv")
    yday_juliandate$doy <- paste(yday_juliandate$month, yday_juliandate$day)

    return(yday_juliandate$doy[yday_juliandate = yday])

}




