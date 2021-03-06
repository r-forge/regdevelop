\name{d.pollZH16}
\alias{d.pollZH16}
\alias{d.pollZH16d}
\docType{data}
\title{
Air Pollution Monitoring in Zurich
}
\description{
  Hourly air pollution measurements from a station in the city center of
  Zurich, in a courtyard, for the whole year 2016, resulting in
  8784 measurements of the two pollution variables
  ozone and nitrogen dioxyde, the three weather variables
  temperature, radiation and precipitation, and 8 variables
  characterizing the date.
  
  \code{pollZH16d}  is the subset of measurements for \code{hour=15}.
}
\usage{data("d.pollZH16")}
\format{
  A data frame with 8784 observations on the following 13 variables.
  \describe{
    \item{\code{date}}{date of the measurement}
    \item{\code{hour}}{hour of the measurement}
    \item{\code{O3}}{Ozone}
    \item{\code{NO2}}{Nitroge dioxyde}
    \item{\code{temp}}{temperature}
    \item{\code{rad}}{solar radiation}
    \item{\code{prec}}{precipitation}
    \item{\code{dateshort}}{two letter identification of the day.
      A-L encodes the month;  1-9, a-x encodes the day within month.}
    \item{\code{weekday}}{day of the week}
    \item{\code{month}}{month}
    \item{\code{sumhalf}}{indicator for summer half year (April to Sept)}
    \item{\code{sunday}}{logical: indicator for Sunday}
    \item{\code{daytype}}{a factor with levels  \code{work} for working
      day, \code{Sat} and \code{Sun}}
  }
}
\source{
Bundesamt fur Umwelt (BAFU), Schw. Eidgenossenschaft
https://www.bafu.admin.ch/bafu/de/home/themen/luft/zustand/daten/datenabfrage-nabel.html\cr
  The data set has been generated by downloading the files
  for the individual variables,
  converting the entries with \code{hour==24} to \code{hour==0} of the
  following day and restricting the data to year 2016.
}
\note{
  Legal threshold for NO2 in the EU:
  The threshold of 200 micrograms/m3 must not be exceeded by
  more than 18 hourly measurements per year.\cr
  Source: Umweltbundesamt, Germany
  http://www.umweltbundesamt.de/daten/luftbelastung/stickstoffdioxid-belastung#textpart-2
}
\examples{
data(d.pollZH16)
dp <- d.pollZH16
names(dp)

dp$date <- gendateaxis(date=dp$date, hour=dp$hour)

plyx(O3+NO2~date, data=dp, subset= month=="May", type="l")

dp$summer <- dp$month \%in\% c("Jun","Jul","Aug") 
dp$daylight <- dp$hour>8 & dp$hour<17
plmatrix(O3~temp+logst(rad)+logst(prec), data=dp, 
         subset = summer & daylight)
}
\keyword{datasets}
