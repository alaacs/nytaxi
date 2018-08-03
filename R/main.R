#' @importFrom sf st_read
#' @importFrom utils read.csv
#' @importFrom methods is
#' @importFrom rgdal readOGR
#' @importFrom sf st_sfc
#' @importFrom sf st_simplify
#' @importFrom sf st_crs
#' @importFrom sf st_union
#' @importFrom sf st_write
#' @importFrom sf st_sf
#' @importFrom utils download.file
#' @importFrom utils unzip
NULL

#' @title Reads the New Yrok taxi trip data
#'
#' @description Reads the NYC Taxi & Limousine Commission green taxi trip  CSV file
#' @param url the url of the New York taxi data for analysis. Parameter is optional, if missing will use the example dataset of the library
#' @export
#' @seealso \code{\link[nytaxi]{preprocess_dataset}}
#' @return A \code{\link{data.frame}} with all the csv file for future preprocessing and processing
#' @examples \dontrun{
#'    read_NYC_trip_dataset("https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2017-02.csv")
#'    #Or
#'    read_NYC_trip_dataset("C:\\Downloads\\green_tripdata_2017-02.csv")
#' }
#' read_NYC_trip_dataset()
read_NYC_trip_dataset = function(url){
  if(missing(url))
    data = read.csv(file=system.file("extdata", "example.csv", package="nytaxi"))
  else data = read.csv(url)
  data
}

#' @title Preprocesses the New York taxi data CSV file to analyze
#'
#' @description Preprocessing includes checking the validity of the dataset, removing unneeded data columns and only leaves necesary ones,
#' parses data into the appropriate data types, creation of new columns that are necessary for analysis, adds and fills borough column for the
#' pickup and drop locations
#' @param taxidata is the dataset resulted from \code{\link[nytaxi]{read_NYC_trip_dataset}}
#' @param zones_newyork is the New York zones sf object, if not present will be downloaded from NYC website
#' @export
#' @seealso \code{\link[nytaxi]{read_NYC_trip_dataset}}
#' @return A \code{\link{data.frame}} that has the resulted dataset after preprocessing
#' @examples \dontrun{
#'    preprocess_dataset(read_NYC_trip_dataset())
#' }
#'
preprocess_dataset = function(taxidata, zones_newyork){
  if(is(taxidata, "data.frame"))
  {
    if("lpep_pickup_datetime" %in% colnames(taxidata) &&
       "lpep_dropoff_datetime" %in% colnames(taxidata) &&
       "fare_amount" %in% colnames(taxidata) &&
       "tip_amount" %in% colnames(taxidata) &&
       "PULocationID" %in% colnames(taxidata) &&
       "DOLocationID" %in% colnames(taxidata))
    {
      #Filter only the needed columns for analysis
      taxidata = taxidata[,c("lpep_pickup_datetime", "lpep_dropoff_datetime", "PULocationID", "DOLocationID","payment_type", "fare_amount", "tip_amount")]
      if(missing(zones_newyork))
      {
        zones = download_NY_zones()
      }
      else zones = zones_newyork
      #Convert the datetime columns into POSIXlt columns
      taxidata$lpep_pickup_datetime = as.POSIXlt(taxidata$lpep_pickup_datetime)
      taxidata$lpep_dropoff_datetime = as.POSIXlt(taxidata$lpep_dropoff_datetime)
      #Create a column for the tip fare ratio
      taxidata$tip_fare_ratio = taxidata$tip_amount / taxidata$fare_amount
      #Create a field for Borough of the pick up location and drop location
      taxidata$borough_PU = as.data.frame(zones)[taxidata$PULocationID,"borough"]
      taxidata$borough_Drop = as.data.frame(zones)[taxidata$DOLocationID,"borough"]

      #Create a column for the date part
      taxidata$datepart = taxidata$lpep_dropoff_datetime$hour
      taxidata$datepart[taxidata$datepart %in% 5:7] = "Early Morning"
      taxidata$datepart[taxidata$datepart %in% 8:9] = "Morning"
      taxidata$datepart[taxidata$datepart %in% 10:11] = "Late Morning"
      taxidata$datepart[taxidata$datepart %in% 12:14] = "Afternoon"
      taxidata$datepart[taxidata$datepart %in% 15:16] = "Late Afternoon"
      taxidata$datepart[taxidata$datepart %in% 17:18] = "Early Evening"
      taxidata$datepart[taxidata$datepart %in% 19:20] = "Evening"
      taxidata$datepart[taxidata$datepart %in% 21:23] = "Night"
      taxidata$datepart[taxidata$datepart %in% 0:4] = "After Midnight"
      #Create a column with weekdays/weekends
      taxidata$weekday = taxidata$lpep_dropoff_datetime$wday
      taxidata$isweekend[taxidata$weekday %in% 0:4] = FALSE
      taxidata$isweekend[taxidata$weekday %in% 5:6] = TRUE

      taxidata
    }
    else
    {
      e <- simpleError("Necessary columns are not included in the dataset")
      stop(e)
    }
  }
  else {
    e <- simpleError("Parameter expected of type data.frame")
    stop(e)
  }
}
##' @title Download NYC zones from NYC website
#'
#' @description Downloads the zip file from NYC website (https://s3.amazonaws.com/nyc-tlc/misc/taxi_zones.zip)
#' unzips it to a temporary directory, read it and then delete the temporary files
#' @param as_sf if needed the zones can be returned as an sf object
#' @return A \code{\link{SpatialPolygonsDataFrame}} with the NY zones
#' @export
#' @examples \dontrun{
#'    zones = download_NY_zones()
#'    #or
#'    zones = download_NY_zones(as_sf = T) #to download as sf object
#' }
download_NY_zones = function(as_sf = FALSE){
  url = "https://s3.amazonaws.com/nyc-tlc/misc/taxi_zones.zip"
  tmpfile = tempfile()
  download.file(url, tmpfile)
  unzipped = unzip(tmpfile,exdir = tempdir())
  shp = unzipped[tolower(substring(unzipped, nchar(unzipped)-3)) == ".shp"]
  if(isTRUE(as_sf))
    zones = st_read(dsn = shp)
  else zones = readOGR(dsn = shp)
  file.remove(unzipped)
  zones
}
##' @title Creates New York borough features for doing borough level map visualization
#'
#' @description Reads zones shapefile provided by user, or download it from NYC website if not provided,
#' joins each borough's zones features together to create a geometry for each borough with its name attribute
#' @param zones_shapefile zones shapefile as an sf object, or downloads it from
#' https://s3.amazonaws.com/nyc-tlc/misc/taxi_zones.zip if it's left empty
#' @export
#' @seealso \code{\link[nytaxi]{read_NYC_trip_dataset}} and \code{\link[nytaxi]{preprocess_dataset}}
#' @return A \code{\link{SpatialPolygonsDataFrame}} with the borough's geometries and borough names
#' @examples \dontrun{
#'    boroughs = create_borough_features()
#' }
create_borough_features = function(zones_shapefile){
  zones = NA
  if(missing(zones_shapefile))
  {
    zones = download_NY_zones(as_sf = T)
  }
  else if (is(zones_shapefile, "sf"))
  {
    zones = zones_shapefile
  }
  else
  {
    e <- simpleError("zones_shapefile should be an sf object or left empty")
    stop(e)
  }
  borough_names = data.frame(borough = unique(zones$borough))
  sfclist = st_sfc(crs = st_crs(zones))
  for (i in 1:nrow(borough_names)) {
    geom = st_simplify(st_union(zones[zones$borough == borough_names[i,],]), dTolerance = 150)
    sfclist[i] = st_sfc(geometry = geom, crs = st_crs(zones))
  }
  dir.create("boroughs")
  st_write(st_sf(data.frame(borough_names, geom=sfclist)), "boroughs/boroughs.shp", delete_layer = TRUE)
  boroughs = readOGR(dsn = "boroughs/boroughs.shp")
  unlink("boroughs", recursive = T)
  boroughs
}

#' @title Returns zones data along with aggregate taxi trip data
#'
#' @description Reads the zones shapefile into SpatialPolygonsDataFrame, aggregates overall tip_fare ratio mean, and also for weekedays and weekends
#' and return back the SpatialPolygonsDataFrame
#' @param taxidata The taxi trip data that were read from \code{\link[nytaxi]{read_NYC_trip_dataset}} and preprocessed \code{\link[nytaxi]{preprocess_dataset}}
#' @param zones_newyork is the New York zones sf object, if not present will be downloaded from NYC website
#' @export
#' @seealso \code{\link[nytaxi]{read_NYC_trip_dataset}} and \code{\link[nytaxi]{preprocess_dataset}}
#' @return A \code{\link{SpatialPolygonsDataFrame}} with the zones features with the \code{mean_tip_fair_ratio}, \code{mean_ratio_weekend} and \code{mean_ratio_weekday}
#' @examples \dontrun{
#'    url = "https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2017-02.csv"
#'    taxidata = read_NYC_trip_dataset(url)
#'    get_zones_with_aggregate_tripdata(taxidata)
#' }
get_zones_with_aggregate_tripdata = function(taxidata, zones_newyork){
  if(missing(zones_newyork))
    zones_newyork = download_NY_zones()

  zones_newyork$mean_tip_fair_ratio = NA
  zones_newyork$mean_ratio_weekend = NA
  zones_newyork$mean_ratio_weekday = NA

  zones_newyork$mean_ratio_weekday = NA
  zones_newyork$mean_ratio_weekday = NA
  zones_newyork$mean_ratio_weekday = NA
  zones_newyork$mean_ratio_weekday = NA
  zones_newyork$mean_ratio_weekday = NA
  zones_newyork$mean_ratio_weekday = NA
  zones_newyork$mean_ratio_weekday = NA
  for (i in 1:nrow(zones_newyork)) {
    zones_newyork$mean_tip_fair_ratio[i] = mean(taxidata[taxidata$DOLocationID == zones_newyork$LocationID[i],]$tip_fare_ratio, na.rm = T)
    zones_newyork$mean_ratio_weekend[i] = mean(taxidata[(taxidata$DOLocationID == zones_newyork$LocationID[i]) & (taxidata$isweekend == T),]$tip_fare_ratio, na.rm = T)
    zones_newyork$mean_ratio_weekday[i] = mean(taxidata[(taxidata$DOLocationID == zones_newyork$LocationID[i]) & (taxidata$isweekend == F),]$tip_fare_ratio, na.rm = T)
  }
  zones_newyork
}

#' @title aggregates data from taxi trip data into the boroughs level
#'
#' @description calculates mean tip-fare ratio for each borough for each day of the week, for each time of the day
#' and for weekend/non-weekend properties
#' @param taxidata The taxi trip data that were read from \code{\link[nytaxi]{read_NYC_trip_dataset}} and preprocessed \code{\link[nytaxi]{preprocess_dataset}}
#' @param boroughs is the New York zones sf object, if not present will be downloaded from NYC website
#' @export
#' @seealso \code{\link[nytaxi]{read_NYC_trip_dataset}} and \code{\link[nytaxi]{preprocess_dataset}}
#' @return A \code{\link{SpatialPolygonsDataFrame}} with the zones features with the \code{mean_tip_fair_ratio}, \code{mean_ratio_weekend} and \code{mean_ratio_weekday}
#' @examples \dontrun{
#'    boroughs = create_borough_features()
#'    get_boroughs_with_aggregate_trip_data(taxidata, boroughs)
#' }
get_boroughs_with_aggregate_trip_data = function(taxidata, boroughs){
  if(missing(taxidata))
  {
    e <- simpleError("Taxi data is necessary for aggergating to boroughs level")
    stop(e)
  }
  if(missing(boroughs))
    boroughs = create_borough_features()
  boroughs$Sunday_mean_tip_fair_ratio = NA
  boroughs$Monday_mean_tip_fair_ratio = NA
  boroughs$Tuesday_mean_tip_fair_ratio = NA
  boroughs$Wednesday_mean_tip_fair_ratio = NA
  boroughs$Thursday_mean_tip_fair_ratio = NA
  boroughs$Friday_mean_tip_fair_ratio = NA
  boroughs$Saturday_mean_tip_fair_ratio = NA

  boroughs$EarlyMorning_mean_tip_fair_ratio = NA
  boroughs$Morning_mean_tip_fair_ratio = NA
  boroughs$LateMorning_mean_tip_fair_ratio = NA
  boroughs$Afternoon_mean_tip_fair_ratio = NA
  boroughs$LateAfternoon_mean_tip_fair_ratio = NA
  boroughs$EarlyEvening_mean_tip_fair_ratio = NA
  boroughs$Evening_mean_tip_fair_ratio = NA
  boroughs$Night_mean_tip_fair_ratio = NA
  boroughs$AfterMidnight_mean_tip_fair_ratio = NA

  boroughs$weekday_mean_tip_fare_ratio = NA
  boroughs$weekend_mean_tip_fare_ratio = NA


  for (i in 1:nrow(boroughs)) {
    boroughs$weekday_mean_tip_fare_ratio[i] = mean(taxidata[(taxidata$borough_Drop == boroughs$borough[i]) & (taxidata$datepart == "Night"),]$tip_fare_ratio, na.rm = T)
    boroughs$weekend_mean_tip_fare_ratio[i] = mean(taxidata[(taxidata$borough_Drop == boroughs$borough[i]) & (taxidata$datepart == "After Midnight"),]$tip_fare_ratio, na.rm = T)

    boroughs$Sunday_mean_tip_fair_ratio[i] = mean(taxidata[(taxidata$borough_Drop == boroughs$borough[i]) & (taxidata$weekday == 6),]$tip_fare_ratio, na.rm = T)
    boroughs$Monday_mean_tip_fair_ratio[i] = mean(taxidata[(taxidata$borough_Drop == boroughs$borough[i]) & (taxidata$weekday == 0),]$tip_fare_ratio, na.rm = T)
    boroughs$Tuesday_mean_tip_fair_ratio[i] = mean(taxidata[(taxidata$borough_Drop == boroughs$borough[i]) & (taxidata$weekday == 1),]$tip_fare_ratio, na.rm = T)
    boroughs$Wednesday_mean_tip_fair_ratio[i] = mean(taxidata[(taxidata$borough_Drop == boroughs$borough[i]) & (taxidata$weekday == 2),]$tip_fare_ratio, na.rm = T)
    boroughs$Thursday_mean_tip_fair_ratio[i] = mean(taxidata[(taxidata$borough_Drop == boroughs$borough[i]) & (taxidata$weekday == 3),]$tip_fare_ratio, na.rm = T)
    boroughs$Friday_mean_tip_fair_ratio[i] = mean(taxidata[(taxidata$borough_Drop == boroughs$borough[i]) & (taxidata$weekday == 4),]$tip_fare_ratio, na.rm = T)
    boroughs$Saturday_mean_tip_fair_ratio[i] = mean(taxidata[(taxidata$borough_Drop == boroughs$borough[i]) & (taxidata$weekday == 5),]$tip_fare_ratio, na.rm = T)


    boroughs$EarlyMorning_mean_tip_fair_ratio[i] = mean(taxidata[(taxidata$borough_Drop == boroughs$borough[i]) & (taxidata$datepart == "Early Morning"),]$tip_fare_ratio, na.rm = T)
    boroughs$Morning_mean_tip_fair_ratio[i] = mean(taxidata[(taxidata$borough_Drop == boroughs$borough[i]) & (taxidata$datepart == "Morning"),]$tip_fare_ratio, na.rm = T)
    boroughs$LateMorning_mean_tip_fair_ratio[i] = mean(taxidata[(taxidata$borough_Drop == boroughs$borough[i]) & (taxidata$datepart == "Late Morning"),]$tip_fare_ratio, na.rm = T)
    boroughs$Afternoon_mean_tip_fair_ratio[i] = mean(taxidata[(taxidata$borough_Drop == boroughs$borough[i]) & (taxidata$datepart == "Afternoon"),]$tip_fare_ratio, na.rm = T)
    boroughs$LateAfternoon_mean_tip_fair_ratio[i] = mean(taxidata[(taxidata$borough_Drop == boroughs$borough[i]) & (taxidata$datepart == "Late Afternoon"),]$tip_fare_ratio, na.rm = T)
    boroughs$EarlyEvening_mean_tip_fair_ratio[i] = mean(taxidata[(taxidata$borough_Drop == boroughs$borough[i]) & (taxidata$datepart == "Early Evening"),]$tip_fare_ratio, na.rm = T)
    boroughs$Evening_mean_tip_fair_ratio[i] = mean(taxidata[(taxidata$borough_Drop == boroughs$borough[i]) & (taxidata$datepart == "Evening"),]$tip_fare_ratio, na.rm = T)
    boroughs$Night_mean_tip_fair_ratio[i] = mean(taxidata[(taxidata$borough_Drop == boroughs$borough[i]) & (taxidata$datepart == "Night"),]$tip_fare_ratio, na.rm = T)
    boroughs$AfterMidnight_mean_tip_fair_ratio[i] = mean(taxidata[(taxidata$borough_Drop == boroughs$borough[i]) & (taxidata$datepart == "After Midnight"),]$tip_fare_ratio, na.rm = T)
  }
  boroughs
}
