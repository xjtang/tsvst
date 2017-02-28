# nobpm.R
# Version  1.0
# Calculate number of observation per month
#
# Project: Fusion
# By Xiaojing Tang
# Created on: 2/28/2017
# Last Update: 2/28/2017
#
# Version 1.0 - 2/28/2017
#   Script to calculate number of cloud free observation per month
#
# -------------------------------------------------------------------

# library
library(sp)
library(raster)
library(rgdal)

# nobpm
# calculate nob per month
nobpm <- function(imgFile,outFile,fmaskBand=7){

  # read image file
  image <- read.table(imgFile,sep=',',stringsAsFactor=F,header=T)

  # initialize output
  nImage <- nrow(image)
  fmaskImg <- raster(image[1,3],band=fmaskBand)
  samples <- ncol(fmaskImg)
  lines <- nrow(fmaskImg)
  r <- array(0,c(samples,lines,12))

  # loop through all pixels
  for(i in 1:nImage){
    fmask <- raster::as.matrix(raster(image[i,3],band=fmaskBand))
    d <- doyToDate(image[i,1])
    r[,,d[2]] <- r[,,d[2]] + (fmask==0)&(fmask==1)
    rm(fmask)
    gc()
  }

  # generate nob raster
  r[r==0] <- -9999
  r2 <- raster(r)
  extent(r2) <- extent(fmaskImg)
  projection(r2) <- projection(fmaskImg)
  NAvalue(r2) <- -9999
  res(r2) <- c(30,30)

  # write output
  writeRaster(r2,filename=outFile,format='GTiff',NAflag=-1,overwrite=T)

  # done
  return(0)

}

#--------------------------------------

# small tools
# calculate the date from day of year
doyToDate <- function(doy){

  # get year
  year <- floor(doy/1000)

  # check the doy
  if(year<0){
    cat('invalid doy\n')
    return(0)
  }

  # 365 days or 366 days in that year
  if((year%%4==0&year%%100!=0)|year%%400==0){
    feb <- 29
  }else{
    feb <- 28
  }
  dom <- c(31,feb,31,30,31,30,31,31,30,31,30,31)

  # get total days
  tday <- doy-year*1000

  # check total days
  if(tday<=0|tday>sum(dom)){
    cat('invalid doy\n')
    return(0)
  }

  # calculate month and day
  for(i in 1:12){
    tday <- tday-dom[i]
    if(tday<=0){
      month <- i
      day <- tday+dom[i]
      break
    }
  }

  # done
  date <- c(year,month,day)
  return(date)
}
