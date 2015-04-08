setwd('../data')

#Packages
library(data.table)
library(dplyr)
library(ggplot2)


#Bring in everything that is from 2004 through to 2014
#We have f2f until 2011 and online from 2011 onwards
# in 2011 we have both
for (i in 2004:2014){
  
  if (i < 2011) {
    assign(paste0("f", i), fread(paste0(i, "f2f.csv")))
    
  } else if(i==2011){
    assign(paste0("f", i), fread(paste0(i, "f2f.csv")))
    assign(paste0("o", i), fread(paste0(i, "online.csv")))
    
  } else if(i > 2011){
    assign(paste0("o", i), fread(paste0(i, "online.csv")))
  }
}

#Check that 04 through to 11 have the same variables
all.equal(names(f2004), 
          names(f2005),
          names(f2006),
          names(f2007),
          names(f2008),
          names(f2009),
          names(f2010),
          names(f2011)
          )


#Combine all the f2f datasets
f2f  <- rbind_all(list(f2004, f2005,
                       f2006, f2007, f2008, f2009, f2010, f2011))



#Find all the midway numeric variables
grep("_a$", names(f2f))
li  <- names(f2f[(grep("_a$", names(f2f)))])

#Any other numberic variables add here
li  <- c(li, "age", "hhsize", "nkids")

#Convert the midway variables into numeric format for clustering
f2f[li]  <- sapply(f2f[li], as.numeric)

#save
f2fall  <- f2f
save(f2fall, file="f2fall.r")



###Now drop pre 2007 data
f2f  <- filter(f2f, year >= 2007)

save(f2f, file ="f2f.r")

#Save on memory
rm(list=ls())
load("f2fall.r")
load("f2f.r")

