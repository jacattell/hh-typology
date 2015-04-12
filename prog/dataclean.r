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

#online datasets
#pure balance sheet variables
pbs <- c('fihhyr2','dfihhyr','saveamount','nvesttot','hsval','mg1tot','ustot')
unsec <- c('us','usa','usb','usc','usd','use','usg','ush','usi','usj')

#classes of tenure
for (i in 2011:2014){
  temp <- data.frame(eval(parse(text=paste0('o',i))))
  h1 <- sprintf('tenure %d',i)
  print(h1)
  print(summary(factor(temp$tenure)))
}
mort <- c('Owned mortgage','Owned outright')
rent <- c('Local authority rented','Private rented')

#mortgagers/homeowners vs. renters
mpbs <- c()
rpbs <- c()
for (i in 2011:2014){
  stri <- as.character(i)
  temp <- data.frame(eval(parse(text=paste0('o',i))))
  if(i<2014){
    mpbs[[stri]] <- temp[temp$tenure %in% mort,names(temp) %in% c(pbs,unsec,'tenure')]
    rpbs[[stri]] <- temp[temp$tenure %in% rent,names(temp) %in% c(pbs,unsec,'tenure')]
  }
  else{
    pbsm <- paste(pbs,'m',sep='_')
    mpbs[[stri]] <- temp[temp$tenure %in% mort,names(temp) %in% c(pbsm,unsec)]
    rpbs[[stri]] <- temp[temp$tenure %in% rent,names(temp) %in% c(pbsm,unsec)]
  }
  h1 <- length(names(mpbs[[stri]]))
  h2 <- sprintf('No.of pbs variables in %d: %d',i,h1)
  print(h2)
  h3 <- nrow(mpbs[[stri]])
  h4 <- sprintf('No.of mortgage/owner households in %d: %d',i,h3)
  print(h4)
  h5 <- nrow(rpbs[[stri]])
  h6 <- sprintf('No.of renter households in %d: %d',i,h5)
  print(h6)
}

#convert to ordinal ranks: load dictionaries for every variable 
#Look at variable factors, compare to data descriptions and amend as necessary
for (name in names(mpbs[['2011']])){
  print(name)
  temp <- gsub('\243','GBP',mpbs[['2011']][,name])
  h1 <- data.frame(sort(unique(factor(temp))))
  print(h1)
}

#housevalue/mortgage ratio (not meaningful to use mgtot1 directly)
for (i in 2011:2014){
  if(i<2014){
    stri <- as.character(i)
    aa <- mpbs[[stri]]$mg1tot
    bb <- mpbs[[stri]]$hsval
  }
  else{
    stri <- as.character(i)
    aa <- mpbs[[stri]]$mg1tot_m
    bb <- mpbs[[stri]]$hsval_m
  }
  aa[aa %in% c("don't know","refused","not applicable")] <- NA
  aa <- gsub('\243|,|>=|<','',aa)
  aa <- sapply(aa,function(x) {
    median(as.numeric(unlist(strsplit(x,'-'))),na.rm=T)
  })
  mpbs[[stri]]$mgtot1new <- aa
  
  bb[bb %in% c("don't know","refused","not applicable")] <- NA
  bb <- gsub('\243|,|>=|<','',bb)
  bb <- sapply(bb,function(x) {
    median(as.numeric(unlist(strsplit(x,'-'))),na.rm=T)
  })
  mpbs[[stri]]$hsvalnew <- bb
  mpbs[[stri]]$hsmgdiff <- bb-aa
  
}


