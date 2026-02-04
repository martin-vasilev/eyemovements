## code to prepare `DATASET` dataset goes here

dat<- readLines('data-raw/OZ_1trial.txt')

dat <- grep("^\\d", dat, value = TRUE)
dat <-  as.data.frame(do.call( rbind, strsplit( dat, '\t' ) )) # V2 is xpos
dat$V1<- as.numeric(dat$V1)
dat$V2<- as.numeric(dat$V2)
dat$V3<- as.numeric(dat$V3)
dat$V4<- as.numeric(dat$V4)
dat$V5<- NULL

colnames(dat)<- c('time', 'x', 'y', 'pupil')

data_Oz<- dat

usethis::use_data(data_Oz, overwrite = TRUE)


