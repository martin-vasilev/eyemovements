## code to prepare `DATASET` dataset goes here

dat<- readLines('data-raw/Binocular_data.txt')

dat <- grep("^\\d", dat, value = TRUE)
dat <-  as.data.frame(do.call( rbind, strsplit( dat, '\t' ) )) # V2 is xpos

dat$V1<- as.numeric(dat$V1)
dat$V2<- as.numeric(dat$V2)
dat$V3<- as.numeric(dat$V3)
dat$V4<- as.numeric(dat$V4)
dat$V5<- as.numeric(dat$V5)
dat$V6<- as.numeric(dat$V6)
dat$V7<- as.numeric(dat$V7)
dat$V8<- NULL
dat$V9<- NULL

colnames(dat)<- c('time', 'x_left', 'y_left', 'pupil_left',
                  'x_right', 'y_right', 'pupil_right')

data_binocular<- dat

usethis::use_data(data_binocular, overwrite = TRUE)


