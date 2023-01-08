megadet_transform <- function(filename){
  require(jsonlite)
  results <- fromJSON(filename)[[1]] %>% as.data.frame() %>% arrange(file)
  for(i in 1:nrow(results)){
   results$category[i] <- round(mean(as.numeric(results$detections[[i]]$category),na.rm=T),0)
  }
  return(results)
}

jsonfiles <- paste0("~/Desktop/phD/CTProcessing/",c("test_output.json","test-MY_output.json","test-BT_output.json"))

test <- megadet_transform(jsonfiles[1]) %>% mutate(Station="test",FileName=file) %>% select(-file,-detections)
test_MY <- megadet_transform(jsonfiles[2])%>% mutate(Station="test-MY",FileName=file) %>% select(-file,-detections)
test_BT <- megadet_transform(jsonfiles[3])%>% mutate(Station="test-BT",FileName=file) %>% select(-file,-detections)

test_all <- rbind(test,test_MY,test_BT) 
test_all$ID <- paste0(test_all$Station,"-",test_all$FileName)

real_all <- read.csv("~/Desktop/phD/CTProcessing/test.csv")
real_all$ID <- paste0(real_all$Station,"-",real_all$FileName)
real_all

test_all$ID%in%real_all$ID
compare <- left_join(real_all,test_all[,c("ID","category","max_detection_conf")],by="ID")
compare <- compare[,c(1,2,10,11,3:9)] %>% filter(!is.na(max_detection_conf))

library(tidyverse)
library(raster)
test2<-megadet_transform(jsonfiles[2])
test_ext <- c(unlist(test2[[3]][[23]]$bbox))*c(3000,4000,3000,4000)
test_ext <-test_ext[c(4,2,3,1)]
img<-raster(paste0("~/Desktop/phD/CTProcessing/test-MY/",test2[23,1])) %>% crop(extent(test_ext))
plot(img)
