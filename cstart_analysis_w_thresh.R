# require(devtools)
# install_github("ckenaley/trackter",force=T)

library(tidyverse)
library(trackter)
library(features)
library(data.table)
library(av)


#list files ending in MOV

#f <- list.files(full.names = T,pattern=".mp4")

#create a place to store images
dir.create("images")

#for each file, f, breakup the .MOV into the images and build avi files
#for(i in f){
  if(dir.exists("images")) unlink("images",recursive = T)
  
  #extract images
  av_video_images(i,destdir ="images",format="tiff")
  
  #list images
  f.i <- list.files("images",full.names = T)
  
  #make the avi from the images from the MOV
  av_encode_video(f.i,gsub("mp4","avi",basename(i)),codec = "rawvideo")
}


#make list of cropped videos
f.cropped <- list.files(full.names = T,pattern="_cropped.avi")

for(i in f.cropped){
  if(dir.exists("images")) unlink("images",recursive = T)
  
  #extract images
  av_video_images(i,destdir ="images",format="tiff")
  
}


#check threshold
#img.ex <-list.files("./images",full.names = T)[43]

#thr.check(img.ex,min = 0.5,max=0.6)

#images <- list.files("/images",full.names = T
#)

#check thresh of all videos (one image each)

thr_df <- data.frame(video=f.cropped)
thresh_vector <- thr_df
thr_df$thesholds <- thresh_vector

#thr <- list()
for(i in f.cropped){
  if(dir.exists("images")) unlink("images",recursive = T)
  
  #extract images
  av_video_images(i,destdir ="images",format="tiff")
  
  img.ex <-list.files("./images",full.names = T)[30]
  
  thr.check(img.ex,min = 0.4,max=0.6)
  
  t <-  readline("threshold? ") %>% as.numeric()
  thr[[i]] <- t
}


thr_df$thresh <- thr
thr_df$thresh <- as.numeric(thr_df$thresh)
write.csv(thr_df,file="thresholds.csv")
thr_df <- read.csv("thresholds.csv")

#thresh check
#thr.check(list.files("./images",full.names = T)[101],otsu=T)


#function for k, curvature
k.fun <- function(z.prime,z.prime2){
  k <- z.prime2/((1+z.prime^2)^(3/2))
  return(k)
}


### START HERE ###

dat.l <- list()

for(i in f.cropped[2]){
  
  if(dir.exists(paste0(getwd(),"/images"))) unlink(paste0(getwd(),"/images"),recursive = T)
  dir.create(paste0(getwd(),"/images"))
  
  #filter to reduce file size and kin run time

  vid.i <- list.files(pattern=basename(i),recursive = TRUE,full.names = T) 
  vid.i <- vid.i[grep(".avi$",vid.i)]
  vid.to.images(vid.i,out.dir="images",overwrite = T)
  
  
  meta <- unlist(strsplit(basename(i),"\\."))
  
  speed <- as.numeric(meta[2])
  condition <- meta[1]
  trial <- as.numeric(gsub("_cropped","",meta[3]))
 
  vid_name <- subset(thr_df,video==i)
  thresh_value <- vid_name$thresh
   
 #compute curvature (k) by spline fitting
  kin.i  <- try(
      kin.free(image.dir = "./images",thr=thresh_value,ant.per = 0.25,red=0.5,smooth.n=1,ml.smooth=list("spline",0.7),par=F,search.for='offset')
                )
  
  if(inherits(kin.i,"try-error")) {kin.i <- list(kin.dat=NA,midline=NA,cont=NA,cont.sm=NA,all.classes=NA,mid.pred=NA,dim=NA)}
    else{
    #dat.l[[i]] <- kin.i
    
}
}

#saveRDS(dat.l,"cstart_data.RDS")

d -> sqrt((kin.dat$head.x-midline$x))


 cstart_data[[3]]$midline %>% 
    filter(frame>1) %>% 
    ggplot(aes(x.sm,y.sm,col=frame))+geom_point()
 #+facet_wrap(.~frame)
  
  gg.overlay(kin=cstart_data[[26]],
             #frames=0:182,
             #under="cont.sm",
             #over="midline",
             size=1,
             animate=TRUE,
             col="red",
             fps=10)
  
  cstart_data[[30]]$midline %>% 
    group_by(frame) %>% 
    mutate(z.2=predict(smooth.spline(x.sm, y.sm, df = 3,spar = 0.5),x.sm,deriv=2)$y) %>% 
    mutate(k=k.fun(z.prime=predict(smooth.spline(x.sm, y.sm, df = 3,spar = 0.5),y.sm,deriv=1)$y,z.prime2=z.2)) %>% 
    filter(frame>1) %>% 
    mutate(per.bl2=round(per.bl,2)) %>% 
    group_by(per.bl2) %>% 
    summarize(m_k=mean(k)) %>% 
    ggplot(aes(per.bl2,m_k))+geom_point()
           
  
###

#for(i in cstart_data[1]){
  
  # first frame integer
  cstart_data[[1]]%>%
    x[cstart_data[[1]]$kin.dat$frame[1]]
  
  #last frame integer
  tail(cstart_data[[1]]$kin.dat$frame, n=1)
  

  }


           