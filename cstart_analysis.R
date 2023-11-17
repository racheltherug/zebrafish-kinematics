# require(devtools)
# install_github("ckenaley/trackter",force=T)

library(tidyverse)
library(trackter)
library(features)
library(data.table)
library(av)


#list files ending in MOV

f <- list.files(full.names = T,pattern=".mp4")

#create a place to store images
dir.create("images")

#for each file, f, breakup the .MOV into the images and build avi files
for(i in f){
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

for(i in f.cropped[102]){
  if(dir.exists("images")) unlink("images",recursive = T)
  
  #extract images
  av_video_images(i,destdir ="images",format="tiff")
  
}


#check threshold
img.ex <-list.files("./images",full.names = T)[43]

thr.check(img.ex,min = 0.2,max=0.6)

images <- list.files("/images",full.names = T
)


#thresh check
thr.check(list.files("./images",full.names = T)[101],otsu=T)



#function for k, curvature
k.fun <- function(z.prime,z.prime2){
  k <- z.prime2/((1+z.prime^2)^(3/2))
  return(k)
}


dat.l <- list()

for(i in f.cropped[1]){
  
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
  
  # #check threshold
  # img.ex <-list.files("./images",full.names = T)[43]
  # 
  # thr.check(img.ex,min = 0.3,max=0.95)
  # 
  # images <- list.files("/images",full.names = T
  # )
  

  
  
 #compute curvature (k) by spline fitting
  kin.i  <- try(
    kin.free(image.dir = "./images",thr="otsu",ant.per = 0.25,red=0.5,smooth.n=1,ml.smooth=list("spline",0.7),par=F)
                )
  
  if(inherits(kin.i,"try-error")) {kin.i <- list(kin.dat=NA,midline=NA,cont=NA,cont.sm=NA,all.classes=NA,mid.pred=NA,dim=NA)}
    else{
    dat.l[[i]] <- kin.i
  #saveRDS(dat.l,"cstart_data.RDS")
}
}
  
 dat.l[[1]]$midline %>% 
    filter(frame>1) %>% 
    ggplot(aes(x.sm,y.sm,col=frame))+geom_point()
 #+facet_wrap(.~frame)
  
  gg.overlay(kin=dat.l[[1]],
             #frames=0:47,
             under="cont.sm",
             over="midline",
             size=1,
             animate=TRUE,
             col="red",
             fps=10)
  
  dat.l[[1]]$midline %>% 
    group_by(frame) %>% 
    mutate(z.2=predict(smooth.spline(x.sm, y.sm, df = 3,spar = 0.5),x.sm,deriv=2)$y) %>% 
    mutate(k=k.fun(z.prime=predict(smooth.spline(x.sm, y.sm, df = 3,spar = 0.5),y.sm,deriv=1)$y,z.prime2=z.2)) %>% 
    filter(frame>1) %>% 
    mutate(per.bl2=round(per.bl,2)) %>% 
    group_by(per.bl2) %>% 
    summarize(m_k=mean(k)) %>% 
    ggplot(aes(per.bl2,m_k))+geom_point()
           
           