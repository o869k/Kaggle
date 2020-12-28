#Download zuckerberg video on:
#https://en.savefrom.net/1-youtube-video-downloader-4/
#https://www.youtube.com/watch?v=7tRL5qczMDk&feature=emb_title

mainDir <- "C:\\deepfake"; setwd(mainDir)
library(av); library(imager); library(gtools); library(ggplot2); library(zoo); library(dplyr); library(TSA)
video <- av_video_images(video = "videoplayback.mp4",destdir = mainDir,format ="png")
#video at 30 frames/sec

list_files = mixedsort(sort(list.files(path = paste0("."),pattern = ".png", all.files = FALSE, recursive = F, full.names = T)))
tmp_file=list_files[1]
mean_r_real_forehead = NULL
mean_r_fake_forehead = NULL
plotter = 0
for (i in c(1:length(list_files))) {
    add = 0
    tmp_file = list_files[i]
    cat(tmp_file,"\n")
    im <- load.image(tmp_file)
    if (plotter) plot(im,rescale = F,xaxs = 'r',yaxs = 'r')
    if (i %in% c(235:250)) add = 5 #allignment face
    #crop the real and fake faces
    real_face = as.cimg(im[c((135+add):(185+add)),c(10:90),1,c(1:3)])
    if (plotter) plot(draw_rect(im = real_face,x0 = 25,y0 = 20,x1 = 30,y1 = 25,color = "green",opacity = 0.25,filled = T),rescale = F,xaxs = 'r',yaxs = 'r')
    if (plotter) ggplot(mutate(as.data.frame(real_face),channel=factor(cc,labels=c('R','G','B'))),aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)
    fake_face = as.cimg(im[c((135+add):(185+add)),c(190:270),1,c(1:3)])
    if (plotter) plot(draw_rect(im = fake_face,x0 = 25,y0 = 20,x1 = 30,y1 = 25,color = "green",opacity = 0.25,filled = T),rescale = F,xaxs = 'r',yaxs = 'r')
    if (plotter) ggplot(mutate(as.data.frame(fake_face),channel=factor(cc,labels=c('R','G','B'))),aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)
    #crop the real and fake forehead
    real_forehead = as.cimg(real_face[c(25:30),c(20:25),1,c(1:3)])
    if (plotter) plot(real_forehead,rescale = F,xaxs = 'r',yaxs = 'r')
    fake_forehead = as.cimg(fake_face[c(25:30),c(20:25),1,c(1:3)])
    if (plotter) plot(fake_forehead,rescale = F,xaxs = 'r',yaxs = 'r')
    mean_r_real_forehead <- c(mean_r_real_forehead,mean((real_face)/max((real_face)),na.rm=T))
    mean_r_fake_forehead <- c(mean_r_fake_forehead,mean((fake_face)/max((fake_face)),na.rm=T))
}
plot(mean_r_real_forehead,type="l",main="Real")
ma_real = rollmean(x=mean_r_real_forehead,k = 30,align = "right",fill = NA)
lines(ma_real,col="red")
plot(mean_r_fake_forehead,type="l",main="Fake")
ma_fake = rollmean(x=mean_r_fake_forehead,k = 30,align = "right",fill = NA)
lines(ma_fake,col="red")

plot(ma_real,type="l",col="red",main="Real") 8*60/17
plot(ma_fake,type="l",col="red",main="Fake")

#PPG TO HR (of course its dynamic and change on time)
periodogram_real = periodogram(ts(ma_real[!is.na(ma_real)]),plot = F)
1/(2*periodogram_real$freq[1+which.max(tail(periodogram_real$spec,-1))]) #divide by 2 cause there are 2 peaks per cycle
periodogram(ts(ma_real[!is.na(ma_real)]),main='Real Periodogram',xlim=c(0,0.1));  abline(h=0)
periodogram_fake = periodogram(ts(ma_fake[!is.na(ma_fake)]),plot = F)
1/(2*periodogram_fake$freq[1+which.max(tail(periodogram_fake$spec,-1))])  #divide by 2 cause there are 2 peaks per cycle
periodogram(ts(ma_fake[!is.na(ma_fake)]),main='Fake Periodogram',xlim=c(0,0.1));  abline(h=0)
