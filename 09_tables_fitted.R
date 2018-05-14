
directorios = list.dirs("./_tables/future", recursive = F)
archivos <- lapply(1:length(directorios), function(i){z <- list.files(directorios[i], recursive = F, full.names = T); return(z)})
model <- list.dirs("./_raster/future/", recursive = F, full.names = F)
model2 <- model
variable <- list.files("_raster/future/_2030s/bcc_csm1_1/", full.names=F)
variable <- gsub(pattern='fitted_values_maxent_', replacement='', variable)
variable <- gsub(pattern='.csv', replacement='', variable)
variable2 <- variable
tablas_future <- lapply(1:length(archivos), function(i){ tablas <- lapply(1:length(archivos[[i]]), function(j){z <- read.csv(archivos[[i]][[j]]); z$variable <- variable[j]; z$model <- model[i]; return(z)}) })
tablas_future <- lapply(1:length(tablas_future), function(i){Reduce(function(...) rbind(..., deparse.level=1), tablas_future[[i]])})
tablas_future <- Reduce(function(...) rbind(..., deparse.level=1), tablas_future)

tablas_current <- list.files("./tables/current", full.names=T)
tablas_current <- lapply(1:length(tablas_current), function(i){z <- read.csv(tablas_current[i]); z$variable <- variable[i]; z$model <- 'current'; return(z)})
tablas_current <- Reduce(function(...) rbind(..., deparse.level=1), tablas_current)

final_table <- rbind(tablas_current, tablas_future)
write.csv(final_table, './final_table.csv', row.names=F)

library(ggplot2)

lapply(1:length(variable2), function(i){
  
  data2 <- subset(final_table, final_table$variable==variable2[i])
  data_current <- subset(data2, data2$model=='current')
  data_current <- data_current[order(data_current$Value),]
  data_future <- subset(data2, data2$model!='current')
  matrix_val <- lapply(1:length(model2), function(j){
    z <- subset(data_future, data_future$model==model2[j])
    z <- z[order(z$Value),]
    z <- z[,c('Idoneidad')]
    return(z)
  })
  matrix_val <- Reduce(function(...) cbind(..., deparse.level=1), matrix_val)
  data_current <- data_current[,c('Value','Idoneidad','model')]
  data_future <- data.frame(Value=data_current$Value, Idoneidad=rowMeans(matrix_val), model='future')
  data2 <- rbind(data_current, data_future)
#   p <- ggplot(data=data2, aes(x=Value, y=Idoneidad, colour=model)) + geom_line(size=0.8)
#   p <- p + ylim(0,1) + xlab(variable2[i])
#   p <- p + theme_bw()
#   p <- p + theme(panel.grid.major.x = element_blank(),
#                  panel.grid.minor.x = element_blank(),
#                  panel.grid.major.y = element_blank(),
#                  panel.grid.minor.y = element_blank(),
#                  axis.text.x = element_text(size=6),
#                  axis.text.y = element_text(size=6),
#                  axis.title.x = element_text(face="bold",size=6),
#                  axis.title.y = element_text(face="bold",size=6),
#                  legend.text = element_text(size=5),
#                  legend.title = element_text(face="bold",size=6),
#                  legend.background=element_blank(),
#                  legend.key=element_blank())
#   ggsave(filename=paste('H:/Trabajo_de_Grado/Maxent/Colombia/Profiles/final_figures/',variable2[i],'_all.png',sep=''), dpi=300, width=5, height=4)
  p <- ggplot(data=data2, aes(x=Value, y=Idoneidad, colour=model)) + geom_line(size=0.8)
  p <- p + ylim(0,1) + xlab(variable2[i]) + ylab('Idoneidad') #ylab("Suitability")
  p <- p + theme_bw()
  p <- p + theme(panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.y = element_blank(),
                 panel.grid.minor.y = element_blank(),
                 axis.text.x = element_text(size=9), #size inicial 6
                 axis.text.y = element_text(size=9), #size inicial 6
                 axis.title.x = element_text(face="bold",size=10), #size inicial 7
                 axis.title.y = element_text(face="bold",size=10), #size inicial 7
                 legend.text = element_text(size=7), #size inicial 7
                 legend.title = element_text(face="bold",size=9), #size inicial 6
                 legend.background=element_blank(),
                 legend.key=element_blank())
  p <- p + labs(colour="")
  #p <- p + scale_color_manual(values=c('black','darkgrey'),breaks=c('current','future'), labels=c('Current','Future'))
  p <- p + scale_color_manual(values=c('black','darkgrey'),breaks=c('current','future'), labels=c('Presente','Futuro'))
  ggsave(filename=paste('graphs/',variable2[i],'_all.png',sep=''), dpi=300, width=5, height=4)
})





