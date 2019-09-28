setwd("/Users/Josu/Dropbox/EHU/Research/Supervision, Colaboration/Valentino Santucci/results_shaking_comparison")
results<-read.csv(file="results_shaking.csv",header=TRUE,sep=";",stringsAsFactor=FALSE)
results_bks<-read.csv(file="best_known_results.csv",header=TRUE,sep=";",stringsAsFactor=FALSE)

# normalizar los valores respecto a los best_knowns
colnames(results)<-c("Instance","Size","Algorithm","Fitness","Normalized")
for (i in 1:length(results$Instance))
{
  best_known<-results_bks[results_bks$Instance==results$Instance[i],]
  best_fit<-as.numeric(as.character(best_known$Result))
  fitness<-results[i,]$Fitness
  results[i,]$Normalized<-abs(fitness-best_fit)/best_fit
  results[i,]$Size<-best_known$Size
}

# Calcular las medias
summary_means<-aggregate(results[,5],list(results$Algorithm,results$Instance,results$Size),mean)
colnames(summary_means)<-c("Algorithm","Instance","Size","Mean")
summary_means$Size<-as.factor(summary_means$Size)
ggplot(summary_means,aes(x=Size,y=Mean,fill=Algorithm),gr)+geom_boxplot()+ theme_bw()+
#  ggtitle("Performance of different shaking procedures")+
  scale_y_continuous(name = "Average Relative Percentage Error")+scale_x_discrete(name="Instance size")+  geom_boxplot(alpha=0.7)+theme(plot.title = element_text(size = 11, family = "Tahoma",hjust = 0.5),
      text = element_text(size = 9, family = "Tahoma"),
      axis.text.x=element_text(size = 8),
      legend.position = "bottom")+scale_fill_manual(values=c("#E69F00", "#56B4E9"), 
                                                   name="Algorithms",
                                                    labels=c("CD Shake", "Swap Shake"))

