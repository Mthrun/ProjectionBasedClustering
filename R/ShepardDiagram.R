ShepardDiagram <-function(InputDist,OutputDist,xlabel='Input Distances',ylabel='Output Distances',fancy=F,label='ProjectionMethod',gPlot=ggplot()){
# ShepardDiagram(InputDist,OutputDist)
# Zeichnet ein Shepard Diagram
#
# INPUT
# InputDist             Matrize der Distanzen des Eingaberaumes
# OutputDist            Matrize der Distanzen des Ausgaberaumes 
# 
# Optional
# xlabel,yxlabel        Achsenbeschriftung
# fancy                 =FALSE for PC, =TRUE for publication
# label                 title of shepard diagram
# gPlot                 objekt of ggplot 2, see doku there
# Author: MT 03/2014
# 1.Editor: MT 01/2016 umstieg auf ggplot2
  df = data.frame("InDist" = InputDist[lower.tri(InputDist, diag = FALSE)], "OutDist" = OutputDist[lower.tri(OutputDist, diag = FALSE)],label=factor(label))
  
  plt1 <- gPlot + geom_point(data = df, aes_string("x = InDist", "y = OutDist")) +
    ylab('Output Distances')+xlab('Input Distances')+ggtitle(label)#+
    #geom_line(data = df, aes(x = InDist, y = InDist),color='red')
if(fancy){
  plt1=plt1+
    theme(panel.background = element_blank(), legend.key = element_blank(),axis.line =element_line(colour='black'),
          axis.title.y = element_text(size = rel(2), angle = 90),
          axis.title.x = element_text(size = rel(2), angle = 00),
          axis.text.x = element_text(size = rel(2)),
          axis.text.y = element_text(size = rel(2)),
          plot.title =  element_text(size = rel(2))
    )+#coord_fixed(ratio=max(df$InDist)/max(df$OutDist))+
    coord_fixed(ratio=1)+
   # geom_line(data = df, aes(x = InDist, y = InDist),color='red',size=1.5)+
    coord_cartesian(xlim=range(df$InDist),ylim=range(df$OutDist),expand=FALSE)
}
  return(plt1)
}