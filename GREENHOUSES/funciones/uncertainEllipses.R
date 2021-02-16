ggplotBivariateEllipses<-function(df1, df2, scaleX, scaleY, minX, maxX, minY, maxY, xlabel, ylabel, title){
  ggplot()+
    geom_point(data = df1, aes(df1[,1], df1[,2]))+
    stat_ellipse(aes(df1[,1], df1[,2], color = "0.99"), level = 0.99, type = "norm")+
    stat_ellipse(aes(df1[,1], df1[,2], color = "0.95"), level = 0.95, type = "norm")+
    stat_ellipse(aes(df1[,1], df1[,2], color = "0.90"), level = 0.90, type = "norm")+
    stat_ellipse(aes(df1[,1], df1[,2], color = "0.85"), level = 0.85, type = "norm")+
    stat_ellipse(aes(df1[,1], df1[,2], color = "0.80"), level = 0.80, type = "norm")+
    stat_ellipse(aes(df1[,1], df1[,2], color = "0.75"), level = 0.75, type = "norm")+
    stat_ellipse(aes(df1[,1], df1[,2], color = "0.70"), level = 0.70, type = "norm")+
    stat_ellipse(aes(df1[,1], df1[,2], color = "0.65"), level = 0.65, type = "norm")+
    stat_ellipse(aes(df1[,1], df1[,2], color = "0.60"), level = 0.60, type = "norm")+
    stat_ellipse(aes(df1[,1], df1[,2], color = "0.55"), level = 0.55, type = "norm")+
    stat_ellipse(aes(df1[,1], df1[,2], color = "0.10"), level = 0.10, type = "norm")+
    
    geom_point(data = df2, aes(df2[,1], df2[,2]))+
    stat_ellipse(aes(df2[,1], df2[,2], color = "0.99"), level = 0.99, type = "norm")+
    stat_ellipse(aes(df2[,1], df2[,2], color = "0.95"), level = 0.95, type = "norm")+
    stat_ellipse(aes(df2[,1], df2[,2], color = "0.90"), level = 0.90, type = "norm")+
    stat_ellipse(aes(df2[,1], df2[,2], color = "0.85"), level = 0.85, type = "norm")+
    stat_ellipse(aes(df2[,1], df2[,2], color = "0.80"), level = 0.80, type = "norm")+
    stat_ellipse(aes(df2[,1], df2[,2], color = "0.75"), level = 0.75, type = "norm")+
    stat_ellipse(aes(df2[,1], df2[,2], color = "0.70"), level = 0.70, type = "norm")+
    stat_ellipse(aes(df2[,1], df2[,2], color = "0.65"), level = 0.65, type = "norm")+
    stat_ellipse(aes(df2[,1], df2[,2], color = "0.60"), level = 0.60, type = "norm")+
    stat_ellipse(aes(df2[,1], df2[,2], color = "0.55"), level = 0.55, type = "norm")+
    stat_ellipse(aes(df2[,1], df2[,2], color = "0.10"), level = 0.10, type = "norm")+

    
    scale_color_manual(values= rainbow(11)) +
    ggtitle(title)+
    labs(x = xlabel, y = ylabel)+
    guides(colour = guide_legend(nrow = 1, title="Levels"))+
    scaleX+
    scaleY+
    coord_cartesian(xlim = c(minX, maxX),
                    ylim = c(minY, maxY))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),
          legend.position="bottom")
}
