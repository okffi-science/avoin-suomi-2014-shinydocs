load("data/ess.rda")

load("data/map.df.rda")

ess_tolppa <- function(var, value, mini, maxi) {
  # load data
  # munge key variable
  bar <- function(x) as.character(eval(parse(text=x)) )
  variable <- as.factor(bar(var))
  cntry <- ess$maa
  pweight <- ess$pweight # painotukset
  idno <- ess$idno # painotukset
  df <- data.frame(cntry,variable,pweight,idno)
  levels(df$variable) <- c(levels(df$variable), 0,10)
  df$variable[df$variable == mini] <- 0
  df$variable[df$variable == maxi] <- 10
  df$cntry <- as.character(df$cntry)
  
  df.plot <- df[df$variable %in% 1:10,]
  df.plot[[2]] <- as.numeric(levels(df.plot[[2]]))[df.plot[[2]]]
  
  # order countries by median income
  library(plyr)
  library(grid)
  order.data <- ddply(df.plot, .(cntry), summarise, 
                      mean    = mean(variable, na.rm=TRUE))
  
  order.data <- order.data[order(order.data$mean), ]
  df.plot$cntry <- factor(df.plot$cntry,
                          levels = order.data$cntry)
  
  library(survey)
  d.df <- svydesign(id = ~idno, 
                    weights = ~pweight, 
                    data = df.plot)
  df.plot2 <- data.frame(prop.table(svytable(~variable+cntry, d.df),2)*100)
  names(df.plot2)[3] <- "rel"
  df.plot2$variable <- as.numeric(levels(df.plot2$variable))[df.plot2$variable]
  
  
  library(ggplot2)
  ggplot(data=df.plot2,
         aes(x=variable,y=rel,fill=variable)) + geom_bar(stat="identity") +
    facet_wrap(~cntry) + scale_fill_gradient(low="red", high="green") +
    annotate("segment", x = value, xend = value,
             y=0, yend = 30,
             color = "red",
             size=0.4,
             linetype="dashed") +
    theme_minimal() +
    theme(legend.position="none") +
    theme(axis.title.x = element_blank())
}

ess_laatikkojana <- function(var, value, mini, maxi) {
  # load data
  # munge key variable
  bar <- function(x) as.character(eval(parse(text=x)) )
  variable <- as.factor(bar(var))
  cntry <- ess$maa
  regime_fi <- ess$ryhma1
  pweight <- ess$pweight # painotukset
  idno <- ess$idno # painotukset
  df <- data.frame(cntry,variable,regime_fi,pweight,idno)
  levels(df$variable) <- c(levels(df$variable), 0,10)
  df$variable[df$variable == mini] <- 0
  df$variable[df$variable == maxi] <- 10
  df$cntry <- as.character(df$cntry)
  
  df.plot <- df[df$variable %in% 1:10,]
  df.plot[[2]] <- as.numeric(levels(df.plot[[2]]))[df.plot[[2]]]
  library(survey)
  d.df <- svydesign(id = ~idno, 
                    weights = ~pweight, 
                    data = df.plot)
  data_mean <- as.numeric(svymean(~as.numeric(variable),d.df)[1])
  # order countries by median income
  library(plyr)
  library(grid)
  order.data <- ddply(df.plot, .(cntry), summarise, 
                      mean    = mean(variable, na.rm=TRUE))
  
  order.data <- order.data[order(order.data$mean), ]
  df.plot$cntry <- factor(df.plot$cntry,
                          levels = order.data$cntry)
  
  library(ggplot2)
  ggplot(data=df.plot,
         aes(x=cntry,y=variable, fill=regime_fi)) + 
    geom_boxplot(alpha=.5) +
    # vastaaja
    annotate("segment", x = 0, xend = 29,
             y=value, yend = value,
             color = "red",
             size=1,
             linetype="dashed") +
    annotate("text", x = 5, y=value+0.2, 
             color = "red",
             size=4,label="Sinä!") +
    # datan keskiarvo
    annotate("segment", x = 0, xend = 29,
             y=data_mean, yend = data_mean,
             color = "blue",
             size=1,
             linetype="solid") +
    annotate("text", x = 20, y=data_mean-0.2, 
             color = "blue",
             size=4,label="Euroopan keskiarvo") +
    theme_minimal() +
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", 
                               "#009E73","#D55E00", "#CC79A7","#0072B2","#F0E442")) +
    theme(axis.text.x  = element_text(angle=90, vjust= 0.5)) +
    theme(legend.position = "top") +
    theme(legend.title = element_blank()) +
    theme(legend.direction = "horizontal") +
    theme(axis.title = element_blank())
  
}

ess_kartta <- function(var, value, mini, maxi) {
  # load data
  # munge key variable
  bar <- function(x) as.character(eval(parse(text=x)) )
  variable <- as.factor(bar(var))
  cntry <- ess$cntry
  df <- data.frame(cntry,variable)
  levels(df$variable) <- c(levels(df$variable), 0,10)
  df$variable[df$variable == mini] <- 0
  df$variable[df$variable == maxi] <- 10
  df$cntry <- as.character(df$cntry)
  
  df.plot <- df[df$variable %in% 1:10,]
  df.plot[[2]] <- as.numeric(levels(df.plot[[2]]))[df.plot[[2]]]
  
  # calculate means for plotting
  library(plyr)
  library(grid)
  df.mean <- ddply(df.plot, .(cntry), summarise, 
                   mean    = mean(variable, na.rm=TRUE))
  map.df.l <- merge(map.df,df.mean,by.x="CNTR_ID",by.y="cntry")
  
  map.df.l <- map.df.l[order(map.df.l$order), ]
  library(ggplot2)
  ggplot(data=map.df.l,
         aes(long,lat,group=group)) +
    geom_polygon(aes(fill = mean),
                 colour="white",
                 size=.2) +
    geom_polygon(data = map.df.l, aes(long,lat),
                 fill=NA,
                 colour="white",
                 size = 0.1) + 
    scale_fill_gradient(low="red", high="green", limits=c(0, 10)) +
     coord_cartesian(xlim=c(-14,34),ylim=c(35,70)) +
    #coord_map(project="orthographic", xlim=c(-15,34),ylim=c(35,70)) +
    theme_minimal() +
    theme(axis.title = element_blank())
}
