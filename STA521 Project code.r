#libraries
library(gridExtra)
library(tidyverse)
library(lubridate)
library(GGally)
library(grid)
library(mixtools)
library(cluster)
library(factoextra)
library(ggfortify)

# load data
library(ggplot2)
log0 <- read.csv('sonoma-data-log.csv',header=T)
net0 <- read.csv('sonoma-data-net.csv',header=T)
all <- read.csv('sonoma-data-all.csv', header=T)
loc <- read.table('mote-location-data.txt', header=T)

# deal with time 
## deal with the sonoma-dates file by turning it into a json file, convert date file

library(rjson)
date0 <- fromJSON(file = "sonoma-dates.json")
## convert dates file to dataframe
a <- unlist(strsplit(as.character(date0[1]),split = " "))
a[1] <- "1"
a <- a[-length(a)]
date = data.frame(epochNums = as.integer(a))
a <- unlist(strsplit(as.character(date0[2]),split = "' '"))
a[1] <- "Tue Apr 27 17:10:00 2004"
a[length(a)] <- "Fri Jun 11 20:25:00 2004"
date$epochDates = a
a <- unlist(strsplit(as.character(date0[3]),split = " "))
a[1] <- "12536.0069444444"
a <- a[-length(a)]
date$epochDays = as.integer(a) - 12535



# remove duplicates
log = log0 %>% 
  subset(select = -result_time) %>% 
  unique()  #remove duplicate data (regardless of result_time)
nrow(log) != nrow(unique(log[c("epoch","nodeid")]))  #whether it has contradictive reading
dup = log %>%
  group_by(epoch,nodeid) %>% 
  summarize(num = n())
dup = dup[dup$num>1,]
log0[log0$epoch==17 & log0$nodeid==16,] #result_time are similar, but data contradicts. The following data removes the "result_time" column
log0[log0$epoch==17 & log0$nodeid==136,]
log0[log0$epoch==18 & log0$nodeid==65,]
log0[log0$epoch==18 & log0$nodeid==101,]
log0[log0$epoch==7077 & log0$nodeid==122,]
#Pick up some of these data. Though these data are all very similar, but not sure what's wrong with node. Thus still remove it.
for (i in 1:nrow(dup)){
  log = log[-which(log$epoch==dup$epoch[i] & log$nodeid==dup$nodeid[i]),]  #remove contradictive data
}

net = net0 %>% 
  subset(select = -result_time) %>% 
  unique()
nrow(net) != nrow(unique(net[c("epoch","nodeid")]))
dup = net %>%
  group_by(epoch,nodeid) %>% 
  summarize(num = n())
dup[dup$num>1,]
net0[net0$epoch==9441 & net0$nodeid==74,]
net = net[-which(net$epoch==9441 & net$nodeid==74),]

##################################################

### 2-a

## check consistency
names(log)
names(net)
interest = c("humidity", "humid_temp", "humid_adj", "hamatop", "hamabot")
histogram2 <- function(variable){
  logx = log[[variable]]
  netx = net[[variable]]
  inf = min(quantile(logx,0.01,na.rm = TRUE), quantile(netx, 0.01,na.rm = TRUE))
  sup = max(quantile(logx,0.99,na.rm = TRUE), quantile(netx, 0.99,na.rm = TRUE))
  df <- data.frame(cbind(value=c(logx,netx), 
                         group=c(rep(0,length(logx)), rep(1,length(netx)))))
  #0 for logger, 1 for network
  #df %>% ggplot2::ggplot() + 
  #  ggplot2::geom_histogram(aes(x = value, group = group, fill = as.factor(group)),
  #                          position = "identity", alpha = 0.4, breaks = seq(inf,sup,length.out = 200))
  ggplot(df, aes(value, fill=as.factor(group), group=as.factor(group))) +
    geom_histogram(aes(y=..density..), breaks=seq(inf,sup,length.out = 100), alpha=0.4, 
                   position="identity") + 
    scale_fill_manual(labels=c("log","net"), values = c("#FF6666","#00CCCC")) +
    theme(legend.key.size = unit(0.2, 'cm')) + 
    labs(title = variable)
}
pdf("2-a hum-consis.pdf")
histogram2(interest[1])
dev.off()
pdf("2-a humtemp-consis.pdf")
histogram2(interest[2])
dev.off()
pdf("2-a humadj-consis.pdf")
histogram2(interest[3])
dev.off()
# plot voltage
log_v = log[["voltage"]]
net_v = net[["voltage"]]
df_logv <- data.frame("voltage_value" = log_v, 
                      "index" = c(1:length(log_v)))
pdf("2-a log-voltage.pdf")
ggplot(data = df_logv,aes(x = voltage_value)) + geom_histogram()+ labs(title = 'log_voltage')
dev.off()
df_netv <- data.frame("voltage_value" = net_v, 
                      "index" = c(1:length(net_v)))
pdf("2-a net-voltage.pdf")
ggplot(data = df_netv,aes(x = voltage_value)) + geom_histogram()+ labs(title = 'net_voltage')
dev.off()
# grid.arrange(p1, p2, p3, p4,p5 ,nrow = 3)

# convert voltage
unique(net[which(net$voltage>1000),]$nodeid)  #see whose voltage is above 1000
unique(net[which(net$nodeid==134),]$voltage)  #see the value of voltage of these node
unique(net[which(net$nodeid=="141"),]$voltage)
unique(net[which(net$nodeid=="145"),]$voltage)
unique(net[which(net$nodeid=="135"),]$voltage)

voltage = merge(log[,c("epoch","nodeid","voltage")],net[,c("epoch","nodeid","voltage")],
                by = c("epoch", "nodeid"), all.x = FALSE, all.y = FALSE)  #fit from intersection of two files
voltage = unique(voltage) %>% filter(voltage.y < 1000)
pdf("2-a lm-voltage.pdf")
ggplot(data = voltage,aes(x=voltage.x,y=voltage.y)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  labs(x = "log's voltage", y = "net's voltage")
dev.off()
volt_lm = lm(voltage.x ~ voltage.y, data = voltage)
summary(volt_lm)
net$voltage = predict(volt_lm, data.frame(voltage.y = net$voltage))

# convert hamatop and hamabot
log$hamabot = log$hamabot / 54
log$hamatop = log$hamatop / 54
net$hamabot = net$hamabot / 54
net$hamatop = net$hamatop / 54

# Draw the hamas plots after conversion
pdf("2-a hamatop-consis.pdf")
histogram2(interest[4])
dev.off()
pdf("2-a hamabot-consis.pdf")
histogram2(interest[5])
dev.off()

### 2-b

# missing data
library(tidyverse)

log_t = merge(log,date,by.x = "epoch", by.y = "epochNums")
net_t = merge(net,date,by.x = "epoch", by.y = "epochNums")

missdate_log = log_t[!complete.cases(log_t),] %>%
  group_by(epochDays) %>%
  summarize(missing_number = n())

missdate_net = net_t[!complete.cases(net_t),] %>%
  group_by(epochDays) %>%
  summarize(missing_number = n())

par(mfrow = c(1,2))
pdf("2-b log-miss.pdf")
plot(missdate_log,xlab = "Day", main = "Log") 
dev.off()
pdf("2-b net-miss.pdf")
plot(missdate_net,xlab = "Day", main = "Net")
dev.off()
log_t = na.omit(log_t)
net_t = na.omit(net_t)


#data with location(some nodes don't have location)
log_tl = merge(log_t,loc,by.x = "nodeid",by.y = "ID")  
net_tl = merge(net_t,loc,by.x = "nodeid",by.y = "ID")
setdiff(log_t$nodeid,loc$ID)    #nodeid in log_t that doesn't have corresponding location
setdiff(net_t$nodeid,loc$ID)
sum(log_t$nodeid==135)
sum(log_t$nodeid==65535)
sum(log_t$nodeid==100)
ncol(log_tl)
ncol(net_tl)


# visually recognize outliers
### log data
log_tl = log_tl[log_tl$humidity > 0,]  #remove humidity < 0

# check humidity and temperature
ggplot(log_tl, aes(x = humidity)) + geom_histogram()
ggplot(log_tl, aes(x = humid_temp)) + geom_histogram()
pdf("2-d box-loghum.pdf")
boxplot(log_tl$humidity, xlab = "humidity",main = "log")
dev.off()
pdf("2-d box-logtemp.pdf")
boxplot(log_tl$humid_temp, xlab = "temperature",main = "log")
dev.off()
log_tl = log_tl[log_tl$humid_temp < 31,] #remove humid_temp > 31

# check hama
pdf("2-d his-loghamatop.pdf")
ggplot(log_tl, aes(x = hamatop)) + geom_histogram() + labs(title = 'log')
dev.off()
quantile(log_tl$hamatop, 0.9)
quantile(log_tl$hamatop, 0.95)
quantile(log_tl$hamatop, 0.99)
ggplot(log_tl[log_tl$hamatop>2000,])+geom_point(aes(x=nodeid, y=epoch))
ggplot(log_tl[log_tl$hamatop>2000,])+geom_point(aes(x=Height, y=voltage))#remove or not??seems don't relate to node,epoch,height,voltage
pdf("2-d his-loghamabot.pdf")
ggplot(log_tl, aes(x = hamabot)) + geom_histogram() + labs(title = 'log') 
dev.off()
quantile(log_tl$hamabot, 0.9)
quantile(log_tl$hamabot, 0.95)
quantile(log_tl$hamabot, 0.99)
ggplot(log_tl[log_tl$hamabot>100,])+geom_point(aes(x=nodeid, y=epoch))
ggplot(log_tl[log_tl$hamabot>100,])+geom_point(aes(x=Height, y=voltage))#remove or not??seems don't relate to node,epoch,height,voltage

# filter log hama
log_tl = log_tl[log_tl$hamatop < 2000,]
log_tl = log_tl[log_tl$hamabot < 150,]


# net data
# check temperature and humidity
net_tl = net_tl[net_tl$humidity > 0,]  #remove humidity < 0
ggplot(net_tl, aes(x = humid_temp)) + geom_histogram()
pdf("2-d box-nettemp.pdf")
boxplot(net_tl$humid_temp, xlab = "temperature", main = "net")
dev.off()
net_tl = net_tl[net_tl$humid_temp < 30,] #remove humid_temp > 31

ggplot(net_tl, aes(x = humidity)) + geom_histogram()
pdf("2-d box-nethum.pdf")
boxplot(net_tl$humidity, xlab = "humidity", main = "net")
dev.off()

# check hama
pdf("2-d his-nethamatop.pdf")
ggplot(net_tl, aes(x = hamatop)) + geom_histogram()+ labs(title = "net")
dev.off()
pdf("2-d his-nethamabot.pdf")
ggplot(net_tl, aes(x = hamabot)) + geom_histogram()+ labs(title = "net")
dev.off()
quantile(net_tl$hamatop, 0.9)
quantile(net_tl$hamatop, 0.95)
quantile(net_tl$hamatop, 0.99)
ggplot(net_tl[net_tl$hamatop>2000,])+geom_point(aes(x=nodeid, y=epoch))
ggplot(net_tl[net_tl$hamatop>2000,])+geom_point(aes(x=Height, y=voltage))#remove or not??seems don't relate to node,epoch,height,voltage
quantile(net_tl$hamabot, 0.9)
quantile(net_tl$hamabot, 0.95)
quantile(net_tl$hamabot, 0.99)
ggplot(net_tl[net_tl$hamabot>100,])+geom_point(aes(x=nodeid, y=epoch))
ggplot(net_tl[net_tl$hamabot>100,])+geom_point(aes(x=Height, y=voltage))#remove or not??seems don't relate to node,epoch,height,voltage

# filter net hama
net_tl = net_tl[net_tl$hamatop < 2000,]
net_tl = net_tl[net_tl$hamabot < 150,]


# voltage outliers
# check log voltage
pdf("2-e orin-volt.pdf")
ggplot(log_tl) + geom_line(mapping = aes(x = epochDays, y = voltage, color = as.factor(nodeid))) +
  scale_colour_discrete() + labs(title = "origin voltage after other cleaning")
dev.off()
log_tl = log_tl[log_tl$voltage > 2,]  #remove nodeid = 143,145

ggplot(log_tl) + 
  geom_boxplot(mapping = aes(x = as.factor(nodeid), y = voltage), 
               outlier.size = 0.5, outlier.colour = "#003366", fill = "#6699CC") +
  labs(x = "Node ID", y = "Voltage",title = "log") +theme(axis.text.x  = element_text(angle=90, vjust=0.5))
png("2-e log-humvol.png")
ggplot(log_tl)+geom_point(aes(x=voltage, y=humidity))+ geom_vline(xintercept = 2.4, color = 'red')+    
  geom_vline(xintercept = 3, color = 'red')+ labs(title = "log")
dev.off()
png("2-e log-tempvol.png")
ggplot(log_tl)+geom_point(aes(x=voltage, y=humid_temp))+ geom_vline(xintercept = 2.4, color = 'red')+ 
  geom_vline(xintercept = 3, color = 'red')+ labs(title = "log")
dev.off()
pdf("2-e log-topvol.pdf")
ggplot(log_tl)+geom_point(aes(x=voltage, y=hamatop))+ geom_vline(xintercept = 2.4, color = 'red')+
  geom_vline(xintercept = 3, color = 'red')+ labs(title = "log")
dev.off()
pdf("2-e log-botvol.pdf")
ggplot(log_tl)+geom_point(aes(x=voltage, y=hamabot))+ geom_vline(xintercept = 2.4, color = 'red')+
  geom_vline(xintercept = 3, color = 'red')+ labs(title = "log")
dev.off()
#don't need to remove voltage >2.4 & <3

# check net voltage
# here we want to check corresponding variables of the special voltage point, so we build a df to extract these values
ggplot(net_tl) + geom_line(mapping = aes(x = epoch, y = voltage, color = as.factor(nodeid))) +
  scale_colour_discrete() +labs(title = 'voltage before cleaning')
df = net_tl[,c("epoch","humidity","humid_temp","hamatop","hamabot")]
df$voltage_les_0 = net_tl$voltage < 0
pdf("2-e hum-volt0.pdf")
ggplot(df, aes(x = epoch, y = humidity)) +
  geom_point(aes(colour = voltage_les_0), alpha = 0.5)
dev.off()
pdf("2-e temp-volt0.pdf")
ggplot(df, aes(x = epoch, y = humid_temp)) +
  geom_point(aes(colour = voltage_les_0), alpha = 0.5)
dev.off()
pdf("2-e top-volt0.pdf")
ggplot(df, aes(x = epoch, y = hamatop)) +
  geom_point(aes(colour = voltage_les_0), alpha = 0.5)
dev.off()
pdf("2-e bot-volt0.pdf")
ggplot(df, aes(x = epoch, y = hamabot)) +
  geom_point(aes(colour = voltage_les_0), alpha = 0.5)
dev.off()
unique(net_tl[net_tl$voltage < 0 ,]$nodeid)  
#don't need to remove?? humidity and temperature's trend is same is voltage < 1000. 
#Though hamabot and hamatop's don't, we can see voltage>1000 is only three node 134,141,145(constant 1023V), maybe due to location
pdf("2-e volt-devi.pdf")
ggplot(net_tl[net_tl$voltage > 0,]) + geom_line(mapping = aes(x = epoch, y = voltage, color = as.factor(nodeid))) + 
  scale_colour_discrete()  #need to rule out some points or not? what do you think?
dev.off()

###################################

# EDA

# merge net and log
intersect <- inner_join(net_tl, log_tl, by=c("nodeid", "epoch"))
nrow(unique(intersect[c("nodeid", "epoch")])) == nrow(intersect)  
#No duplicate data in intersection with unique epoch and epoch!
nrow(intersect) / nrow(net_tl)  
nrow(intersect) / nrow(log_tl)  
which(rowSums(select(intersect, humidity.x, humid_temp.x, hamatop.x, hamabot.x) - 
                select(intersect, humidity.y, humid_temp.y, hamatop.y, hamabot.y)) != 0)  
#data in net and log are all same (after we remove our own bad sample)
name = c("parent", "voltage", "depth", "humidity", "humid_temp", "humid_adj", "hamatop",
         "hamabot", "epochDates", "epochDays", "Height", "Direc", "Dist", "Tree")
intersect = select(intersect, -paste0(name, ".x"))
names(intersect)[3:16] = name
nrow(intersect[intersect$Tree=="edge",]) == 0 #check that all nodes left are interior, not edge.

# pick time period
intersect$epochDates = as.POSIXct(strptime(intersect$epochDates, format = "%A %b %d %H:%M:%S %Y"))
pdf("3-a datadistri.pdf")
intersect %>%
  select(nodeid,epochDates,Height) %>% 
  ggplot() + 
  geom_point(aes(x = epochDates,y = as.factor(nodeid),color = Height), size = .1) + 
  labs(x = "Days", y = "nodeid") + 
  geom_vline(xintercept = as.numeric(as.POSIXct(c("2004/05/07 11:25:00","2004/05/26 9:00:00"))),
             linetype = 4) +
  annotate('text',x = as.POSIXct("2004/05/24 23:00:00"),y=3,
           label="9:00am, May 26",size=4) + 
  annotate('text',x = as.POSIXct("2004/05/09 9:00:00"),y=3,
           label="11:25am, May 7",size=4)
intersect = intersect[intersect$epochDates < as.POSIXct("2004/05/26 9:00:00"),]
dev.off()

## pairwise scatterplots
pdf("3-a pairwise.pdf")
ggpairs(select(intersect, humidity, humid_temp, hamatop, hamabot, Height)) 
dev.off()

library(scales)
library(reshape2)

## temporal trend
intersect$epochDayF = format(intersect$epochDates, "%b %d")  #F for final
#humidity
humid_day_mean = intersect %>%
  select(epochDayF,humidity, Height) %>% 
  group_by(epochDayF,Height) %>% 
  mutate(mean = mean(humidity)) %>%
  select(epochDayF,Height,mean) %>%
  unique() %>% 
  arrange(epochDayF)
pdf("3-c humtrend.pdf")
humid_day_mean %>% 
  melt(id.vars = c("epochDayF", "Height")) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = as.POSIXct(strptime(epochDayF, format = "%b %d", tz = "GMT")),
                          y = value, group = Height, color = Height), size = 0.3) + 
  scale_color_gradient(low = "coral4", high = "lightskyblue") + 
  scale_x_datetime(breaks=date_breaks('2 day'),
                   labels=date_format('%b %d')) + 
  labs(x = "Day", y = "humidity", color = "Height")
dev.off()
#temperature
temp_day_mean = intersect %>%
  select(epochDayF,humid_temp, Height) %>% 
  group_by(epochDayF,Height) %>% 
  mutate(mean = mean(humid_temp)) %>%
  select(epochDayF,Height,mean) %>%
  unique() %>% 
  arrange(epochDayF)
pdf("3-c temptrend.pdf")
temp_day_mean %>% 
  melt(id.vars = c("epochDayF", "Height")) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = as.POSIXct(strptime(epochDayF, format = "%b %d", tz = "GMT")),
                          y = value, group = Height, color = Height), size = 0.3) + 
  scale_color_gradient(low = "coral4", high = "lightskyblue") + 
  scale_x_datetime(breaks=date_breaks('2 day'),
                   labels=date_format('%b %d')) + 
  labs(x = "Day", y = "temperature", color = "Height")
dev.off()
#hamatop
htop_day_mean = intersect %>%
  select(epochDayF,hamatop, Height) %>% 
  group_by(epochDayF,Height) %>% 
  mutate(mean = mean(hamatop)) %>%
  select(epochDayF,Height,mean) %>%
  unique() %>% 
  arrange(epochDayF)
pdf("3-c toptrend.pdf")
htop_day_mean %>% 
  melt(id.vars = c("epochDayF", "Height")) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = as.POSIXct(strptime(epochDayF, format = "%b %d", tz = "GMT")),
                          y = value, group = Height, color = Height), size = 0.3) + 
  scale_color_gradient(low = "coral4", high = "lightskyblue") + 
  scale_x_datetime(breaks=date_breaks('2 day'),
                   labels=date_format('%b %d')) + 
  labs(x = "Day", y = "Incident PAR", color = "Height")
dev.off()
#hamabot
hbot_day_mean = intersect %>%
  select(epochDayF,hamabot, Height) %>% 
  group_by(epochDayF,Height) %>% 
  mutate(mean = mean(hamabot)) %>%
  select(epochDayF,Height,mean) %>%
  unique() %>% 
  arrange(epochDayF)
pdf("3-c bottrend.pdf")
hbot_day_mean %>% 
  melt(id.vars = c("epochDayF", "Height")) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = as.POSIXct(strptime(epochDayF, format = "%b %d", tz = "GMT")),
                          y = value, group = Height, color = Height), size = 0.3) + 
  scale_color_gradient(low = "coral4", high = "lightskyblue") + 
  scale_x_datetime(breaks=date_breaks('2 day'),
                   labels=date_format('%b %d')) + 
  labs(x = "Day", y = "Reflected PAR", color = "Height")
dev.off()

# pca
pca = intersect %>%
  select(humidity,humid_temp,hamatop,hamabot) %>% 
  prcomp(center = TRUE, scale = TRUE)
pdf("3-d pca.pdf")
pca$sdev^2 %>% 
  data.frame("PC" = c(1:4), "Eigenvalue" = .) %>%
  ggplot(.,aes(x = PC, y = Eigenvalue)) + geom_point() + geom_line()
dev.off()

#########################################

# Interesting findings

## GMM
#em = mvnormalmixEM(scale(intersect[intersect$hamatop> 10 & intersect$hamabot> 0,
#                                   c("humidity","humid_temp","hamatop")]))  #not evident
#em1 = normalmixEM(scale(intersect[intersect$epochDaysF == "May 12",]))  #not evident
df = intersect[intersect$epochDayF == "May 07",]
gmm = mvnormalmixEM(df[, c("humidity","humid_temp")])
plot(gmm, whichplots = 2)
df$gmm_group1 = gmm$posterior[,1] > 0.5  #TRUE for group 1, FLASE for group 2
df %>% 
  ggplot() + 
  geom_point(aes(humidity,humid_temp,group = gmm_group1, color = gmm_group1))
df %>% 
  ggplot() + 
  geom_point(aes(Height,epochDates,group = gmm_group1, color = gmm_group1))
#plot_ly(x=df$humidity, y=df$humid_temp, z=df$hamatop, type="scatter3d", mode="markers",color=df$group1,alpha=0.8)

setdiff(as.character(unique(df[df$gmm_group1==TRUE,]$epochDates)),
        as.character(unique(df[df$gmm_group1==FALSE,]$epochDates)))  #in group1 but not in group2
setdiff(as.character(unique(df[df$gmm_group1==FALSE,]$epochDates)),
        as.character(unique(df[df$gmm_group1==TRUE,]$epochDates)))  #in group2 but not in group1
intersect(as.character(unique(df[df$gmm_group1==TRUE,]$epochDates)),
          as.character(unique(df[df$gmm_group1==FALSE,]$epochDates)))  #both in group1 and 2

setdiff(unique(df[df$gmm_group1==TRUE,]$Dist),unique(df[df$gmm_group1==FALSE,]$Dist))
setdiff(unique(df[df$gmm_group1==FALSE,]$Dist),unique(df[df$gmm_group1==TRUE,]$Dist))

setdiff(unique(df[df$gmm_group1==TRUE,]$Height),unique(df[df$gmm_group1==FALSE,]$Height))
setdiff(unique(df[df$gmm_group1==FALSE,]$Height),unique(df[df$gmm_group1==TRUE,]$Height))

setdiff(unique(df[df$gmm_group1==TRUE,]$nodeid),unique(df[df$gmm_group1==FALSE,]$nodeid))
setdiff(unique(df[df$gmm_group1==FALSE,]$nodeid),unique(df[df$gmm_group1==TRUE,]$nodeid))
df[df$nodeid==78,]$humidity
df[df$nodeid==78,]$humid_temp


## PCA
pca
autoplot(pca, col = "hamabot", label = FALSE, loadings.label = TRUE) + 
  theme_classic()
intersect = cbind(intersect, data.matrix(pca$x)[,c(1,2)])


## hierarchical clustering
#df1 = scale(intersect[intersect$epochDayF == "May 07",
#               c("humidity","humid_temp","hamatop")])
df1 = intersect[intersect$epochDayF == "May 07",
                c("PC1","PC2")]
m = c( "average", "single", "complete", "ward")
names(m) = c( "average", "single", "complete", "ward")
ac <- function(x) {
  agnes(df1, method = x)$ac
}
result = sapply(m, ac)
result  #method with the highest agglomerative coefficient is the best 
clust = df1 %>% 
  agnes(method = "ward")  #train the model with method "ward"
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram")   #draw the tree
gap_stat = clusGap(df1, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)  #choose the k with the highest gap statistic
final_clust = hclust(dist(df1, method = "euclidean"), method = "ward.D2" )
agnes_group = cutree(final_clust, k=5)
df1 <- data.frame(cbind(intersect[intersect$epochDayF == "May 07",], agnes_group = as.factor(agnes_group)))
df1 %>% 
  ggplot() + 
  geom_point(aes(epochDates,Height,group = agnes_group, color = agnes_group),alpha = 0.5)
df1[df1$agnes_group==4 | df1$agnes_group==5,] %>% 
  ggplot() + 
  geom_point(aes(humid_temp,humidity,group = agnes_group, color = agnes_group),alpha = 0.5)
df1[df1$agnes_group==4 & df1$humid_temp < 11 & df1$humidity > 95,]$hamabot
df1[df1$agnes_group==4 & df1$humid_temp < 11 & df1$humidity > 95,]$hamatop
#5 doesn't have low temperature and high humidity;
#low temp & high humitidy in 4: hamabot all between 20-40, hamatop all 0
unique(df1[df1$agnes_group==4,]$Direc)
unique(df1[df1$agnes_group==5,]$Direc)
#plot_ly(x=df1$PC1, y=df1$PC2, z=df1$PC3, type="scatter3d", mode="markers",color=df1$agnes_group,alpha=0.8)


##################################\

# Graph critique

## Figure 3a
log_variable = log(1 + select(intersect,hamatop,hamabot))
pdf("5-a logtop.pdf")
hist(log_variable$hamatop,xlab = "μmol/m2/s", ylab = "counts", main = "Incident PAR")
dev.off()
pdf("5-a logbot.pdf")
hist(log_variable$hamabot,xlab = "μmol/m2/s", ylab = "counts", main = "Reflected PAR")
dev.off()

## Figure 3c 3d
hour = as.integer(format(intersect$epochDates, "%H"))
intersect$epochPeriod = ifelse(hour < 6, "  0-  6",
                               ifelse(hour < 13, "  6-13", 
                                      ifelse(hour < 20, "13-20","20-24")))
#humid
humid_period_mean = intersect %>%
  select(epochPeriod,humidity, Height) %>% 
  group_by(epochPeriod,Height) %>% 
  mutate(mean = mean(humidity)) %>%
  select(epochPeriod,Height,mean) %>%
  unique()
pdf("5-b humtime.pdf")
humid_period_mean %>%   #mean by period(morning etc)
  ggplot() + 
  geom_line(mapping = aes(x = Height,y = mean, group = epochPeriod, color = epochPeriod), size = 0.3) + 
  labs(y = "humidity")
dev.off()
pdf("5-b humday.pdf")
humid_day_mean %>%   #mean by days
  ggplot() + 
  geom_line(mapping = aes(x = Height,y = mean, group = epochDayF, color = epochDayF), size = 0.3) + 
  labs(y = "humidity")
dev.off()
#temperature
temp_period_mean = intersect %>%
  select(epochPeriod,humid_temp, Height) %>% 
  group_by(epochPeriod,Height) %>% 
  mutate(mean = mean(humid_temp)) %>%
  select(epochPeriod,Height,mean) %>%
  unique()
pdf("5-b temptime.pdf")
temp_period_mean %>%   #mean by period(morning etc)
  ggplot() + 
  geom_line(mapping = aes(x = Height,y = mean, group = epochPeriod, color = epochPeriod), size = 0.3) + 
  labs(y = "temperature")
dev.off()
pdf("5-b tempday.pdf")
temp_day_mean %>%   #mean by days
  ggplot() + 
  geom_line(mapping = aes(x = Height,y = mean, group = epochDayF, color = epochDayF), size = 0.3) + 
  labs(y = "temperature")
dev.off()
#hamatop
htop_period_mean = intersect %>%
  select(epochPeriod,hamatop, Height) %>% 
  group_by(epochPeriod,Height) %>% 
  mutate(mean = mean(hamatop)) %>%
  select(epochPeriod,Height,mean) %>%
  unique()
pdf("5-b toptime.pdf")
htop_period_mean %>%   #mean by period(morning etc)
  ggplot() + 
  geom_line(mapping = aes(x = Height,y = mean, group = epochPeriod, color = epochPeriod), size = 0.3) + 
  labs(y = "Incident PAR")  #0-6: all zero,too
dev.off()
pdf("5-b topday.pdf")
htop_day_mean %>%   #mean by days
  ggplot() + 
  geom_line(mapping = aes(x = Height,y = mean, group = epochDayF, color = epochDayF), size = 0.3) + 
  labs(y = "Incident PAR")
dev.off()
#hamabot
hbot_period_mean = intersect %>%
  select(epochPeriod,hamabot, Height) %>% 
  group_by(epochPeriod,Height) %>% 
  mutate(mean = mean(hamabot)) %>%
  select(epochPeriod,Height,mean) %>%
  unique()
pdf("5-b bottime.pdf")
hbot_period_mean %>%   #mean by period(morning etc)
  ggplot() + 
  geom_line(mapping = aes(x = Height,y = mean, group = epochPeriod, color = epochPeriod), size = 0.3) + 
  labs(y = "Reflected PAR")  #0-6: all zero,too
dev.off()
pdf("5-b botday.pdf")
hbot_day_mean %>%   #mean by days
  ggplot() + 
  geom_line(mapping = aes(x = Height,y = mean, group = epochDayF, color = epochDayF), size = 0.3) + 
  labs(y = "Reflected PAR")
dev.off()

## Figure 4
#humid
humid_height_mean = intersect[intersect$epochDayF == "May 07",] %>%
  select(epochDates,humidity, Height) %>% 
  mutate(Height_group = cut(Height, 5)) %>% 
  group_by(epochDates,Height_group) %>% 
  mutate(mean = mean(humidity)) %>% 
  select(epochDates,Height_group,mean) %>%
  unique()

pdf("5-c humtime.pdf")
humid_height_mean %>% 
  melt(id.vars = c("epochDates", "Height_group")) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = epochDates,
                          y = value, group = Height_group, color = Height_group), size = 0.3) +  
  scale_color_brewer(type="equal",palette="Set2") + 
  labs(x = "Time", y = "humidity", color = "Height")  # For May 07
dev.off()

#temperature
temp_height_mean = intersect[intersect$epochDayF == "May 07",] %>%
  select(epochDates,humid_temp, Height) %>% 
  mutate(Height_group = cut(Height, 5)) %>% 
  group_by(epochDates,Height_group) %>% 
  mutate(mean = mean(humid_temp)) %>% 
  select(epochDates,Height_group,mean) %>%
  unique()
pdf("5-c temptime.pdf")
temp_height_mean %>% 
  melt(id.vars = c("epochDates", "Height_group")) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = epochDates,
                          y = value, group = Height_group, color = Height_group), size = 0.3) +  
  scale_color_brewer(type="equal",palette="Set2") + 
  labs(x = "Time", y = "temperature", color = "Height")  # For May 07
dev.off()

#hamatop
htop_height_mean = intersect[intersect$epochDayF == "May 07",] %>%
  select(epochDates,hamatop, Height) %>% 
  mutate(Height_group = cut(Height, 5)) %>% 
  group_by(epochDates,Height_group) %>% 
  mutate(mean = mean(hamatop)) %>% 
  select(epochDates,Height_group,mean) %>%
  unique()
pdf("5-c toptime.pdf")
htop_height_mean %>% 
  melt(id.vars = c("epochDates", "Height_group")) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = epochDates,
                          y = value, group = Height_group, color = Height_group), size = 0.3) +  
  scale_color_brewer(type="equal",palette="Set2") + 
  labs(x = "Time", y = "Incident PAR", color = "Height")  # For May 07
dev.off()

#hamabot
hbot_height_mean = intersect[intersect$epochDayF == "May 07",] %>%
  select(epochDates,hamabot, Height) %>% 
  mutate(Height_group = cut(Height, 5)) %>% 
  group_by(epochDates,Height_group) %>% 
  mutate(mean = mean(hamabot)) %>% 
  select(epochDates,Height_group,mean) %>%
  unique()
pdf("5-c bottime.pdf")
hbot_height_mean %>% 
  melt(id.vars = c("epochDates", "Height_group")) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = epochDates,
                          y = value, group = Height_group, color = Height_group), size = 0.3) +  
  scale_color_brewer(type="equal",palette="Set2") + 
  labs(x = "Time", y = "Reflected PAR", color = "Height")  # For May 07
dev.off()
 
## figure 7
log_tl$origin = "log"
net_tl$origin = "net"
all = rbind(log_tl,net_tl)
png("5-d fig7.png")
all %>%
  select(Height,epochDates,origin) %>% 
  unique() %>% 
  ggplot() + 
  geom_point(aes(x = as.POSIXct(strptime(epochDates, format = "%A %b %d %H:%M:%S %Y")),
                 y = as.factor(Height),color = origin,group = origin),
             size = .01, position=position_dodge(width=0.6)) + 
  theme(axis.text.y = element_text(size = 6,color="black"))  + 
  labs(x = "Days", y = "Height") + 
  scale_x_datetime(breaks=date_breaks('5 day'), labels=date_format('%b %d'))
dev.off()