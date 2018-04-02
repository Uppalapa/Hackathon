#Data Visualization Hackathon 
df <- read.csv('C:/Users/kamal/OneDrive/Documents/data visu hackathon/crime.csv', stringsAsFactors = F)

head(df)
colSums(is.na(df))
dim(df)

install.packages(c("ggplot2","RColorBrewer","scales"))
library(ggplot2)
library(scales)
library(grid)
library(RColorBrewer)
install.packages("ggplotThemes")
library(ggplotThemes)
library(ggplot2)
library(lubridate)
library(dplyr)

#df$MONTH <- as.character(df$MONTH)
ggplot(data = df) +
  geom_line(data = df[df$YEAR == 2016,],aes(x = (MONTH)), stat="count", group = 1
            , color = 'red', size=4) +
  geom_line(data = df[df$YEAR == 2017,],aes(x = (MONTH)), stat="count", group = 1
            , color = 'skyblue',size=4) +
  theme_bw() + 
  annotate('text', x = 7.5, y = 8450, label = "2016", size = 5) +
  annotate('text', x = 4.5, y = 8800, label = "2017", size = 5) +
  #scale_x_discrete(labels = month.abb)
  xlab('') + ylab('Total count') +
  scale_x_continuous(breaks=c(1:12), labels = month.abb) +
  labs(title = 'Crime Distribution over Time')



##############################################################################
#Offence Code

a <- df[df$OFFENSE_CODE_GROUP %in% c('Larceny','Motor Vehicle Accident Response'
                                     ,'Medical Assistance','Investigate Person','Drug Violation'),]
head(a)

b = subset(df, !(OFFENSE_CODE_GROUP %in% c('Larceny','Motor Vehicle Accident Response'
                                            ,'Medical Assistance','Investigate Person')))
b$OFFENSE_CODE_GROUP <- 'Other'

#df_sagar <- rbind(a,b)
df_sagar <- a[,c(3,4)]
head(df_sagar)
write.csv(df_sagar, 'offence_group.csv', row.names = F)

##############################################################################
df$Hour <- hour(as.character(df$OCCURRED_ON_DATE))

df_arrest_time <- df %>%
  mutate(Hour = hour(as.character(df$OCCURRED_ON_DATE))) %>%
  group_by(DAY_OF_WEEK, Hour) %>%
  summarize(count = n())

ggplot(df_arrest_time, aes(x = Hour, y = DAY_OF_WEEK, fill = count)) +
  geom_tile() +
  #fte_theme() +
  theme_bw() +
  labs(x = "Hour of Crime", y = "Day of Week of Crime"
       , title = "Number of crimes in Boston (2015-2018), by Time") +
  scale_fill_gradient(low = "white", high = "firebrick4", name = 'Count')


                      
#################################################################################
install.packages("treemapify")
install.packages("dplyr")
library(treemapify)
library(ggplot2)
df_ucr = subset(df, (UCR_PART %in% c('Part One','Part Two','Part Three')))

df_crime_type <- dplyr::df_ucr %>%
  group_by(UCR_PART, OFFENSE_CODE_GROUP) %>%
  summarize(count = n())

table(df_ucr$UCR_PART)
df_crime_type <- as.data.frame(df_crime_type)
df_crime_type$area <- ifelse(df_crime_type$UCR_PART == 'Part One',53095
                             ,ifelse(df_crime_type$UCR_PART == 'Part Two',83782,134386))

df_crime_type$col <- ifelse(df_crime_type$UCR_PART == 'Part One','red'
                             ,ifelse(df_crime_type$UCR_PART == 'Part Two','blue','yellow'))

colfunc_one <- colorRampPalette(c("white", "darkred"))
colfunc_two <- colorRampPalette(c("white", "darkgreen"))
colfunc_three <- colorRampPalette(c("white", "darkblue"))

df_crime_type <- df_crime_type[order(df_crime_type$UCR_PART,df_crime_type$count),]

df_crime_type$col <- c(colfunc_one(9),colfunc_three(26),colfunc_two(29))

ggplot(df_crime_type, aes(area = count, fill = col, label = OFFENSE_CODE_GROUP,
                subgroup = UCR_PART)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  guides(fill=FALSE)  

##################################################################################
library(ggmap)
m <- get_map("Boston",zoom = 12, maptype="toner",source="stamen")

main <- df[!duplicated(df$REPORTING_AREA, fromLast = T),]
check <- table(df$REPORTING_AREA)
check <- data.frame(names(check),check)
names(check) <- c('REPORTING_AREA','count')
main <- merge(main,check, by = 'REPORTING_AREA')

ggmap(m) + 
  #geom_point(aes(y=Lat,x=Long, fill=..level..),data=main) + geom_point(alpha=0.3) +
  #theme(legend.position="none") +
  stat_density2d(aes(x = Long, y = Lat, fill=..level..), data=main,geom="polygon", alpha=0.2) +
  scale_fill_gradient(low = "yellow", high = "#E41919") +
  xlim(-71.1,-71.02) +
  ylim(42.295,42.39) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())







