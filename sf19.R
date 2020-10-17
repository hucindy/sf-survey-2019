# 2019 San Francisco Survey Analysis
# Cindy Hu

# Load libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggmosaic)
library(choroplethrZip)
library(reshape2)
library(gridExtra)
library(scales)
library(treemapify)

# Load data
setwd("~/projects/sf-survey-2019")
sf_19 <- read_excel("San Francisco 2019 Survey Data.xlsx")
issue_values <- read_excel("City Survey Master Data 1996-2019.xlsx", sheet = "Issue Values")
names(issue_values) <- c("value","descr")
issue_values$value <- as.numeric(issue_values$value)
issue_values <- issue_values %>% select(value, descr) %>% filter(!is.na(value))

# Choropleth map
zip_count <- sf_19 %>% filter(dem_zip!=99999) %>% group_by(dem_zip) %>% count
names(zip_count) <- c("region", "value")
zip_count$region <- as.character(zip_count$region)
zip_choropleth(zip_count, county_zoom=6075, title="San Francisco 2019 City Survey Respondents", num_colors = 1) + scale_fill_continuous(low="#eff3ff", high="#084594", na.value="white", name="Respondents")

# Gender
gender_count <- sf_19 %>% group_by(dem_gender) %>% count
gender_count %>%
  mutate(percentage = paste(as.character(round(n/sum(gender_count$n)*100,1)),"%"))%>%
  ggplot(aes(dem_gender, n, fill = dem_gender)) + geom_col() + 
  geom_label(aes(label=percentage), position = position_dodge(width=1))+
  theme(legend.position = "none")+
  labs(x="", y="") + ggtitle("Respondents by Gender")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())

# Age
age_group_count <- sf_19 %>% filter(!is.na(dem_age)) %>% group_by(dem_age) %>% count
age_group_count %>%
  mutate(percentage = paste0(as.character(round(n/sum(age_group_count$n)*100,1)),"%")) %>%
  ggplot(aes(dem_age, n, fill = dem_age)) +
  geom_col() + labs(x="", y="") + 
  theme(legend.position="none") + ggtitle("Respondents by Age") +
  geom_label(aes(label=paste0(percentage)), position = position_dodge(width=1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())

# Age and gender
sf_19 %>%
  filter(!is.na(dem_age) & dem_gender %in% c("Male","Female")) %>%
  ggplot(aes(dem_age, fill=dem_gender)) +
  geom_bar(position = "fill") + geom_abline(slope=0, intercept=0.5, lty=2) +
  labs(x="", y="")+ ggtitle("Proportion of Respondents by Age and Sex") +
  scale_fill_discrete(name = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())

# Sexual orientation
sorient_count <- sf_19 %>% filter(!is.na(dem_sorient)) %>% group_by(dem_sorient) %>% count 
sorient_count[nrow(sorient_count)+1,] = list("LGBTQ+",252)
sorient_count2 <- rbind(sorient_count[7,1:2],sorient_count[4,1:2])
sorient_count2$n <- as.numeric(sorient_count2$n)

sorient_count2 %>% 
  mutate(percentage = paste(as.character(round(n/sum(sorient_count2$n)*100,1)),"%")) %>%
  ggplot(aes(dem_sorient, n, fill = dem_sorient)) + geom_col(width=0.5) +
  labs(x="", y="") + theme(legend.position="none") + ggtitle("Respondents by Sexual Orientation") +
  geom_label(aes(label=percentage), position = position_dodge(width=1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
raceeth_count <- sf_19 %>% filter(!is.na(dem_raceeth))%>% group_by(dem_raceeth) %>% count
raceeth_count %>% 
  mutate(percentage = paste(as.character(round(n/sum(raceeth_count$n)*100,1)),"%"))%>%
  ggplot(aes(dem_raceeth, n, fill = dem_raceeth)) + geom_col() + labs(x="", y="") +
  ggtitle("Respondents by Race/Ethnicity") +  theme(legend.position ="none") +
  geom_label(aes(label=percentage), position = position_dodge(width=1)) +
  scale_x_discrete(labels= function(x) lapply(str_wrap(x, width=13), paste, collapse = "\n"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())

# Household Income
income_group_count <- sf_19 %>% filter(!is.na(dem_income)) %>% group_by(dem_income) %>% count
income_group_count %>% 
  mutate(percentage = paste(as.character(round(n/sum(income_group_count$n)*100,1)),"%")) %>%
  ggplot(aes(dem_income, n, fill = dem_income)) +
  geom_col() + labs(x="", y="") + 
  theme(legend.position="none") + ggtitle("Respondents by Household Income") +
  scale_x_discrete(limits = c("$10k or less", "$10k-$25k", "$25k-$35k", "$35k-$50k", "$50k-$100k", "$100k-$200k", "Over $200k")) +
  geom_label(aes(label=percentage), position = position_dodge(width=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())

# Highest education level
ed_count <- sf_19 %>% filter(!is.na(dem_ed)) %>% group_by(dem_ed) %>% count
ed_count %>% 
  mutate(percentage = paste(as.character(round(n/sum(ed_count$n)*100,1)),"%")) %>%
  ggplot(aes(dem_ed, n, fill = dem_ed)) +
  geom_col() + labs(x = "", y="") + 
  ggtitle("Respondents by Highest Level of Education") + 
  theme(legend.position = "none")+
  geom_label(aes(label=percentage), position = position_dodge(width=1)) +
  scale_x_discrete(limits = c("Less than high school","High school","Some college, <4yr deg","4 year college degree","Advanced degree"), labels= function(x) lapply(str_wrap(x, width=15), paste, collapse = "\n"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())

# Household size
hhsize_count <- sf_19 %>% group_by(dem_hhsize) %>% count
hhsize_count %>% 
  mutate(percentage = paste(as.character(round(n/sum(hhsize_count$n)*100,1)),"%")) %>%
  ggplot(aes(dem_hhsize, n, fill = as.factor(dem_hhsize))) +
  geom_col() + labs(x = "", y="") + 
  ggtitle("Respondents by Household Size") +
  geom_label(aes(label=percentage), position = position_dodge(width=1)) +
  theme(legend.position="none") + scale_x_discrete(limits = c(0:6))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())

# Children
child <- as.data.frame(prop.table(table(sf_19$child_yes)))
child <- child %>% mutate(Var1=recode_factor(Var1, "1"="Yes", "0"="No"))

ggplot(child, aes(x= "", y=Freq, fill=Var1)) + geom_col(width=1, color="white") + coord_polar("y", start=0)+theme_void()+  geom_text(aes(label=paste0(round(Freq*100),"%")), position = position_stack(vjust=0.5), size=I(5.5), color="white") + scale_fill_discrete(name="") + ggtitle("Do you have any children under 18 who live with you in SF?") + theme(plot.title = element_text(size = 16))

# Home status
homeown <- as.data.frame(prop.table(table(sf_19$dem_homeown)))

ggplot(homeown, aes(x= "", y=Freq, fill=Var1)) + geom_col(width=1, color="white") + coord_polar("y", start=0)+theme_void()+  geom_text(aes(label=paste0(round(Freq*100),"%")), position = position_stack(vjust=0.5), size=I(5.5), color="white") + scale_fill_discrete(name="") + ggtitle("Do you own or rent your home?") + theme(plot.title = element_text(size = 16))
sflived_count <- sf_19 %>% filter(!is.na(dem_sflived)) %>%
  group_by(dem_sflived) %>% count

# Residence
sflived_count  %>%
  mutate(percentage = paste(as.character(round(n/sum(sflived_count$n)*100,1)),"%")) %>%
  ggplot(aes(dem_sflived, n, fill = as.factor(dem_sflived))) +
  geom_col() + labs(x = "", y="") + 
  ggtitle("Respondents by Years Lived in SF") +
  geom_label(aes(label=percentage), position = position_dodge(width=1)) +
  theme(legend.position="none") +
  scale_x_discrete(limits = c("<=2 years","3-5 years","6-10 years","11-20 years","21-30 years",">30 years"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank())

# Average ratings
cat_mean <- sf_19 %>% 
  select("general_rate","inf_rate","rec_rate","lib_rate","muni_rate","safe_rate","u311_rate","child_school") %>%
  sapply(., function(col) as.numeric((col))) %>% 
  colMeans(na.rm = TRUE) 

cat_mean <- melt(t(cat_mean)) %>%
  rename(category=Var2, mean=value) %>%
  select(category, mean) %>%
  mutate(category=(case_when(
    category=="general_rate"~"Local Government",
    category=="inf_rate"~"Infrastructure",
    category=="rec_rate"~"Recreation and Parks",
    category=="lib_rate"~"Libraries",
    category=="muni_rate"~"Muni",
    category=="safe_rate"~"Public Safety",
    category=="u311_rate"~"311 Experience",
    category=="child_school"~"Schools"))) 

cat_mean$letter <- cut(cat_mean$mean, 
                       breaks = c(1.00,1.33,1.67,2.00,2.33,2.67,3.00,3.33,3.67,4.00,4.33,4.67,5.00), 
                       labels = c("F", "D-", "D", "D+", "C-","C","C+","B-","B","B+","A-","A")
)
cat_mean %>%
  ggplot(aes(reorder(x=factor(category),mean, FUN=min), mean, fill=factor(category))) + geom_col()+ ggtitle("Average Ratings out of 5 points") + labs(x="", y="")+coord_flip() +
  labs(x="", y="")+
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.y=element_blank())+
  geom_text(aes(label=round(mean,2)), hjust=1.85, size=4) +
  geom_label(aes(label=letter),position = position_dodge(width=1), size=5, fontface =2)

# Local government
general_rate_count <- sf_19 %>% filter(!is.na(general_rate)) %>% group_by(general_rate)%>% count
general_rate_count %>% mutate(percentage = paste(as.character(round(n/sum(general_rate_count$n)*100,1)),"%")) %>%
  ggplot(aes(general_rate, n, fill=factor(general_rate))) + geom_col()+ ggtitle("Overall Job of Local Government in Providing Services") + labs(x="Rating", y="Respondents")+
  geom_text(aes(label=paste(n,"\n",percentage)), vjust=0.5, size=4) +
  theme(legend.position="none", axis.title.x=element_blank(),axis.ticks.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
grate_raceeth_long <- as.data.frame(round(prop.table(table(sf_19$general_rate, sf_19$dem_raceeth),2),2))
grate_raceeth_long <- grate_raceeth_long %>% 
  rename(grade=Var1, raceeth=Var2, prop=Freq)
grate_raceeth_long$grade = as.numeric(as.character(grate_raceeth_long$grade))
grate_raceeth_long[grate_raceeth_long$grade >=4,]$grade = "Good/Excellent"
grate_raceeth_long[grate_raceeth_long$grade ==3,]$grade = "Average"
grate_raceeth_long[grate_raceeth_long$grade <=2,]$grade = "Fail/Poor"

grate_raceeth_long$grade = factor(grate_raceeth_long$grade, levels=c("Fail/Poor","Average","Good/Excellent"))

grate_raceeth_long <- grate_raceeth_long %>% group_by(grade, raceeth) %>% summarize(prop=sum(prop))

grate_raceeth_long %>% filter(grade=="Good/Excellent",raceeth!="American Indian or Alaska Native") %>%   ggplot(aes(reorder(x=raceeth,prop,FUN=min), prop, fill = raceeth)) + 
  geom_col() + 
  ggtitle("Proportion of Good/Excellent Ratings on Local Government Performance") +
  geom_label(aes(label=paste0(prop*100,"%")), position = position_dodge(width=1))+
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank()) + 
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
  guides(fill=guide_legend(override.aes=aes(label=""), title =""))+
  scale_x_discrete(labels= function(x) lapply(str_wrap(x, width=13), paste, collapse = "\n"))

# Infrastructure
inf_rate_group <- sf_19 %>% select(inf_rate) %>% filter(!is.na(inf_rate)) %>% group_by(grp = cut(inf_rate, c(-Inf,1.5,2,2.5,3,3.5,4,4.5,Inf))) 

inf_rate_group %>%
  ggplot(aes(x=inf_rate, y = (..count..)/sum(..count..), fill=factor(grp))) + 
  geom_histogram(binwidth=0.5, boundary=1) + 
  labs(x="Rating", y="Respondents") +
  ggtitle("Percent of Average Infrastructure Ratings") +
  theme(legend.position="none", axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(labels = scales::percent)

inf_raceeth_count <- as.data.frame(aggregate(inf_rate~dem_raceeth,data=sf_19,length)) %>% 
  rename(total_count=inf_rate)

inf_raceeth_4_5 <- sf_19 %>% select(dem_raceeth, inf_rate) %>% filter(inf_rate >=4, !is.na(dem_raceeth)) %>% group_by(dem_raceeth) %>% count 

inf_raceeth_4_5 <- merge(inf_raceeth_4_5,inf_raceeth_count,by="dem_raceeth") %>% 
  mutate(percent=n/total_count*100)

inf_raceeth_4_5 %>% filter(dem_raceeth != "American Indian or Alaska Native") %>%
  ggplot(aes(reorder(x=dem_raceeth,percent,FUN=min), percent, fill = dem_raceeth)) + 
  geom_col() + 
  ggtitle("Proportion of Good/Excellent Ratings on Infrastructure") +
  geom_label(aes(label=paste(round(percent,1),"%")), position = position_dodge(width=1))+
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank()) + 
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
  guides(fill=guide_legend(override.aes=aes(label=""), title =""))+
  scale_x_discrete(labels= function(x) lapply(str_wrap(x, width=13), paste, collapse = "\n"))

# Infrastructure subratings
inf <- rbind(prop.table(table(sf_19$inf_water)),prop.table(table(sf_19$inf_sewer)),prop.table(table(sf_19$inf_clean)),prop.table(table(sf_19$inf_stcond)),prop.table(table(sf_19$inf_sidecond)))
inf <- as.data.frame(t(inf))
names(inf) <- c("Quality of water services","Reliability of sewer services","Cleanliness of streets and sidewalks in your neighborhood","Condition of street pavement in your neighborhood","Condition of sidewalk pavement and curb ramps in your neighborhood")
inf$rating <- rownames(inf)
inf_long <- melt(inf, id.vars = c("rating"), value.name="proportion")

ggplot(inf_long, aes(y=proportion, x=variable, fill=rating)) +geom_col()+ scale_x_discrete(labels= function(x) lapply(str_wrap(x, width=15), paste, collapse = "\n")) + labs(x="", y="Proportion") + ggtitle("Infrastructure")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+ theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())

inf_clean_1_2 <- sf_19 %>% select(inf_clean, dem_zip) %>% filter(inf_clean == 1 | inf_clean ==2) %>% group_by(dem_zip) %>% count %>% arrange
names(inf_clean_1_2) <- c("region", "value")
inf_clean_1_2$region <- as.character(inf_clean_1_2$region)
zip_choropleth(inf_clean_1_2, county_zoom=6075, title="Areas with low ratings on cleanliness of streets and sidewalks", num_colors=1) + scale_fill_continuous(low="#eff3ff", high="#084594", na.value="white", name = "% rated low")

# Recreation and parks
rec_rate_count <- sf_19 %>% filter(!is.na(rec_rate)) %>% group_by(rec_rate)%>% count
rec_rate_count %>% mutate(percentage = paste(as.character(round(n/sum(rec_rate_count$n)*100,1)),"%")) %>% ggplot(aes(rec_rate, n, fill=factor(rec_rate))) + geom_col()+ ggtitle("Overall Quality of Recreation and Park System") + labs(x="", y="")+
  geom_text(aes(label=paste(n,"\n",percentage)), vjust=0.6, size=4) +
  theme(legend.position="none",axis.ticks.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

rec_freq <- as.data.frame(prop.table(table(sf_19$rec_freq)))

ggplot(rec_freq, aes(x= "", y=Freq, fill=Var1)) + geom_col(width=1, color="white") + coord_polar("y", start=0)+theme_void()+  geom_text(aes(label=paste0(round(Freq*100),"%")), position = position_stack(vjust=0.5), size=I(5.5), color="white") + scale_fill_discrete(name="") + ggtitle("Frequency of Park Visits") + theme(plot.title = element_text(size = 16))

rec_freq_4_5 <- as.data.frame(round(prop.table(table(sf_19$rec_rate, sf_19$rec_freq),2),2)) %>%
  mutate(Var1=recode_factor(Var1, "1"="Fail/Poor", "2"="Fail/Poor", "3"="Average", "4"="Good/Excellent", "5"="Good/Excellent")) %>% rename(grade=Var1, rec_freq=Var2, prop=Freq) %>%
  group_by(grade, rec_freq) %>% summarize(prop=sum(prop)) %>% 
  filter(grade=="Good/Excellent")

rec_freq_4_5 %>% ggplot(aes(rec_freq, prop, fill = rec_freq)) + 
  geom_col() +
  ggtitle("Proportion of Good/Excellent Overall Quality of Parks by Usage") +
  geom_label(aes(label=paste0(prop*100,"%")), position = position_dodge(width=1))+
  labs(x="", y="Proportion") +
  scale_x_discrete(limits = c("Never","Once or twice a year","Several times a year","At least once a month","At least once a week"), labels= function(x) lapply(str_wrap(x, width=13), paste, collapse = "\n")) + theme(legend.position="none",axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) 
rec_child_4_5 <- sf_19 %>% select(rec_rate, dem_raceeth, child_yes) %>% filter(!is.na(child_yes), !is.na(dem_raceeth),dem_raceeth!="American Indian or Alaska Native", rec_rate==4|rec_rate==5) %>% group_by(dem_raceeth, child_yes) %>% count 

rec_child_count <- aggregate(rec_rate~dem_raceeth+child_yes, data=subset(sf_19, dem_raceeth!="American Indian or Alaska Native" & !is.na(child_yes)) ,length) %>%
  rename(total=rec_rate)

rec_child_4_5 <- merge(rec_child_4_5,rec_child_count) %>% mutate(percent=round(n/total*100,1)) %>%
  mutate(child_yes=(case_when(
    child_yes==1~"Yes",
    child_yes==0~"No"
  )))

ggplot(data = rec_child_4_5, aes(x=dem_raceeth, y=(child_yes), fill=percent)) + geom_tile()+ scale_x_discrete(labels= function(x) lapply(str_wrap(x, width=20), paste, collapse = "\n")) + labs(x="", y="Living with Children") + ggtitle("Proportion of Good/Excellent Park Ratings\nBy Status of Living With Children and Race/Ethnicity") + geom_text(aes(label=paste(round(percent,1),"%")),color="white")+scale_fill_continuous(high="#132B43", low = "#56B1F7")+
  theme(legend.position="none",axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

rec <- rbind(prop.table(table(sf_19$rec_plant)),prop.table(table(sf_19$rec_field)),prop.table(table(sf_19$rec_clean)),prop.table(table(sf_19$rec_ctr_clean)),prop.table(table(sf_19$rec_ctr_qual)))
rec <- as.data.frame(t(rec))
names(rec) <- c("Quality of landscaping and plantings","Quality of athletic fields and courts","Cleanliness of parks","Condition of Recreation and Parks Dept. buildings and structures","Quality of recreation programs and activities")
rec$rating <- rownames(rec)
rec_long <- melt(rec, id.vars = c("rating"), value.name="proportion")

ggplot(rec_long, aes(y=proportion, x=variable, fill=rating)) +geom_col()+ scale_x_discrete(labels= function(x) lapply(str_wrap(x, width=15), paste, collapse = "\n")) + labs(x="", y="Percent") + ggtitle("Quality of Parks")+theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) 
rec_ctr_freq <- as.data.frame(prop.table(table(sf_19$rec_ctr_yes))) %>%
  mutate(Var1=case_when(
    Var1==1 ~ "Yes",
    Var1==0 ~ "No"
  ))

ggplot(rec_ctr_freq, aes(x= "", y=Freq, fill=Var1)) + geom_col(width=1, color="white") + coord_polar("y", start=0)+theme_void()+  geom_text(aes(label=paste0(round(Freq*100),"%")), position = position_stack(vjust=0.5), size=I(5.5), color="white") + scale_fill_discrete(name="") + ggtitle("Recreation Program and Recreation Center Usage") + theme(plot.title = element_text(size = 16))

rec_ctr <- rbind(prop.table(table(sf_19$rec_ctr_clean)),prop.table(table(sf_19$rec_ctr_qual)))
rec_ctr <- as.data.frame(t(rec_ctr))
names(rec_ctr) <- c("Condition of Recreation and Parks Department buildings and structures", "Quality of recreation programs and activities")
rec_ctr$rating <- rownames(rec_ctr)
rec_ctr <- melt(rec_ctr, id.vars = c("rating"), value.name="proportion")

ggplot(rec_ctr, aes(y=proportion, x=variable, fill=rating)) +geom_col(width=.4)+ scale_x_discrete(labels= function(x) lapply(str_wrap(x, width=25), paste, collapse = "\n")) + labs(x="", y="Proportion") + ggtitle("Recreation") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+ theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())

# Libraries
lib_rate_count <- sf_19 %>% filter(!is.na(lib_rate)) %>% group_by(lib_rate)%>% count
lib_rate_count %>% mutate(percentage = paste(as.character(round(n/sum(lib_rate_count$n)*100,1)),"%")) %>% ggplot(aes(lib_rate, n, fill=factor(lib_rate))) + geom_col()+ ggtitle("Overall Quality of Library System") + labs(x="Rating", y="Respondents")+
  geom_text(aes(label=paste(n,"\n", percentage)), vjust=0.5, size=4) +
  theme(legend.position="none",axis.ticks.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

branch_freq <-sf_19 %>% select(lib_branch_freq) %>% mutate(lib="Branch") %>%
  rename("freq"="lib_branch_freq")
main_freq <- sf_19 %>% select(lib_main_freq) %>% mutate(lib="Main") %>%
  rename("freq"="lib_main_freq")
online_freq <- sf_19 %>% select(lib_online_freq) %>% mutate(lib="Online") %>%
  rename("freq"="lib_online_freq")
lib_freq <- bind_rows(branch_freq, main_freq, online_freq) 
lib_freq$freq <- factor(lib_freq$freq, levels=c("Never","Once or twice a year","Several times a year","At least once a month","At least once a week"))

ggplot(data = lib_freq) +
  geom_mosaic(aes(x = product(lib, freq), fill = lib)) + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 15, hjust = 1)) +
  labs(x="",y="") +
  ggtitle("Frequency of Library Visits")+ 
  theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

# Branch Library
branch_freq_long <- as.data.frame(round(prop.table(table(sf_19$lib_rate, sf_19$lib_branch_freq),2),2)) 
branch_freq_4_5 <- branch_freq_long %>%
  mutate(Var1=recode_factor(Var1, "1"="Fail/Poor", "2"="Fail/Poor", "3"="Average", "4"="Good/Excellent", "5"="Good/Excellent")) %>% rename(grade=Var1, lib_branch_freq=Var2, prop=Freq) %>%
  group_by(grade, lib_branch_freq) %>% summarize(prop=sum(prop)) %>% 
  filter(grade=="Good/Excellent")

p1 <- branch_freq_4_5 %>% ggplot(aes(lib_branch_freq, prop, fill = lib_branch_freq)) + 
  geom_col() +
  ggtitle("Proportion of Good/Excellent Ratings \nBranch Library") +
  geom_label(aes(label=paste0(prop*100,"%")), position = position_dodge(width=1))+
  labs(x="", y="") +
  scale_x_discrete(limits = c("Never","Once or twice a year","Several times a year","At least once a month","At least once a week"), labels= function(x) lapply(str_wrap(x, width=13), paste, collapse = "\n")) +
  theme(legend.position="none",axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

# Main Library
main_freq_long <- as.data.frame(round(prop.table(table(sf_19$lib_rate, sf_19$lib_main_freq),2),2)) 
main_freq_4_5 <- main_freq_long %>%
  mutate(Var1=recode_factor(Var1, "1"="Fail/Poor", "2"="Fail/Poor", "3"="Average", "4"="Good/Excellent", "5"="Good/Excellent")) %>% rename(grade=Var1, lib_main_freq=Var2, prop=Freq) %>%
  group_by(grade, lib_main_freq) %>% summarize(prop=sum(prop)) %>% 
  filter(grade=="Good/Excellent") 

p2 <- main_freq_4_5 %>% ggplot(aes(lib_main_freq, prop, fill = lib_main_freq)) + 
  geom_col() +
  ggtitle("\nMain Library") +
  geom_label(aes(label=paste0(prop*100,"%")), position = position_dodge(width=1))+
  labs(x="", y="") +
  scale_x_discrete(limits = c("Never","Once or twice a year","Several times a year","At least once a month","At least once a week"), labels= function(x) lapply(str_wrap(x, width=13), paste, collapse = "\n")) +
  theme(legend.position="none",axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

# Online Library
online_freq_long <- as.data.frame(round(prop.table(table(sf_19$lib_rate, sf_19$lib_online_freq),2),2)) 
online_freq_4_5 <- online_freq_long %>%
  mutate(Var1=recode_factor(Var1, "1"="Fail/Poor", "2"="Fail/Poor", "3"="Average", "4"="Good/Excellent", "5"="Good/Excellent")) %>% rename(grade=Var1, lib_online_freq=Var2, prop=Freq) %>%
  group_by(grade, lib_online_freq) %>% summarize(prop=sum(prop)) %>% 
  filter(grade=="Good/Excellent")

p3 <- online_freq_4_5 %>% ggplot(aes(lib_online_freq, prop, fill = lib_online_freq)) + 
  geom_col() +
  ggtitle("\nOnline Library Services") +
  geom_label(aes(label=paste0(prop*100,"%")), position = position_dodge(width=1))+
  labs(x="", y="") +
  scale_x_discrete(limits = c("Never","Once or twice a year","Several times a year","At least once a month","At least once a week"), labels= function(x) lapply(str_wrap(x, width=13), paste, collapse = "\n")) + 
  theme(legend.position="none",axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

grid.arrange(p1, p2, p3, ncol=3)
lib_child_4_5 <- sf_19 %>% select(lib_rate, dem_raceeth, child_yes) %>% filter(!is.na(child_yes), !is.na(dem_raceeth),dem_raceeth!="American Indian or Alaska Native", lib_rate>=4) %>% group_by(dem_raceeth, child_yes) %>% count 

lib_child_count <- aggregate(lib_rate~dem_raceeth+child_yes, data=subset(sf_19, dem_raceeth!="American Indian or Alaska Native" & !is.na(child_yes)) ,length) %>%
  rename(total=lib_rate)

lib_child_4_5 <- merge(lib_child_4_5,lib_child_count) %>% mutate(percent=round(n/total*100,1)) %>%
  mutate(child_yes=(case_when(
    child_yes==1~"Yes",
    child_yes==0~"No"
  )))

ggplot(data = lib_child_4_5, aes(x=dem_raceeth, y=(child_yes), fill=percent)) + geom_tile()+ scale_x_discrete(labels= function(x) lapply(str_wrap(x, width=20), paste, collapse = "\n")) + labs(x="", y="Living with Children") + ggtitle("Percentage of Good/Excellent Library Ratings\nBy Status of Living With Children and Race/Ethnicity") + geom_text(aes(label=paste(round(percent,1),"%")),color="white")+theme(legend.position = "none")+scale_fill_continuous(high="#132B43", low = "#56B1F7") +
  theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

lib <- rbind(prop.table(table(sf_19$lib_books)),prop.table(table(sf_19$lib_online)),prop.table(table(sf_19$lib_wifi)),prop.table(table(sf_19$lib_staff)),prop.table(table(sf_19$lib_clean)), prop.table(table(sf_19$lib_qual)))
lib <- as.data.frame(t(lib))
names(lib) <- c("Books, DVDs, CDs, etc.","Online services, SF Library website, eBooks, apps" ,"Internet access, wifi","Assistance from library staff","Condition, cleanliness, maintenance of the library","Quality of library programs, classes, events")
lib$rating <- rownames(lib)
lib_long <- melt(lib, id.vars = c("rating"), value.name="proportion")

ggplot(lib_long, aes(y=proportion, x=variable, fill=rating)) +geom_col()+ scale_x_discrete(labels= function(x) lapply(str_wrap(x, width=15), paste, collapse = "\n")) + labs(x="", y="Proportion") + ggtitle("Libraries")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+ theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())

# Transportation/Muni
muni_rate_count <- sf_19 %>% filter(!is.na(muni_rate)) %>% group_by(muni_rate) %>% count
muni_rate_count %>% mutate(percentage = paste(as.character(round(n/sum(muni_rate_count$n)*100,1)),"%")) %>% ggplot(aes(muni_rate, n, fill=factor(muni_rate))) + geom_col()+ ggtitle("Overall Quality of Muni") + labs(x="Rating", y="Respondents")+
  geom_text(aes(label=paste(n,"\n",percentage)), vjust=0.5, size=4) +
  theme(legend.position="none",axis.ticks.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

sf_19 %>% filter(!is.na(muni_rate)) %>% summarize(mean(muni_rate))

trans_freq <-  sf_19 %>% select("trans_walk_freq","trans_bike_freq","trans_muni_freq", "trans_taxi_freq","trans_uber_freq","trans_drive_freq","trans_pool_freq")

trans_freq <- melt(t((trans_freq))) %>% select(Var1, value) %>% rename("trans"=Var1, "freq"=value) %>%
  mutate(trans=(case_when(
    trans=="trans_walk_freq"~"Walk",
    trans=="trans_bike_freq"~"Bike",
    trans=="trans_muni_freq"~"Muni",
    trans=="trans_taxi_freq"~"Taxi",
    trans=="trans_uber_freq"~"Uber",
    trans=="trans_drive_freq"~"Drive",
    trans=="trans_pool_freq"~"Carpool"
  ))) 
trans_freq$freq <- factor(trans_freq$freq, levels=c("Never","Less than 1 Day/week","1-2 Days/week","3-6 Days/week","Daily"))

ggplot(data = trans_freq) +
  geom_mosaic(aes(x = product(trans, freq), fill = trans)) + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 15, hjust = 1)) +
  labs(x="",y="") +
  ggtitle("Frequency of Transportation") +
  theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
trans_used <- trans_freq %>% filter(freq!="Never") %>% group_by(trans) %>% count %>% mutate(percentage = n/2218*100) 

trans_used %>% ggplot(aes(reorder(x=trans,n, FUN=min), n, fill = trans)) + geom_col() + 
  geom_text(aes(label=paste(round(percentage,1),"%")), vjust=-0.5, size=4)+
  labs(x="", y="") + ggtitle("Transportation Used") +
  theme(legend.position="none", axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

muni_freq_4_5 <- as.data.frame(round(prop.table(table(sf_19$muni_rate, sf_19$trans_muni_freq),2),2)) %>%
  mutate(Var1=recode_factor(Var1, "1"="Fail/Poor", "2"="Fail/Poor", "3"="Average", "4"="Good/Excellent", "5"="Good/Excellent")) %>% rename(grade=Var1, muni_freq=Var2, prop=Freq) %>%
  group_by(grade, muni_freq) %>% summarize(prop=sum(prop)) %>% 
  filter(grade=="Good/Excellent")

muni_freq_4_5 %>% ggplot(aes(muni_freq, prop, fill = muni_freq)) + 
  geom_col() +
  ggtitle("Proportion of Good/Excellent Overall Quality of Muni by Usage") +
  geom_label(aes(label=paste0(prop*100,"%")), position = position_dodge(width=1))+
  labs(x="", y="") +
  scale_x_discrete(limits = c("Never","Less than 1 Day/week","1-2 Days/week","3-6 Days/week","Daily"), labels= function(x) lapply(str_wrap(x, width=13), paste, collapse = "\n"))+
  theme(legend.position="none",axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
muni_raceeth_4_5 <- sf_19 %>% select(muni_rate, dem_raceeth, muni_user_yes) %>% filter(!is.na(muni_user_yes), !is.na(dem_raceeth),dem_raceeth!="American Indian or Alaska Native", muni_rate>=4) %>% group_by(dem_raceeth, muni_user_yes) %>% count 

muni_raceeth_count <- aggregate(muni_rate~dem_raceeth+muni_user_yes, data=subset(sf_19, dem_raceeth!="American Indian or Alaska Native" & !is.na(muni_user_yes)) ,length) %>%
  rename(total=muni_rate)

muni_raceeth_4_5 <- merge(muni_raceeth_4_5,muni_raceeth_count) %>% mutate(percent=round(n/total*100,1)) %>%
  mutate(muni_user_yes=(case_when(
    muni_user_yes==1~"Yes",
    muni_user_yes==0~"No"
  )))

ggplot(data = muni_raceeth_4_5, aes(x=dem_raceeth, y=(muni_user_yes), fill=percent)) + geom_tile()+ scale_x_discrete(labels= function(x) lapply(str_wrap(x, width=20), paste, collapse = "\n")) + labs(x="", y="Muni User") + ggtitle("Percentage of Good/Excellent Muni Ratings\nBy Status of Muni User and Race/Ethnicity") + geom_text(aes(label=paste(round(percent,1),"%")),color="white")+scale_fill_continuous(high="#132B43", low = "#56B1F7") + 
  theme(legend.position="none",axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

trans <- rbind(prop.table(table(sf_19$muni_reliable)),prop.table(table(sf_19$muni_clean)),prop.table(table(sf_19$muni_safe)),prop.table(table(sf_19$muni_crowd)),prop.table(table(sf_19$muni_driver)))
trans <- as.data.frame(t(trans))
names(trans) <- c("Reliability","Cleanliness" ,"Safety","Managing crowding","Courtesy of drivers")
trans$rating <- rownames(trans)
trans_long <- melt(trans, id.vars = c("rating"), value.name="proportion")

ggplot(trans_long, aes(y=proportion, x=variable, fill=rating)) +geom_col()+ scale_x_discrete(labels= function(x) lapply(str_wrap(x, width=15), paste, collapse = "\n")) + labs(x="", y="Proportion") + ggtitle("Muni")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+ theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())

safe_rate_group <- sf_19 %>% select(safe_rate) %>% filter(!is.na(safe_rate)) %>% group_by(grp = cut(safe_rate, c(-Inf,1.5,2,2.5,3,3.5,4,4.5,Inf))) 

safe_rate_group %>%
  ggplot(aes(x=safe_rate, y = (..count..)/sum(..count..), fill=factor(grp))) + 
  geom_histogram(binwidth=0.5, boundary=1) + 
  labs(x="Rating", y="Respondents") +
  ggtitle("Percent of Public Safety Ratings") +
  theme(legend.position="none", axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(labels = scales::percent)

# Public Safety 
safe <- rbind(prop.table(table(sf_19$safe_day)),prop.table(table(sf_19$safe_night)))
safe <- as.data.frame(t(safe))
names(safe) <- c("Safety while walking alone in neighborhood during the day","Safety while walking alone in neighborhood at night")
safe$rating <- rownames(safe)
safe_long <- melt(safe, id.vars = c("rating"), value.name="proportion")

ggplot(safe_long, aes(y=proportion, x=variable, fill=rating)) +geom_col(width=.4)+ scale_x_discrete(labels= function(x) lapply(str_wrap(x, width=35), paste, collapse = "\n")) + labs(x="", y="Proportion") + ggtitle("Public Safety")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+ theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())

# Public Day Safety by Race/Ethnicity and Gender

safe_day_4_5_gr <- sf_19 %>% select(safe_day, dem_raceeth, dem_gender) %>% filter(dem_gender=="Male"|dem_gender=="Female", safe_day==4|safe_day==5) %>% group_by(dem_raceeth, dem_gender) %>% count 

safe_day_4_5_gr_N <- aggregate(safe_day~dem_raceeth+dem_gender, data=subset(sf_19, dem_gender=="Male"|dem_gender=="Female") ,length)

safe_day_4_5_gr_merge <- merge(safe_day_4_5_gr,safe_day_4_5_gr_N) %>%
  filter(dem_raceeth != "American Indian or Alaska Native") %>%
  mutate(percent=round(n/safe_day*100,1))

ggplot(data = safe_day_4_5_gr_merge, aes(x=dem_raceeth, y=(dem_gender), fill=percent)) + geom_tile()+ scale_x_discrete(labels= function(x) lapply(str_wrap(x, width=10), paste, collapse = "\n")) + labs(x="", y="") + ggtitle("Percent feeling safe/very safe walking alone during the day") + geom_text(aes(label=paste(percent,"%")),color="white")+
  scale_fill_continuous(high="#132B43", low = "#56B1F7")+ 
  theme(legend.position="none",axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

# Public Night Safety by Race/Ethnicity and Gender
safe_night_4_5_gr <- sf_19 %>% select(safe_night, dem_raceeth, dem_gender) %>% filter(dem_gender=="Male"|dem_gender=="Female", dem_raceeth !="NA", safe_night==4|safe_night==5) %>% group_by(dem_raceeth, dem_gender) %>% count 

safe_night_4_5_gr_N <- aggregate(safe_night~dem_raceeth+dem_gender, data=subset(sf_19, dem_gender=="Male"|dem_gender=="Female") ,length)

safe_night_4_5_gr_merge <- merge(safe_night_4_5_gr,safe_night_4_5_gr_N) %>% mutate(percent=round(n/safe_night*100,1))

ggplot(data = safe_night_4_5_gr_merge, aes(x=dem_raceeth, y=(dem_gender), fill=percent)) + geom_tile()+ scale_x_discrete(labels= function(x) lapply(str_wrap(x, width=10), paste, collapse = "\n")) + labs(x="", y="") + ggtitle("Percent feeling safe/very safe walking alone at night") + geom_text(aes(label=paste(percent,"%")),color="white")+theme(legend.position = "none")+
  scale_fill_continuous(high="#132B43", low = "#56B1F7") +
  theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
safe_night_4_5 <- sf_19 %>% select(safe_night, dem_zip) %>% filter(safe_night == 4 | safe_night ==5) %>% group_by(dem_zip) %>% count
names(safe_night_4_5) <- c("region", "high_ratings")
safe_night_4_5_prop <- merge(zip_count, safe_night_4_5, by="region") 
safe_night_4_5_prop<- safe_night_4_5_prop %>% filter(value >30) %>% mutate(percent = high_ratings/value*100)
names(safe_night_4_5_prop) <- c("region","n","high_ratings","value")
safe_night_4_5_prop$region <- as.character(safe_night_4_5_prop$region)
zip_choropleth(safe_night_4_5_prop, county_zoom=6075, title="Areas with high ratings on safety while \nwalking alone in neighborhood at night", num_colors=1) + scale_fill_continuous(low="#eff3ff", high="#084594", na.value="white", name="% rated high on safety")

# 311
u311_heard_yes <- as.data.frame(prop.table(table(sf_19$u311_heard, sf_19$u311_user_yes)[2,]))
names(u311_heard_yes) <- c("percentage")
u311_heard_yes$user <- c("Have not used 311", "Have used 311")

ggplot(u311_heard_yes, aes(x= "", y=percentage, fill=user)) + geom_col(width=1, color="white") + coord_polar("y", start=0)+theme_void()+  geom_text(aes(label=paste0(round(percentage*100),"%")), position = position_stack(vjust=0.5), size=I(5.5), color="white") + scale_fill_discrete(name="") + ggtitle("311 usage in the past year for those who have heard of 311") + theme(plot.title = element_text(size = 16))

u311_rate_count <- sf_19 %>% filter(!is.na(u311_rate)) %>% group_by(u311_rate) %>% count
u311_rate_count %>% mutate(percentage = paste(as.character(round(n/sum(u311_rate_count$n)*100,1)),"%")) %>% ggplot(aes(u311_rate, n, fill=factor(u311_rate))) + geom_col()+ ggtitle("311 Experience") + labs(x="", y="")+
  geom_text(aes(label=paste(n,"\n",percentage)), vjust=0.5, size=4) +
  theme(legend.position="none",axis.ticks.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

u311_raceeth_4_5 <- as.data.frame(round(prop.table(table(sf_19$u311_rate, sf_19$dem_raceeth),2),3))%>% filter(Var2 !="American Indian or Alaska Native", as.numeric(Var1)>=4) %>% rename(grade=Var1, raceeth=Var2, prop=Freq) %>% 
  mutate(grade=(case_when(
    as.numeric(grade)>=4~"Good/Excellent"
  ))) %>% 
  group_by(raceeth) %>% summarize(prop=sum(prop))

u311_raceeth_4_5 %>% 
  ggplot(aes(reorder(x=raceeth,prop,FUN=min), prop, fill = raceeth)) + 
  geom_col() + 
  ggtitle("Percentage of Good/Excellent Ratings on 311 Experience") +
  geom_label(aes(label=paste0(prop*100,"%")), position = position_dodge(width=1))+
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank()) + 
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
  guides(fill=guide_legend(override.aes=aes(label=""), title =""))+
  scale_x_discrete(labels= function(x) lapply(str_wrap(x, width=13), paste, collapse = "\n"))

# Schools
school_rate_count <- sf_19 %>% filter(!is.na(child_school)) %>% group_by(child_school)%>% count
school_rate_count %>% mutate(percentage = paste(as.character(round(n/sum(school_rate_count$n)*100,1)),"%")) %>%
  ggplot(aes(child_school, n, fill=factor(child_school))) + geom_col()+ ggtitle("Quality of School Child(ren) Attend") +
  geom_text(aes(label=paste(n,"\n",percentage)), vjust=0.5, size=4) +
  theme(legend.position="none", axis.title.x=element_blank(),axis.ticks.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

sfprivate_rate <- sf_19 %>% select(child_school, child_sfprivate) %>% filter(!is.na(child_school) & child_sfprivate==1)
sfpublic_rate <- sf_19 %>% select(child_school, child_sfpublic) %>% filter(!is.na(child_school) & child_sfpublic==1)

school_rate <- as.data.frame(cbind(
  c("Public","Private"), 
  c(mean(sfpublic_rate$child_school),mean(sfprivate_rate$child_school))
))
names(school_rate) <- c("school","mean")
school_rate$mean <- as.numeric(school_rate$mean)

school_rate %>% ggplot(aes(x=school, y=mean, fill=school)) + 
  geom_col(width=0.3) +
  labs(x="", y="")+
  ggtitle("Mean Rating of Public vs. Private Schools") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
  theme(axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_text(aes(label=round(mean,2)), vjust=-0.5, size=4)

school_raceeth_4_5 <- as.data.frame(round(prop.table(table(sf_19$child_school, sf_19$dem_raceeth),2),3))%>% filter(Var2 !="American Indian or Alaska Native", as.numeric(Var1)>=4) %>% rename(grade=Var1, raceeth=Var2, prop=Freq) %>% 
  mutate(grade=(case_when(
    as.numeric(grade)>=4~"Good/Excellent"
  ))) %>% 
  group_by(raceeth) %>% summarize(prop=sum(prop))

school_raceeth_4_5 %>% 
  ggplot(aes(reorder(x=raceeth,prop,FUN=min), prop, fill = raceeth)) + 
  geom_col() + 
  ggtitle("Percentage of Good/Excellent Ratings on Quality of Schools") +
  geom_label(aes(label=paste0(prop*100,"%")), position = position_dodge(width=1))+
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank(), axis.ticks.y=element_blank()) + 
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
  guides(fill=guide_legend(override.aes=aes(label=""), title =""))+
  scale_x_discrete(labels= function(x) lapply(str_wrap(x, width=13), paste, collapse = "\n"))

# Summary of grades
general_rate_prop <- as.data.frame(prop.table(table(sf_19$general_rate))) %>% rename(prop=Freq, rating=Var1) %>% mutate(lab.ypos = cumsum(prop) - 0.5*prop)

inf_rate_prop <- sf_19 %>% select(inf_rate) %>% 
  mutate(inf_rate_round = ifelse(inf_rate %% 1 == 0.5, ceiling(inf_rate), round(inf_rate))) %>%
  select(inf_rate_round) %>%
  table() %>% prop.table() %>% as.data.frame() %>%
  select(prop=2, rating=1) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

rec_rate_prop <- as.data.frame(prop.table(table(sf_19$rec_rate))) %>% rename(prop=Freq, rating=Var1) %>% mutate(lab.ypos = cumsum(prop) - 0.5*prop)

lib_rate_prop <- as.data.frame(prop.table(table(sf_19$lib_rate))) %>% rename(prop=Freq, rating=Var1) %>% mutate(lab.ypos = cumsum(prop) - 0.5*prop)

muni_rate_prop <- as.data.frame(prop.table(table(sf_19$muni_rate))) %>% rename(prop=Freq, rating=Var1) %>% mutate(lab.ypos = cumsum(prop) - 0.5*prop)

safe_rate_prop <- sf_19 %>% select(safe_rate) %>% 
  mutate(safe_rate_round = ifelse(safe_rate %% 1 == 0.5, ceiling(safe_rate), round(safe_rate))) %>%
  select(safe_rate_round) %>%
  table() %>% prop.table() %>% as.data.frame() %>%
  select(prop=2, rating=1) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

u311_rate_prop <- as.data.frame(prop.table(table(sf_19$u311_rate))) %>% rename(prop=Freq, rating=Var1) %>% mutate(lab.ypos = cumsum(prop) - 0.5*prop)

school_rate_prop <- as.data.frame(prop.table(table(sf_19$child_school))) %>% rename(prop=Freq, rating=Var1) %>% mutate(lab.ypos = cumsum(prop) - 0.5*prop)

c1 <- ggplot(general_rate_prop, aes(x = 2, y = prop, fill = rating)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  theme_void()+ 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 15)) +
  xlim(0.5, 2.5)+
  ggtitle("Local Government") +
  annotate(geom = 'text', x = 0.5, y = 0, size = 6, label = paste0(cat_mean$letter[cat_mean$category=="Local Government"]))


c2 <- ggplot(inf_rate_prop, aes(x = 2, y = prop, fill = rating)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  theme_void()+ 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 15)) +
  xlim(0.5, 2.5)+
  ggtitle("Infrastructure") +
  annotate(geom = 'text', x = 0.5, y = 0, size = 6, label = paste0(cat_mean$letter[cat_mean$category=="Infrastructure"]))

c3 <- ggplot(rec_rate_prop, aes(x = 2, y = prop, fill = rating)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  theme_void()+ 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 15)) +
  xlim(0.5, 2.5)+
  ggtitle("Recreation and Parks") +
  annotate(geom = 'text', x = 0.5, y = 0, size = 6, label = paste0(cat_mean$letter[cat_mean$category=="Recreation and Parks"]))


c4 <- ggplot(lib_rate_prop, aes(x = 2, y = prop, fill = rating)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  theme_void()+ 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 15)) +
  xlim(0.5, 2.5)+
  ggtitle("Libraries") +
  annotate(geom = 'text', x = 0.5, y = 0, size = 6, label = paste0(cat_mean$letter[cat_mean$category=="Libraries"]))

c5 <- ggplot(muni_rate_prop, aes(x = 2, y = prop, fill = rating)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  theme_void()+ 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 15)) +
  xlim(0.5, 2.5)+
  ggtitle("Muni") +
  annotate(geom = 'text', x = 0.5, y = 0, size = 6, label = paste0(cat_mean$letter[cat_mean$category=="Muni"]))


c6 <- ggplot(safe_rate_prop, aes(x = 2, y = prop, fill = rating)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  theme_void()+ 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 15)) +
  xlim(0.5, 2.5)+
  ggtitle("Public Safety") +
  annotate(geom = 'text', x = 0.5, y = 0, size = 6, label = paste0(cat_mean$letter[cat_mean$category=="Public Safety"]))

c7 <- ggplot(u311_rate_prop, aes(x = 2, y = prop, fill = rating)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  theme_void()+ 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 15)) +
  xlim(0.5, 2.5)+
  ggtitle("311 Experience") +
  annotate(geom = 'text', x = 0.5, y = 0, size = 6, label = paste0(cat_mean$letter[cat_mean$category=="311 Experience"]))


c8 <- ggplot(school_rate_prop, aes(x = 2, y = prop, fill = rating)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  theme_void()+ 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 15)) +
  xlim(0.5, 2.5)+
  ggtitle("Schools") +
  annotate(geom = 'text', x = 0.5, y = 0, size = 6, label = paste0(cat_mean$letter[cat_mean$category=="Schools"]))

grid.arrange(c1, c2, c3, c4, c5, c6, c7, c8, ncol=4)

# City issues
issue <-  sf_19 %>% select(issue_1,issue_2,issue_3,issue_4)
issue <- melt(t((issue))) %>% select(Var1, value)
issue_count <- issue %>% group_by(value) %>% count
issue_count <- merge(issue_count, issue_values, by="value")
issue_count <- issue_count %>% filter(as.numeric(value)>0) %>%
  mutate(grp=(case_when(
    n>=200 ~ "Extremely Important",
    n>=100 & n<200 ~ "Very Important",
    n>=50 & n<100 ~ "Moderately Important",
    n>10 & n<50 ~ "Slightly Important",
    n<=10 ~ "Not Important At All"
  ))) 

issue_count %>%
  ggplot(aes(area = log10(n), fill = factor(value), label = descr, subgroup = grp))+
  geom_treemap() +
  geom_treemap_subgroup_border(color ="black") +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, color =
                               "black", fontface = "italic") +
  geom_treemap_text(colour = "white", reflow = T) +
  theme(legend.position = "none") +
  ggtitle("2019 Issues")

# Homelessness
updown_homeless_count <- sf_19 %>% filter(!is.na(updown_homeless))%>% group_by(updown_homeless) %>% count
updown_homeless_count %>% mutate(percent=round(n/sum(updown_homeless_count$n)*100),2) %>% ggplot(aes(x=factor(updown_homeless),y=n,  fill = factor(updown_homeless)))+geom_col()+coord_flip()+ theme(legend.position = "none")+ labs(x="", y="") + ggtitle("Has homelessness gotten better or worse in the last two years?") +
  geom_text(aes(label=paste0(percent,"%")), hjust=-0.1, size=4) +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
  theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())

# Street cleanliness
updown_stclean_count <- sf_19 %>% filter(!is.na(updown_stclean))%>% group_by(updown_stclean) %>% count
updown_stclean_count %>% mutate(percent=round(n/sum(updown_stclean_count$n)*100),2) %>% ggplot(aes(x=factor(updown_stclean),y=n,  fill = factor(updown_stclean)))+geom_col()+coord_flip()+ theme(legend.position = "none")+ labs(x="", y="") + ggtitle("Has street and sidewalk cleanliness gotten better or worse in the last two years?")+
  geom_text(aes(label=paste0(percent,"%")), hjust=-0.1, size=4)+
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
  theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())

# Public safety
updown_pubsafe_count <- sf_19 %>% filter(!is.na(updown_pubsafe))%>% group_by(updown_pubsafe) %>% count
updown_pubsafe_count %>% mutate(percent=round(n/sum(updown_pubsafe_count$n)*100),2) %>% ggplot(aes(x=factor(updown_pubsafe),y=n,  fill = factor(updown_pubsafe)))+geom_col()+coord_flip()+ theme(legend.position = "none")+ labs(x="", y="") + ggtitle("Has public safety gotten better or worse in the last two years?") +
  geom_text(aes(label=paste0(percent,"%")), hjust=-0.1, size=4)+
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
  theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
sfmove_count <- sf_19 %>% filter(!is.na(dem_sfmove)) %>%
  group_by(dem_sfmove) %>% count

# Moving out of SF
sfmove_count  %>%
  mutate(percentage = paste(as.character(round(n/sum(sfmove_count$n)*100,1)),"%")) %>%
  ggplot(aes(dem_sfmove, n, fill = as.factor(dem_sfmove))) +
  geom_col() + labs(x = "", y="") + 
  ggtitle("Likelihood of Moving Out") +
  geom_label(aes(label=percentage), position = position_dodge(width=1)) +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
  theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
sfmove_count <- sf_19 %>% select(dem_sfmove, dem_raceeth, dem_sflived, dem_income) %>% filter(!is.na(dem_raceeth), !is.na(dem_sflived), !is.na(dem_income), dem_raceeth!="American Indian or Alaska Native") %>%
  mutate(dem_sflived=case_when(
    dem_sflived=="11-20 years"| dem_sflived==">30 years"| dem_sflived=="21-30 years" ~ "Lived more than 10 years in SF",
    dem_sflived=="<=2 years" | dem_sflived=="3-5 years" | dem_sflived=="6-10 years" ~ "Lived 10 years or less in SF")) %>%
  mutate(dem_income=case_when(
    dem_income=="$10k or less" | dem_income=="$10k-$25k" | dem_income=="$25k-$35k" |   dem_income=="$35k-$50k"~"<$50k",
    dem_income=="$50k-$100k" | dem_income=="$100k-$200k" | dem_income=="Over $200k" ~">=$50k"
  )) %>% group_by(dem_raceeth, dem_sflived, dem_income) %>% count %>% rename(total=n)

sfmove_likely <- sf_19 %>% select(dem_sfmove, dem_raceeth, dem_sflived, dem_income) %>% filter(!is.na(dem_raceeth), !is.na(dem_sflived), !is.na(dem_income), dem_sfmove=="Very likely"|dem_sfmove=="Somewhat likely", dem_raceeth!="American Indian or Alaska Native") %>%
  mutate(dem_sflived=case_when(
    dem_sflived=="11-20 years"| dem_sflived==">30 years"| dem_sflived=="21-30 years" ~ "Lived more than 10 years in SF",
    dem_sflived=="<=2 years" | dem_sflived=="3-5 years" | dem_sflived=="6-10 years" ~ "Lived 10 years or less in SF")) %>%
  mutate(dem_income=case_when(
    dem_income=="$10k or less" | dem_income=="$10k-$25k" | dem_income=="$25k-$35k" |   dem_income=="$35k-$50k"~"<$50k",
    dem_income=="$50k-$100k" | dem_income=="$100k-$200k" | dem_income=="Over $200k" ~">=$50k"
  )) %>% group_by(dem_raceeth, dem_sflived, dem_income) %>% count

sfmove_likely <- merge(sfmove_likely, sfmove_count) %>% 
  mutate(percent=n/total*100)

sfmove_likely %>% ggplot(aes(dem_raceeth, dem_income, fill=percent)) +
  facet_grid(dem_sflived~., scales = "free") +
  geom_tile() +
  scale_fill_continuous(high="#132B43", low = "#56B1F7", name="%") +
  scale_x_discrete(labels= function(x) lapply(str_wrap(x, width=10), paste, collapse = "\n")) +
  labs(x="", y="Income") + 
  ggtitle("Percent likely to move out of SF in the next 3 years")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
  theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
sfmove_zip_count <- zip_count %>% rename(total_count = value)

sfmove_zip_likely <- sf_19 %>% select(dem_zip, dem_sfmove) %>%
  filter(!is.na(dem_zip), dem_zip!=99999, dem_sfmove=="Very likely"| dem_sfmove=="Somewhat likely") %>%
  group_by(dem_zip) %>% 
  count %>%
  rename(region=dem_zip)

sfmove_zip_likely <- merge(sfmove_zip_count, sfmove_zip_likely, by="region") %>%
  mutate(percent_likely = n/total_count*100) %>% rename(value=percent_likely)

zip_choropleth(sfmove_zip_likely, county_zoom=6075, title="Percent somewhat/very likely to move out of SF by geographic area", num_colors = 1) + scale_fill_continuous(low="#eff3ff", high="#084594", na.value="white", name="%")