library(tidyverse)
library(mvtnorm)

set.seed(23)
no.sims<-100000

#locations
data.folder <- "./Data"
output.folder <- "./Output"
study.hours.file<-"Exam.study.hours.csv"
hist.attend<-"Historical.Exam.Attendees.csv"

#exam map
exam.map<-tribble(~"old",~"new",
                  "CT1","CM1",
                  "CT2","CB1",
                  "CT3","CS1",
                  "CT4","CS2",
                  "CT5","CM1",
                  "CT6","CS2",
                  "CT7","CB2",
                  "CT8","CM2",
                  "CA1","CP1")

#get historical exam results
exam.results<-read.csv(file.path(data.folder,hist.res.file))
study.hours<-read.csv(file.path(data.folder,study.hours.file))
exam.students<-read.csv(file.path(data.folder,hist.attend))

exam.students <- exam.students %>%
  gather(key="Exam","Attendees",-Date)
exam.students$Attendees <- parse_number(exam.students$Attendees)

er<-exam.results %>%
  gather(key="Exam","Pass.Rate",-Date) %>%
  left_join(exam.students) %>%
  left_join(exam.map,by=c("Exam"="old")) %>%
  rename("New.Exam"="new") %>%
  mutate(Type=ifelse(Exam %in% exam.map$old,"Old","New")) %>%
  filter(!is.na(Pass.Rate))
er$New.Exam[is.na(er$New.Exam)] <- er$Exam[is.na(er$New.Exam)]
er$Date <- er$Date %>% parse_date(format = "%d/%m/%Y")
er.mean <-er %>% group_by(Exam,Type) %>% summarise(Mean=mean(Pass.Rate,na.rm=T)) %>% ungroup()

er2 <- er %>% mutate(Winners = Pass.Rate * Attendees) %>% mutate(Losers = (1-Pass.Rate) * Attendees)


corr.test<-er2 %>% select(New.Exam,Attendees,Pass.Rate) %>% split(~New.Exam)

cor.res<-list()

for (z in 1:length(corr.test)) {
  df<-corr.test[[z]]
  cor.res[[z]] <- cor(df$Attendees,df$Pass.Rate,method="spearman")
}

names(cor.res) <- names(corr.test)






#start loop to try different correlations, as well as old vs new
era<-c("new","old")
inter.exam.corr<-c(0,0.25,0.5,0.75,1)
combos<-crossing(era,inter.exam.corr) %>%
  data.frame(id=1:(length(era)*length(inter.exam.corr)),.)
loop.results<-list()

for (i in 1:nrow(combos)) {

#specify parameters from combos table
era <- combos$era[i]
inter.exam.corr <- combos$inter.exam.corr[i]

#the exams to be passed...
the.exams<-exam.map %>% select(era) %>% unique() %>% .[[1]]
no.exams<-length(the.exams)
pass.probs <- er.mean %>% filter(Exam %in% the.exams)

#correlation matrix construction
base.matrix<-matrix(rep(inter.exam.corr,no.exams^2),nrow=no.exams)
for (m in 1:no.exams) {
  base.matrix[m,m]<-1
}

#random number generation
corr.norm.randos<-rmvnorm(no.sims,sigma=base.matrix)
corr.unif.randos<-pnorm(corr.norm.randos)

#get data.frame so can apply pmap()
c.u.r<-lapply(seq_len(ncol(corr.unif.randos)), function(i) corr.unif.randos[,i])
qgeom_params <-c.u.r %>%
  enframe(name="row.no",value="p") %>%
  data.frame(prob=pass.probs$Mean,.) %>%
  select(-row.no)

#map to geometric distributions to get fails
no.fails<-pmap(qgeom_params,.f=qgeom)
names(no.fails) <- the.exams

hour.fails<-list()
for (exam in the.exams) {
  exam.h <- study.hours %>% filter(Exam==exam) %>% .$Hours
  hour.fails[[exam]] <- no.fails[[exam]] * exam.h
}

hour.fails.df<-hour.fails %>% bind_cols()
sum.fail.hours <-hour.fails.df %>% rowSums()
base.hours<-sum(study.hours %>% filter(Exam %in% the.exams) %>% .$Hour)
sum.hours <- sum.fail.hours + base.hours

exam.hours.index <- sum.hours/base.hours

loop.results[[i]] <- exam.hours.index
}

#how many years for minimum qualification travel time?
travel.time.multiplier<-2

#get long df with all results in
res.data<-loop.results %>% bind_cols()
names(res.data) <- seq_along(res.data)
res.data <- res.data %>% gather(key="id",value="travel.time")
res.data$id <- res.data$id %>% parse_number()
res.data <- left_join(res.data,combos)
res.data <- res.data %>% mutate(tt.y.round=ceiling(2*travel.time*travel.time.multiplier)/2)

res.data.grp<- res.data %>% group_by(era,inter.exam.corr,tt.y.round) %>%
  summarise(proportion=n()/no.sims) %>% ungroup()

ggplot(res.data.grp %>% filter(era=="old", inter.exam.corr!=1),aes(y=proportion,
                                                                  x=tt.y.round,
                                                                  fill=factor(inter.exam.corr))) +
  geom_col() +
  coord_flip() +
  scale_x_continuous(name="Travel Time (yrs)",limits = c(1.75,6.25)) +
  scale_y_continuous(name="Proportion of Students Completing Core Exams at Travel Time",
                     limits=c(0,0.3),
                     breaks=seq(0,0.3,by=0.05),
                     labels=scales::percent(seq(0,0.3,by=0.05))) +
  scale_fill_brewer(name="",palette="PuOr") +
  theme_classic() +
  theme(legend.position = "none",
        strip.background = element_rect(fill="lightgrey",colour="black",linewidth = 0.25)) +
  facet_wrap(~str_c("Correlation = ",scales::percent(inter.exam.corr)),nrow=1)


ggsave("Travel Time diff sims - new.jpg",height=3,width=8)


percentile.steps<-1/75
#get percentiles of results
get.100.p<-function(x) {
x %>% quantile(seq(0,1,by=percentile.steps))
}
percentiles<-map(loop.results,get.100.p) %>% bind_rows(.id="id") %>%
  gather(key="percentile",value="travel.time",-id)
percentiles$id <- percentiles$id %>% as.numeric()
percentiles$percentile <- percentiles$percentile %>% parse_number()
key.percetniles<-left_join(percentiles,combos)  %>% mutate(tt.y.round=ceiling(2*travel.time*travel.time.multiplier)/2)

#get differences between new era and old era
kp.diff<-left_join(
key.percetniles %>% filter(era=="old") %>% select(inter.exam.corr,percentile,tt.old=travel.time),
key.percetniles %>% filter(era=="new") %>% select(inter.exam.corr,percentile,tt.new=travel.time)
) %>% mutate(diff=(tt.new-tt.old)/base.hours*travel.time.multiplier)

#plot differences
ggplot(kp.diff %>% filter(inter.exam.corr %in% c(0.25,0.5,0.75)),aes(x=percentile,y=diff*base.hours)) +
  geom_area(fill="steelblue",alpha=0.5) +
  scale_x_continuous(name="Percentile of Student Travel Time",limits=c(0,90)) +
  scale_y_continuous(name="Extra Years of Exams in New era",breaks=seq(0,2,by=0.5)) +
  facet_wrap(~str_c("Correlation = ",scales::percent(inter.exam.corr)),nrow=1) +
  theme(legend.position = "none",
        strip.background = element_rect(fill="lightgrey",colour="black",linewidth = 0.25)) +
  theme_bw()
ggsave("Increased Study Times - Years.jpg",height=3,width=7)










