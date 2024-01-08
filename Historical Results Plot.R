library(tidyverse)
library(tibbletime)
library(RcppRoll)

# locations
data.folder <- "./Data"
output.folder <- "./Output"
hist.res.file <- "Raw.Historical.Exam.Results.csv"
study.hours.file <- "Exam.study.hours.csv"

# control inputs
roll.period <- 5 # must be an odd number
exams.to.show <- list(c("CB1", "CB2", "CP1"), # determines the grouping of exams for plot(s)
                      c("CM1", "CM2"),
                      c("CS1", "CS2"))

# get historical exam results
exam.results <- read.csv(file.path(data.folder, hist.res.file))

# get study hours weights
study.hours <- read.csv(file.path(data.folder, study.hours.file))

# exam map
exam.map <- tribble(
  ~"old", ~"new",
  "CT1", "CM1",
  "CT2", "CB1",
  "CT3", "CS1",
  "CT4", "CS2",
  "CT5", "CM1",
  "CT6", "CS2",
  "CT7", "CB2",
  "CT8", "CM2",
  "CA1", "CP1"
)

# process pass rates dataframe
er <- exam.results %>%
  gather(key = "Exam", "Pass.Rate", -Date) %>%
  left_join(exam.map, by = c("Exam" = "old")) %>%
  rename("New.Exam" = "new") %>%
  mutate(Type = ifelse(Exam %in% exam.map$old, "Old", "New")) %>%
  filter(!is.na(Pass.Rate))
er$New.Exam[is.na(er$New.Exam)] <- er$Exam[is.na(er$New.Exam)]
er <- er %>%
  group_by(Date, New.Exam, Type) %>%
  summarise(Pass.Rate = mean(Pass.Rate)) %>%
  ungroup()
er$Date <- er$Date %>% parse_date(format = "%d/%m/%Y")


# construct the rolling averages for each exam
# code gives the median and is rolling so that median value is attached to middle data of the date subset
rolling.exams <- er %>%
  group_by(New.Exam, Type, Date) %>%
  summarise(Pass.Rate = mean(Pass.Rate)) %>%
  arrange(Date) %>%
  split(f = .$New.Exam)

exams <- names(rolling.exams)
end.trim <- floor(roll.period / 2)
roll.avg <- map(.x = rolling.exams, .f = ~ roll_median(.$Pass.Rate, roll.period))
roll.dates <- map(.x = rolling.exams, .f = ~ .$Date[(roll.period - end.trim):(length(.$Date) - end.trim)])
roll.avg.l <- map(exams, .f = function(x) data.frame(Date = roll.dates[[x]], roll.pass.rate = roll.avg[[x]]))
names(roll.avg.l) <- exams
roll.avg.t <- bind_rows(roll.avg.l, .id = "New.Exam")

er2 <- er %>% left_join(roll.avg.t)


# Plot 1: Historical results with rolling median

for (i in seq_along(exams.to.show)) {
  ggplot(data = er2 %>% filter(New.Exam %in% exams.to.show[[i]]), aes(x = Date, group = New.Exam)) +
    geom_point(mapping = aes(y = Pass.Rate, colour = Type), size = 2) +
    geom_line(mapping = aes(y = roll.pass.rate), colour = "black") +
    scale_color_manual(name = "Exam Era", values = c("New" = "Wheat4", "Old" = "Purple")) +
    scale_y_continuous(name = "Pass Rate", lim = c(0.2, 0.8), breaks = seq(0.2, 0.8, by = 0.1), labels = scales::percent_format()) +
    scale_x_date(name = "") +
    facet_wrap(~New.Exam) +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = "lightgrey"),
      panel.grid.minor.y = element_blank()
    )

  ggsave(str_c(output.folder,"/",str_c(exams.to.show[[i]], collapse = "."), ".jpg"), width = 7 * length(exams.to.show[[i]]), height = 8, units = "cm")
}


# Plot 2: Old vs New Curriculum Pass Rates Comparison

er.mean <- er %>%
  group_by(New.Exam, Type) %>%
  summarise(Mean = mean(Pass.Rate, na.rm = T)) %>%
  ungroup()
o.vs.n <- er.mean %>%
  spread(key = "Type", value = "Mean") %>%
  rename("Exam" = "New.Exam") %>%
  left_join(study.hours)
ggplot(o.vs.n, aes(x = Old, y = New)) +
  geom_abline(slope = 1, size = 0.85) +
  geom_point(aes(size = Hours), shape = 21, alpha = 0.75, colour = "blue", fill = "sky blue", stroke = 0.85) +
  geom_text(aes(label = Exam), size = 2.5, colour = "blue", alpha = 0.75) +
  scale_x_continuous(name = "Old Pass Rate", limits = c(0.35, 0.7), labels = scales::percent_format()) +
  scale_y_continuous(name = "New Pass Rate", limits = c(0.35, 0.7), labels = scales::percent_format()) +
  scale_size_area(max_size = 15) +
  theme_bw() +
  theme(legend.position = "none")
ggsave(str_c(output.folder,"/","Old.vs.New.jpg"), height = 12, width = 14, units = "cm")
