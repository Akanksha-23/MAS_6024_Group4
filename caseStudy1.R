## @knitr CreateData

library(tidyverse)


pay17 <- read_csv("data/UK Gender Pay Gap Data - 2017 to 2018.csv")
pay18 <- read_csv("data/UK Gender Pay Gap Data - 2018 to 2019.csv")
pay19 <- read_csv("data/UK Gender Pay Gap Data - 2019 to 2020.csv")
pay20 <- read_csv("data/UK Gender Pay Gap Data - 2020 to 2021.csv")
pay21 <- read_csv("data/UK Gender Pay Gap Data - 2021 to 2022.csv")
universities <- read_csv("data/universities.csv")

##### Adding EmployerId column to the universities data
universitiesId <- pay17 %>% inner_join(universities, by = "EmployerName") %>%
                  select(EmployerId, EmployerName, institution)

##### Extracting universities data for each year based strictly on employerId
uniPayGap17 <- pay17 %>% 
              mutate( year = "2017/18"  ) %>%
              select(-EmployerName) %>%
              inner_join(universitiesId, by = "EmployerId")

uniPayGap18 <- pay18 %>% 
              mutate( year = "2018/19"  ) %>%
              select(-EmployerName) %>%
              inner_join(universitiesId, by = "EmployerId")

uniPayGap19 <- pay19 %>% 
              mutate( year = "2019/20"  ) %>%
              select(-EmployerName) %>%
              inner_join(universitiesId, by = "EmployerId")

uniPayGap20 <- pay20 %>% 
              mutate( year = "2020/21"  ) %>%
              select(-EmployerName) %>%
              inner_join(universitiesId, by = "EmployerId")

payGapData <- rbind(uniPayGap17, uniPayGap18, uniPayGap19, uniPayGap20)


#### Count of records each year and how many universities record each year
payGapData %>% count(year)

payGapData %>%
  select(EmployerName, year, DiffMedianHourlyPercent) %>%
  pivot_wider(names_from = year, 
              values_from = DiffMedianHourlyPercent) %>%
  na.omit()


#### Plot difference between percent of male and female paid bonus 
bonusData <- payGapData %>% 
            select(`EmployerName`, `MaleBonusPercent`, `FemaleBonusPercent`, `year`) %>% 
            arrange(`EmployerName`)

uniBonusData <- bonusData %>% 
                group_by(EmployerName) %>%
                summarise(Male = mean(MaleBonusPercent),
                          Female = mean(FemaleBonusPercent)) %>%
                pivot_longer(cols = -`EmployerName`,
                             names_to = 'Gender',
                             values_to = 'BonusPercent')

uniBonusData %>% ggplot(aes(x = `BonusPercent`, y = `EmployerName`)) + geom_point(aes(color = `Gender`))


#### Plot to show difference between males in upper quartile and female in upper quartile

payGapData %>% ggplot(aes(x = year, y = MaleTopQuartile)) + geom_point(aes(color = `institution`))
payGapData %>% ggplot(aes(x = year, y = FemaleTopQuartile)) + geom_point(aes(color = `institution`))

#### Plot to show difference between pre and post universities 

uniCategoryDataMedian <- payGapData %>%
                   select(EmployerId, EmployerName, institution, year, DiffMedianHourlyPercent) %>%
                   group_by(year, institution) %>%
                   summarise(meanDiffHourlyPay = mean(`DiffMedianHourlyPercent`))

uniCategoryDataMedian %>% ggplot(aes(x = year, y = meanDiffHourlyPay)) + 
                    geom_bar(stat = 'identity', aes(fill = institution), position = position_dodge(0.9)) +
                    scale_fill_discrete(limits = c("pre-92", "post-92"))

uniCategoryDataMean <- payGapData %>%
  select(EmployerId, EmployerName, institution, year, DiffMeanHourlyPercent) %>%
  group_by(year, institution) %>%
  summarise(meanDiffHourlyPay = mean(`DiffMeanHourlyPercent`))

uniCategoryDataMean %>% ggplot(aes(x = year, y = meanDiffHourlyPay)) + 
  geom_bar(stat = 'identity', aes(fill = institution), position = position_dodge(0.9)) +
  scale_fill_discrete(limits = c("pre-92", "post-92"))

payGapDataFrame <- payGapData %>% select(6:23) %>% 
                    as.data.frame()


bonusData <- payGapData %>% 
  select(`EmployerId`, `EmployerName`, `MaleBonusPercent`, `FemaleBonusPercent`, `year`) %>% 
  arrange(`EmployerId`)

uniBonusDataMale <- bonusData %>% 
  group_by(EmployerId, EmployerName) %>%
  summarise(Male = mean(MaleBonusPercent))

uniBonusDataFemale <- bonusData %>% 
  group_by(EmployerId, EmployerName) %>%
  summarise(Female = mean(FemaleBonusPercent)) 

ggplot(uniBonusDataMale, mapping = aes(y = `Male`, x = `EmployerId`)) + 
  geom_line(uniBonusDataMale, mapping = aes(y = `Male`, x = `EmployerId`, color = "Male")) +
  geom_point(uniBonusDataMale, mapping = aes(y = `Male`, x = `EmployerId`, color = "Male")) + 
  geom_line(uniBonusDataFemale, mapping = aes(y = `Female`, x = `EmployerId`, color = "Female")) +
  geom_point(uniBonusDataFemale, mapping = aes(y = `Female`, x = `EmployerId`, color = "Female"))+
  scale_color_manual(aesthetics =  c("Male"="blue", "Female"="red")) +
  labs(title = "Average Percent of employees getting bonus in different universities", 
        x = "EmployerId",
        y = "Percent of employees receiving bonus")
  
