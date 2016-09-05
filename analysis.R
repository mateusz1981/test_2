df <- read.csv("school_fire_cases_1998_2014.csv", header = T, sep = ";", encoding="UTF-8")
library(dplyr)

df %>% group_by(Municipality) %>% summarise(li = sum(Cases), ratio = (sum(Cases)/mean(Population)) * 100) %>% 
  ungroup() %>% mutate(sr_r = mean(ratio), diff = ratio-sr_r) %>% top_n(10, diff) %>%
  ggplot(aes(x = Municipality, y = diff)) + geom_point() + coord_flip()

df %>% group_by(Municipality) %>% summarise(sum_cases = sum(Cases), ratio = (sum(Cases)/mean(Population)) * 100) %>% 
  ungroup() %>% mutate(mean_r = mean(ratio), diff = ratio-mean_r) %>% top_n(10, diff) %>%
  ggplot(aes(x = Municipality, y = diff)) + geom_bar(stat = "identity", fill = "maroon") + coord_flip()


df %>% group_by(Municipality) %>% summarise(sum_cases = sum(Cases), ratio = (sum(Cases)/mean(Population)) * 100) %>% 
  ungroup() %>% mutate(mean_r = mean(ratio), diff = ratio-mean_r) %>% top_n(10, ratio) %>%
  ggplot(aes(x = Municipality, y = ratio)) + geom_bar(stat = "identity", fill = "maroon") + coord_flip()




char <- read.csv("simplified_municipality_indicators.csv", header = T, sep = ";", encoding="UTF-8")

char <- char %>% rename(Municipality = name)

df <- left_join(df, char, by = "Municipality")
head(df)

df1 <- df[complete.cases(df), ]

df %>% group_by(Municipality) %>% summarise(li = sum(Cases), inc = mean(youthUnemployment2010), ratio = (sum(Cases)/mean(Population)) * 100) %>% 
  ungroup() %>% mutate(sr_r = mean(ratio), diff = ratio-sr_r) %>% top_n(10, diff)

df %>% group_by(Municipality) %>% summarise(sr = mean(unemployment2010)) %>% top_n(10, sr)
head(df)


ggplot(aes(x = Year, y = Cases), data = df) + stat_summary(fun.y = "mean", geom = "point")

df %>% group_by(Municipality) %>% summarise(ref = mean(refugees), cases = sum(Cases)) %>%
ggplot(aes(x = ref, y = cases)) + geom_point()

df %>% filter(Population < 200000) %>% group_by(Municipality) %>% 
  summarise(inc = mean(medianIncome), cases = sum(Cases), pop = mean(Population), sr_ref = mean(refugees)) %>%
  ggplot(aes(x = inc, y = cases)) + geom_point(aes(size = sr_ref)) + 
  geom_smooth(method = "lm")

df %>% group_by(Municipality) %>% summarise(sr_pop = mean(Population)) %>%
  ggplot(aes(x = sr_pop)) + geom_histogram()

df %>% filter(Population < 200000) %>% group_by(Municipality) %>% 
  summarise(inc = mean(medianIncome), cases = sum(Cases), pop = mean(Population), sr_ref = mean(refugees)) %>%
  ggplot(aes(x = inc, y = sr_ref)) + geom_point(aes(size = cases)) + 
  geom_smooth(method = "lm")

df %>% mutate(lat = substr(latitude, 1, 2)) %>% ungroup() %>%
  group_by(lat) %>% summarise(cases = sum(Cases), sr_pop = mean(Population)) %>%
  ggplot(aes(x = lat, y = cases/sr_pop)) + geom_bar(stat = "identity")

df %>% group_by(Municipality) %>% summarise(cases = sum(Cases), sr_pop = mean(Population)) %>%
  ggplot(aes(x = lat, y = cases/sr_pop)) + geom_bar(stat = "identity")

df %>% group_by(municipalityTypeBroad) %>% summarise(cases = sum(Cases), sr_pop = mean(Population)) %>%
  ggplot(aes(x = municipalityTypeBroad, y = cases/sr_pop)) + geom_bar(stat = "identity")

df %>% group_by(governing) %>% summarise(cases = sum(Cases), sr_pop = mean(Population)) %>%
  ggplot(aes(x = governing, y = cases/sr_pop)) +  geom_bar(stat = "identity")

df %>% filter(Population < 200000 & refugees < 100) %>% group_by(Municipality, municipalityType) %>% 
  summarise(inc = mean(medianIncome), cases = sum(Cases), pop = mean(Population), sr_ref = mean(refugees)) %>%
  ggplot(aes(x = pop, y = cases/pop)) + geom_point(aes(size = sr_ref, color = municipalityType)) + 
  geom_smooth(method = "lm")

df %>% filter(Population < 200000 & refugees < 100) %>% group_by(Municipality, municipalityType) %>% 
  summarise(inc = mean(medianIncome), cases = sum(Cases), pop = mean(Population), sr_ref = mean(refugees),
            forg_bor = mean(foreignBorn, na.rm = T)) %>%
  ggplot(aes(x = pop, y = cases/pop)) + geom_point(aes(size = forg_bor, color = municipalityType)) + 
  geom_smooth(method = "lm")

head(df)
?cut
df <- df %>% mutate(has_edu_clas = cut(hasEducation, breaks = seq(0, 100, 10)))

df %>% filter(Population < 200000) %>% group_by(Year) %>% summarise(li = sum(Cases)) %>%
  ggplot(aes(x = Year, y = li)) +  geom_bar(stat = "identity", fill = "blue") + geom_line(color = "red", size= 2)

df %>% filter(Population < 200000) %>% group_by(has_edu_clas) %>% 
  summarise(li = sum(Cases), pop = mean(Population)) %>%
  ggplot(aes(x = has_edu_clas, y = li/pop)) +  geom_bar(stat = "identity", fill = "blue") + geom_line(color = "red", size= 2)

df %>% mutate(fokrank_clas = cut(fokusRanking, breaks = seq(0, 320, 40))) %>% 
  filter(Population < 200000) %>% group_by(fokrank_clas) %>% 
  summarise(li = sum(Cases), pop = mean(Population)) %>%
  ggplot(aes(x = fokrank_clas, y = li)) +  geom_bar(stat = "identity", fill = "blue") 

df %>% mutate(fokrank_clas = cut(fokusRanking, breaks = seq(0, 320, 40))) %>% 
  filter(Population < 200000) %>% group_by(fokrank_clas) %>% 
  summarise(li = sum(Cases), pop = sum(Population)) %>%
  ggplot(aes(x = fokrank_clas, y = pop)) +  geom_bar(stat = "identity", fill = "blue") 

library(randomForest)
head(df)

df1 <- df %>% mutate(Cases = Cases + 0.001) %>% filter(Cases > 0) %>%
  select(c(Cases, Population, governing, latitude))
rf <- randomForest(Cases ~ ., data = df1, ntree=500, importance=TRUE)
barplot(rf$importance[,2],las=2, main="Random forest feature importance (node purity)")
barplot(rf$importance[,1],las=2, main="Random forest feature importance (inc MSE)")

library(corrr)
head(df1)
#select numeric columns
nums <- sapply(df, is.numeric)
df1 <- df[, nums]
M <- df1 %>% select(-population) %>% correlate() %>% focus(Cases) %>% filter(Cases > 0.2 | Cases < -0.2)
M

library(corrplot)
M1 <- cor(df1[, c("Cases", unique(M$rowname))])
corrplot(M1, method = "circle")

M <- df1 %>% select(-population) %>% correlate() %>% focus(Cases) %>% filter(Cases > 0.2 | Cases < -0.2)
pairs(df1[, c(unique(M$rowname), "Cases")])






