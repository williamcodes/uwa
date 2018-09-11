na_strings = c('NA', 'N/A', 'na', 'n/a')
mydata=read.csv("UA Dive Log 201617 18072018.csv", na.strings = na_strings)

# separate database into dive and ocean safari
dives <- mydata[mydata$Activity...2=='Dive', ]
safaris <- mydata[mydata$Activity...2 %in% c('OS', 'Ocean Safari'),]

# separate test and train data
require(caTools)
require(nnet)
set.seed(101)
sample = sample.split(dives$X..Whale.Sharks...57, SplitRatio = .75)

train = subset(dives, sample == TRUE)
test = subset(dives, sample == FALSE)


# declare x variables
# x variables are Year, Month, Dive.Site (for dive data),time, duration as an offset (offset =(duration),
# cloud cover, sst, bst (for dive data), visibility, current direction, current strength, wind direction
# wind strength, swell
# need to force R to recognise certain variables as categoric variable, use function as.categoric()
# includes: year, month, cloud, current dir, current str, win dir, wind str, and swell

# Quantitative
train$duration = train$Duration...9
train$sst = train$SST...12
train$bst = train$BST..Dive.Only....13
train$visibility = train$Visibility...14

# Categoric
train$year = as.factor(train$Year...3)
train$month = as.factor(train$Month...4)
train$dive_site = train$Dive.Site...6
train$time = as.factor(substring(train$Time...8, 1, 2))
train$cloud_cover = as.factor(train$Cloud.Cover...11)
train$current_direction = train$Current.Direction...15
train$current_strength = train$Current.Strength...16
train$wind_direction = train$Wind.Direction...17
train$wind_strength = train$Wind.Strength...18
train$swell = as.factor(train$Swell...19)


# Quantitative
test$duration = test$Duration...9
test$sst = test$SST...12
test$bst = test$BST..Dive.Only....13
test$visibility = test$Visibility...14

# Categoric
test$year = as.factor(test$Year...3)
test$month = as.factor(test$Month...4)
test$dive_site = test$Dive.Site...6
test$time = as.factor(substring(test$Time...8, 1, 2))
test$cloud_cover = as.factor(test$Cloud.Cover...11)
test$current_direction = test$Current.Direction...15
test$current_strength = test$Current.Strength...16
test$wind_direction = test$Wind.Direction...17
test$wind_strength = test$Wind.Strength...18
test$swell = as.factor(test$Swell...19)

# declare y variables
train$whale_shark_sightings = as.factor(train$X..Whale.Sharks...57)
test$whale_shark_sightings = as.factor(test$X..Whale.Sharks...57)

# creating the model
fit1 <- multinom(whale_shark_sightings ~ 
              sst 
              + visibility 
              + year
              + month
              + time
              + cloud_cover
              + current_direction
              + current_strength
              + wind_direction
              + wind_strength
              + swell
              , data = train
            )

pred = predict(fit1, newdata = test)

test$pred <- pred
summary(fit1)
df <- test[, c("pred", "whale_shark_sightings")]
table(df$pred, df$whale_shark_sightings)

#summary(fit1)
#plot(fit1)
#plot(wind_direction, whale_shark_sightings, xlab = 'current direction', ylab = 'whale shark count')