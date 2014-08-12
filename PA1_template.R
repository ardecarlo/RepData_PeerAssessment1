PA1_template <- function(){
  ## loading and preprocessing the data
  # 1. Load data
  dt <- read.csv("activity.csv",colClasses="character")
  # 2. Convert the data to the proper formats
  dt$steps <- as.numeric(dt$steps)
  dt$date <- as.Date(dt$date,"%Y-%m-%d")
  dt$interval <- as.numeric(dt$interval)
  
  ## What is mean total number of steps taken per day?
  # get date vector
  dates <- unique (dt$date)
  # get total number of steps for all days
  totalStepsPerDay <- as.numeric(lapply(split(dt$steps,dates), 
                                  function (x) sum (x[!is.na(x)])))
  # histogram of steps per day
  hist (totalStepsPerDay, xlab=sprintf("Number of Steps (mean=%f, std=%f)",
                                 mean(totalStepsPerDay),sd(totalStepsPerDay)), 
        ylab="Frequency", col = "blue", main="Number of Steps Per Day")
  # report mean and standard deviation of steps per day
  #  (see above)
  
  ## What is the average daily activity pattern?
  # get 5-minute intervals
  intervals <- unique (dt$interval)
  # get mean steps per interval
  meanStepsPerInterval <- as.numeric(lapply(split(dt$steps,intervals), 
                                            function(x) mean(x[!is.na(x)])))
  # plot mean steps per interval as a line plot
  plot(meanStepsPerInterval, type="l", xlab="Interval", ylab="Mean Steps per Interval")
  
  
  ## Imputing missing values
  # (((still clunky!)))
  adjSteps <- dt$steps
  NaIndices <-(1:length(dt$steps))[is.na(dt$steps)]
  NaIntervals <- dt$interval[is.na(dt$steps)]
  for (i in 1:length(NaIntervals)){
       adjSteps[NaIndices[i]] <- meanStepsPerInterval[which (intervals == NaIntervals[i])]
  }
  dt2=data.frame(dt$date, adjSteps, dt$interval)
  
  
  totalStepsPerDay2 <- as.numeric(lapply(split(adjSteps,dates), 
                                        function (x) sum (x[!is.na(x)])))
  hist (totalStepsPerDay2, xlab=sprintf("Number of Steps (mean=%f, std=%f)",
                                       mean(totalStepsPerDay),sd(totalStepsPerDay)), 
        ylab="Frequency", col = "blue", main="Number of Steps Per Day")
  meanStepsPerInterval2 <- as.numeric(lapply(split(adjSteps,intervals), 
                                            function(x) mean(x[!is.na(x)])))
  plot(meanStepsPerInterval2, type="l", xlab="Interval", ylab="Mean Steps per Interval")
  
  ## Are there differences in activity patterns between weekdays and weekends?
  weekdayIndex <- (wday(dt$date) > 1) & (wday(dt$date) < 7)
  dtWeekend <- dt[!weekdayIndex,]
  dtWeekday <- dt[weekdayIndex,]
  meanStepsPerIntervalWeekday <- as.numeric(lapply(split(dtWeekday$steps,intervals), 
                                                   function(x) mean(x[!is.na(x)])))
  meanStepsPerIntervalWeekend <- as.numeric(lapply(split(dtWeekend$steps,intervals), 
                                                   function(x) mean(x[!is.na(x)])))
  par(mfrow=c(2,1))
  plot(meanStepsPerIntervalWeekday,type="l", 
       ylab="Steps", xlab="Interval", main="Weekday")
  plot(meanStepsPerIntervalWeekend,type="l", ylab="Steps", xlab="Interval", 
       main="Weekend")
}