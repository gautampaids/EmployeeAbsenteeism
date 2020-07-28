#Load Libraries
x = c("readxl","ggplot2", "DMwR", "caret")

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

#Load the data
raw_xls_data = read.csv("Employee Absenteeism Imputation.csv")
str(raw_xls_data)
glimpse(raw_xls_data)

#~~~~~~~~~~~~~~~~~~~~~~~~~Missing value Analysis~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
for (i in names(raw_xls_data)) {
    print(paste("The number of NAs in ", i, "is", sum(is.na(raw_xls_data[[i]]))))
}

df <- raw_xls_data
names(df) <- sapply(names(df), function(x) { gsub(" ", "_", x) })
names(df)
names(df)[10] = "Work_load_Average_per_day"

#~~~~~~~~~~~~~~~~~~~~~~~~~Chsq Test Analysis~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
print_chisq_statistics <- function(df_test, target){
    for (i in names(df_test)) {
        print(paste("Chisq test for Feature", i))
        print(chisq.test(target, df_test[[i]], correct = FALSE))
    }
}

print_chisq_statistics(df, df$Absenteeism.time.in.hours)

#The following variables are the ones which are having very significant p-values
table(df$Reason.for.absence)
table(df$Distance.from.Residence.to.Work)
table(df$Transportation.expense)
table(df$Disciplinary.failure)

bar1 = ggplot(data = df, aes(x = Reason.for.absence)) + geom_bar() + ggtitle("Frequency plot of Reason for absence") + theme_bw()
bar2 = ggplot(data = df, aes(x = Distance.from.Residence.to.Work)) + geom_bar() + ggtitle("Frequency plot of Distance from REsidence to Work") + theme_bw()
bar3 = ggplot(data = df, aes(x = Transportation.expense)) + geom_bar() + ggtitle("Frequency plot of Transportation expense") + theme_bw()
bar4 = ggplot(data = df, aes(x = Disciplinary.failure)) + geom_bar() + ggtitle("Frequency plot of Disciplinary failure") + theme_bw()

gridExtra::grid.arrange(bar1, bar2, bar3, bar4, ncol = 2)

nrow(df[(df$Distance.from.Residence.to.Work %in% c(26, 51) | df$Transportation.expense == 179 | df$Reason.for.absence %in% c(23, 27, 28)),])
#440

df2 = df[(df$Distance.from.Residence.to.Work %in% c(26, 51) | df$Transportation.expense == 179 | df$Reason.for.absence %in% c(23, 27, 28)),]

df2 = subset(df2, select = -c(Reason.for.absence, Disciplinary.failure, Transportation.expense, Distance.from.Residence.to.Work, Education, Social.smoker))

print_chisq_statistics(df2, df2$Absenteeism.time.in.hours)

table(df2$Month.of.absence)
table(df2$Son)

bar1 = ggplot(data = df2, aes(x = Month.of.absence)) + geom_bar() + ggtitle("Frequency plot of Month of absence") + theme_bw()
bar2 = ggplot(data = df2, aes(x = Son)) + geom_bar() + ggtitle("Frequency plot of Son") + theme_bw()

gridExtra::grid.arrange(bar1, bar2, ncol = 2)

quantile(table(df2$Month.of.absence)[2], 0.9) #gives 37

nrow(df2[(df2$Month.of.absence %in% c(1, 2, 3, 10) | df2$Son %in% c(0, 1)),])
#383

df3 <- df2[(df2$Month.of.absence %in% c(1, 2, 3, 10) | df2$Son %in% c(0, 1)),]
df3 = subset(df3, select = -c(Month.of.absence, Son, Pet, Day.of.the.week))


print_chisq_statistics(df3, df3$Absenteeism.time.in.hours)

table(df3$Work_load_Average_per_day)
quantile(table(df3$Work_load_Average_per_day)[2], 0.9) #26

ggplot(data = df3, aes(x = Work_load_Average_per_day)) + geom_bar() + ggtitle("Frequency plot of Work_load_Average_per_day") + theme_bw()

df4 <- df3[(df3$Work_load_Average_per_day %in% c(222196,264249)),]
df4 <- subset(df4, select = -c(Work_load_Average_per_day, Body.mass.index, Height, Social.drinker))

print_chisq_statistics(df4, df4$Absenteeism.time.in.hours)

#Summary
#1) Distance.from.Residence.to.Work in (26,51) & Transportation.expense = 179 & Reason_for_absence in (23,27,28)
#2) More number of absentees in the month (1,2,3,10) with no Sons or having 1 Son
#3) Work_load_Average_per_day in (222196, 264249)

result_table <- aggregate(df$Absenteeism.time.in.hours, by = list(df$Month.of.absence, df$Work_load_Average_per_day, df$Hit.target), FUN = sum)

ggplot(data = result_table) +
    geom_line(aes(Group.1, x)) 
