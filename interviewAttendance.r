#Who shows up for interviews? @Author: Sabirah Shuaybi attach​(Int)

#Data Cleaning/Filtering and Renaming Variables
Int$​Call​[Int$Can.I.Call.you.three.hours.before.the.interview.and.follow.up.on.your.attendance.for.the.i nterview==​"No"​|Int$Can.I.Call.you.three.hours.before.the.interview.and.follow.up.on.your.attendanc e.for.the.interview==​"Na"​|Int$Can.I.Call.you.three.hours.before.the.interview.and.follow.up.on.your.a ttendance.for.the.interview==​"No Dont"​]<-​0 Int$​Call​[Int$Can.I.Call.you.three.hours.before.the.interview.and.follow.up.on.your.attendance.for.the.i nterview==​"yes"​|Int$Can.I.Call.you.three.hours.before.the.interview.and.follow.up.on.your.attendanc e.for.the.interview==​"Yes"​]<-​1
Int$​Showed​[Int$Observed.Attendance==​"no"​|Int$Observed.Attendance==​"No"​|Int$Observed.Attenda nce==​"NO"​]<-​0
Int$​Showed​[Int$Observed.Attendance==​"yes"​|Int$Observed.Attendance==​"Yes"​]<-​1
Int$​IntType​[Int$Interview.Type==​"Scheduled"​]<-​"Sch"
Int$​IntType​[Int$Interview.Type==​"Scheduled Walkin"​|Interview.Type==​"Scheduled Walk In"​]<-​"SchW" Int$​IntType​[Int$Interview.Type==​"Walkin"​]<-​"W"
Int$​Clear​[Int$Are.you.clear.with.the.venue.details.and.the.landmark.==​"No"​|Int$Are.you.clear.with.th e.venue.details.and.the.landmark. == ​"No- I need to check"​]<-​0 Int$​Clear​[Int$Are.you.clear.with.the.venue.details.and.the.landmark.==​"Yes"​]<-​1
 Int$​Printout​[Int$Have.you.taken.a.printout.of.your.updated.resume..Have.you.read.the.JD.and.unders tood.the.same==​"Yes"​]<-​2 Int$​Printout​[Int$Have.you.taken.a.printout.of.your.updated.resume..Have.you.read.the.JD.and.unders tood.the.same==​"No- will take it soon"​|Int$Have.you.taken.a.printout.of.your.updated.resume..Have.you.read.the.JD.and.understood.t he.same==​"No"​|Int$Have.you.taken.a.printout.of.your.updated.resume..Have.you.read.the.JD.and.un derstood.the.same==​"Not yet"​]<-​1 Int$​Printout​[Int$Have.you.taken.a.printout.of.your.updated.resume..Have.you.read.the.JD.and.unders tood.the.same==​"No"​]<-​0
#Creating a new variable called IntClose (comparison of candidate loc vs. interview loc)
for​(i ​in​ ​1:1234​) {
​if​((as.character(Int$Candidate.Current.Location[i])) == (as.character(Int$Interview.Venue[i]))) {
Int$​IntClose​[i]=​1 }
​else​ {
Int$​IntClose​[i]=​0 }
}
#Creating a new variable called JobClose (comparison of candidate loc vs. job loc)
for​(j​ ​in​ 1:1234​) {
​ if​((as.character(Int$Candidate.Current.Location[j])) == (as.character(Int$Candidate.Job.Location[j]))) {
Int$​JobClose​[j]=​1 }
​else​ {
Int$​JobClose​[j]=​0 }
}
#Exploratory Analysis of Dataset
prop.table(table(Int$Observed.Attendance, Int$Gender)) prop.table(table(Int$, Int$Gender)) prop.table(table(Int$`Gender`, Int$`Showed`), 1)
#Multiple Linear Regression Model (Not used as part of Analysis due to Violation of Conditions)
mlrMod <-lm(Showed~Gender+Industry+Marital.Status+IntType+IntClose+JobClose+Clear+Printout+Call, data=Int)
summary(mlrMod)
plot(mlrMod)

#Logistic Regression
modLog<-glm(Showed~Gender+Industry+Marital.Status+IntType+IntClose+JobClose+Clear+Printout+C all, family=binomial, data=Int)
summary(modLog)
#Best Subsets
library​(leaps) mymodels=regsubsets(Showed~Gender+Industry+Marital.Status+IntType+JobClose+IntClose+Clear+Pri ntout+Call, data=Int)
summary(mymodels)
summary(mymodels)$rsq
summary(mymodels)$adjr2
#Building a better model from the best subsets procedure (5-predictor model)
modbest<-glm(Showed~Clear+Printout+JobClose+IntClose+Call, family=binomial, data=Int) summary(modbest)
#Obtaining Predicted Values
logpred<-predict(modbest) mean(logpred[Int$Clear==1], na.rm=​TRUE​)
newdata = data.frame(Clear=​1​, Printout=​2​, JobClose=​0​, IntClose=​1​, Call=​1​) predict(modbest, newdata, type="response") ​#Probability = 0.903
newdata2 = data.frame(Clear=​0​, Printout=​0​, JobClose=​1​, IntClose=​0​, Call=​0​) predict(modbest, newdata2, type="response") ​#Probability = 0.001
