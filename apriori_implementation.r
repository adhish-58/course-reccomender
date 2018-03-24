#install.packages("arules")
#install.packages("arulesViz")
library("arules")
library("arulesViz")

#Read all the data for Fall and Spring courses that students have taken (as training data)
fall_courses <- read.transactions(file = "Student_fall.csv", format = "basket", sep = ",")
spring_courses <- read.transactions(file="Student_spring.csv", format = "basket", sep = ",")

#Read the list of all courses and concentrations offered at Gtech
all_courses <- read.csv(file = "courses_list.csv", header = TRUE)
courses.ece.concentrations <- read.csv(file = "ece_courses_by_tig.csv", header = TRUE)
courses.cs.concentrations <- read.csv(file = "cs_courses_by_tig.csv", header = TRUE)
all_concentrations <- read.csv(file = "tig_list.csv", header = TRUE)

#Create lists of all courses and concentrations
courses.list <- as.list(all_courses)
courses.ece.list <- as.list(courses.ece.concentrations)
courses.cs.list <- as.list(courses.cs.concentrations)
concentrations.list <- as.list(all_concentrations)

#Create Association rules from the training data
rules.fall_courses <- apriori(fall_courses, parameter = list(support = 0.1, confidence = 0.5))
rules.spring_courses <- apriori(spring_courses, parameter = list(support = 0.1, confidence = 0.5))

#Sort and trim the rules for better association
rules.fall_courses.sorted <- sort(rules.fall_courses, by = "confidence")
rules.spring_courses.sorted <- sort(rules.spring_courses, by = "confidence")
rules.fall_courses.trimmed <-rules.fall_courses.sorted[!is.redundant(rules.fall.sorted)]
rules.spring_courses.trimmed <-rules.spring_courses.sorted[!is.redundant(rules.fall.sorted)]

#Create data frames from the best rules
rules1.fall_courses = data.frame(lhs = labels(lhs(rules.fall_courses)), rhs = labels(rhs(rules.fall_courses)))
rules1.spring_courses = data.frame(lhs = labels(lhs(rules.spring_courses)), rhs = labels(rhs(rules.spring_courses)))

#For testing purposes:
  #rules.fall_courses
  #rules.spring_courses
  #summary(rules.fall_courses)
  #summary(rules.spring_courses)
  #inspect(rules.fall_courses)
  #inspect(rules.spring_courses)
  #plot(rules.fall_courses)
  #plot(rules.spring_courses)