'''
Subsetting and calculating number of individuals in data with AAA, BBB, CCC name 
'''

#count of how many individuals are named AAA
name.count.AAA<-length(data$name[data$name=='AAA'])

#count of how many individuals are named BBB
name.count.BBB<-length(data$name[data$name=='BBB'])

#count of how many individuals are named CCC
name.count.CCC<-length(data$first_name[data$name=='CCC'])

#subset all individuals named john
name.AAA<-subset(data, data$name =='AAA')
name.BBB<-subset(data, data$name =='BBB')
name.CCC<-subset(data, data$ame =='CCC')

#check for duplicates of AAA
duplicated(name.AAA)

#check for duplicates of BBB
duplicated(name.BBB)

#check for duplicates of CCC
duplicated(name.CCC)

#number of individuals named AAA
cat("Number of individuals named AAA in data: ", name.count.AAA)
cat("Be aware of alternative names for AAA")
cat("Number of individuals named BBB in data: ", name.count.BBB)
cat("Number of individuals named CCC in data: ", name.count.CCC)
