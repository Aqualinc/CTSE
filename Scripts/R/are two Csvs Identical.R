#Jonathan Dixon

#rm(list=ls()) # clear memory

newData <- read.csv("Q:\\Temp\\Jono\\June update\\extended\\3470 extended.csv")


originalData <- read.csv("Q:\\Temp\\Jono\\June update\\extended interrupted\\3470 extended.csv")



isIdentical <- identical(newData,originalData)

print(isIdentical)


#which(!originalData[1] == newData[1])

