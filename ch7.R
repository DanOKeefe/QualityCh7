# Question 1

Data1 <- read.csv(file="Q1data.csv", header=F, sep=",")

# Data contains number of defective bearings in each sample. Sample size = 100 bearings
summary(Data1)

library(ggplot2)
library(qcc)

qplot(Data1, bins=6)

# Construct a fraction nonconforming control chart for these data.
# If any points plot out of control, 
# assume that assignable causes can be found and determine the revised control limits.

np_graph1 <- qcc(data = Data1, type = "np", size = 100)

# Notice the number of defects in the 12th sample is above the upper control limit. 15 defects > 12.38 defects

np_graph1$limits

# Remove that row and regenerate a new control chart

Data1 <- as.data.frame(Data1[-c(12),])

np_graph1Edited <- qcc(data = Data1, type = "np", size = 100)

# --------------------------------------------------------------------------

# Question 2

# Based on the data in Table 7E.6, what would you recommend as the center line and the control limits? Assume n = 500

Data2 <- read.csv(file = 'Q2data.csv', header = T, sep = ',')
summary(Data2)
np_graph2 <- qcc(data = Data2, type = "np", size = 500)

# Notice there are 12 defects in sample 6, so remove that sample from the data you use.

Data2 <- as.data.frame(Data2[-c(6),])

np_graph2Edited <- qcc(data = Data2, type = "np", size = 500)

# All points are withing the control limits.

np_graph2Edited$limits   # LCL = 0.000, UCL = 8.386
np_graph2Edited$center   # center line = 3.111

# --------------------------------------------------------------------------

# Question 3

# A maintenance group improves the effectiveness of its repair work by 
# monitoring the number of maintenance requests that require a second call to complete the repair. 
# Twenty weeks of data are shown in Table 7E.8.
# Create a control chart for controlling future production.

Data3 <- read.csv(file = 'Q3data.csv', header = T, sep = ',')
summary(Data3)

# Notice the number of data points in each sample varies greatly

SecondVisits <- qcc(Data3$SecondVisitRequired, type = "np", size = Data3$TotalRequests)

# Theres a few samples where the number of second visits required are below the LCL. (Samples 11 through 14)

# --------------------------------------------------------------------------

# Question 4

# Use the data from the previous problems ot create a standardized control chart for the data

install.packages('qicharts')
library(qicharts)

qic(data = Data3,
    n = TotalRequests,
    y = SecondVisitRequired,
    chart = 'p',
    standardised = T)

# -------------------------------------------------------------------------

# Question 5

# The data in Table 7E.19 are the number of information errors found in customer records in a marketing company database
# Five records were sampled each day.
# Set up a c chart for the total number of errors. Is the process in control?

Data5 <- read.csv(file = 'Q5data.csv', header = TRUE)

ErrorSum <- as.data.frame(rowSums(Data5))

qcc(ErrorSum, type = 'c')

# The process is not in control. The samples from day 24 contain an amount of errors above the upper control limit
