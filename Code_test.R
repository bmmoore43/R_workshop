# Open R-studio, familiarize with environment

# ONLY RUN ONCE
# download install packages to be used and restart if necessary
install.packages('dplyr')
install.packages("tidyverse")
install.packages("ggplot2")

# If asked to update packages select "all" (may take a few minutes)
source("http://bioconductor.org/biocLite.R")
biocLite("edgeR") #  to dowload the package

###################################
############ R Basics #############
###################################

### Assigning variables and doing math
t0 <- 2.5
t1 <- 3.3
fc <- t0/t1
fc
t1 <- 1.8
fc
fc <- t0/t1
fc
log10(fc)

### Making and working with lists
l <- c(1, 2, 5, 5, 1, 7)
mean(l)
median(l)
#subset
l[3]
l[1:3]
l2<-l[1:3]
l2
l2<- l[c(1,3)]
l2
#subset based on conditional
l3 <- l[l>2 & l<7]
l3
#multiply list
l4 <- l*10
l4
### subset l4 to be greater than 20

#functions with lists
class(l)
length(l)
l5<- c("dog","cat","mouse","rabbit")
class(l5)
l6 <- c(8, 15, 2, "4")
class(l6)
#math with list
l4[1]+l4[2]
### explain answer
l6[1]+l6[2]
### why do we get an error here? how can we fix it?
l6<- as.numeric(l6)
l6

### Set a working directory and load in some data
# PC users
setwd("C:/Users/Beth/Desktop/R_Workshop")
# Mac users
setwd('/Users/Beth/Desktop/R_Workshop/')
getwd()

data <- read.table('starch_data.txt', header=TRUE, sep='\t')

# Checking out your data
head(data) # look at the first 5 rows of your data
tail(data) # look at last 5 rows of your data
summary(data) # gives you a summary of each column in the dataset
dim(data) #check out dimensions
### Subset data
# get a specific cell- data[x,y]
data[6,2] #row 6, column 2
data[6,] # get all columns for row 6
data[,1] # get all rows for column 1
data[1:3,] # get rows 1-3 for both columns

# get a column using column name
data$starch # view starch column
data$starch <- as.numeric(data$starch) #make sure starch is all numeric
starch_data <- data$starch # get column as separate vector
starch_data2 <- data["starch"] # get column as a separate dataframe
## add and rename columns

library(dplyr) #call library previously downloaded- run every time you start R
library("tidyverse")
# add a column to starch_data2
#make a vector, NOTE should be same size as number of rows in your data frame (so 36)
starch_data2 <- starch_data2[1:6,] # reduce number of rows in starch_data2
starch_data2 <- as.data.frame(starch_data2) # remake into dataframe
vec <- c(3, 2, 3, 2, 3, 1) # create a new vector
starch_data3 <- cbind(starch_data2, new_col = vec)
starch_data3 # look at new dataframe
# rename columns with dplyr- rename function: rename(newname = oldname)
# %>% is pipe operator in dplyr- means functions will be applied to the dataframe left of the operator
starch_data3 <- starch_data3 %>%
  rename(starch = starch_data2,
         leaf_weight = new_col)
starch_data3
# mutate; add column by calculation from another column
starch_data3 <- starch_data3 %>%                              #the data
  mutate(weight_kg = leaf_weight / 1000)
starch_data3
# what if there are NAs?
vec2 <- c(25,NA,7,2,NA,4)
starch_data3 <- cbind(starch_data3, new_col = vec2)
starch_data3
# What columns have missing values?
# use gather function to convert data variables to 2 columns
starch_gather<-starch_data3%>%
  gather(key = metric, 
         value = value)
starch_gather
# from gathered data how many NAs are there
na_table<-starch_gather%>%
  summarize(nas = sum(is.na(value)))
na_table
# remove NAs
starch_complete <- starch_data3 %>% 
  filter(!is.na(new_col)) # remove missing 
starch_complete
# make a new data frame with only certain genotypes
# Note: & = and, | = or
data_WT_MutD <- subset(data, genotype == 'WT' | genotype == 'MutD') # Subset data by keeping rows where genotype is WT or gi-2.
# how to check out new dataframe?

### Basic plots
#cran install ggplot2

library(ggplot2)
# dot plot 
#ggplot(data, aes(x,y))+geom_point()
ggplot(data, aes(genotype, starch)) +
  geom_point()
#add some color
ggplot(data, aes(genotype, starch,colour = genotype)) +
  geom_point()

###### Basic statistics in R ######
###################################

### T-test/pairwise tests
# Determine if the mean starch values for WT and MutD are significantly different.
# Assumptions: data is normally distributed, variances are equal,data follows continuous scale
t.test(starch ~ genotype, data = data_WT_MutD)
t.test(starch ~ genotype, data = data_WT_MutD, var.equal=TRUE)
# drawbacks: unequal sample size can affect variance, and can affect Type I error (reject the null 
#            hypothesis when you actually cannot- thus deemed significant when actually not)
# Try wilcox rank sum- non-parametric uses rank instead of actual value
wilcox.test(starch ~ genotype, data = data_WT_MutD, alternative = "two.sided")
# doing pairwise tests instead of anova can also increase Type I error

### ANOVA
# Determine if there are significant differences between the genotypes using a One-Way ANOVA 
fit <- aov(starch ~ genotype, data = data)
summary.aov(fit) 
#Df= degrees of freedom (# of variables -1), residuals (# observations - # groups)
#Sum Sq= sum of squares (sum(yi-y)^2)
#Mean Sq= SS/df (sample variance)
#F value= observed(SS/df)/residual(SS/df)
#Pr(>F) - probability of observed F value give H0 is true

##Tukeys HSD
#Determine which genotypes are different
#non-parametric test- can be used if sample sizes are unequal
tuk <- TukeyHSD(fit)
tuk
plot(tuk)

## Linear model- are genotypes the same or different?
# ANOVA and T.test are both special cases of linear models
lin.fit = lm(starch ~ genotype, data = data)
summary(lin.fit)
#means parameterization estimation, pvalue just indicates means are different from 0
# look at residuals
#Residuals are essentially the difference between the actual observed response values he response values that the model predicted
#residuals- look for symmetrical distribution across these points on the mean value zero (0-abline)
#residuals
lin.fit.res = resid(lin.fit)
#plot against observed values
plot(data$starch, lin.fit.res, ylab="Residuals", xlab="Starch", 
     main="Residuals vs. Observed values")
abline(0, 0)
#intercept- sort of the average of the model
#each variable is a slope.. are the slopes significantly different from the average?
#estimate- how different are you from the average (intercept)
#error- standard error (measure of variance- how much to coeffecients vary from the average)
#t-value- how many standard deviations our coefficient estimate is far away from 0- the further away,
##the more likely we reject the null
#Pr(>ltl) probability of observing any value equal or larger than t. (probability observed value falls within the H0)
#Df= degrees of freedom (# of variables -1), residuals (# observations - # groups)
#Sum Sq= sum of squares (sum(yi-y)^2)
#Mean Sq= SS/df (sample variance)
#F value= observed(SS/df)/residual(SS/df)

confint(lin.fit) #look at confidence intervals, do they overlap? if not they are significantly different
# example calculation
mutB1<-8.4-7.9
mutB2<-11.8-3.7
print(c(mutB1, mutB2))
#mutB conf int does not overlap with mutA, so significantly different

### Correlation ###
###################
cor(l, l4) #Pearsons correlation- linear relationship (covariance) between two variables
cor.test(l, l4)
# What if we just get a random list of 6 numbers between 1 and 10?
l3 <- sample(1:10, 6, replace=T)
cor(l, l3)
#attach(data)
#plot(starch, genotype)
x <- subset(data, genotype == 'WT')
y <- subset(data, genotype == 'MutB')
x <- x$starch
y <- y$starch
plot(x, y)
cor(x, y)
cor.test(x, y)
#correlate to random set
l <- sample(1:10, 8, replace=T)
cor(x, l)
cor(y, l)
# correlate using spearman's rank
cor(x, y, method="spearman")
cor.test(x, y, method="spearman")
# correlate MutB and MutC using PCC then SP

###################################
# Differential expression (edgeR) #
###################################

# load the package and data
# Run every time to you start a new R session.
library(edgeR)

# Download the expression data  
counts <- read.table('counts_data.csv', header=T, row.names='gene', sep=',')
head(counts)
summary(counts$control_1) # Get a summary of the expression levels for the first replicate of the control treatment


# You first need to tell edgeR where to look for your data (i.e. the genes, counts, and treatment groups) using DGEList
# DGEList is a data container used by edgeR to organize this information
# To define the treatment groups, you need to tell EdgeR what columns belong to the same treatment group (meaning they are replicates) 

treatments <- c("C","C","C","Cold","Cold","Cold") # you are making a new list called "group" using c() of your treatments
d <- DGEList(counts, group=treatments, genes=rownames(counts)) # rownames(counts) tells DGEList that the gene names are rownames in your counts dataframe

# d is now the variable that contains your DGEList
# You can visualize the frequency distribution of your counts in your samples using a histogram.
# Due to the large variability in counts per gene it is useful to use the log transform counts for normalization.
hist(d$counts, breaks = 50, xlab = "Counts")
hist(log2(d$counts), xlim=c(0, 20), breaks=50, xlab = "LogCounts")


# Normalize between samples
# The goal is to normalize the counts across samples so that MOST genes have the same abundance across the different samples. 
# The idea is that while you expect some genes will be differentially expressed across the datasets, most genes will not be affected
# by your treatment. We can use those "unchanging" genes to normalize between the samples. 
d <- calcNormFactors(d) #This calculates the effective library size and normalization factors. 
d$samples 



# Another way to look at the variation between your samples is to look at the "Biological Coefficient of Variation" (BCV)
# Because of the way variance is calculated, genes with higher expression would have higher variance.
# The BCV corrects for this by taking mean expression level into consideration. 
# The BCV is determined by estimating the common (across all genes in all samples) and 
# tagwise (across all samples for an individual gene) dispersal. 
# A common BCV between 0.2-0.4 is considered good enough to be able to identify differentially expressed genes. 

# Estimate the dispersion (i.e. the BCV between biological replicates).
d <- estimateCommonDisp(d)
d <- estimateTagwiseDisp(d)


# Counts per million (CPM) mapped reads are counts scaled by the number of fragments you sequenced (N) times one million.
# CPM is not scaled by the gene length. 
# Average log CPM : average log CPM in each sample for each gene.



###### Use the Fisher's Exact test to look for differentially expressed genes. #######

# First we'll look for genes differentially expressed between control & cold treatment

# The Fisher's exact test takes into account the "Tagwise Dispersian" when calculating the p-value
c_vs_cold <- exactTest(d, pair=c('C','Cold')) 

# Since we are calculating significance for thousands of genes, we need to use false discovery rate correction
# Here we use the Benjamini-Hochberg (BH) method (BH) to correct the p-values, the correct p-values are called q-values.
# We are saving the corrected p-values in a new column in the c_vs_cold$table called "FDR"
c_vs_cold$table$FDR <- p.adjust(c_vs_cold$table$PValue, method='BH') 

# Select the genes that are significantly differentially expressed (q-value <= 0.05) and have a logFC > 2 or < -2
c_vs_cold_genes <- subset(c_vs_cold$table, c_vs_cold$table$FDR <= 0.05 & abs(c_vs_cold$table$logFC) >=2)

# Check out the relationship between logFC and significance by making a "volcano" plot:
plot(-log(c_vs_cold_genes$FDR)~c_vs_cold_genes$logFC, 
     ylab= "logFDR",
     xlab = "logFC") 

# Save your results (you do not need to turn these in, but you may want to refer to these results later)
write.table(c_vs_cold$table,file="Control_vs_Cold.txt",quote=FALSE,sep="\t",row.names=TRUE) #quote=FALSE, to remove the "" that are automatically added on. 




