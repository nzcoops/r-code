wc_perc <- function (data, male="M", female="F", wc="waist", age="age", sex="sex")
  {
# Author: Matt Cooper
# Version 0.9
# Last updated: 29/04/10
# Waist Circumference Percentiles in Children and Adolescents, written for R
# http://www.ncbi.nlm.nih.gov/pubmed/15480363?dopt=Abstract

# waist in cm
# default values for males and females are "M" and "F" respectively, this can be specified by the user
# key variable names are assumed to be all in lower case, this can be specified by the user

# Pulling out required data into vectors

  age <- round(data[,age],0)
  wc <- data[,wc]
  sex <- data[,sex]
  waist_perc <- vector()

# Function checks

  if(any(age < 2 | age > 18)) stop("You have one or more subjects in your dataset younger than 2 years or older than 18 years, please remove and resubmit.", call.=F)

# Set up WC percentile lookup matrix

wcp <- as.data.frame(matrix(c(
2,43.2,45.0,47.1,48.8,50.8,43.8,45.0,47.1,49.5,52.2,
3,44.9,46.9,49.1,51.3,54.2,45.4,46.7,49.1,51.9,55.3,
4,46.6,48.7,51.1,53.9,57.6,46.9,48.4,51.1,54.3,58.3,
5,48.4,50.6,53.2,56.4,61.0,48.5,50.1,53.0,56.7,61.4,
6,50.1,52.4,55.2,59.0,64.4,50.1,51.8,55.0,59.1,64.4,
7,51.8,54.3,57.2,61.5,67.8,51.6,53.5,56.9,61.5,67.5,
8,53.5,56.1,59.3,64.1,71.2,53.2,55.2,58.9,63.9,70.5,
9,55.3,58.0,61.3,66.6,74.6,54.8,56.9,60.8,66.3,73.6,
10,57.0,59.8,63.3,69.2,78.0,56.3,58.6,62.8,68.7,76.6,
11,58.7,61.7,65.4,71.7,81.4,57.9,60.3,64.8,71.1,79.7,
12,60.5,63.5,67.4,74.3,84.8,59.5,62.0,66.7,73.5,82.7,
13,62.2,65.4,69.5,76.8,88.2,61.0,63.7,68.7,75.9,85.8,
14,63.9,67.2,71.5,79.4,91.6,62.6,65.4,70.6,78.3,88.8,
15,65.6,69.1,73.5,81.9,95.0,64.2,67.1,72.6,80.7,91.9,
16,67.4,70.9,75.6,84.5,98.4,65.7,68.8,74.6,83.1,94.9,
17,69.1,72.8,77.6,87.0,101.8,67.3,70.5,76.5,85.5,98.0,
18,70.8,74.6,79.6,89.6,105.2,68.9,72.2,78.5,87.9,101.0
),nrow=17, ncol=11, byrow=T))

names(wcp) <- c("<10th","10th","25th","50th","75th","90th","10th","25th","50th","75th","90th")
# Loop to calculate overweight/obesity

  for(i in 1:nrow(data))
    {
      # Females
      if(sex[i]==female)
        {
        temp <- sum(wc[i] >= wcp[(age[i]-1),7:11])
        waist_perc[i] <- names(wcp)[1+temp]
        }
      # Males
      if(sex[i]==male)
        {
        temp <- sum(wc[i] >= wcp[(age[i]-1),2:6])
        waist_perc[i] <- names(wcp)[1+temp]
        }
      }

         return(waist_perc)
#         }
  }