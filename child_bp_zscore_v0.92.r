bp_z <- function (data, male="M", female="F", sbp="sbp", dbp="dbp", zht="zht", age="age", sex="sex",  age.month=F)
  {
# Author: Matt Cooper
# Version 0.92
# Last updated: 21/09/11
# Blood pressure z scores in Children and Adolescents, written for R
# 
# http://www.nhlbi.nih.gov/health/prof/heart/hbp/hbp_ped.pdf

# Default values for males and females are "M" and "F" respectively, this can be specified by the user
# Key variable names are assumed to be all in lower case, this can be specified by the user

# Pulling out required data into vectors

  if(age.month==F){age <- round(data[,age],1)} else {age <- round(data[,age]/12,1)} # in months
  z_ht <- data[,zht]
  sbp <- data[,sbp]
  dbp <- data[,dbp]
  sex <- data[,sex]
  sbp_z <- vector()
  dbp_z <- vector() 

# Note:
# height in cm
# age will be selected based on which of the ages in the table the subject is closest to. if an age lies directly inbetween two the lower will be selected
# if you feel this is not appropriate then please round your ages to those in the table (24.5, 25.5, 26.5 etc) via your own rule prior to submitting
# and set age.month to TRUE

# Function checks

  if(any(age < (2) | age > (18))) stop("You have one or more subjects in your dataset younger than 2 years or older than 18 years, please remove and resubmit.", call.=F)

# Set up WC percentile lookup matrix

bplu <- as.data.frame(matrix(c(
102.19768,102.01027,61.01217,60.5051,
1.82416,1.94397,0.68314,1.01301,
0.12776,0.00598,-0.09835,0.01157,
0.00249,-0.00789,0.01711,0.00424,
-0.00135,-0.00059,0.00045,-0.00137,
2.73157,2.03526,1.46993,1.16641,
-0.19618,0.02534,-0.07849,0.12795,
-0.04659,-0.01884,-0.03144,-0.03869,
0.00947,0.00121,0.00967,-0.00079,
10.7128,10.4855,11.6032,10.9573
),nrow=10, ncol=4, byrow=T))

names(bplu) <- c("sbpm","sbpf","dbpm","dbpf")

# Loop to calculate z-score for sbp

  for(i in 1:nrow(data))
    {
      # Females
      if(sex[i]==female)
        {
        mu <- bplu$sbpf[1] +
        sum(bplu$sbpf[2]*(age[i]-10),bplu$sbpf[3]*(age[i]-10)^2,bplu$sbpf[4]*(age[i]-10)^3,bplu$sbpf[5]*(age[i]-10)^4) +
        sum(bplu$sbpf[6]*z_ht[i],bplu$sbpf[7]*z_ht[i]^2,bplu$sbpf[8]*z_ht[i]^3,bplu$sbpf[9]*z_ht[i]^4)
        sbp_z[i] <- (sbp[i] - mu)/bplu$sbpf[10]
        }
      # Males
      if(sex[i]==male)
        {
        mu <- bplu$sbpm[1] +
        sum(bplu$sbpm[2]*(age[i]-10),bplu$sbpm[3]*(age[i]-10)^2,bplu$sbpm[4]*(age[i]-10)^3,bplu$sbpm[5]*(age[i]-10)^4) + 
        sum(bplu$sbpm[6]*z_ht[i],bplu$sbpm[7]*z_ht[i]^2,bplu$sbpm[8]*z_ht[i]^3,bplu$sbpm[9]*z_ht[i]^4)
        sbp_z[i] <- (sbp[i] - mu)/bplu$sbpm[10]
        }
      }
  sbp_z <- replace(sbp_z, sbp_z=="",NA)

# Loop to calculate z-score for dbp

  for(i in 1:nrow(data))
    {
      # Females
      if(sex[i]==female)
        {
        mu <- bplu$dbpf[1] +
        sum(bplu$dbpf[2]*(age[i]-10),(bplu$dbpf[3]*(age[i]-10))^2,(bplu$dbpf[4]*(age[i]-10))^3,(bplu$dbpf[5]*(age[i]-10))^4) +
        sum(bplu$dbpf[6]*z_ht[i],bplu$dbpf[7]*z_ht[i]^2,bplu$dbpf[8]*z_ht[i]^3,bplu$dbpf[9]*z_ht[i]^4)
        dbp_z[i] <- (dbp[i] - mu)/bplu$dbpf[10]
        }
      # Males
      if(sex[i]==male)
        {
        mu <- bplu$dbpm[1] +
        sum(bplu$dbpm[2]*(age[i]-10),(bplu$dbpm[3]*(age[i]-10))^2,(bplu$dbpm[4]*(age[i]-10))^3,(bplu$dbpm[5]*(age[i]-10))^4) + 
        sum(bplu$dbpm[6]*z_ht[i],bplu$dbpm[7]*z_ht[i]^2,bplu$dbpm[8]*z_ht[i]^3,bplu$dbpm[9]*z_ht[i]^4)
        dbp_z[i] <- (dbp[i] - mu)/bplu$dbpm[10]
        }
      }
  dbp_z <- replace(dbp_z, dbp_z=="",NA)

return(cbind(sbp_z,dbp_z))
  }
