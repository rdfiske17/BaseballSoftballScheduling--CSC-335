library(readxl)
library(tidyverse)
library(dplyr)
library(lpSolve)
library(lpSolveAPI)
library(pracma)

wd = "C:\\Users\\riley\\OneDrive - cord.edu\\CSC-335 Final Project Files"
setwd(wd)

coinorDirectory = "C:\\Users\\riley\\CBC\\bin"

maxNodes = 100000 #This is for COIN-OR CBC, a beefier solver than LPSolve

## --------------------------------------------------------------------

# Define Helper Functions: Thanks Dr. Axvig

# Finds and eliminates all-zero rows from constraint matrix, and corresponding vectors
# of inequalities and right hand side vector.  Returns cleaned up versions of all three.
# NOTE:  the <<- assigns a value to a global variable declared outside the scope of the function
# Contrast this with <- or =, which deal with local variables only in R.
cleanUpModel = function()
{
  whereIsZero <- which(abs(constraintMatrix) %*% matrix(1,ncol(constraintMatrix),1) == 0)
  if(length(whereIsZero) > 0)
  {
    constraintMatrix <<- constraintMatrix[-whereIsZero, ]
    inequalities <<- inequalities[-whereIsZero, ,drop=FALSE]
    rightHandSide <<- rightHandSide[-whereIsZero, ,drop=FALSE]
  }
}

# pads the model with all-zeros (or "") in preparation for adding vectors
# this significantly speeds up the construction of the model by pre-allocating space
# as opposed to massive copy/paste operations that R uses when you bind rows to a 
# matrix one at a time.
padModel = function(numberOfRowsToAdd)
{
  oldNumberOfConstraints <- nrow(constraintMatrix)
  constraintMatrix <<- rbind(constraintMatrix,matrix(0,numberOfRowsToAdd,ncol(constraintMatrix)))
  inequalities <<- rbind(inequalities,matrix("",numberOfRowsToAdd,1))
  rightHandSide <<- rbind(rightHandSide,matrix(0,numberOfRowsToAdd,1))
  nrc <- oldNumberOfConstraints + 1
  return(nrc)
}

# generates a new constraint, ineq, rhs, and returns it
# takes in an arbitrary length list of regular expressions and values, one of "=", "<=", and ">=", and a value for rhs
generateConstraint = function(regexList,valueList,ineq,rhs)
{
  newConstraint <- matrix(0,1,length(namesOfVariables))
  colnames(newConstraint) <- namesOfVariables
  for(ii in 1:length(regexList))
  {
    regex <- regexList[ii]
    indicesToModify <- grep(pattern = regex,namesOfVariables)
    newConstraint[indicesToModify] <- valueList[ii]
  }
  constraintMatrix[newRowCounter,] <<- newConstraint
  inequalities[newRowCounter,1] <<- ineq
  rightHandSide[newRowCounter,1] <<- rhs
  newRowCounter <<- newRowCounter + 1
}

## --------------------------------------------------------------------

## Get the Excel Files in first to build the set of decision variables

# List of Sports Import
namesOfSports = c("Baseball","Softball")

# List of Teams Import
teams_turf = as.matrix(read_excel("ARC_DATA/ARC_Teams.xlsx"))
namesOfTeams = teams_turf[,1] # list of all team names
teamsTurfMatrix = matrix(as.numeric(teams_turf[,2:ncol(teams_turf)]),nrow = nrow(teams_turf),ncol = ncol(teams_turf) -1) # rows and cols are named instead of numeric with name column
colnames(teamsTurfMatrix) = colnames(teams_turf)[2:ncol(teams_turf)]
rownames(teamsTurfMatrix) = namesOfTeams

# Extract list of Turf Schools
turfTeams = as.data.frame(teamsTurfMatrix) %>% filter(Turf == 1)
nonturfTeams = as.data.frame(teamsTurfMatrix) %>% filter(Turf == 0)
namesOfTurfTeams = row.names(turfTeams)
namesOfTurfTeamsListRegExp = paste0("(",paste(namesOfTurfTeams, collapse = "|"),")")
namesOfNonTurfTeams = row.names(nonturfTeams)
namesOfNonTurfTeamsListRegExp = paste0("(",paste(namesOfNonTurfTeams, collapse = "|"),")")

# List of Series Import

baseball_series = as.matrix(read_excel("ARC_DATA/ARC_Baseball_Dates_2026.xlsx"))
namesOfBaseballSeries = baseball_series[,1]
baseballSeriesMatrix = matrix(as.numeric(baseball_series[,3:ncol(baseball_series)]), nrow = nrow(baseball_series), ncol = ncol(baseball_series) -2)
colnames(baseballSeriesMatrix) = colnames(baseball_series)[3:ncol(baseball_series)]
rownames(baseballSeriesMatrix) = namesOfBaseballSeries
namesOfBaseballDates = baseball_series[,2]# Getting the series dates

baseballWeekendRegExp = paste0("(",paste(row.names(as.data.frame(baseballSeriesMatrix) %>% filter(Weekend == 1)),collapse = "|"),")")
baseballWeekdayRegExp = paste0("(",paste(row.names(as.data.frame(baseballSeriesMatrix) %>% filter(Weekday == 1)),collapse = "|"),")")

softball_series = as.matrix(read_excel("ARC_DATA/ARC_Softball_Dates_2026.xlsx"))
namesOfSoftballSeries = baseball_series[,1]
softballSeriesMatrix = matrix(as.numeric(softball_series[,3:ncol(softball_series)]), nrow = nrow(softball_series), ncol = ncol(softball_series) -2)
colnames(softballSeriesMatrix) = colnames(softball_series)[3:ncol(softball_series)]
rownames(softballSeriesMatrix) = namesOfSoftballSeries
namesOfSoftballDates = softball_series[,2] # Getting the series dates

softballWeekendRegExp = paste0("(",paste(row.names(as.data.frame(softballSeriesMatrix) %>% filter(Weekend == 1)),collapse = "|"),")")
softballWeekdayRegExp = paste0("(",paste(row.names(as.data.frame(softballSeriesMatrix) %>% filter(Weekday == 1)),collapse = "|"),")")

namesOfSeries = namesOfBaseballSeries

# List of Orderings Import

namesofOrderingsBaseball = c("SD","DS")
namesofOrderingsSoftball = c("D")

## --------------------------------------------------------------------

## Get the Excel Files for additional constraints and format

# Softball as Key for Weekday Conflicts with Baseball

weekday_conflicts = as.matrix(read_excel("ARC_DATA/ARC_Weekday_Conflicts.xlsx"))
namesOfWeekdaySoftballSeries = matrix(weekday_conflicts[,1],nrow(weekday_conflicts),1)
namesOfWeekdayBaseballSeries = c("Series 3","Series 6")
weekdayConflictsList = matrix(weekday_conflicts[,2],nrow(weekday_conflicts),1) # extract just the conflicts column
#weekdayConflictsListRegExp = paste("(",gsub(",","|",weekdayConflictsList),")") # place a vertical bar between each program conflict number
#weekdayConflictsListRegExp = gsub("\\s*\\(\\s*", "(", 
#                 gsub("\\s*\\)\\s*", ")", 
##                 gsub("\\|\\s+", "|",weekdayConflictsListRegExp))) # remove the extra spaces on either end of the () and |
weekdayConflictsListRegExp = matrix(weekdayConflictsList,length(weekdayConflictsList),1) # make conflicts into a vector

row.names(weekdayConflictsListRegExp) = namesOfWeekdaySoftballSeries # name the rows that have the conflicts

# Long Distance Teams Import

longdistance_teams = as.matrix(read_excel("ARC_DATA/ARC_LongDistance_Teams.xlsx"))
longDistanceMatrix = matrix(longdistance_teams[,2:ncol(longdistance_teams)],nrow = nrow(longdistance_teams),ncol = ncol(longdistance_teams) -1) # rows and cols are named instead of numeric with name column
colnames(longDistanceMatrix) = colnames(longdistance_teams)[2:ncol(longdistance_teams)]
rownames(longDistanceMatrix) = namesOfTeams
longDistanceListRegExp = paste("(",gsub(",","|",longDistanceMatrix),")") # remove the extra spaces on either end of the () and |
longDistanceListRegExp = gsub(" ","",longDistanceListRegExp) # remove the extra spaces
longDistanceListRegExp = matrix(longDistanceListRegExp,length(longDistanceListRegExp),1) # make conflicts into a vector
row.names(longDistanceListRegExp) = namesOfTeams # name the rows that have the conflicts

# Local Teams Import

local_teams = as.matrix(read_excel("ARC_DATA/ARC_Local_Teams.xlsx"))
localMatrix = matrix(local_teams[,2:ncol(local_teams)],nrow = nrow(local_teams),ncol = ncol(local_teams) -1) # rows and cols are named instead of numeric with name column
colnames(localMatrix) = colnames(local_teams)[2:ncol(local_teams)]
rownames(localMatrix) = namesOfTeams
localListRegExp = paste("(",gsub(",","|",localMatrix),")") # remove the extra spaces on either end of the () and |
localListRegExp = gsub(" ","",localListRegExp) # remove the extra spaces
localListRegExp = matrix(localListRegExp,length(localListRegExp),1) # make conflicts into a vector
row.names(localListRegExp) = namesOfTeams # name the rows that have the conflicts

## --------------------------------------------------------------------

## Create the set of decision variables and the blank constraint matrix

namesOfVariables = c()#initialize an empty list

# Create all the variable names from people and program names
for(sport in namesOfSports){
  for(s in namesOfSeries){
    for(t1 in namesOfTeams){
      for(t2 in namesOfTeams){
        if(sport == "Baseball"){
          for(o in namesofOrderingsBaseball){
            if(t1 != t2){
              # Create a new variable for the potential series to be played
              newGameVariable = paste("x",t1,t2,s,o,sport,sep = ".") 
              namesOfVariables = c(namesOfVariables,newGameVariable)#tack the new one onto the end of the list
            }
          }
        }
        else if(sport == "Softball"){
          for(o in namesofOrderingsSoftball){
            if(t1 != t2){
              # Create a new variable for the potential series to be played
              newGameVariable = paste("x",t1,t2,s,o,sport,sep = ".") 
              namesOfVariables = c(namesOfVariables,newGameVariable)#tack the new one onto the end of the list
            }
          }
        }
      }
      # Create a new variable for the potential bye to be had
      if((t1 == "LOR" & ((sport == "Baseball" & s == "Series 4") | (sport == "Softball" & s == "Series 1"))) | 
         (t1 == "UD" & ((sport == "Baseball" & s %in% namesOfWeekdayBaseballSeries 
                         | sport == "Softball" & s %in% namesOfSoftballSeries
         ))) |
         (t1 != "LOR" & t1 != "UD")){
        newByeVariable = paste("b",t1,s,sport,sep = ".")
        namesOfVariables = c(namesOfVariables,newByeVariable)
      }
      
      if(s != "Series 1"){
        # Create a new variable for the potential for the team to play a team coming off a bye
        #newOffByeVariable = paste("c",t1,s,sport,sep = ".")
        #namesOfVariables = c(namesOfVariables,newOffByeVariable)
      }
    }
  }
}

#namesOfVariables
#length(namesOfVariables)

# Construct the blank constraint matrix with the appropriate column names
constraintMatrix = matrix(0,0,length(namesOfVariables))
colnames(constraintMatrix) = namesOfVariables
inequalities = matrix("",0,1) # to hold our inequalities attached to the constraint matrix for corresponding rows of the same index
rightHandSide = matrix(0,0,1) # to hold our r.h.s attached to the constraint matrix for corresponding rows of the same index

## --------------------------------------------------------------------

## Constraints

## Constraint 1: For each sport and for each team, that team gets exactly one bye per season.

newRowCounter = padModel(numberOfRowsToAdd = length(namesOfTeams) * length(namesOfSports))

for(sport in namesOfSports){
  for(t1 in namesOfTeams){
    newConstraint = matrix(0,1,length(namesOfVariables)) # create a blank (0) row that will be added to the constraint matrix
    colnames(newConstraint) = namesOfVariables # assign column names to this blank row
    regexList = c(paste("^b",t1,".*",sport,sep="\\."))
    valueList = c(1)
    newIneq = "="
    newRhs = 1
    generateConstraint(regexList,valueList,newIneq,newRhs)
  }
}

cleanUpModel()

## --------------------------------------------------------------------

## Constraint 2: For each sport and for each team, they must play each other team exactly once across all series.

newRowCounter = padModel(numberOfRowsToAdd = length(namesOfTeams) * (length(namesOfTeams) - 1 ) * length(namesOfSports))

for(sport in namesOfSports){
  for(t1 in namesOfTeams){
    for(t2 in namesOfTeams){
      if(t1 != t2){
        newConstraint = matrix(0,1,length(namesOfVariables))
        colnames(newConstraint) = namesOfVariables
        regexList = c(paste("^x",t1,t2,".*",".*",sport,sep = "\\."),
                      paste("^x",t2,t1,".*",".*",sport,sep = "\\."))
        valueList = c(1,
                      1)
        newIneq = "="
        newRhs = 1
        generateConstraint(regexList,valueList,newIneq,newRhs)
      }
    }
  }
}

cleanUpModel()

## --------------------------------------------------------------------

## Constraint 3: For each sport and for each series, it must be either home, away, or a bye.

newRowCounter = padModel(numberOfRowsToAdd = length(namesOfSports) * length(namesOfTeams) * length(namesOfSeries))

for(sport in namesOfSports){
  for(t1 in namesOfTeams){
    for(series in namesOfSeries){
      newConstraint = matrix(0,1,length(namesOfVariables))
      colnames(newConstraint) = namesOfVariables
      regexList = c(paste("^b",t1,series,sport,sep = "\\."), # byes
                    paste("^x",t1,".*",series,".*",sport,sep = "\\."),
                    paste("^x",".*",t1,series,".*",sport,sep = "\\.")) # home/away
      valueList = c(1,
                    1,
                    1)
      newIneq = "="
      newRhs = 1
      generateConstraint(regexList,valueList,newIneq,newRhs)
    }
  }
}

cleanUpModel()

## --------------------------------------------------------------------

## Constraint 4: For each sport and for each team, there may not be more than 2 consecutive home series.

newRowCounter = padModel(numberOfRowsToAdd = length(namesOfSports) * length(namesOfTeams) * (length(namesOfSeries) - 2))

for(sport in namesOfSports){
  for(t1 in namesOfTeams){
    for(series in namesOfSeries){
      if(series != "Series 8" & series != "Series 9"){
        newConstraint = matrix(0,1,length(namesOfVariables))
        colnames(newConstraint) = namesOfVariables
        
        seriesNumber = as.numeric(substr(series, nchar(series), nchar(series)))
        seriesToFind = c(seriesNumber, seriesNumber + 1, seriesNumber + 2)
        seriesRegExp = paste0("Series (",paste(seriesToFind, collapse = "|"),")")
        
        regexList = c(paste("^x",t1,".*",seriesRegExp,".*",sport,sep = "\\."))
        valueList = c(1)
        newIneq = "<="
        newRhs = 2
        generateConstraint(regexList,valueList,newIneq,newRhs)
        
        
      }
    }
  }
}


cleanUpModel()

## --------------------------------------------------------------------

## Constraint 5: For each sport and for each team, there may not be more than 2 consecutive away series.

newRowCounter = padModel(numberOfRowsToAdd = length(namesOfSports) * length(namesOfTeams) * (length(namesOfSeries) - 2))

for(sport in namesOfSports){
  for(t1 in namesOfTeams){
    for(series in namesOfSeries){
      if(series != "Series 8" & series != "Series 9"){
        newConstraint = matrix(0,1,length(namesOfVariables))
        colnames(newConstraint) = namesOfVariables
        
        seriesNumber = as.numeric(substr(series, nchar(series), nchar(series)))
        seriesToFind = c(seriesNumber, seriesNumber + 1, seriesNumber + 2)
        seriesRegExp = paste0("Series (",paste(seriesToFind, collapse = "|"),")")
        
        regexList = c(paste("^x",".*",t1,seriesRegExp,".*",sport,sep = "\\."))
        valueList = c(1)
        newIneq = "<="
        newRhs = 2
        generateConstraint(regexList,valueList,newIneq,newRhs)
        
        
      }
    }
  }
}


cleanUpModel()

## --------------------------------------------------------------------

## Constraint 6: Each Team per season gets 4 away games and 4 home games.

newRowCounter = padModel(numberOfRowsToAdd = length(namesOfSports) * length(namesOfTeams) * 2)

# home
for(sport in namesOfSports){
  for(t1 in namesOfTeams){
    newConstraint = matrix(0,1,length(namesOfVariables))
    colnames(newConstraint) = namesOfVariables
    
    
    regexList = c(paste("^x",t1,".*",".*",".*",sport,sep = "\\."))
    valueList = c(1)
    newIneq = "="
    newRhs = 4
    generateConstraint(regexList,valueList,newIneq,newRhs)
  }
}

# away
for(sport in namesOfSports){
  for(t1 in namesOfTeams){
    newConstraint = matrix(0,1,length(namesOfVariables))
    colnames(newConstraint) = namesOfVariables
    
    
    regexList = c(paste("^x",".*",t1,".*",".*",sport,sep = "\\."))
    valueList = c(1)
    newIneq = "="
    newRhs = 4
    generateConstraint(regexList,valueList,newIneq,newRhs)
  }
}


cleanUpModel()

## --------------------------------------------------------------------

## Constraint 7: For each sport and for each team, they must have at least one home weekend series in one of the last **3** weekend series.

newRowCounter = padModel(numberOfRowsToAdd = length(namesOfSports) * length(namesOfTeams))

for(sport in namesOfSports){
  for(t1 in namesOfTeams){
    newConstraint = matrix(0,1,length(namesOfVariables))
    colnames(newConstraint) = namesOfVariables
    
    if(sport == "Baseball"){
      seriesToFindRegex = "Series (7|8|9)"
    }
    else if(sport == "Softball"){
      seriesToFindRegex = "Series (5|7|9)"
    }
    
    regexList = c(paste("^x",t1,".*",seriesToFindRegex,".*",sport,sep = "\\."))
    valueList = c(1)
    newIneq = ">="
    newRhs = 1
    generateConstraint(regexList,valueList,newIneq,newRhs)
    
  }
}

cleanUpModel()

## --------------------------------------------------------------------

## Constraint 8: For each weekday series in softball, if a team is at home for a series, the corresponding baseball team is not at home for the corresponding weekday series.

newRowCounter = padModel(numberOfRowsToAdd = length(namesOfTeams) * length(namesOfWeekdaySoftballSeries))

for(t1 in namesOfTeams){
  for(series in namesOfWeekdaySoftballSeries){
    newConstraint = matrix(0,1,length(namesOfVariables))
    colnames(newConstraint) = namesOfVariables
    
    correspondingBaseballSeries = weekdayConflictsListRegExp[series,]
    
    regexList = c(paste("^x",t1,".*",correspondingBaseballSeries,".*","Baseball",sep = "\\."),
                  paste("^x",".*",t1,series,".*","Softball",sep = "\\."))
    valueList = c(1,
                  1)
    newIneq = "<="
    newRhs = 1
    generateConstraint(regexList,valueList,newIneq,newRhs)
    
  }
}

cleanUpModel()

## --------------------------------------------------------------------

## Constraint 9: For the first baseball series of the  season, have nonturf schools be not at home.

newRowCounter = padModel(numberOfRowsToAdd = length(namesOfNonTurfTeams))

for(t1 in namesOfNonTurfTeams){
  newConstraint = matrix(0,1,length(namesOfVariables))
  colnames(newConstraint) = namesOfVariables
  
  regexList = c(paste("^x",t1,".*","Series 1",".*","Baseball",sep = "\\.")
  )
  valueList = c(1
  )
  newIneq = "="
  newRhs = 0
  generateConstraint(regexList,valueList,newIneq,newRhs)   
}

cleanUpModel()

## --------------------------------------------------------------------

## Constraint 10: For each sport and for each team, for each team they are long distance from, they must play at home or away on one weekend series.

newRowCounter = padModel(numberOfRowsToAdd = length(namesOfSports) * length(namesOfTeams) * 4)

for(sport in namesOfSports){
  if(sport == "Baseball"){
    seriesToSearch = baseballWeekendRegExp
  }
  else if(sport == "Softball"){
    seriesToSearch = softballWeekendRegExp
  }
  for(t1 in namesOfTeams){
    namesOfLongDistanceTeams = trimws(unlist(strsplit(longDistanceMatrix[t1,], split = "\\,")))
    
    for(t2 in namesOfLongDistanceTeams){
      if(!is.na(t2)){
        
        newConstraint = matrix(0,1,length(namesOfVariables))
        colnames(newConstraint) = namesOfVariables
        
        regexList = c(paste("^x",t1,t2,seriesToSearch,".*",sport,sep = "\\."),
                      paste("^x",t2,t1,seriesToSearch,".*",sport,sep = "\\.")
        )
        valueList = c(1,
                      1
        )
        newIneq = "="
        newRhs = 1
        generateConstraint(regexList,valueList,newIneq,newRhs)
      }
    }
  }
}

cleanUpModel()

## --------------------------------------------------------------------

## Constraint 11: For each sport and for each team, for each team they are close to, they must play at home or away on one weekday series.

newRowCounter = padModel(numberOfRowsToAdd = length(namesOfSports) * length(namesOfTeams) * 2)

for(sport in namesOfSports){
  if(sport == "Baseball"){
    seriesToSearch = baseballWeekdayRegExp
  }
  else if(sport == "Softball"){
    seriesToSearch = softballWeekdayRegExp
  }
  
  for(t1 in namesOfTeams){
    namesOfLocalTeams = trimws(unlist(strsplit(localMatrix[t1,], split = "\\,")))
    
    for(t2 in namesOfLocalTeams){
      if(!is.na(t2)){
        
        newConstraint = matrix(0,1,length(namesOfVariables))
        colnames(newConstraint) = namesOfVariables
        
        regexList = c(paste("^x",t1,t2,seriesToSearch,".*",sport,sep = "\\."),
                      paste("^x",t2,t1,seriesToSearch,".*",sport,sep = "\\.")
        )
        valueList = c(1,
                      1
        )
        newIneq = "="
        newRhs = 1
        generateConstraint(regexList,valueList,newIneq,newRhs)
      }
    }
  }
}

cleanUpModel()

## --------------------------------------------------------------------

## Constraint 12: The baseball series 4, 7, and 9 must be played with an order of DS or a bye. The baseball series 3, 5, 6, and 8 must be played with an order of SD or a bye.

newRowCounter = padModel(numberOfRowsToAdd = length(namesOfTeams) * 7)

for(t1 in namesOfTeams){
  newConstraint = matrix(0,1,length(namesOfVariables))
  colnames(newConstraint) = namesOfVariables
  regexList = c(paste("^x",t1,".*","Series (4|7|9)","SD","Baseball",sep = "\\."),
                paste("^x",".*",t1,"Series (4|7|9)","SD","Baseball",sep = "\\."),
                paste("^x",t1,".*","Series (3|5|6|8)","DS","Baseball",sep = "\\."),
                paste("^x",".*",t1,"Series (3|5|6|8)","DS","Baseball",sep = "\\."))
  valueList = c(1,
                1,
                1,
                1)
  newIneq = "="
  newRhs = 0
  generateConstraint(regexList,valueList,newIneq,newRhs)
}

cleanUpModel()

## --------------------------------------------------------------------

## Set up Linear Program

# Create LP object
LP = make.lp(NROW(constraintMatrix),NCOL(constraintMatrix))
set.type(LP,1:NCOL(constraintMatrix),type=c("binary"))

# Create objective function
objectiveFunction = matrix(0,1,length(namesOfVariables))
colnames(objectiveFunction) = namesOfVariables

# Set the obj function
set.objfn(LP,objectiveFunction)

# Minimization Problem
lp.control(LP,sense='min')

#Set each row of LP individually
for(rowCounter in 1:NROW(constraintMatrix)){
  set.row(LP,rowCounter,constraintMatrix[rowCounter,])
  set.constr.type(LP,inequalities[rowCounter,1],rowCounter)
  set.rhs(LP, rightHandSide[rowCounter,1], rowCounter)
}

## --------------------------------------------------------------------

## Solve for a solution using COIN-OR CBC

setwd(coinorDirectory)

write.lp(LP,'problem.mps',type='mps')

system(paste0("cbc.exe problem.mps maxN ",maxNodes," solve solution LPSolution.txt exit"))

dataFromCoinOrCBC = data.frame(read.table(text=readLines("LPSolution.txt")[count.fields("LPSolution.txt") == 4]))
#View(dataFromCoinOrCBC)
#dataFromCoinOrCBC

dataFromCoinOrCBC = data.frame(read.table(text=readLines("LPSolution.txt")[count.fields("LPSolution.txt") == 4]))
partialSolutionLocations = dataFromCoinOrCBC$V2
partialSolutionValues = dataFromCoinOrCBC$V3
partialSolutionLocations= gsub("C","",partialSolutionLocations)
partialSolutionLocations = as.numeric(partialSolutionLocations)
fullSolutionVector = rep(0,length(namesOfVariables))
for(ii in 1:length(partialSolutionLocations)){
  fullSolutionVector[partialSolutionLocations[ii]] = partialSolutionValues[ii]
}
names(fullSolutionVector) = namesOfVariables
# return(fullSolutionVector)
# }
fullSolutionVector = as.matrix(fullSolutionVector)
fullSolutionVector = t(fullSolutionVector)
solutionVector = colnames(fullSolutionVector) [which(fullSolutionVector[1,] == 1)]

#solutionVector

## --------------------------------------------------------------------

## Format Output according to A-R-C Specifications

#Baseball Schedule
scheduleB = matrix("",length(namesOfBaseballSeries),length(namesOfTeams))
row.names(scheduleB) = namesOfBaseballSeries
colnames(scheduleB) = namesOfTeams
for(t1 in namesOfTeams){
  for(t2 in namesOfTeams){
    for(s in namesOfSeries){
      for(o in namesofOrderingsBaseball) {
        newVariable = paste("x",t1,t2,s,o,"Baseball",sep = ".")
        for(variable in solutionVector){
          if(variable == newVariable){
            scheduleB[s,t2] = paste0("@ ",t1," ",o)
          }
        }
        newVariable = paste("x",t2,t1,s,o,"Baseball",sep = ".")
        for(variable in solutionVector){
          if(variable == newVariable){
            scheduleB[s,t2] = paste0("v ",t1, " ",o)
          }
        }
      }
      
    }
  }
}
row.names(scheduleB) = namesOfBaseballDates

#Softball Schedule
scheduleS = matrix("",length(namesOfSoftballSeries),length(namesOfTeams))
row.names(scheduleS) = namesOfSoftballSeries
colnames(scheduleS) = namesOfTeams
for(t1 in namesOfTeams){
  for(t2 in namesOfTeams){
    for(series in namesOfSoftballSeries){
      newVariable = paste("x",t1,t2,series,"D","Softball",sep = ".")
      for(variable in solutionVector){
        if(variable == newVariable){
          scheduleS[series,t2] = paste0("@ ",t1)
        }
      }
      newVariable = paste("x",t2,t1,series,"D","Softball",sep = ".")
      for(variable in solutionVector){
        if(variable == newVariable){
          scheduleS[series,t2] = paste0("v ",t1)
        } }
    }
  }
}
row.names(scheduleS) = namesOfSoftballDates

View(scheduleB)
View(scheduleS)
setwd(paste0(wd,"\\results"))

baseballCSV = paste("A-R-C_Baseball_Schedule.csv")
softballCSV = paste("A-R-C_Softball_Schedule.csv")

write.csv(x = scheduleB,file = baseballCSV)
write.csv(x = scheduleS,file = softballCSV)
