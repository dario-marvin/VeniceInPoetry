## R file for the analysis of references of Venice in the medium of poetry. 
## This file was created as part of the SHS project "Venice in Poetry"
## of the course Digital Humanities taught by Prof. Frédéric Kaplan 
## at EPFL in 2015-2016.
##
## Copyright (C) 2016 Marvin
## 
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
## Author: Dario Marvin
## Created: 2016-04-28

## ----------------- Load useful packages ------------------

#install.packages("stringr")              # if needed
library(stringr)

## ---------- Erase everything stored in memory ------------

rm(list=ls())
cat("\014")
options(warn=-1)

## ------------------- Useful functions --------------------

readinteger <- function() { 
  n <- readline(prompt = "Enter your choice: ")
  if(!grepl("^[0-9]+$",n) || n>2) {
    return(readinteger())
  }
  return(as.integer(n))
}

read_y_n <- function() { 
  n <- readline(prompt = "Enter your choice: ")
  if(n == "y" || n == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

## ---------------------- Text choice ----------------------

cat ("Please choose a poem:","\n","1: Commedia, by Dante Alighieri","\n",
     "2: Orlando Furioso, by Ludovico Ariosto","\n","\n");
choice <- readinteger()

if (choice == 1) {
  text = scan("http://www.gutenberg.org/files/1012/1012-0.txt", what="character", sep="\n", encoding="UTF-8")
} else if (choice == 2) {
  text = scan("http://www.gutenberg.org/files/3747/3747-0.txt", what="character", sep="\n", encoding="UTF-8")
} else 
  print("Error! Choose a possible value")

## --------------------- Text analisis ---------------------

if (choice == 1) {
  poem.lines = text[19:14356]                 # clear metadata out of the text
} else if (choice == 2) {
  poem.lines = text[19:43645]
} 

poem.lines <- tolower(poem.lines)             # change all letters to lowercase
poem.lines <- str_trim(poem.lines)            # eliminate initial spaces in each line
poem.length <- length(poem.lines)
poem.words <- strsplit(poem.lines, "\\W")     # list all words in the poem 
poem.words <- unlist(poem.words)
not.blanks <- which(poem.words != "")         # clear of all blank spaces
poem.words <- poem.words[not.blanks]

words.freqs <- table(poem.words)
sorted.words.freqs <- sort(words.freqs , decreasing=TRUE)
sorted.words.freqs[1:30]

keywords = c("venezia","venetiae","veneziani","viniziani","rïalto","rialto","san marco","vinegia","doge","mestre","venetia")
#keywords = c("ugolino");

for (i in 1:length(keywords)) {
  pos <- str_extract(poem.lines,keywords[i])
  pos <- which(!is.na(pos))
  count <- length(pos)
  if (count == 0) {
    cat("Found 0 references of the word",keywords[i],"\n")
  } else {
    if (count == 1) {
      cat("Found 1 reference of the word",keywords[i],"\n","Do you want to visualize it? [y/n]")
    } else {
      cat("Found ",count," references of the word",keywords[i],"\n","Do you want to visualize them? [y/n]")
    } 
    bool <- read_y_n()
    cat("\n")
    if (bool == TRUE) {
      for (j in 1:length(pos)) {
        
        if (choice == 1) {
          tmp = max(5,pos[j]-200)
          test1 <- grepl("canto",poem.lines[tmp:pos[j]]) 
          test2 <- (grepl("inferno",poem.lines[tmp:pos[j]]) 
                    | grepl("purgatorio",poem.lines[tmp:pos[j]]) 
                    | grepl("paradiso",poem.lines[tmp:pos[j]]))
          test <- test1 & test2
          
          if (is.element(TRUE,test)) {
            ref <- max(which(test == TRUE))
          } else {
            ref <- 4
          }

          diff <- ref
          canto <- max(4,tmp - 1 + ref)
          if (ref == 4) {
            line <- pos[j]-4
            cat(poem.lines[ref],", ",line,":","\n\n",sep="");
          } else {
            line <- 201 - diff
            cat(poem.lines[canto],", ",line,":","\n\n",sep="");
          }

        } else if (choice == 2) {
          num <- as.numeric(poem.lines[(pos[j]-4):(pos[j]+4)])
          loc <- which(!is.na(num))
          num <- num[!is.na(num)]
          if (loc > 5){
            num <- num - 1
          }
          
          one <- as.numeric(poem.lines[(max(5,pos[j]-num*9-6)):(max(pos[j]-num*9+8,14))])
          loc2 <- which(one == 1)
          canto <- max(4,pos[j]-num*9+loc2-8)
          ottava <- num
          if (loc == 1) {
            line <- 4
          } else if (loc == 2) {
            line <- 3
          } else if (loc == 3) {
            line <- 2
          } else if (loc == 4) {
            line <- 1
          }else if (loc == 6) {
            line <- 8
          }else if (loc == 7) {
            line <- 7
          }else if (loc == 8) {
            line <- 6
          }else if (loc == 9) {
            line <- 5
          }
          
          cat(poem.lines[canto],", ",ottava,", ",line,":","\n\n",sep="");
        }
       
        cat(poem.lines[(pos[j]-4):(pos[j]+4)], sep="\n")
        cat("\n","--------------------------------------------","\n\n")
      }
    }
  }
}
