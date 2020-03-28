#################################################################
# Settings ######################################################
#################################################################

gc()
rm(list=ls())
options(scipen=999)

#################################################################
# Packages ######################################################
#################################################################

library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(tidyquant)
library(magrittr)
library(googlesheets)
library(Cairo)
library(scales)

#################################################################
# Theme  ########################################################
#################################################################

f_size <- 10

t <- theme(plot.title = element_text(face="bold"),
           axis.text.x = element_text(size=f_size,color='#000000',angle=0),
           axis.text.y = element_text(size=f_size,color='#000000'),
           axis.title.x = element_text(face="bold", size=f_size,color='#000000'),
           axis.title.y = element_text(face="bold", size=f_size,color='#000000'),
           panel.background = element_rect(fill='#ffffff', color='#a5a5a5',size=0.5),
           panel.ontop = F,
           panel.grid.major = element_line(color='#a5a5a5', linetype='dotted',size=0.2),
           panel.grid.minor = element_line(color='#a5a5a5', linetype='blank', size=0),
           legend.text = element_text(size=f_size,color='#000000'),
           legend.title = element_text(face='bold',size=f_size,color='#000000'),
           legend.box.background = element_rect(fill='#ffffff', color='#ffffff', size=1.5),
           strip.text = element_text(size=f_size,color='#000000', face='bold'),
           strip.background = element_rect(colour = NA, fill = '#ffffff'))

pal <- c('#e5ac13','#43A39E','#e3e117','#3e21aa','#af1313','#ea360c','#a26b24','#333745','#8f8f8f','#515151','#000000')

#################################################################
# Tools #########################################################
#################################################################

up <- function() {
  for (df in ls(.GlobalEnv)[1:(length(ls(.GlobalEnv))-1)]){
    e <<- .GlobalEnv
    if (is.data.frame(e[[df]])) {
      colnames(e[[df]]) <<- toupper(colnames(e[[df]]))
    }
  }
  rm(e,pos=".GlobalEnv")
}