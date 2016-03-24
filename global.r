library(shiny)
library(readr)
library(RTextTools)
library(corpustools)
library(semnet)
library(DT)
source('lib.r')

d <- reactiveValues(tokens = NULL, meta=NULL, text=NULL, termstats=NULL, tokenstats=NULL) 