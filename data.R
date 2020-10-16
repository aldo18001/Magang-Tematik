library(readxl)
y <- read_excel("cewe.xlsx")
x <- read_excel("cowo.xlsx")

expense <- read_excel("expense.xlsx")
e <- expense$biaya
source("function.R")
annuity(0.06,10,20)
premik(0.0)