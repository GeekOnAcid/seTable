source("seTable.R")
#create example data
x <- rnorm(100, mean = 0, sd = 2)
y <- rnorm(100, mean = 1, sd = 2)
z <- rnorm(100, mean = 1, sd = 0.5)
X <- cbind(x, y, z)
#produce seTable and save it in .tex in current directory
seTable(X, 
        yoffset = -0.015, 
        unitlength = "5cm", 
        circlesize = 0.03,
        filename="seTable_example.tex")
#to insert table in latex document, use: 
#\input{seTable_example.tex}