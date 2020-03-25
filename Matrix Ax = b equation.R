#wpisz macierz!!!
A <- matrix(data=c(2,1,5,3),nrow =2,ncol=2,byrow=TRUE)
#ustaw odpowiednią ścieżkę dostępu
setwd("C:/Users/Krzysiek/Desktop/github/Ax = b")

# funkcja pozwalająca usunąć plik file.remove("Matrix Ax = b equation.tex")
#funkcja tworząca plik.tex 
file.create("Matrix Ax = b equation.tex")
funkcja(A)
#pokazanie wyniku
file.show("Matrix Ax = b equation.tex")
#kod funkcji
funkcja <- function(A)
  {
    if(class(A)!='matrix')
     {
     print('A need to be matrix')
     }
    else
     {
       cat('\\documentclass[10pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage{amsmath}
\\usepackage{amsfonts}
\\usepackage{amssymb}
\\usepackage[T1]{fontenc}
\\begin{document}
\\begin{LARGE}
\\begin{center}\\textbf{Ax = b equation}\\\\\\end{center}
\\end{LARGE}',file='Matrix Ax = b equation.tex',append=TRUE)
          write("where b - vector and $b_j$ = j",file='Matrix Ax = b equation.tex',append=TRUE)
          #macierz A
          znaki<- '$$\\begin{pmatrix}'
          for(i in 1:nrow(A)){
            for(n in 1:ncol(A)){
              if(n<ncol(A)){
                znaki<-paste(znaki,ceiling(A[i,n]*100)/100,'&')
              }
              if(n==ncol(A)){
                znaki<-paste(znaki,ceiling(A[i,n]*100)/100,'\\\\')
              }
            }
          }
          znaki<-paste(znaki,'\\end{pmatrix}')
          #print(znaki)
          write(znaki,'Matrix Ax = b equation.tex',append = TRUE)
          
         
          
          #macierz x
          znaki<- '* \\begin{pmatrix}'
          for(i in 1:nrow(b)){
            znaki<-paste(znaki,ceiling(solve(A,b)[i][1]*100)/100,'\\\\')
          }
          znaki<-paste(znaki,'\\end{pmatrix}')
          write(znaki,'Matrix Ax = b equation.tex',append = TRUE)
          
          write("=",file='Matrix Ax = b equation.tex',append=TRUE)
          
          ##macierz b
          znaki<- ' \\begin{pmatrix}'
          h <- c()
          h[1] <- 1
          for(i in 2:nrow(A)){
            h[i]=i
          }
          b <- t(t(h))
          for(i in 1:nrow(b)){
            znaki<-paste(znaki,h[i][1],'\\\\')
          }
          znaki<-paste(znaki,'\\end{pmatrix} $$')
          write(znaki,'Matrix Ax = b equation.tex',append = TRUE)
          write("so $$ x = ",file='Matrix Ax = b equation.tex',append=TRUE)
          
          #wynik ostateczny
          znaki<- ' \\begin{pmatrix}'
          for(i in 1:nrow(b)){
            znaki<-paste(znaki,ceiling(solve(A,b)[i][1]*100)/100,'\\\\')
          }
          znaki<-paste(znaki,'\\end{pmatrix}')
          write(znaki,'Matrix Ax = b equation.tex',append = TRUE)
          
          cat('$$ \\end{document}',file='Matrix Ax = b equation.tex',append=TRUE)

        
     }
}

