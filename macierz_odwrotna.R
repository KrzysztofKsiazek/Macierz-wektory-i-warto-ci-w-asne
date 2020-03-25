funkcja <- function(A)
  {
    if(class(A)!='matrix')
     {
     print('A must to be a Matrix')
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
\\begin{center}\\textbf{Inverse Matrix}\\\\\\end{center}
\\end{LARGE}',file='macierz_odwrotna.tex',append=TRUE)
      if(dim(A)[1] != dim(A)[2])
        {
        write('Matrix must be square',file='macierz_odwrotna.tex',append=TRUE)
        cat('\\end{document}',file='macierz_odwrotna.tex',append=TRUE)
        print("Matrix muste be square")
        }
      else
        {
          write("Matrix",file='macierz_odwrotna.tex',append=TRUE)
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
          znaki<-paste(znaki,'\\end{pmatrix}$$')
          #print(znaki)
          write(znaki,'macierz_odwrotna.tex',append = TRUE)
          write("Inverse Matrix",file='macierz_odwrotna.tex',append=TRUE)
          znaki<- '$$\\begin{pmatrix}'
          for(i in 1:nrow(solve(A))){
            for(n in 1:ncol(solve(A))){
              if(n<ncol(solve(A))){
                znaki<-paste(znaki,ceiling(solve(A)[i,n]*100)/100,'&')
              }
              if(n==ncol(solve(A))){
                znaki<-paste(znaki,ceiling(solve(A)[i,n]*100)/100,'\\\\')
              }
            }
          }
          znaki<-paste(znaki,'\\end{pmatrix}$$')
          #print(znaki)
          write(znaki,'macierz_odwrotna.tex',append = TRUE)
          cat('\\end{document}',file='macierz_odwrotna.tex',append=TRUE)
          print("Matrix:")
          print(A)
          print("Inverse Matrix:")
          print(solve(A))
        }
     }
}
#wpisz macierz!!!
A <- matrix(data=c(2,1,5,3),nrow =2,ncol=2,byrow=TRUE)
setwd("C:/Users/Krzysiek/desktop/github/macierz odwrotna")
file.create('macierz_odwrotna.tex')
funkcja(A)
file.show('macierz_odwrotna.tex')
file.remove('macierz_odwrotna.tex')

