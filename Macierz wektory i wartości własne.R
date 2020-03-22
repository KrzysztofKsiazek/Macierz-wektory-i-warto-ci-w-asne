file.create('pliktex02.tex')
funkcja <- function(A){
  if(class(A)!='list'){
    print('Musisz podaæ ci¹g macierzy w liœcie')
  }
  else{
    cat('\\documentclass[10pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage{amsmath}
\\usepackage{amsfonts}
\\usepackage{amssymb}
\\usepackage[T1]{fontenc}
\\begin{document}
\\begin{LARGE}
\\begin{center}\\textbf{Projekcik}\\\\ \\textbf{Krzysztof Ksi\\k{a}\\.zek}\\end{center}
\\end{LARGE}',file='pliktex02.tex',append=TRUE)
        write('Utworzyc plik \\textit{pliktex02.tex} . Napisac funkcj\\k{e}, ktora jako argument przyjmie wektor (ci\\k{a}g) macierzy i zapisze w pliku \\textit{pliktex02.tex} macierze maj\\k{a}ce rzeczywiste wartosci w\\l{}asne oraz wektory w\\l{}asne tych macierzy. Wspo\\l{}rz\\k{e}dne wektorow w\\l{}asnych zaokr\\k{a}glic do jednego miejsca po przecinku w gore. Sprawdzic mo\\.zliwosc kompilacji pliku \\textit{pliktex02.tex}.\\\\ \\\\',file='pliktex02.tex',append=TRUE)
        n <-length(A)
        for(i in 1:n){
          wymiar <- dim(A[[i]])
          if(wymiar[1] != wymiar[2]){
            print("macierz musi byæ kwadratowa")
          }
          else{
            write('\\noindent\\rule[0.5cm]{\\textwidth}{1pt}','pliktex02.tex',append=TRUE)
            write(paste('\\textbf{Macierz ',i,':}\\\\'),'pliktex02.tex',append = TRUE)
            macierz<-function(m){
              znaki<- '$$\\begin{pmatrix}'
              for(i in 1:nrow(m)){
                for(n in 1:ncol(m)){
                  if(n<ncol(m)){
                    znaki<-paste(znaki,m[i,n],'&')
                  }
                  if(n==ncol(m)){
                    znaki<-paste(znaki,m[i,n],'\\\\')
                  }
                }
              }
              znaki<-paste(znaki,'\\end{pmatrix}$$')
              #print(znaki)
              write(znaki,'pliktex02.tex',append = TRUE)
            }
            macierz(A[[i]])
            e <- eigen(A[[i]]) 
            WW <- e$values 
            VW <- e$vectors
            if(class(WW)!= 'complex'){
            ilosc <- length(WW)
            cat('Wartosci w\\l{}asne:', WW, '\\\\',file='pliktex02.tex',append = TRUE )
            for(j in 1:ilosc){
              wszystko = paste("Dla wartosci w\\l{}asnej  $\\lambda = ", WW[j], "$", "\\\\" )
              write(wszystko,'pliktex02.tex',append = TRUE)
              write('Przyporz\\k{a}dkowywujemy nast\\k{e}pujce wektory w\\l{}asne: \\','pliktex02.tex',append = TRUE)
                znaki<- '$$ \\begin{pmatrix} '
                for(y in 1:ncol(VW)){
                  if(y <ncol(VW)){
                  znaki<-paste(znaki,ceiling(VW[y,j]*10)/10,'&')}
                  else{
                    znaki<-paste(znaki,ceiling(VW[y,j]*10)/10,'\\\\')
                  }
                }
              
              znaki<-paste(znaki,'\\end{pmatrix}$$')
              #print(znaki)
              write(znaki,'pliktex02.tex',append = TRUE)
            }
            }
            else{
              cat('Wartosci wlasne oraz wektory wlasne nie sa liczbami rzeczywistymi',file='pliktex02.tex',append = TRUE )
            }
          }
        }
  }
  cat('\\end{document}',file='pliktex02.tex',append=TRUE)}


  

lista<-list(A,B,C)
lista
A <- matrix(data=c(1,2,0,0,2,0,-2,-2,-1),nrow =3,ncol=3,byrow=TRUE)
B <- matrix(data=c(3,2,4,1), nrow=2, ncol=2, byrow=TRUE)
C <- matrix(data=c(0,1,-1,0),2,2,byrow=TRUE)

funkcja(lista)
file.show('pliktex02.tex')
file.remove('pliktex02.tex')

file.exists('pliktex06.tex')

dim(A[[1]])
e <- eigen(B) 
WW1 <- e$values 
(VW1 <- e$vectors)
VW1[2][1]
class(VW1)
