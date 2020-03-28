file.remove('pliktex09.tex')
file.create('pliktex09.tex')
czy_diag(przykladowalista)
file.show('pliktex09.tex')
#DANE
A <- matrix(c(1,2,3,4,5,6),2,3)
B <- matrix(c(3,2,4, 2,0, 2, 4,2,3 ), 3, 3)
C <- matrix(c(1,2,3,5,-4,3,2,6,7),3,3)
przykladowalista <- list(A,B,C)

czy_diag<-function(ciag_macierzy){
  write("\\documentclass[10pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage{amsmath}
\\usepackage{amsfonts}
\\usepackage{amssymb}
\\usepackage{graphicx}
      \\begin{document}
      \\begin{center}\\Huge{
      Projekt}
      \\end{center}
      Napisac funkcje, ktora jako argument przyjmie wektor (ciag) macierzy i dla kazdej macierzy diagonalizowalnej zapisze w pliku macierz oraz jej postac diagonalna. Wyrazy postaci diagonalnej zaokraglamy do jednego miejsca po przecinku w dol. Sprawdzic mozliwosc kompilacji pliku \"pliktex09.tex\" . \\\\ \\\\",
        file = 'pliktex09.tex', append = TRUE)
  m<-1
  for(macierz in ciag_macierzy ){
    if(nrow(macierz)!=ncol(macierz)){
      print(paste('Macierz',m,'nie jest kwadratowa'))
    }
    else if(is.complex(eigen(macierz)$val)==TRUE){
      print(paste('Macierz',m,'ma zespolone wartosci wlasne'))
    }
    else{
      temp_val <- eigen(macierz)$val
      temp_vec <- eigen(macierz)$vec
      if(abs(det(temp_vec)-0.01)<=0.01){
        print(paste('Macierz',m,'nie jest diagonalizowalna'))
      }
      else{
        write('\\noindent\\rule[0.5cm]{\\textwidth}{1pt}','pliktex09.tex',append=TRUE)
        write('Macierz: \\\\','pliktex09.tex',append = TRUE)
        znaki<- '$$\\begin{pmatrix}'
        for(i in 1:nrow(macierz)){
          for(n in 1:ncol(macierz)){
            if(n<ncol(macierz)){
              znaki<-paste(znaki,macierz[i,n],'&')
            }
            if(n==ncol(macierz)){
              znaki<-paste(znaki,macierz[i,n],'\\\\')
            }
          }
        }
        znaki<-paste(znaki,'\\end{pmatrix}$$')
        #print(znaki)
        write(znaki,'pliktex09.tex',append = TRUE)
        write('Postac diagonalna macierzy: \\\\','pliktex09.tex',append = TRUE)
        #########################################
        diagonalized <-diag(diag(temp_val))
        diagonalized2 <- solve(temp_vec)%*%macierz%*%temp_vec
        if(as.vector(abs(diagonalized2-0.01*diag(dim(macierz)[1]))<=diagonalized)&&TRUE){
          print(paste('Macierz', m , 'jest diagonalizowalna'))
          e <- eigen(macierz)
          WW1 <- e$values
          WW2 <- floor(WW1*10)/10
          hh <- diag(WW2, ncol(macierz), ncol(macierz))
          print(hh)
          znaki1<- '$$\\begin{pmatrix}'
          for(i in 1:nrow(hh)){
            for(n in 1:ncol(hh)){
              if(n<ncol(hh)){
                znaki1<-paste(znaki1,hh[i,n],'&')
              }
              if(n==ncol(hh)){
                znaki1<-paste(znaki1,hh[i,n],'\\\\')
              }
            }
          }
          znaki1<-paste(znaki1,'\\end{pmatrix}$$')
          write(znaki1,'pliktex09.tex',append = TRUE)
        }
      }
    }
    m=m+1
  }
  write("\\end{document}", file = 'pliktex09.tex', append = TRUE)
  
}

