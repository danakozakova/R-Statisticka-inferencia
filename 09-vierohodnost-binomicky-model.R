# Príklad 9.1 (Maximálne vierohodný odhad parametra binomického modelu)
##Nech X ~ Bin(N, p) a realizácie X sú x.Predpokladajme, že sme pozorovali (i) x = 2,
## (ii) x = 10 a (iii) x = 18 úspechov v N = 20 pokusoch.

##Dosadením do vzorcov stanovte presnú hodnotu odhadu parametra p
# a odhad rozptylu odhadu parametra 

x <- c(2, 10, 18) # vektor poctu uspechov pre (i)-(iii)
N <- 20 # pocet Bernoulliho pokusov N
p <- x/N # vektor MLE odhadu parametra p pre (i)-(iii) 
Var_p <- p*(1-p)/N # vektor rozptylu odhadu parametra p pre (i)-(iii) 
 # sumarizacna tabulka vysledkov
tab <- data.frame(cbind(p, Var_p))
rownames(tab) <- paste('x =', x)
tab

tab_comp <- data.frame(round(t(p), 6))
names(tab_comp) = c('MLE_i', 'MLE_ii', 'MLE_iii')

##################################################################
## 3. Pomocou maximalizácie logaritmu vierohodnostnej funkcie binomického
## modelu nájdite maximálne vierohodný odhad parametra p pre situácie (i), (ii) a (iii). 
## Pre každú situáciu vykreslite krivku logaritmu vierohodnostnej funkcie binomického modelu 
## spolu s maximálne vierohodným odhadom parametra p. Maximalizáciu vykonajte:

## a. pomocou funkcie optimize():
# logaritmus verohodnostnej funkcie rozdelenia Bin(N, p) 
l <- function(p, x, N){
  return (x*log(p) + (N-x)*log(1-p))
}  

# vykreslenie pomoocou funkcie optimize()
mle_bin_optim <- function(x, N){
  poptim <- optimize(l, c(0.0001, 0.9999), x = x, N = N, maximum = T) 
  # MLE odhad p pomocou funkcie optimize(), na výstupe je #maximum (resp. minimum) a #objective
  # v #maximum je lokalizacia a v #objective je hodnota toho maxima / minima
  pmax <- poptim$maximum # vyber maxima zo vsetkych vystupov funkcie optimize() 
  par( mar = c(5,4,2,2)) # okraje grafu 5, 4, 2, 2
  p <- seq(from = 0.00001, to = 0.99999, length = 2048) # postupnost od 0.00001 do 0.99999 o dlzke 2048
  plot(p, l(p, x, N), ylim = c(-100, 5), type = 'l', ylab = '', xlab = '') # graf l(p|x)
  abline(v = pmax, col = 'red', lwd = 1, lty = 2) # zvisla cervena ciara (MLE odhad p)
  mtext('p', side = 1, line = 2.1)
  mtext(paste('maximum v bode p = ', round(pmax, 2)), side = 1, line = 3.2)
  mtext('l(p|x)', side = 2, line = 2.1)
  return(pmax) # vrat MLE odhad parametra p
}

MLE_i <- mle_bin_optim(x = x[1], N = 20) # graf + MLE odhad p pre (i) 
MLE_ii <- mle_bin_optim(x = x[2], N = 20) # graf + MLE odhad p pre (ii)
MLE_iii <- mle_bin_optim(x = x[3], N = 20) # graf + MLE odhad p pre (iii)

tab_opt <- data.frame(rbind(cbind(x, round(c(MLE_i, MLE_ii, MLE_iii), 6)))) # sumarizacna tabulka MLE odhadov pre (i)-(iii)
names(tab_opt) <- c('x', 'p_opt')
print('Sumarizacna tabulka MLE odhadov s vyuzitim optimize()')
tab_opt

tab_comp <- rbind(tab_comp,
                   data.frame(round(cbind(MLE_i, 
                                          MLE_ii, 
                                          MLE_iii), 6)))

## b. pomocou vlastnorucne naprogramovanej Newton-Raphsonovej metody RBin():
# skore funkcie pre Bin(N, p) 
S <- function(p, x, N){ # skore funkcia S(p) rozdelenia Bin(N, p)
  return (x /p - (N-x)/(1-p))
}  

# Fisherovo informacne cislo I(p) rozdelenia Bin(N, p)
I <- function(p, N) {
  return (N /(p*(1-p)))
  } 

NRbin <- function(p0, x, N, treshold = 0.00005, max_it = 100){ 
  for (i in 1:max_it){ # cyklus (konci najneskor po max_it iteraciach) 
    p1 <- p0 + S(p0, x, N)/I(p0, N) # iteracny krok
    kriterium <- abs(p1-p0) # aktualizacia rozhodovacieho kriteria 
    p0 <- p1 # aktualizacia premennej p0
    pmax <- p1 # aktualizacia MLE odhadu p
    if(kriterium < treshold){break} 
    # kontrola rozhodovacieho kriteria; splnene -> ukoncenie cyklu
  }
  return(list(pmax = pmax, k = i)) # vystup: MLE odhad p, pocet iteracii k
}

mle_bin_nr <- function(x, N, p0 = 0.001){
  pmax <- NRbin(p0, x, N)$pmax # MLE odhad p pomocou funkcie NRbin() 
  par(mar = c(5, 4, 2, 2)) # okraje grafu 5, 4, 2, 2
  p <- seq(from = 0.00001, to = 0.9999, length = 2048) # postupnost p
  plot(p, l(p, x, N), ylim = c(-100, 5), type = 'l', ylab = '', xlab = '') # graf l(p|x)
  abline(v = pmax, col = 'red', lwd = 1, lty = 2 ) # zvisla cervena ciara (MLE odhad p)
  mtext('p', side = 1, line = 2.1)
  mtext(paste('maximum v bode p = ', round(pmax, 2)), side = 1, line = 3.2)
  mtext('l(p|x)', side = 2, line = 2.1)
  return(pmax) # vrat MLE odhad parametra p
}

MLE_i <- mle_bin_nr(x = x[1], N = N) # graf + MLE odhad p pre (i) 
MLE_ii <- mle_bin_nr(x = x[2], N = N) # graf + MLE odhad p pre (ii)
MLE_iii <- mle_bin_nr(x = x[3], N = N) # graf + MLE odhad p pre (iii)

tab_NR <- data.frame(rbind(cbind(x, round(c(MLE_i, MLE_ii, MLE_iii), 6)))) # sumarizacna tabulka MLE odhadov pre (i)-(iii)
names(tab_NR) <- c('x', 'p_NR')
print('Sumarizacna tabulka MLE odhadov s vyuzitim vlastnej funkcie pre Newton-Raphson metodu()')
tab_NR

tab_comp <- rbind(tab_comp,
                  data.frame(round(cbind(MLE_i, 
                                         MLE_ii, 
                                         MLE_iii), 6)))

## c. pomocou vlastnoručne naprogramovanej metódy sečníc MSbin():
MSbin <- function(p01, p02, x, N, treshold = 1e-5, max_it = 100){
  p1 <- p01 # iniciacia premennej p1 (p01)
  p2 <- p02 # iniciacia premennej p2 (p02)
  for (i in 1:max_it){ # cyklus (konci najneskor po max_it iteraciach)
    df <- S(p2, x, N)*(p2-p1)/(S(p2, x, N) - S(p1, x, N)) # prepocet clena df
    p3 <- p2 - df # iteracny krok (s vyuzitim df)
    p1 <- p2 # aktualizacia premennej p1 
    p2 <- p3 # aktualizacia premennej p2 
    if (S(p3, x, N) == 0) { # kontrola presneho korena funkcie S(p) -> S(p3) = 0 -> koniec a vystup
      return(result <- list(pmax = p3, k = i, chyba = 0)) # vystup
    }
    else if (abs(df) < treshold){ # kontrola rozh. kriteriu; splnene -> koniec a vystup
      chyba = S(p2, x, N) * (p2 - p1) / (S(p2, x, N) - S(p1, x, N)) # vypocet chyby 
      return(result <- list(pmax = p3, k = i, chyba = chyba)) # vystup
    }
  }
}

mle_bin_ms <- function(x, N, p01 = 0.001, p02 = 0.999){
  pmax <- MSbin(p0, p1, x, N)$pmax # MLE odhad p pomocou
  par(mar = c(5, 4, 2, 2)) # okraje grafu 5, 4, 2, 2
  p <- seq(from = 0.00001, to = 0.9999, length = 2048) # postupnost 
  plot(p, l(p, x, N), ylim = c(-100, 5), type = 'l', ylab = '', xlab = '') # graf l(p|x)
  abline(v = pmax, col = 'red', lwd = 1, lty = 2) # zvisla cervena ciara (MLE odhad p)
  mtext('p', side = 1, line = 2.1)
  mtext(paste('maximum v bode p = ', round(pmax, 2)), side = 1, line = 3.2)
  mtext('l(p|x)', side = 2, line = 2.1)
  return(pmax) # vrat MLE odhad parametra p
}

MLE_i <- mle_bin_ms(x = x[1], N = N) # graf + MLE odhad p pre (i) 
MLE_ii <- mle_bin_ms(x = x[2], N = N) # graf + MLE odhad p pre (ii)
MLE_iii <- mle_bin_ms(x = x[3], N = N) # graf + MLE odhad p pre (iii)
tab_MS <- data.frame(rbind(cbind(x, round(c(MLE_i, MLE_ii, MLE_iii), 2)))) # sumarizacna tabulka MLE odhadov pre (i)-(iii)
names(tab_MS) <- c('x', 'p_MS')
print('Sumarizacna tabulka MLE odhadov s vyuzitim vlastnej funkcie pre metodu secnic')
tab_MS

tab_comp <- rbind(tab_comp,
                  data.frame(round(cbind(MLE_i, 
                                         MLE_ii, 
                                         MLE_iii), 6)))
rownames(tab_comp) <- c('exaktný výpočet', 'Odhad - optimize()', 'odhad - Newton-Raphsonova metóda', 'odhad - metóda sečníc')
tab_comp

## 4. Pre situácie (i), (ii) a (iii) vykreslite krivku vierohodnostnej funkcie binomického modelu spolu s maximálne vierohodným odhadom parametra p
# získaným optimalizáciou vierohodnostnej funkcie L (p|x) pomocou funkcie optimize().
L <- function(p, x, N) { # vierohodnostna funkcia L(p|x) pre Bin(N, p)
  return ( choose(N, x) * p^x * (1-p)^(N-x))
} 

mle_bin_optim_L <- function(x, N){
  poptim <- optimize(L, c(0.0001, 0.9999), x = x, N = N, maximum = T) # MLE odhad p pomocou L(p|x) a optimize()
  pmax <- poptim$maxim # vyber maxima zo vsetkych vystupov funkcie optimize()
  par(mar = c(5,4, 2, 2)) # okraje grafu  
  p <- seq(from = 0.00001, to = 0.99999, length = 2048) # sekvencia p od 0.00001 do 0.99999 s dlzkou 2048
  plot(p, L(p, x, N), type = 'l', ylab = 'L(p|x)', xlab = '', las = 1) # graf L(p|x)  
  
  abline(v = pmax, col = 'red', lwd = 1, lty = 1) # zvisla cervena ciara (MLE odhad p) 
  mtext('p', side = 1, line = 2.1)
  mtext(paste('maximum v bode p = ', round(pmax, 2)), side = 1, line = 3.2)
  return(pmax) # vrati MLE odhad parametra p 
}

## Poznámka: Tie grafy sa mierne lisia od zadania. Rozdiel je zjavne v tom, ze ja som definovala vypocet hodnoty vierohodnostnej funkcie L celej, 
## vo vzore je zjavne len jadro nezavisle od parametra p
MLE_i <- mle_bin_optim_L(x[1], N = N) # graf + MLE odhad p pre (i)
MLE_ii <- mle_bin_optim_L(x[2], N = N) # graf + MLE odhad p pre (ii)
MLE_iii <- mle_bin_optim_L(x[3], N = N) # graf + MLE odhad p pre (iii)

tab_opt_L <- data.frame(rbind(cbind(x, round(c(MLE_i, MLE_ii, MLE_iii), 6)))) # sumarizacna tabulka MLE odhadov funkcie L(p|x) pre (i)-(iii)
names(tab_opt) <- c('x', 'p_opt')
print('Sumarizacna tabulka MLE odhadov funkcie L(p|x) s vyuzitim optimize()')
tab_opt

## Príklad 9.2: Kvadratická aproximácia v binomickom modeli
# 1. Nakreslite škálovaný logaritmus funkcie vierohodnosti binomického rozdelenia. Na x-ovej osi bude parameter p a na osi y-ovej osi bude škálovaný logaritmus.
# Porovnajte škálovaný logaritmus s kvadratickou aproximáciou vypočítanou pomocou Taylorovho rozvoja
  
kvadr_aprox <- function(p, x, N, max_ylim, plot = 'aproximace'){
  l <- function (p, x, N){ return (x*log(p) + (N-x)*log(1-p))} # logaritmicka vierohodnostna funkcia
  pmax <- optimize(l, c(0.0001, 0.9999), x = x, N = N, maximum = T)$maximum # MLE odhad parametra p
  lr <- l(p, x, N) - l(pmax, x, N)  # skalovany logaritmus vierohodnostnej funkcie v sekvencii p
  FIM <- function(p, N) {return (N /(p*(1-p)))}  # Fisherovo informacne cislo (FIM)
  Ip <- FIM(pmax, N) # hodnota FIM pre p = pmax
  kv_aproximace <- -1 / 2 * Ip * (p - pmax) ^ 2 # kvadraticka aproximacia
  
  # graf kvadratickej aproximacie
  if (plot == 'aproximace'){
    par(mar = c(4, 4, 1, 1)) # nastavenie okrajov 4, 4, 1, 1
    plot(p, lr, ylim = c(-2.5, 0.6), xlab = '', ylab = 'skalovany l(p|x)', 
         type = 'l', col = 'red', lty = 5, lwd = 2) # graf skalovaneho logaritmu
    lines(p, kv_aproximace, col = 'black', lty = 1, lwd = 2) # krivka kvadratickej aproximacie
    mtext('p', side = 1, line = 2.1) # popis osi x
    mtext(paste('x = ', x , ', N = ', N, sep = ''), side = 1, line = 3.2) # druhy popis osi x
    legend('topleft', legend = c('škálovana l(p|x)', 'kvadr. aproximácia'),
           col = c('red', 'black'), lty = c(5, 1), lwd = 2, bty = 'n')
  }
  
  # graf linearity skalovaneho logaritmu
  if (plot == 'linearita') {
    S <- function (p, x, N){ return (x /p - (N-x)/(1-p)) } # skore funkcia
    Sp <- S(p, x, N) # vypocet konkretnej hodnoty skore funkcie
    par(mar = c(4, 4, 1, 1)) # nastavenie okrajov 4, 4, 1, 1
    plot(Ip ^ (1 / 2) * (p - pmax), - Ip ^ (-1 / 2) * Sp, 
         xlim = c(-2, 2), ylim = c(-2, max_ylim), 
         xlab = '', ylab = '',
         type = 'l', lty = 1, lwd = 1) # graf skalovaneho logaritmu
    abline(a = 0, b = 1, lty = 2, lwd = 1, col = 'red') # referencna priamka y = x
    mtext(bquote(paste(I^{1 / 2} * (widehat(p)) * (p - widehat(p)))), 
          side = 1, line = 2.1) # popis osi x
    mtext(paste('x = ', x , ', N = ', N, sep = ''), side = 1, line = 3.2) # druhy popis osi x
    mtext(bquote(paste(-I^{-1 / 2} * (widehat(p)) * S(p))), 
          side = 2, line = 2.1) # popis osi y
    legend('topleft', legend = c('škálovana skóre funkcia', 'priamka y = x'),
           col = c('black','red'), lty = c(1, 5), lwd = 2, bty = 'n')
  }
}

p <- seq(0.5, 0.99999, length = 2048)
max_ylim = 10

# Grafy: Kvadratická aproximácia škálovaného logaritmu funkcie vierohodnosti binomického rozdelenia
MLE_i_a <- kvadr_aprox(p, 8, 10, max_ylim, plot = 'aproximace')  # graf + MLE odhad p pre (i)
MLE_ii_a <- kvadr_aprox(p, 80, 100, max_ylim, plot = 'aproximace') # graf + MLE odhad p pre (ii)
MLE_iii_a <- kvadr_aprox(p, 800, 1000, max_ylim, plot = 'aproximace') # graf + MLE odhad p pre (iii)

# Grafy: Asymptotická linearita skóre funkcie logaritmu funkcie vierohodnosti binomického rozdelenia
MLE_i_l <- kvadr_aprox(p, 8, 10, max_ylim, plot = 'linearita') # graf + MLE odhad p pre (i)
MLE_ii_l <- kvadr_aprox(p, 80, 100, max_ylim, plot = 'linearita') # graf + MLE odhad p pre (ii)
MLE_iii_l <- kvadr_aprox(p, 800, 1000, max_ylim, plot = 'linearita') # graf + MLE odhad p pre (iii)
