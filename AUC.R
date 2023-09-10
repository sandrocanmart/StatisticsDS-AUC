####UTILITIES#####
rm(list = ls())
library(dplyr)
#library(stats)
df <- data.frame(Score= c(150,180,200,205,230,260,280,300,150,190,200,250,260),
                 Status=c("Good","Good","Good","Good","Good","Good","Good","Good","Bad","Bad","Bad","Bad","Bad"))
#View(df)

#labelliamo "good" e "bad" con valori 1 e 0 con metodo label-encoding
Status_encoding <- c("Good"=0,"Bad"=1)
df$Status <- Status_encoding[df$Status]

df_sorted <- df %>% 
  arrange(df$Score)
#View(df_sorted)
####METODO 1 CONCORDANT####
# Definiamo una funzione per calcolare C(Si) per un dato score Si che determina il numero di istanze "bads" con score inferiore ad Si
calculate_CSi <- function(score, df) {
  # Filtriamo le righe con punteggio inferiore a Si
  filtered_df <- df[df$Score < score, ]
  
  # Calcoliamo C(Si) come la somma dei cattivi per quegli score
  CSi <- sum(filtered_df$Status == 1)
  
  return(CSi)
}

# Identifichiamo i punteggi Si con almeno una buona condizione (ng(Si) > 0)
unique_scores <- unique(df_sorted$Score)
scores_with_good_status <- unique_scores[sapply(unique_scores, function(score) any(df_sorted[df_sorted$Score == score, ]$Status == 0))]

nC <- sum(sapply(scores_with_good_status, function(score) {
  ng_Si <- sum(df_sorted[df_sorted$Score == score, ]$Status == 0)
  CSi <- calculate_CSi(score, df_sorted)
  ng_Si * CSi
}))

print(nC)


#conta la cardinalità di istanze con Status=0,
#che dovrebbe rappresentare il numero di casi "buono".
Ng <- function(data) {
  count_0 <- sum(data$Status == 0)
  return(count_0)
}

#conta la cardinalità di istanze con Status=1,
#che dovrebbe rappresentare il numero di casi "cattivo".
Nb <- function(data) {
  count_1 <- sum(data$Status == 1)
  return(count_1)
}

Ng_<- Ng(df_sorted)
Nb_ <- Nb(df_sorted)

print(paste("Numero di record con classe 0 =", Ng_))
print(paste("Numero di record con classe 1 =", Nb_))


# Funzione per calcolare Ng_s, definita come il numero di istanze good con punteggio "s"
Ng_s <- function(data) {
  Ng_result <- Ng(data)  
  Ng_s_values <- numeric(nrow(data))  
  
  for (score in data$Score) {
    count_0 <- sum(data$Status[data$Score == score] == 0)
    Ng_s_value <- count_0 / Ng_result
    Ng_s_values[data$Score == score] <- Ng_s_value
  }
  
  return(Ng_s_values)
}

# Funzione per calcolare Nb_s, definita come il numero di istanze bad con punteggio "s"
Nb_s <- function(data) {
  Nb_result <- Nb(data)  
  Nb_s_values <- numeric(nrow(data))  
  
  for (score in data$Score) {
    count_1 <- sum(data$Status[data$Score == score] == 1)
    Nb_s_value <- count_1 / Nb_result
    Nb_s_values[data$Score == score] <- Nb_s_value
  }
  
  return(Nb_s_values)
}


Ng_s_values <- Ng_s(df_sorted)
Nb_s_values <- Nb_s(df_sorted)

# integriamo i risultati delle funzioni impletate di sopra dentro il nostro dataframe
df_sorted$Ng_s <- Ng_s_values
df_sorted$Nb_s <- Nb_s_values

#View(df_sorted)
#eliminiamo i record con score duplicati
df_sorted <- subset(df_sorted, !duplicated(Score))

#quanto segue concorre a calcolare il numero di coppie pari con punteggio "s"
t_s<-(df_sorted$Ng_s* df_sorted$Nb_s)
nT = sum(t_s != 0)


#printiamo i risultati dell'area sotto la curva adoperando la formula proposta.
perc_concordant<-((nC/(Ng_*Nb_))+0.5*(nT/(Ng_*Nb_)))
perc_concordant



##########METODO 2 GEOMETRICO######
# FSG E FSB
# Calcoliamo la distribuzione cumulativa dei "buoni" (F(s|G)) per ciascun punteggio Si
F_s_G <- numeric(nrow(df_sorted)) 
cumulative_G <- cumsum(df_sorted$Status == 0) / sum(df_sorted$Status == 0)  

for (i in seq_along(cumulative_G)) {
  F_s_G[i] <- cumulative_G[i]
}
#aggiungiamo i risultati dei calcoli di sopra in una apposita colonna nel nostro dataframe
df_sorted$F_s_G <- F_s_G

# Calcoliamo la distribuzione cumulativa dei "cattivi" (F(s|B)) per ciascun punteggio Si
F_s_B <- numeric(nrow(df_sorted))  
cumulative_B <- cumsum(df_sorted$Status == 1) / sum(df_sorted$Status == 1)  

for (i in seq_along(cumulative_B)) {
  F_s_B[i] <- cumulative_B[i]
}
#aggiungiamo i risultati dei calcoli di sopra in una apposita colonna nel nostro dataframe
df_sorted$F_s_B <- F_s_B

#GRAFICO
#dev.off() = se da prblemi usare questo
library(ggplot2)

roc_data <- data.frame(x = cumulative_G, y = cumulative_B)

roc_plot <- ggplot(roc_data, aes(x = x, y = y)) +
  geom_line() +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(title = "Curva ROC",
       x = "F(s|G)",
       y = "F(s|B)") +
  theme_minimal()


print(roc_plot)


#cancelliamo i valori di "score" duplicati
df_sorted <- subset(df_sorted, !duplicated(Score))

#calcolo della funzione che calcola e somma le aree, afferente al metodo geometrico
calculate_AUC1 <- function(F_s_G, F_s_B) {
  AUC <- numeric(length(F_s_G))  # Inizializza il vettore AUC
  
  for (i in seq_along(F_s_G)) {
    if (i == 1) {
      if (!is.na(F_s_G[i]) && !is.na(F_s_B[i]) && F_s_G[i] != 0 && F_s_B[i] != 0) {
        AUC[i] <- ((F_s_G[i] * F_s_B[i]) / 2)
      } else if (!is.na(F_s_G[i]) && !is.na(F_s_B[i]) && F_s_G[i] == 0 && F_s_B[i] == 0) {
        AUC[i] <- 0
      } else if (!is.na(F_s_G[i]) && !is.na(F_s_B[i]) && F_s_G[i] == 1 && F_s_B[i] == 1) {
        AUC[i] <- 1
      } else if (!is.na(F_s_G[i]) && !is.na(F_s_B[i]) && F_s_G[i] == 0 && F_s_B[i] != 0) {
        AUC[i] <- ((1 * F_s_B[i]) / 2)
      }
    } else {
      if (!is.na(F_s_B[i]) && !is.na(F_s_B[i+1])) {
        if (F_s_B[i] == F_s_B[i+1]) {
          AUC[i] <- (F_s_G[i+1] - F_s_G[i]) * F_s_B[i]
        } else if (F_s_B[i] < F_s_B[i+1]) {
          if (!is.na(F_s_B[i-1]) && !is.na(F_s_G[i-1])) {
            AUC[i] <- AUC[i - 1] + (((F_s_B[i-1] + F_s_B[i]) * (F_s_G[i] - F_s_G[i-1])) / 2)
          }
        }
      }
    }
  }
  
  return(AUC)
}

#stampiamo i risulati del metodo
AUC2 <- calculate_AUC1(df_sorted$F_s_G, df_sorted$F_s_B)
print(AUC2)
print(sum(AUC2))




########METODO 3 Wilcoxon rank-sum statistic#########
#I ranghi vengono assegnati in base all'ordine dei valori,
#quindi l'osservazione con il valore più basso ,
#riceve il rango 1, 
# l'osservazione con il secondo valore più basso  riceve il rango 2, e così via.
Status_encoding <- c("Good"=0,"Bad"=1)
df$Status <- Status_encoding[df$Status]

df_sorted <- df %>% 
  arrange(df$Score)
#View(df_sorted)


# Aggiungiamo una colonna con i ranghi all'interno del nostro dataframe
df_sorted$Rank <- rank(df_sorted$Score)
df_sorted
RB <- sum(df_sorted$Rank[df_sorted$Status == 1])
RG <- sum(df_sorted$Rank[df_sorted$Status == 0])

cat("Sum of ranks for bads (RB):", RB, "\n")
cat("Sum of ranks for goods (RG):", RG, "\n")


# Calcoliamo la statistica del Wilcoxon Rank-Sum
W_r_s <-  (RG - (Ng_ * (Ng_ + 1)) / 2) / (Ng_ * Nb_)
print(W_r_s)


#####ANALISI AGGIUNTIVA######
#la statistica del Wilcoxon Rank-Sum con una
#distribuzione nota (solitamente una distribuzione
#di probabilità critica) per determinare la significatività 
#statistica del risultato.
group_good <- c(150, 180, 200, 205, 230, 260, 280, 300)
group_bad <- c(150, 190, 200, 250, 260)
#quando ci sono due o più osservazioni con lo stesso valore,
#il che può influenzare il calcolo del test del Wilcoxon 
#Rank-Sum. il calcolo esatto del valore
#p non è possibile a causa dei legami nei dati
#utilizzando l'opzione exact = FALSE nella funzione 
#wilcox.test(), che utilizzerà un'approssimazione anziché 
#un calcolo esatto.


# Eseguiamo il test del Wilcoxon Rank-Sum con approssimazione
result <- wilcox.test(group_good, group_bad, alternative = "two.sided", exact = FALSE)

print(result)
#Il valore p è un indicatore di quanto sia probabile
#ottenere un risultato simile o più estremo rispetto 
#a quello osservato, supponendo che l'ipotesi nulla sia vera. 
#In questo caso, l'ipotesi nulla è che non ci sia alcuna 
#differenza significativa tra i due gruppi (il gruppo "Good" 
#e il gruppo "Bad").

#Un valore p elevato, come 0.5566, suggerisce che c'è
#una probabilità piuttosto alta di ottenere un risultato
#simile a quello osservato sotto l'ipotesi nulla.
#In altre parole, la differenza nei ranghi tra i due gruppi 
#potrebbe essere dovuta al caso, piuttosto che a una vera 
#differenza nelle distribuzioni.
#Quindi, in base a questo valore p, non ho
 #prove sufficienti per rigettare l'ipotesi nulla di
 #assenza di differenza significativa tra i gruppi.
####FINE ANALISI AGGIUNTIVA####

####CONCLUSIONI E STAMPA DEI RISULTATI OTTENUTI#####

perc_concordant
print(sum(AUC2))
W_r_s

















