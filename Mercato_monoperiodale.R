##################################################################
  Author: "Ruggero Cazzato"
  Title: "Appunti di Gestione del Rischio - Mercato Monoperiodale"
  Date: 27/04/2021
##################################################################

#input (esempio)
price <- c(95, 60, 90) #prezzo dei titoli al tempo 0
scenarios <- c(100, 100, 100, 100, 100, 0, -100, 0, 200) #prezzo dei titoli al tempo 1 per ogni scenario possibile
alpha <- c(2, -2, -1) #quantità dei titoli in portafoglio (il segno negativo vuol dire che sto shortando)

###funzione per calcolare valore del portafoglio in t = 0 e payoffs degli scenari (t = 1)
pf_value <- function(price, scenarios, quantity) {


index_title <- NULL
for (k in 1:length(price)){
  
  index_title[k] <- paste0("s", as.character(k)) 
  
}
  
price <- setNames(price, index_title)

num_scenarios <-length(scenarios)/length(price)

s1 <- matrix(data = scenarios, nrow = length(price), ncol = num_scenarios, byrow = TRUE) #valore dei titoli per ogni scenario

index_scen <- NULL
for (j in 1:num_scenarios) {
  
  index_scen[j] <- paste0("omega", as.character(j))
  
}

colnames(s1) <- index_scen

PF_alpha_1 <- sum(price * quantity) #valore del portafolio al tempo 0
payoffs <- quantity %*% s1 #valore del portafoglio al tempo 1 per ogni scenario identificato

for (i in 1:length(PF_alpha_1)) {
  
if (PF_alpha_1 == 0 & payoffs[i] > 0 ) {
  cat("There is an arbitrage opportunity \n")
} else if (PF_alpha_1 < 0 & payoffs[i] >= 0 ) {
  cat("There is an arbitrage opportunity \n")
} else {
  cat("No arbitrage opportunity \n")
}

}

cat("------------------------------------------ \n")

V <- cbind(PF_alpha_1, payoffs)
rownames(V) <- "V_alpha"

tab <- cbind(price, s1)
tab <- rbind(tab, V)
print(knitr::kable(tab, "simple", caption = "Portfolio Table"))

cat("------------------------------------------ \n")
cat("- The cost of the portfolio today is:", as.character(PF_alpha_1), "; \n")

best_scenario <- max(payoffs)
worst_scenario <- min(payoffs)
cat("- The best scenario is:", as.character(best_scenario), "; \n")
cat("- The worst scenario is:", as.character(worst_scenario), ". \n")

}

#output
valore_pf <- pf_value(price = price, scenarios = scenarios, quantity = alpha)

