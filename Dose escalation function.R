# Calculating the probability of stopping at each successive dose of a Phase I 
# Dose Escalation trial.    

# This function is useful in helping to determining the maximum tolerable dose 
# of a new drug. 


stopping_probs <- function(p){
  # Probability of stopping at dose 1; step 1:    
  P11 <- 1 - pbinom(1, 3, p[1])
  # Probability of stopping at dose 1; step 2:   
  P12 <- dbinom(1, 3, p[1])*(1-dbinom(0, 3, p[1]))   
  # Probability of stopping at dose 1:  
  Pstop_at1 <- P11 + P12
  # Probability of stopping at dose 2; step 1:    
  P21 <- 1 - pbinom(1, 3, p[2])
  # Probability of stopping at dose 2; step 2:   
  P22 <- dbinom(1, 3, p[2])*(1-dbinom(0, 3, p[2]))   
  # Probability of stopping at dose 2:  
  P2dot <- P21 + P22  
  # Probability of stopping at dose 2 having already made it past dose 1:  
  Pstop_at2 <- P2dot*(1-Pstop_at1)
  # Probability of stopping at dose 3; step 1:    
  P31 <- 1 - pbinom(1, 3, p[3])
  # Probability of stopping at dose 3; step 2:   
  P32 <- dbinom(1, 3, p[3])*(1-dbinom(0, 3, p[3]))   
  # Probability of stopping at dose 3:  
  P3dot <- P31 + P32  
  # Probability of stopping at dose 3 having already made it past dose 1 and 2:  
  Pstop_at3 <- P3dot*(1-(Pstop_at2+Pstop_at1))
  # Probability of stopping at some dose:    
  Pstop <- Pstop_at1 + Pstop_at2 + Pstop_at3;
  
  print(paste("Probability of stopping at dose 1 = ", round(Pstop_at1,3)));
  print(paste("Probability of stopping at dose 2 = ", round(Pstop_at2,3)));
  print(paste("Probability of stopping at dose 3 = ", round(Pstop_at3,3)));
  print(paste("Probability of stopping at during the trial = ", round(Pstop,3)));
  print(paste("Probability of finishing the trial = ", round(1-Pstop,3)));
}




p <- c(.1, .22, .42)
stopping_probs(p)
