#Qualitative Sample Size

#Qualitative research relies on "stopping rules" to dictate sample size. What is the chance of missing
#important information at various stopping rules? 

#Underlying Parameters

topics_per_interview_mu <- 5
topics_per_interview_sd <- 2
true_concerns_pop <- 50
importance_falloff <- 0.019 

#this model assumes linear falloff, therefore true_concerns_falloff*importance_falloff must be > 0 

if(true_concerns_pop*importance_falloff <= 0){
  printf("Warning: Topics with importance ratings below 0 cannnot be discovered.")
}

#create a vector of importances of the discoverable topics

importance_numbers <- seq(from=1,by=-(importance_falloff),length.out = true_concerns_pop)

#create a probablility space that allows the topics to be quickly sampled based on importance

choice_set <- 0
choice_set[1] <- importance_numbers[1]
for(i in 2:length(importance_numbers)){
  choice_set[i] <- choice_set[(i-1)] + importance_numbers[i]
}

#simulate an interview

interview <- function(){
  number_topics <- round(rnorm(1,topics_per_interview_mu,topics_per_interview_sd),digits = 0)
  selected_topics <- FALSE
  for (k in 1:true_concerns_pop){
    selected_topics[k] <- FALSE
  }
  for(i in 1:number_topics){
    a <- runif(1,0,choice_set[length(choice_set)])
    esc <- 0
    j <- 1
    while(esc == 0){
      if(a > choice_set[j] && a < choice_set[j+1]){
        selected_topics[j] <- TRUE
        esc <- 1
      }
      j <- j + 1
      if (j == length(choice_set)){
        selected_topics[j] <- TRUE
        esc <- 1
      }
    }
  }
  return(selected_topics)
}

#do interviews until stopping rule is satisfied

use_stopping_rule <- function(number = 3){
  discovered_topics <- list()
  discovered_topics[[1]] <- interview()
  i <- 2
  time_since_new <- 0
  while(time_since_new < number){
    discovered_topics[[i]] <- (interview() | discovered_topics[[(i-1)]])
    if (isTRUE(all.equal(discovered_topics[[i]],discovered_topics[[(i-1)]]))){
      time_since_new <- time_since_new + 1
    }else{
      time_since_new <- 0
    }
    i <- i + 1
  }
  return(c(i,discovered_topics[[(i-1)]]))
}

#calculate the percentage of topics found and number of expected interviews required at stoppping rules from 1 to 5

found <- 0
attempts <- 0
results <- list()

for(j in 1:8){
  for(i in 1:1000){
    a <- use_stopping_rule(j)
    found[i] <- sum(a[2:length(a)])
    attempts[i] <- a[1]
  }
  results[[j]] <- c(mean(found)/true_concerns_pop,mean(attempts),mean(found)/mean(attempts))
}

results #percentage of topics found, number of interviews, average new topics/interview




