#exo1


x<-c(0,1,2,3)
probax<-c(3/15,4/15,5/15,3/15)
frex<-cumsum(probax)
frex
d<-stepfun(x,c(0,frex))
d
plot(d,vertical=FALSE)





#exo2


simu <- rnorm(8000)
simu
{hist(simu, prob=T,
      main="Histogramme de 8000 tirages")}
      curve(dnorm(x), add=T)



#exo 3

# Creating a vector congaing the 3 colors R,V,N

test1 = c('R', 'V', 'N')

# Running a sample function with size 40 and probability of each color

q3 = sample(x = test1, 
            size = 40, 
            prob =c(0.45,0.35,0.20), 
            replace=T)

# Plotting the result of each color in a barplot
barplot(table(q3), 
        ylab = "Number of occurences", 
        xlab = "Colored balls N=Black, R=Red, V=Green", 
        col = c("#000000", "#F92C00", "#6AF900"))





#exo 4




# Number of events
total_events=10000

# d1,d2,d3 are three dices that are rolled 10000 times each

d1 <- sample(1:6,size = 10000,replace = TRUE)
d2 <- sample(1:6,size = 10000,replace = TRUE)
d3 <- sample(1:6,size = 10000,replace = TRUE)

# Multiplying result of 3 dices and recording the result

result <- d1*d2*d3

# Creating a data frame with frequency of each multiplication result

occurences <- data.frame(table(result))
result2 <- occurences[(8:9),]
result2

# Calculating and Checking Probability of 9 and 10

Probability_of_9 <- result2[1,2]/total_events
Probability_of_10 <- result2[2,2]/total_events
Probability_of_9
Probability_of_10

if (Probability_of_9 == Probability_of_10)
  message('YES') else message('NO')




#exo 5

# We create our Urns with the colored
# balls inside each one of them
URN1 = c('V','V', 'V', 'V', 'V', 'V', 'N', 'N')
URN2 = c('V','V', 'V', 'V', 'N', 'N', 'N')

# We calculate the frequency of each color inside the Urn1
occurences1 <- as.data.frame(table(URN1))
occurences1
Probability_Black_Urn1 = occurences1[1,2]/8
Probability_Black_Urn1

# We calculate the frequency of each color inside the Urn1
occurences2 <- as.data.frame(table(URN2))
occurences2
Probability_Black_Urn2 = occurences1[1,2]/7
Probability_Black_Urn2

# We roll our dice 100 times and record the results
dice = sample(1:6, size = 100, replace=T)
result_dice <- as.data.frame(table(dice))
result_dice

# From the Frequency table we calculate the probability of 
# getting 1 from the dice and probability of the others too
Probability_Of_1_From_Dice <- result_dice[1,2]/100
Probability_Of_1_From_Dice
Probability_Of_others_From_Dice <- 1-Probability_Of_1_From_Dice
Probability_Of_others_From_Dice

# Finally we calculate the probability of a black ball by 
# multiplying probability of each black ball with its  
# corresponding probability of the dice and we add the 
#2 probabilities for the final answer  
Probability_Of_Black_Ball <- Probability_Of_1_From_Dice * Probability_Black_Urn1 + Probability_Black_Urn2 * Probability_Of_others_From_Dice
Probability_Of_Black_Ball






#exo 6





# create a vector to store the results of 100000 exam results
result <- vector("integer", x)

# start a loop of the desired test size
for (i in 1:100000){

# An answer to a question can be either true with 
  #probability 1/5 or false with probability 4/5
Question <- c('True', 'False')

# we start the a single exam experiment with 40 questions
Expirement_Exam = sample(x = Question, 
                        size = 40, 
                        prob =c(0.2,0.8), 
                        replace=T)

# we calculate true and false answers
occurences <- data.frame(table(Expirement_Exam))

# we multiply all true answers with the note of 0.5
#and store the total result of an exam
result[i] <- occurences[2,2] * 0.5
}
# display a histogram of all results
hist(result)

# check the total number of each score we observed
result <- data.frame(table(result))
result