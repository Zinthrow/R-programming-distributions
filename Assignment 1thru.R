#--- Alexander Larsen
#MIT 18.440 Introduction to statistics 

#Assignment I

x <- c('red', 'blue', 'black', 'green', 'yellow' )
f <- c(1,2,3,4,5,6,7,8)

(choose(4,2)*12)*choose(4,3)*13

choose(4,1)*1
#you want hand of 5 cards total 4 of one suit and 1 of another
4*choose(13,4)*3*choose(13,1) 

#You have eight distinct pieces of food. You want to choose three 
#for breafast, two for lunch, and three for dinner.
factorial(8)/(factorial(3)*factorial(3) * factorial(2))

#class of 27 needs 9 teams of 3 students each. Combinations?
factorial(27)/(factorial(3)**9 * factorial(9))

#there are 90 indistinguishable pizza and 90 distinguishable students. ways?
choose(179,90)

#3 balls are randomly drawn from a bowl containing 6 white and 5 black balls,
#what is the probability that one ball drawn will be white and the other two black
choose(6,1)*choose(5,2)/choose(11,3)

#Urn with 8 red and 4 white, if we draw 2 without replacement. P(R)?
choose(8,2)/choose(12,2)


#important formulations for n and m
# n!*m!/(n+m)!

#52 cards are partitioned into 4 piles 13 cards each. chance 1 ace per?
((52-13)*(39-13)*(26-13))/(51*50*49)

#-------------------------------------------
#Assignment II conditional probability


#Eight coins are tossed, what is the probability that exatly five of them
# are heads

.5**8

#52 cards are dealt out equally to four players--, if two have a 
#total of eight spades among them, what is the probability that another  
#has 3 of the remaining five spades?

choose(5,3)*choose(21,10)/choose(26,13)


#bayes
#.3 population is accident prone .4 chance a year for an accident
# .2 chance for less accident prone

.3*.4/(.3*.4+.7*.2)

# .95 effetive detecting a certain disease. also yeilds false positive
# .01 of the time. if .005 has disease. what is the chance they will get 
# a positive and have the disease

x <- .005*.95 + (1-.005)*.01
.005*.95/x


choose(19,2)/choose(20,3)
choose(20,3)

