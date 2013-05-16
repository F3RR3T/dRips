# goat.R
#   a numerical approach to the monty hall paradox
#   http://en.wikipedia.org/wiki/Monty_hall_problem
#   SJP 25 Oct 2007
# 

#------------------------ Instructions -----------------
#  Read the wikipedia article for background
#  source this file into R.
#  The game() function is played out, using both strategies, swapping or not swapping
#   After Monty opens one of the doors that you didn't choose and invites you to swap.

#   Play around with the game() function, 
#   changing the number of trials (iterations) and doors etc.
#     The comments in the code are fairly good (EIIDSSM!)
#
#   consts
NUMDOORS <- 3    #
GOAT <- 0
PRIZE <- 1
ITERATIONS <- 100
#   FUNCTIONS
###############################################################
#   select a door
getDoor <- function(d=NUMDOORS){
  z <- rep(1, d)      		# vector of 1s
	z <- cumsum(z) / sum(z)     # vector of props a..1
  	my_door <- which.min(z < runif(1))   # which door is chosen
	return(my_door)
}
###############################################################
# put the prize behind a door
allocPrize <- function(d=NUMDOORS){
	door <- rep(GOAT, d)                   # vector of goat doors (0)
	door[getDoor()] <- PRIZE               # put the prize behind a door
	return(door)
}
#######################################################################
#	Get a result based on a probability  (default is 0.5)
#	 a typical usage is: 'if(pr(x)) then ...
pr <- function(x = 0.5){
	if (!length(x)) {return()}
	if (x<0 || x>1) stop(" prob out of range\n")
	return(ifelse(rbinom(length(x), 1, x), TRUE, FALSE))
}
#######################################################################
#	junkYard()		place a silent stop() at the end of a source file
#	in effect, the rest of the file is the junkYard of half-good script
junkYard <- function(){
	op <- getOption('show.error.messages')		# save settings
#	options(show.error.messages = F)	# don't warn
	cat('called from junkYard()\n')
#	print( parent.frame())
	try(stop(), silent=T)
	return('how did this get here?')
}


###############################################################
#  game() function
#	call game(T) to switch, game(F) to not swap
#   put a prize behind a door
game <- function(swap_decision, iter=ITERATIONS){
	wins <- 0            # counter for winning trials
	for(i in 1:iter){
		doors <- allocPrize()
		#   the contestant chooses a door
		guess_door <- getDoor()             # could be the same as the prize door

		#   Monty Hall opens a goat door
		while(T){
			host_door <- getDoor()          # Monty chooses a door
			if(host_door == guess_door) next    # Monty not very smart
			if(doors[host_door] == PRIZE) next   # oops not that one either
			break
		}
		#	extra bit: add in 'toss a coin'
		swap <- ifelse((class(swap_decision)=='logical'), swap_decision, pr())
		# test the original guess
		#! 1: I guessed right AND I swap:        win <- 0
		#! 2: I guessed right AND I DON'T swap:  win <- 1
		#! 3: I guessed wrong AND i swap:        win <- 1
		#! 4: I guessed wrong AND I DON'T swap:  win <- 0
    	win <- (xor(doors[guess_door]==PRIZE, swap))
		wins <- wins + win
	}
	return(sprintf('Iterations: %i Swap: %5s  Wins: %3.1f%%', iter, swap_decision, wins/iter*100))
}

cat('\n==========  SWAP or not, to win a car =============\n')
print(game(swap=T))
print(game(swap=F))
print(game(swap='TOSS'))
cat('===================================================\n\n\n')


#  ######################################
junkYard()			# stop() silently
#   test functions
#   have we got a fair distribution of prizes?
x <- t(replicate(1000, allocPrize()))
colsum <- apply(x, 2, sum)
barplot(colsum)
