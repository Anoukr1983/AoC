input <- read.csv("Day 5.csv", header = FALSE)

ex <- FALSE

stapels <- ifelse(ex, 3, 9)
hoogte <- ifelse(ex, 3, 8)

head(input, hoogte + 1)

beginstacks <- matrix(c(1:72), ncol = stapels, byrow = TRUE)

for (i in c(1:hoogte)) {
  for (j in c(1:stapels)) {
    index = (j-1)*4 + 1
    beginstacks[i,j] = substring(input[i, 1], index, index + 2)
  }
}

stacks <- data.frame(stack = c(1:stapels), onTop = NA)

for (i in c(1:stapels)) {
  helpstack = ""
  for (j in c(1:hoogte)) {
    if (beginstacks[j,i] != "   ") {
      helpstack = paste(helpstack, beginstacks[j,i], sep = "")
    }
  }
  stacks$stack[i] = helpstack
}

stacks$onTop = substring(stacks$stack,2,2)

kopiestacks = stacks

pop <- function(x) {
  e = substring(x,1,3)
  x = substring(x,4, nchar(x))
  poplist = list("element" = e, "stack" = x)
  return(poplist)
}

pop9001 <- function(x,n) {
  e = substring(x,1,3*n)
  x = substring(x,(3*n)+1, nchar(x))
  poplist = list("element" = e, "stack" = x)
  return(poplist)
}

push <- function(x, e) {
  x = paste(e, x, sep = "")
}

movebox <- function(from, to) {
  pops <- pop(stacks$stack[from])
  stacknew <- push(stacks$stack[to], pops$element)
  stacks$stack[from] <<- pops$stack
  stacks$stack[to] <<- stacknew
}

movebox9001 <- function(nbox, from, to) {
  pops <- pop9001(stacks$stack[from], nbox)
  stacknew <- push(stacks$stack[to], pops$element)
  stacks$stack[from] <<- pops$stack
  stacks$stack[to] <<- stacknew
}

moves = input[c(10:nrow(input)), ]

movecollector = function(x) {
  help = strsplit(x, " ")
  box = as.numeric(help[[1]][2])
  pop = as.numeric(help[[1]][4])
  push = as.numeric(help[[1]][6])
  moveslist = list("nbox" = box, "stackpop" = pop, "stackpush" = push)
}

for (i in c(1:length(moves))) {
  nextmove = movecollector(moves[i])
  nmove = 0
  print(moves[i])
  while(nmove < nextmove$nbox) {
    movebox(nextmove$stackpop, nextmove$stackpush)
    nmove = nmove + 1
    print(stacks[ , c(1:2)])
  }
}

stacks$onTop = substring(stacks$stack,2,2)

stacks

answer1 = stacks$onTop

answer1

stacks = kopiestacks

stacks

for (i in c(1:length(moves))) {
  nextmove = movecollector(moves[i])
  print(moves[i])
  movebox9001(nextmove$nbox, nextmove$stackpop, nextmove$stackpush)
  print(stacks[ , c(1:2)])
}

stacks$onTop = substring(stacks$stack,2,2)

answer2 = stacks$onTop

answer2