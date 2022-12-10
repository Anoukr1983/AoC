instructions <- read.table("Day 10.txt", fill = TRUE, header = FALSE)

cycles = data.frame(cycle = c(1:250))
cycles$before = NA
cycles$instruction = NA
cycles$nsteps = NA
cycles$after = NA

cycles$before[1] = 1

cycle = 1
for (i in c(1:nrow(instructions))) {
  if (instructions$V1[i] == "noop") {
    cycles$instruction[cycle] = instructions$V1[i]
    cycles$nsteps[cycle] = 0
    cycles$after[cycle] = cycles$before[cycle] + cycles$nsteps[cycle]
    cycles$before[cycle + 1] = cycles$after[cycle]
    cycle = cycle + 1
  } else if (instructions$V1[i] == "addx") {
    cycles$instruction[cycle] = instructions$V1[i]
    cycles$nsteps[cycle] = 0
    cycles$after[cycle] = cycles$before[cycle] + cycles$nsteps[cycle]
    cycles$before[cycle + 1] = cycles$after[cycle]
    
    cycles$instruction[cycle+1] = instructions$V1[i]
    cycles$nsteps[cycle+1] = instructions$V2[i]
    cycles$after[cycle+1] = cycles$before[cycle+1] + cycles$nsteps[cycle+1]
    cycles$before[cycle + 2] = cycles$after[cycle+1]
    
    cycle = cycle + 2
  }
}

cycles$strength = cycles$before * cycles$cycle

cycles

answer1 <- sum(cycles$strength[c(20, 60, 100, 140, 180, 220)])

answer1

cycles$crt = (cycles$cycle - 1) %% 40

for (i in c(1:240)) {
  visible <- cycles$crt[i] - cycles$before[i]
  if (visible == -1 | visible == 0 | visible == 1 ) {
    cycles$signal[i] <- "#"
  } 
  else { 
    cycles$signal[i] <- "." 
  }
}

sig = cycles$signal[c(1:40)]

row1 <- paste(sig[1], sig[2], sig[3], sig[4], sig[5], sig[6], sig[7], sig[8], sig[9], sig[10],
            sig[11], sig[12], sig[13], sig[14], sig[15], sig[16], sig[17], sig[18], sig[19], sig[20],
            sig[21], sig[22], sig[23], sig[24], sig[25], sig[26], sig[27], sig[28], sig[29], sig[30],
            sig[31], sig[32], sig[33], sig[34], sig[35], sig[36], sig[37], sig[38], sig[39], sig[40],
            sep = "")

sig = cycles$signal[c(41:80)]

row2 <- paste(sig[1], sig[2], sig[3], sig[4], sig[5], sig[6], sig[7], sig[8], sig[9], sig[10],
            sig[11], sig[12], sig[13], sig[14], sig[15], sig[16], sig[17], sig[18], sig[19], sig[20],
            sig[21], sig[22], sig[23], sig[24], sig[25], sig[26], sig[27], sig[28], sig[29], sig[30],
            sig[31], sig[32], sig[33], sig[34], sig[35], sig[36], sig[37], sig[38], sig[39], sig[40],
            sep = "")

sig = cycles$signal[c(81:120)]

row3 <- paste(sig[1], sig[2], sig[3], sig[4], sig[5], sig[6], sig[7], sig[8], sig[9], sig[10],
            sig[11], sig[12], sig[13], sig[14], sig[15], sig[16], sig[17], sig[18], sig[19], sig[20],
            sig[21], sig[22], sig[23], sig[24], sig[25], sig[26], sig[27], sig[28], sig[29], sig[30],
            sig[31], sig[32], sig[33], sig[34], sig[35], sig[36], sig[37], sig[38], sig[39], sig[40],
            sep = "")

sig = cycles$signal[c(121:160)]

row4 <- paste(sig[1], sig[2], sig[3], sig[4], sig[5], sig[6], sig[7], sig[8], sig[9], sig[10],
            sig[11], sig[12], sig[13], sig[14], sig[15], sig[16], sig[17], sig[18], sig[19], sig[20],
            sig[21], sig[22], sig[23], sig[24], sig[25], sig[26], sig[27], sig[28], sig[29], sig[30],
            sig[31], sig[32], sig[33], sig[34], sig[35], sig[36], sig[37], sig[38], sig[39], sig[40],
            sep = "")

sig = cycles$signal[c(161:200)]

row5 <- paste(sig[1], sig[2], sig[3], sig[4], sig[5], sig[6], sig[7], sig[8], sig[9], sig[10],
            sig[11], sig[12], sig[13], sig[14], sig[15], sig[16], sig[17], sig[18], sig[19], sig[20],
            sig[21], sig[22], sig[23], sig[24], sig[25], sig[26], sig[27], sig[28], sig[29], sig[30],
            sig[31], sig[32], sig[33], sig[34], sig[35], sig[36], sig[37], sig[38], sig[39], sig[40],
            sep = "")

sig = cycles$signal[c(201:240)]

row6 <- paste(sig[1], sig[2], sig[3], sig[4], sig[5], sig[6], sig[7], sig[8], sig[9], sig[10],
            sig[11], sig[12], sig[13], sig[14], sig[15], sig[16], sig[17], sig[18], sig[19], sig[20],
            sig[21], sig[22], sig[23], sig[24], sig[25], sig[26], sig[27], sig[28], sig[29], sig[30],
            sig[31], sig[32], sig[33], sig[34], sig[35], sig[36], sig[37], sig[38], sig[39], sig[40],
            sep = "")

output <- matrix(1:6)
output[1,1] = row1
output[2,1] = row2
output[3,1] = row3
output[4,1] = row4
output[5,1] = row5
output[6,1] = row6

output