moves <- read.table("Day 9.txt")

moves

nrow(moves)


positions <- data.frame(movenr = c(1:12800),
                        hx = NA,
                        hy = NA,
                        direction = NA,
                        steps = NA)
colnames(positions) <- c("movenr", "Hx", "Hy", "direction", "steps")

positions$movenr <- NA
positions$stepsx <- 0
positions$stepsy <- 0

pos = 1
for (i in c(1: nrow(moves))) {
  nmoves <- moves$V2[i]
  range = c(pos: (pos + nmoves - 1))
  positions$steps[range] <- nmoves
  positions$direction[range] <- switch(moves$V1[i],
                                   "U" = "Up",
                                   "D" = "Down",
                                   "L" = "Left",
                                   "R" = "Right")
  positions$stepsx[range] <- switch(moves$V1[i],
                                       "U" = 0,
                                       "D" = 0,
                                       "L" = -1,
                                       "R" = 1)
  positions$stepsy[range] <- switch(moves$V1[i],
                                    "U" = 1,
                                    "D" = -1,
                                    "L" = 0,
                                    "R" = 0)
  positions$movenr[range] <- i
  pos = pos + nmoves
}

moveropex <- function(Tx, Hx, Ty, Hy) {
  deltax = Tx-Hx
  deltay = Ty-Hy
  movesd = (deltax != 0 && deltay != 0)
  movesx = !(deltax %in% c(-1,0,1))
  movesy = !(deltay %in% c(-1,0,1))
  if (movesd) { 
    move = "diagonaal"
    if (!movesx && !movesy) { Tx = Tx }
    else if (deltax < 0) { Tx = Tx + 1 }
    else if (deltax > 0) { Tx = Tx - 1 }
    }
  else if (movesx) { 
    move = "x-as" 
    if (deltax < -1) { Tx = Tx + 1 }
    else if (deltax > 1) { Tx = Tx - 1}
  }
  else if (movesy) { 
    move = "y-as" 
    Tx = Tx
    }
  else { 
    move = "no move"
    Tx = Tx
    }
}

moveropey <- function(Tx, Hx, Ty, Hy) {
  deltax = Tx-Hx
  deltay = Ty-Hy
  movesd = (deltax != 0 && deltay != 0)
  movesx = !(deltax %in% c(-1,0,1))
  movesy = !(deltay %in% c(-1,0,1))
  if (movesd) { 
    move = "diagonaal"
    if (!movesx && !movesy) { Ty = Ty }
    else if (deltay < 0) { Ty = Ty + 1 }
    else if (deltay > 0) { Ty = Ty - 1}
  }
  else if (movesy) { 
    move = "y-as" 
    if (deltay < -1) { Ty = Ty + 1 }
    else if (deltay > 1) { Ty = Ty - 1}
  }
  else if (movesx) { 
    move = "x-as" 
    Ty = Ty
  }
  else { 
    move = "no move"
    Ty = Ty
  }
}

for (i in c(1:nrow(positions))) {
  if (rownames(positions)[i] == 1) {
    positions$Hx[i] = 0
    positions$Hy[i] = 0
    positions$B1x[i] = 0
    positions$B1y[i] = 0
    positions$B2x[i] = 0
    positions$B2y[i] = 0
    positions$B3x[i] = 0
    positions$B3y[i] = 0
    positions$B4x[i] = 0
    positions$B4y[i] = 0
    positions$B5x[i] = 0
    positions$B5y[i] = 0
    positions$B6x[i] = 0
    positions$B6y[i] = 0
    positions$B7x[i] = 0
    positions$B7y[i] = 0
    positions$B8x[i] = 0
    positions$B8y[i] = 0
    positions$B9x[i] = 0
    positions$B9y[i] = 0
    positions$Tx[i] = 0
    positions$Ty[i] = 0
  }
  else {
    positions$Hx[i] = positions$Hx[i-1] + positions$stepsx[i-1]
    positions$Hy[i] = positions$Hy[i-1] + positions$stepsy[i-1]
    
    B1x = positions$B1x[i-1]
    B1y = positions$B1y[i-1]  
    positions$B1x[i] = moveropex(B1x, positions$Hx[i], B1y, positions$Hy[i])
    positions$B1y[i] = moveropey(B1x, positions$Hx[i], B1y, positions$Hy[i])
    
    B2x = positions$B2x[i-1]
    B2y = positions$B2y[i-1]    
    positions$B2x[i] = moveropex(B2x, positions$B1x[i], B2y, positions$B1y[i])
    positions$B2y[i] = moveropey(B2x, positions$B1x[i], B2y, positions$B1y[i])
    
    B3x = positions$B3x[i-1]
    B3y = positions$B3y[i-1]    
    positions$B3x[i] = moveropex(B3x, positions$B2x[i], B3y, positions$B2y[i])
    positions$B3y[i] = moveropey(B3x, positions$B2x[i], B3y, positions$B2y[i])
    
    B4x = positions$B4x[i-1]
    B4y = positions$B4y[i-1]    
    positions$B4x[i] = moveropex(B4x, positions$B3x[i], B4y, positions$B3y[i])
    positions$B4y[i] = moveropey(B4x, positions$B3x[i], B4y, positions$B3y[i])
    
    B5x = positions$B5x[i-1]
    B5y = positions$B5y[i-1]    
    positions$B5x[i] = moveropex(B5x, positions$B4x[i], B5y, positions$B4y[i])
    positions$B5y[i] = moveropey(B5x, positions$B4x[i], B5y, positions$B4y[i])
    
    B6x = positions$B6x[i-1]
    B6y = positions$B6y[i-1]    
    positions$B6x[i] = moveropex(B6x, positions$B5x[i], B6y, positions$B5y[i])
    positions$B6y[i] = moveropey(B6x, positions$B5x[i], B6y, positions$B5y[i])
    
    B7x = positions$B7x[i-1]
    B7y = positions$B7y[i-1]    
    positions$B7x[i] = moveropex(B7x, positions$B6x[i], B7y, positions$B6y[i])
    positions$B7y[i] = moveropey(B7x, positions$B6x[i], B7y, positions$B6y[i])
    
    B8x = positions$B8x[i-1]
    B8y = positions$B8y[i-1]    
    positions$B8x[i] = moveropex(B8x, positions$B7x[i], B8y, positions$B7y[i])
    positions$B8y[i] = moveropey(B8x, positions$B7x[i], B8y, positions$B7y[i])
    
    B9x = positions$B9x[i-1]
    B9y = positions$B9y[i-1]    
    positions$B9x[i] = moveropex(B9x, positions$B8x[i], B9y, positions$B8y[i])
    positions$B9y[i] = moveropey(B9x, positions$B8x[i], B9y, positions$B8y[i])
    
    Tx = positions$Tx[i-1]
    Ty = positions$Ty[i-1]
    positions$Tx[i] = moveropex(Tx, positions$B9x[i], Ty, positions$B9y[i])
    positions$Ty[i] = moveropey(Tx, positions$B9x[i], Ty, positions$B9y[i])
  }
}

head(positions, 20)
tail(positions)

answer <- nrow(unique(positions[c(24:25)]))

answer

ncol(positions)

sum(moves$V2[moves$V1 == "U"]) - sum(moves$V2[moves$V1 == "D"])
sum(moves$V2[moves$V1 == "R"]) - sum(moves$V2[moves$V1 == "L"])
