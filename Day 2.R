rps <- as.vector(read.csv("RData/AoC Puzzle Input/Day 2.csv", header = FALSE))


rps

ncol(rps)

rps_game <- data.frame (
  input = rps,
  ABC = NA,
  XYZ = NA,
  score_elf = NA,
  score_you = NA,
  winner = NA,
  Score_winner = NA
)

colnames(rps_game) = c("input", "ABC", "XYZ", "score_elf", "score_you", "winner", "score_winner")

for (i in c(1:nrow(rps_game))) {
  rps_game$ABC[i] = substring(rps_game$input[i], 1, 1)
  rps_game$XYZ[i] = substring(rps_game$input[i], 3, 3)
  rps_game$score_elf[i] = ifelse(rps_game$ABC[i] == "A", 1, 
                                ifelse(rps_game$ABC[i] == "B", 2,
                                       ifelse(rps_game$ABC[i] == "C", 3, NA)))
  rps_game$score_you[i] = ifelse(rps_game$XYZ[i] == "X", 1, 
                                 ifelse(rps_game$XYZ[i] == "Y", 2,
                                        ifelse(rps_game$XYZ[i] == "Z", 3, NA)))
  rps_game$winner[i] = ifelse(rps_game$score_elf[i] == rps_game$score_you[i], "tie", 
                              ifelse((rps_game$score_elf[i] - rps_game$score_you[i]) %% 3 == 1, "elf",
                                     ifelse((rps_game$score_elf[i] - rps_game$score_you[i]) %% 3 == 2, "you", NA)))
  rps_game$score_winner[i] = ifelse(rps_game$winner[i] == "tie", 3, 
                                 ifelse(rps_game$winner[i] == "you", 6,
                                        ifelse(rps_game$winner[i] == "elf", 0, NA)))
}

any(is.na(rps_game))

solution1 <- sum(rps_game$score_you) + sum(rps_game$score_winner)

solution1

rps_game$score_you = NA
rps_game$winner = NA
rps_game$score_winner = NA


for (i in c(1:nrow(rps_game))) {
  rps_game$winner[i] = ifelse(rps_game$XYZ[i] == "X", "elf", 
                                 ifelse(rps_game$XYZ[i] == "Y", "tie",
                                        ifelse(rps_game$XYZ[i] == "Z", "you", NA)))
  rps_game$score_you[i] = ifelse(rps_game$winner[i] == "tie", rps_game$score_elf[i], 
                              ifelse(rps_game$winner[i] == "elf", (rps_game$score_elf[i] - 1) %% 3,
                                     ifelse(rps_game$winner[i] == "you", (rps_game$score_elf[i] + 1) %% 3, NA)))
  rps_game$score_you[i] = ifelse(rps_game$score_you[i] == 0, 3, rps_game$score_you[i])
  rps_game$score_winner[i] = ifelse(rps_game$winner[i] == "tie", 3, 
                                    ifelse(rps_game$winner[i] == "you", 6,
                                           ifelse(rps_game$winner[i] == "elf", 0, NA)))
}

any(is.na(rps_game))

solution2 <- sum(rps_game$score_you) + sum(rps_game$score_winner)

solution2
