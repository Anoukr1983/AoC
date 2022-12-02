calories <- as.vector(read.csv("Day 1.csv", header = FALSE, blank.lines.skip = FALSE))

elfscalories <- data.frame(elf = NA, calories)
colnames(elfscalories) <- c("elf", "cals")


elfcount <- 1
for (i in c(1:nrow(elfscalories))) {
  if (!is.na(elfscalories$cals[i])) {
    elfscalories$elf[i] = elfcount
  } else { 
    elfcount = elfcount + 1 
  }
}

elfscalories = elfscalories[complete.cases(elfscalories), ]

caloriesperelf <- aggregate(elfscalories$cals, list(elfscalories$elf), sum)

max(caloriesperelf$x)

sortedcalories <- sort(caloriesperelf$x, decreasing = TRUE)

sum(sortedcalories[c(1:3)])
