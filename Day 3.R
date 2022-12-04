input <- read.table("Day 3.txt")

compartment1 <- as.data.frame(matrix(c(1:16500), 300, byrow = TRUE))
compartment2 <- as.data.frame(matrix(c(1:16500), 300, byrow = TRUE))

compartment1$V1= input
compartment2$V1 = input

compartment1[, c(2:55)] = 0
compartment2[, c(2:55)] = 0

colnames(compartment1) <- c("contents", "nitems", "itemspercompartment", LETTERS, letters)
colnames(compartment2) <- c("contents", "alsoin1", "itemspercompartment", LETTERS, letters)

asc <- function(x) { strtoi(charToRaw(x),16L) }

priority <- function(x) {
  if (x < 91) { x = x - 38 }
  else { x = x - 96 }
}

for (i in c(1: nrow(compartment1))) {
  compartment1$nitems[i] = nchar(compartment1$contents[i, ])
  compartment1$itemspercompartment[i] = compartment1$nitems[i]/2
  for (j in c(1: compartment1$itemspercompartment[i])) {
    item <- substring(compartment1$contents[i,], j,j)
    compartment1[i, item] = compartment1[i, item] +1
  }
}

for (i in c(1: nrow(compartment2))) {
  compartment2$itemspercompartment[i] = nchar(compartment2$contents[i, ])/2
  for (j in c(1: compartment2$itemspercompartment[i])) {
    item <- substring(compartment2$contents[i,], j + compartment2$itemspercompartment[i], j + compartment2$itemspercompartment[i])
    compartment2[i, item] = compartment2[i, item] + 1
    if (compartment1[i, item] != 0) {
      compartment2$alsoin1[i] = item
    }
  }
}

for (k in c(1: nrow(compartment2))) {
  compartment2$alsoin1[k] = priority(asc(compartment2$alsoin1[k]))
}

answer1 = sum(as.numeric(compartment2$alsoin1))

elfsgroups <- as.data.frame(matrix(c(1:500), 100))

elfsgroups$V1 = input[seq(1, 300, by = 3), ]
elfsgroups$V2 = input[seq(2, 300, by = 3), ]
elfsgroups$V3 = input[seq(3, 300, by = 3), ]

elf1ingroup <- as.data.frame(matrix(c(1:5500), 100, byrow = TRUE))
elf2ingroup <- as.data.frame(matrix(c(1:5500), 100, byrow = TRUE))
elf3ingroup <- as.data.frame(matrix(c(1:5500), 100, byrow = TRUE))

elf1ingroup$V1 = elfsgroups$V1
elf2ingroup$V1 = elfsgroups$V2
elf3ingroup$V1 = elfsgroups$V3

elf1ingroup[c(1:10), c(1:10)]
elf2ingroup[c(1:10), c(1:10)]
elf3ingroup[c(1:10), c(1:10)]

colnames(elf1ingroup) <- c("contents", "nitems", "itemspercompartment", LETTERS, letters)
colnames(elf2ingroup) <- c("contents", "alsoin1", "itemspercompartment", LETTERS, letters)
colnames(elf3ingroup) <- c("contents", "alsoin12", "itemspercompartment", LETTERS, letters)

elf1ingroup[, c(2:55)] = 0
elf2ingroup[, c(2:55)] = 0
elf3ingroup[, c(2:55)] = 0

for (i in c(1: nrow(elf1ingroup))) {
  elf1ingroup$nitems[i] = nchar(elf1ingroup$contents[i])
  elf1ingroup$itemspercompartment[i] = elf1ingroup$nitems[i]
  for (j in c(1: elf1ingroup$itemspercompartment[i])) {
    item <- substring(elf1ingroup$contents[i], j,j)
    elf1ingroup[i, item] = elf1ingroup[i, item] +1
  }
}

for (i in c(1: nrow(elf2ingroup))) {
  elf2ingroup$itemspercompartment[i] = nchar(elf2ingroup$contents[i])
  for (j in c(1: elf2ingroup$itemspercompartment[i])) {
    item <- substring(elf2ingroup$contents[i], j, j)
    if (elf1ingroup[i, item] != 0) {
      elf2ingroup$alsoin1[i] = paste(elf2ingroup$alsoin1[i], item)
      elf2ingroup[i, item] = elf2ingroup[i, item] + 1
    }
  }
}

for (i in c(1: nrow(elf3ingroup))) {
  elf3ingroup$itemspercompartment[i] = nchar(elf3ingroup$contents[i])
  for (j in c(1: elf3ingroup$itemspercompartment[i])) {
    item <- substring(elf3ingroup$contents[i], j, j)
    if (elf2ingroup[i, item] != 0) {
      elf3ingroup$alsoin12[i] = item
      elf3ingroup[i, item] = elf3ingroup[i, item] + 1
    }
  }
}

elf1ingroup[c(1:10), c(1:26)]
elf2ingroup[c(1:10), c(1:26)]
elf3ingroup[c(1:10), c(1:26)]

for (k in c(1: nrow(elf3ingroup))) {
  elf3ingroup$alsoin12[k] = priority(asc(elf3ingroup$alsoin12[k]))
}

answer1 = sum(as.numeric(compartment2$alsoin1))

answer2 = sum(as.numeric(elf3ingroup$alsoin12))

print(answer1)
print(answer2)