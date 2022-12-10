input <- read.table("Day 7.txt", fill = TRUE)

input$command = NA
input$current = NA
input$directory = list(NA)
input$filename = list(NA)
input$size = NA

for (i in c(1:nrow(input))) {
  if (input$V1[i] == "$") {
    input$command[i] <- paste(input$V2[i], input$V3[i])
    input$size[i] = 0
    if (substring(input$command[i], 1,2) == "cd") {
      ## input$current[i] <- substring(input$command[i], 3, nchar(input$command[i]))
    }
  } else if (input$V1[i] == "dir") {
    input$directory[i] <- input$V2[i]
  } else {
    input$filename[i] <- input$V2[i]
    input$size[i] <- as.numeric(input$V1[i])
  }
}

head(input, 20)

cd <- function(x) {
  dirs <- strsplit(x, "/")
  newdir <- "home "
  i = 2
  while (i < length(dirs[[1]])) {
  newdir <- paste(newdir, dirs[[1]][i], sep = "/")
  i = i + 1
  }
  return(newdir)
}

cdset <- subset(input, substring(input$command,1,2) == "cd")

for (i in c(1: nrow(cdset))) {
  dir <- substring(cdset$command[i], 4, nchar(cdset$command[i]))
  if (dir == "/") {
    cdset$current[i] <- "home " 
  } else if (dir == "..") {
    cdset$current[i] = cd(cdset$current[i-1])
  } else {
    cdset$current[i] <- paste(cdset$current[i-1], dir, sep = "/") 
  }
}

##cdset$current <- StrAlign(cdset$current, sep ="\\l")

##cdset

cdsetindices = as.numeric(rownames(cdset))

input$current[cdsetindices] = cdset$current
for (i in c(1:nrow(input))) {
  if (is.na(input$current[i])) {
    input$current[i] <- input$current[i-1]
  }
}

head(input)
as.vector(subset(input, input$current == "home /fhhwv/bjml/cwghv", select = size))

countsize <- function(x) {
  thisdir <- as.vector(subset(input, input$current == x, select = size))
}

## cdset$sizevector <- countsize(cdset$current)

head(input,30)
head(cdset)


herhaling1 <- data.frame(aggregate(input$size, by = list(input$current), sum))
colnames(herhaling1) <- c("dir", "size")

deepest <- function(x) {
  dirs <- strsplit(x, "/")
  dpst <- length(dirs[[1]])
  return(dirs[[1]][dpst])
}

for (i in c(1:nrow(herhaling1))) {
  herhaling1$deepest[i] <- deepest(herhaling1$dir[i])
} 

sum(is.na(herhaling1$size))

nrow(herhaling6)
nrow(cd)

for (i in c(1:nrow(herhaling1))) {
  if (!(is.na(herhaling1$size[i]))) {
    input$size[input$directory == herhaling1$deepest[i]] <- herhaling1$size[i] 
  } 
}


herhaling2 <- data.frame(aggregate(input$size, by = list(input$current), sum))
colnames(herhaling2) <- c("dir", "size")

for (i in c(1:nrow(herhaling2))) {
  herhaling2$deepest[i] <- deepest(herhaling2$dir[i])
} 

sum(is.na(herhaling2$size))

for (i in c(1:nrow(herhaling2))) {
  if (!(is.na(herhaling2$size[i]))) {
    input$size[input$directory == herhaling2$deepest[i]] <- herhaling2$size[i] 
  } 
}

herhaling3 <- data.frame(aggregate(input$size, by = list(input$current), sum))
colnames(herhaling3) <- c("dir", "size")

for (i in c(1:nrow(herhaling3))) {
  herhaling3$deepest[i] <- deepest(herhaling3$dir[i])
} 

sum(is.na(herhaling3$size))

for (i in c(1:nrow(herhaling3))) {
  if (!(is.na(herhaling3$size[i]))) {
    input$size[input$directory == herhaling3$deepest[i]] <- herhaling3$size[i] 
  } 
}

herhaling4 <- data.frame(aggregate(input$size, by = list(input$current), sum))
colnames(herhaling4) <- c("dir", "size")

for (i in c(1:nrow(herhaling4))) {
  herhaling4$deepest[i] <- deepest(herhaling4$dir[i])
} 

sum(is.na(herhaling4$size))

for (i in c(1:nrow(herhaling4))) {
  if (!(is.na(herhaling4$size[i]))) {
    input$size[input$directory == herhaling4$deepest[i]] <- herhaling4$size[i] 
  } 
}

herhaling5 <- data.frame(aggregate(input$size, by = list(input$current), sum))
colnames(herhaling5) <- c("dir", "size")

for (i in c(1:nrow(herhaling5))) {
  herhaling5$deepest[i] <- deepest(herhaling5$dir[i])
} 

sum(is.na(herhaling5$size))

for (i in c(1:nrow(herhaling5))) {
  if (!(is.na(herhaling5$size[i]))) {
    input$size[input$directory == herhaling5$deepest[i]] <- herhaling5$size[i] 
  } 
}

herhaling6 <- data.frame(aggregate(input$size, by = list(input$current), sum))
colnames(herhaling6) <- c("dir", "size")

for (i in c(1:nrow(herhaling6))) {
  herhaling6$deepest[i] <- deepest(herhaling6$dir[i])
} 

herhaling6
sum(is.na(herhaling6$size))

herhaling7 <- data.frame(aggregate(input$size, by = list(input$current), sum))
colnames(herhaling7) <- c("dir", "size")

for (i in c(1:nrow(herhaling7))) {
  herhaling7$deepest[i] <- deepest(herhaling6$dir[i])
} 

answer <- subset(herhaling7, herhaling7$size <= 100000)

sum(answer$size)

herhaling6$size[1]
sum(herhaling6$size[2:nrow(answer)])