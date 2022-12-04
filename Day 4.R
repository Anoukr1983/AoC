sections <- read.csv("day 4.csv", header = FALSE)

sections$begin1 <- substring(sections$V1, 1,2)
sections$begin2 <- substring(sections$V2, 1,2)
sections$lengte1 <- nchar(sections$V1)
sections$lengte2 <- nchar(sections$V2)
sections$eind1 <- substring(sections$V1, sections$lengte1 - 1, sections$lengte1)
sections$eind2 <- substring(sections$V2, sections$lengte2 - 1, sections$lengte2)


for (i in c(1: nrow(sections))) {
  if (substring(sections$begin1[i],2,2) == "-") {
    sections$begin1[i] = substring(sections$begin1[i],1,1)
  }
  if (substring(sections$eind1[i],1,1) == "-") {
    sections$eind1[i] = substring(sections$eind1[i],2,2)
  }
}


for (i in c(1: nrow(sections))) {
  if (substring(sections$begin2[i],2,2) == "-") {
    sections$begin2[i] = substring(sections$begin2[i],1,1)
  }
  if (substring(sections$eind2[i],1,1) == "-") {
    sections$eind2[i] = substring(sections$eind2[i],2,2)
  }
}

sections$begin1 = as.numeric(sections$begin1)
sections$begin2 = as.numeric(sections$begin2)
sections$eind1 = as.numeric(sections$eind1)
sections$eind2 = as.numeric(sections$eind2)


sections$overlap <- ((sections$begin1 <= sections$begin2) & (sections$eind1 >= sections$eind2)) |
                     ((sections$begin2 <= sections$begin1) & (sections$eind2 >= sections$eind1))
  
head(sections)
tail(sections)

sections

answer1 <- sum(sections$overlap)

for (i in c(1:nrow(sections))) {
  if (sections$begin1[i] > sections$begin2[i]) {
    hulp = sections$begin1[i]
    sections$begin1[i] = sections$begin2[i]
    sections$begin2[i] = hulp
    hulp = sections$eind1[i]
    sections$eind1[i] = sections$eind2[i]
    sections$eind2[i] = hulp
  }
  if (sections$eind1[i] < sections$begin2[i]) { sections$overlapby[i] <- 0 } 
  if (sections$eind1[i] == sections$begin2[i]) { sections$overlapby[i] <- 1}
  if (sections$eind1[i] > sections$begin2[i]) {
    if (sections$eind2[i] >= sections$eind2[i]) { sections$overlapby[i] <-  sections$eind1[i] - sections$begin2[i] +1 }
    if (sections$eind2[i] < sections$eind1[i]) { sections$overlapby[i] <- sections$eind2[i] - sections$begin2[i] + 1 }
  }
}

sections$overlap = sections$overlapby > 0
  
sections

answer2 <- sum(sections$overlap)

answer1
answer2
