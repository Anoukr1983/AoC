input <- read.table("Day 6.txt")
example <- read.table("Day 6 ex.txt")

nchar(input)

firstsignal = NA

for (n in c(4: nchar(input))) {
  eerste = substring(input, n-3, n-3)
  tweede = substring(input, n-2, n-2)
  derde = substring(input, n-1, n-1)
  vierde = substring(input,n,n)
  signal = paste(n, ": ", eerste,tweede,derde,vierde)
  marker = ((eerste != tweede & eerste != derde & eerste != vierde) &
          (tweede != derde & tweede != vierde) &
          (derde != vierde))
  print(marker)
  if (is.na(firstsignal) & marker == TRUE) {
    firstsignal = signal
  }
}

print(firstsignal)

firstmessenger = NA
nmessenger = -1
foundfirst = FALSE

for (n in c(14: nchar(input))) {
  check = substring(input, n-14, n)
  print(paste(n, check))
  notmessage = FALSE
  for (i in c(1:14)) {
    char = substring(check, i, i)
    checknow = substring(check, i+1, 14)
    notmessage = notmessage | grepl(char, checknow, fixed = TRUE)
    ## print(paste(n, char, checknow))
    ## print(notmessage)
  }
  if (foundfirst == FALSE & notmessage == FALSE) {
    firstmessenger = check
    nmessenger = n
    foundfirst = TRUE
  }
}

print(grepl("c", "cdhccdbdggfjjg", fixed = TRUE))

print(paste(nmessenger, ": ", firstmessenger))

example