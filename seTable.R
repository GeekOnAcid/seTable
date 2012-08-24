seTable <- function (X, xmin = NULL, xmax = NULL, labels = NULL, at = NULL, 
                     unitlength = "5cm", linethickness = NULL, cnames = colnames(X), 
                     circlesize = 0.01, xoffset = 0, yoffset = 0, dec = 2L, filename = NULL) 
{
  X <- as.matrix(X)
  if (nrow(X) < 10L) 
    warning("'X' has less than 10 rows")
  if (!is.null(at) && is.null(labels)) 
    stop("'labels' must be provided")
  if (!is.null(labels) && is.null(at)) 
    stop("'at' must be provided")
  require(psych)#psych got 'SE' function to compute standard deviation for columns    
  M = colMeans(X)#column means
  SE = SD(X)/sqrt(nrow(X))#standard error
  SELo = M-SE#lower bound
  SEHi = M+SE#upper bound
  A = t(data.frame(SELo,M,SEHi))#combines it together
  B <- array(0, dim = c(3L, dim(A)[2L]))
  B[1L:3L, ] <- A
  if (is.null(xmin)) 
    xmin <- min(B)
  if (is.null(xmax)) 
    xmax <- max(B)
  joli <- pretty(c(xmin, xmax), n = 3L)
  a <- min(joli)
  b <- max(joli)
  if (is.null(labels)) 
    labels <- joli
  if (is.null(at)) 
    at <- joli
  B <- (B - a)/(b - a)
  at <- (at - a)/(b - a)
  fff <- function(x) formatC(x, digits = dec, format = "f")
  ff <- function(x) formatC(x, digits = 5L, format = "f")
  `%p1%` <- function(a, b) paste(a, b, sep = "")
  `%p2%` <- function(a, b) paste(a, " & ", b, sep = "")
  STR <- c("\n&\\\\")
  for (cc in 1L:dim(X)[2L]) {
    str0 <- cnames[cc]
    str1 <- "& \\begin{picture}(1,0)(" %p1% xoffset %p1% 
      "," %p1% yoffset %p1% ")"
    str2 <- "\\put(" %p1% ff(B[1, cc]) %p1% ",0){\\line(1,0){" %p1% 
      ff(B[2, cc] - B[1, cc]) %p1% "}}"
    str3 <- "\\put(" %p1% ff(B[2, cc]) %p1% ",0){\\circle*{" %p1% 
      circlesize %p1% "}}"
    str4 <- "\\put(" %p1% ff(B[2, cc]) %p1% ",0){\\line(1,0){" %p1% 
      ff(B[3, cc] - B[2, cc]) %p1% "}}"
    str5 <- "\\end{picture}\\\\"
    temp <- paste(str0, str1, str2, str3, str4, str5, sep = "")
    STR <- rbind(STR, "\n", temp)
  }
  strScale <- "&\\begin{picture}(1,0)(" %p1% xoffset %p1% 
    "," %p1% yoffset %p1% ")\\put(0,0){\\line(1,0){1}}"
  for (i in seq(along.with = labels)) {
    strScale <- strScale %p1% "\\put(" %p1% ff(at[i]) %p1% 
      ",0) {\\line(0,-1){0.01}}\n"
    strScale <- strScale %p1% "\\put(" %p1% ff(at[i]) %p1% 
      ",-0.1){" %p1% labels[i] %p1% "}\n"
  }
  strScale <- paste(strScale, "\\end{picture}\\\\", sep = "")
  STR <- rbind("\\begin{tabular}{rrrrrr}", STR, "\n", strScale, 
               "\n\\end{tabular}")
  if (!is.null(linethickness)) 
    STR <- rbind("\\linethickness{" %p1% linethickness %p1% 
      "}\n", STR)
  temp <- "\\setlength{\\unitlength}{" %p1% unitlength %p1% 
    "}\n"
  STR <- rbind(temp, STR)
  STR <- rbind("{\n", STR, "\n}\n")
  if (!is.null(filename)) {
    cat(STR, "\n", sep = "", file = filename)
  }
  STR
}