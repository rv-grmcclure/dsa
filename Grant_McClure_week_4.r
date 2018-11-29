###### WEEK 4 #######
#new comment
#20.3.5 #1-4

#1)Describe the difference between is.finite(x) and !is.infinite(x).

#The is.finite(x) function considers non-missing numeric values to be finite,
#and missing, and pos/neg infinity to not be finite. 
#is.infinite considers `Inf` and `-Inf` to be infinite, and everything else to not be infinite.

#2) Read the source code for dplyr::near() (Hint: to see the source code, drop the ()). 
#How does it work?
#checks for being within a certain tolerance instead of exact equality
  
#3) A logical vector can take 3 possible values. How many possible values can an integer vector take? 
#How many possible values can a double take? Use google to do some research.

#integer --> 2^32
#double --> 2^64

#4) Brainstorm at least four functions that allow you to convert a double to an integer. 
#How do they differ? Be precise.

#thought i escaped numerical analysis back in college...

#truncating or rounding to the nearest integer. lots of different ways to do this, 
#and will depend on how to round + how to break ties

#20.4.6 #1-6

#1) What does mean(is.na(x)) tell you about a vector x? What about sum(!is.finite(x))?
#mean(is.na(x)) calculates the proportion of missing values in a vector
#sum(!is.finite(x)) calculates the number of elements in the vector that are equal to missing, not-a-number, or inifinity

#2) Carefully read the documentation of is.vector(). What does it actually test for? 
#Why does is.atomic() not agree with the definition of atomic vectors above?

#only checks whether the object has no attributes other than names
#is.atomic() checks whether an object is one of the atomic types 
#("logical", "integer", "numeric", "complex", "character", and "raw") or NULL.
#will consider objects to be atomic even if they have extra attributes
  
#3) Compare and contrast setNames() with purrr::set_names().
#setNames() takes two arguments, a vector to be named and a vector of names to apply to its elements.
#purrr::set_names() allows for using a function or formula to transform the existing names

#4) Create functions that take a vector as input and returns:
x <- c(1,2,3)
#a) The last value. Should you use [ or [[?
lastVal <- function(x) {
  x[[length(x)]]
}

#b) The elements at even numbered positions.
evens <- function(x) {
    x[seq_along(x) %% 2 == 0]
}
                                        
#c) Every element except the last value.
notLastVal <- function(x) {
  x[1:length(x)-1]
}      

#d) Only even numbers (and no missing values).
evenNums <- function(x) {
  x[x %% 2 == 0]
}
                                        
#5) Why is x[-which(x > 0)] not the same as x[x <= 0]?
#They handle missing expressions differently.

#6) What happens when you subset with a positive integer that’s bigger than the 
#length of the vector? What happens when you subset with a name that doesn’t exist?

#will return NA for both situations

#20.5.4 #1-2

#1) Draw the following lists as nested sets:
#list(a, b, list(c, d), list(e, f))
#list(list(list(list(list(list(a))))))

#honestly don't really understand what to do here.

#2) What happens if you subset a tibble as if you’re subsetting a list? 
#What are the key differences between a list and a tibble?
#Subsetting a tibble is the same thing as subsetting a list.
#The difference between a tibble and a list is that all the columns of a tibble must have the same number of rows.
#Lists can have vectors with different lengths as elements.

#21.2.1 1-4
#1) Write for loops to:
#Compute the mean of every column in mtcars.
output <- vector("double", ncol(mtcars))
names(output) <- names(mtcars)
for (i in names(mtcars)) {
  output[i] <- mean(mtcars[[i]])
}
output

#Determine the type of each column in nycflights13::flights.
data("flights", package = "nycflights13")
output <- vector("list", ncol(flights))
names(output) <- names(flights)
for (i in names(flights)) {
  output[[i]] <- class(flights[[i]])
}
output

#Compute the number of unique values in each column of iris.
data("iris")
output <- vector("double", ncol(iris))
names(output) <- names(iris)
for (i in names(iris)) {
  output[i] <- length(unique(iris[[i]]))
}
output

#Generate 10 random normals for each of  μ=−10, 0, 10, and 100.
n <- 10
mu <- c(-10, 0, 10, 100)
normals <- vector("list", length(mu))
for (i in seq_along(normals)) {
  normals[[i]] <- rnorm(n, mean = mu[i])
}
normals

#2) Eliminate the for loop in each of the following examples by taking advantage 
#of an existing function that works with vectors:

out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}
#with function:
out <- stringr::str_c(letters, collapse = "")

x <- sample(100)
sd <- 0
for (i in seq_along(x)) {
  sd <- sd + (x[i] - mean(x)) ^ 2
}
sd <- sqrt(sd / (length(x) - 1))
#with function:
sd(x)

x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}
#with function:
all.equal(cumsum(x),out)

#3) Combine your function writing and for loop skills:
#Write a for loop that prints() the lyrics to the children’s song “Alice the camel”.
humps <- c("five", "four", "three", "two", "one", "no")
for (i in humps) {
  cat(str_c("Alice the camel has ", rep(i, 3), " humps.",
            collapse = "\n"), "\n")
  if (i == "no") {
    cat("Now Alice is a horse.\n")
  } else {
    cat("So go, Alice, go.\n")
  }
  cat("\n")
}

#Convert the nursery rhyme “ten in the bed” to a function. 
#Generalise it to any number of people in any sleeping structure.
numbers <- c("ten", "nine", "eight", "seven", "six", "five",
             "four", "three", "two", "one")
for (i in numbers) {
  cat(str_c("There were ", i, " in the bed\n"))
  cat("and the little one said\n")
  if (i == "one") {
    cat("I'm lonely...")
  } else {
    cat("Roll over, roll over\n")
    cat("So they all rolled over and one fell out.\n")
  }
  cat("\n")
}
#Convert the song “99 bottles of beer on the wall” to a function. Generalise to any number of any vessel containing any liquid on any surface.
beers <- function(x, drink, where) {
l = c(rev(seq_len(x)), 0)
  for (i in l) {
    if (i == 0) {
      cat(glue::glue("No more bottles of {drink} on the {where}, no more bottles of {drink}.\n Go to the store and buy some more, {x} bottles of {drink} on the {where}."))
    } else {
      cat(glue::glue("{i} {if (i == 1) 'bottle' else 'bottles'} of {drink} on the {where}, {i} {if (i == 1) 'bottle' else 'bottles'} of {drink}.\n Take one down and pass it around, {if (i == 1) 'no more' else i - 1} bottles of {drink} on the {where}. \n \n \n"))
    }
  }
}
beers(10, 'beer', 'wall')
beers(10, 'wine', 'floor')

#4) It’s common to see for loops that don’t preallocate the output and 
#instead increase the length of a vector at each step:
add_to_vector <- function(n) {
  output <- vector("integer", 0)
  for (i in seq_len(n)) {
    output <- c(output, i)
  }
  output  
}
start <- Sys.time()
add_to_vector(1000)
end <- Sys.time()
duration1 <- end-start

add_to_vector_2 <- function(n) {
  output <- vector("integer", n)
  for (i in seq_len(n)) {
    output[[i]] <- i
  }
  output
}
start2 <- Sys.time()
add_to_vector_2(1000)
end2 <- Sys.time()
duration2 <- end2-start2

(duration1-duration2)
#pre-allocation > allocating

#21.3.5 #1-3

#1) Imagine you have a directory full of CSV files that you want to read in. 
#You have their paths in a vector, files <- dir("data/", pattern = "\\.csv$", full.names = TRUE),
#and now want to read each one with read_csv(). 
#Write the for loop that will load them into a single data frame.

all_csv <- c("one.csv", "two.csv")
all_dfs <- vector("list", length(all_csv))
for (i in all_csv) {
  all_dfs[[i]] <- read_csv(all_csv[[i]])
}
bind_rows(all_dfs)

#2) What happens if you use for (nm in names(x)) and x has no names? 
#What if only some of the elements are named? What if the names are not unique?

#if x has no names, then the body of the for loop will never get run
#if some of the elements are named, we get an error if we try to access an element without a name
#if names are not unique, and we try to access one of those elements, the call will just provide the first one in the list

#3) Write a function that prints the mean of each numeric column in a data frame, along with its name. 
show_means <- function(x) {
  
  the_class <- vector("logical", length(x))
  for (i in seq_along(x)) the_class[[i]] <- is.numeric(x[[i]])
  
  x <- x[the_class]
  
  for (i in seq_along(x)) {
    cat(paste0(names(x)[i], ": ", round(mean(x[[i]]), 2)), fill = TRUE)
  }
}
show_means(iris)

#21.5.3 #1
#1) Write code that uses one of the map functions to:
#Compute the mean of every column in mtcars.
map_dbl(mtcars, mean)
#Determine the type of each column in nycflights13::flights.
map_chr(nycflights13::flights, typeof)
#Compute the number of unique values in each column of iris.
map_int(iris, function(x) length(unique(x)))
#Generate 10 random normals for each of μ=−10, 0, 10, and 100. 
map(c(-10, 0, 10, 100), ~ rnorm(n = 10, mean = .))
