# Homework 1
# Name: Taksh Yadav


student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section    <- c("A", "B", "A", "B", "A", "B")
quiz1      <- c(82, 91, 76, 88, 95, 69)
quiz2      <- c(85, 89, 80, 92, 94, 74)
passed     <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)


section <- factor(section, levels = c("A", "B"))

students <- data.frame(
  student_id = student_id,
  section    = section,
  quiz1      = quiz1,
  quiz2      = quiz2,
  passed     = passed,
  stringsAsFactors = FALSE
)

score_matrix <- matrix(
  c(quiz1, quiz2),
  nrow = length(student_id),
  ncol = 2,
  dimnames = list(student_id, c("quiz1", "quiz2"))
)

course_record <- list(
  course  = "R Programming",
  scores  = students,
  cutoffs = c(pass = 70, excellent = 90)
)

typeof(students);      class(students);      length(students);      str(students);      dim(students)
typeof(score_matrix);  class(score_matrix);  length(score_matrix);  str(score_matrix);  dim(score_matrix)
typeof(course_record); class(course_record); length(course_record); str(course_record); dim(course_record)
typeof(section);       class(section);       length(section);       str(section)
typeof(quiz1);         class(quiz1);         length(quiz1);         str(quiz1)

# A vector is a single flat sequence of values and they're all the same type of values 
# A factor is a special type of vector vector for catagories of data. It stores integer codes plus a set of levels.
# A matrix is a 2-dimensional collection of values also of the same type which is arranged in rows and columns.
# A list is a flexible container where each element can be a different
# A data frame is a special list where each element is a vector of the same length, but can be different in type 



score_matrix["S04", "quiz2"]

score_matrix[1:2, , drop = FALSE]

course_record["course"]
course_record[["course"]]
course_record$course

# '[' returns a sub-object of the same class as the original 
# '[[' extracts a single element and drops the outer structure, returning the element itself.
# '$' is shorthand for '[[' by name only and only works with a literal name, not with a variable holding a name.

students$average <- rowMeans(students[, c("quiz1", "quiz2")])

students$excellent <- students$average >= 90

sectionA_high <- students[students$section == "A" & students$average >= 80, ]

sectionA_high_subset <- sectionA_high[, c("student_id", "section", "average")]
sectionA_high_subset

avg_named <- students$average
names(avg_named) <- students$student_id
avg_named

# These calculations are vectorized because functions like rowMeans() and the comparison operators (>=, ==, &) operate on entire vectors/columns at once,
# element by element, without an explicit loop


csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"



measurements <- read.csv(text = csv_text, na.strings = c("", "NA"), stringsAsFactors = FALSE)

head(measurements)
str(measurements)
dim(measurements)
names(measurements)

colSums(is.na(measurements))

measurements_complete <- measurements[complete.cases(measurements), ]

removed_ids <- setdiff(measurements$sample_id, measurements_complete$sample_id)
removed_ids

# x == NA is not valid because NA represents an unknown value.

measurements$site   <- factor(measurements$site)
measurements$status <- factor(measurements$status)
levels(measurements$site)
levels(measurements$status)

measurements$temp_f <- measurements$temp_c * 9 / 5 + 32

measurements$ph_below_7 <- ifelse(is.na(measurements$ph), NA, measurements$ph < 7)

subset_ns_ok <- measurements[
  complete.cases(measurements) &
  measurements$site %in% c("North", "South") &
  measurements$status == "ok",
]

subset_ns_ok <- subset_ns_ok[, c("sample_id", "site", "temp_c", "temp_f", "ph")]
subset_ns_ok

mean_temp_overall <- mean(measurements$temp_c, na.rm = TRUE)
mean_temp_south   <- mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)
mean_temp_overall
mean_temp_south


A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

elementwise <- A * B
matmul      <- A %*% B

elementwise
matmul
dim(elementwise)
dim(matmul)

# A * B multiplies elements of A and B one-by-one, requiring both matrices to have the same dimensions
# A %*% B performs matrix multiplication (row-by-column, dot products), the number of columns of A to match the number of rows of B


student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)

grade_one <- function(score, a_min = 90, b_min = 80, c_min = 70, d_min = 60) {
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return("A")
  } else if (score >= b_min) {
    return("B")
  } else if (score >= c_min) {
    return("C")
  } else if (score >= d_min) {
    return("D")
  } else {
    return("F")
  }
}

grade_one(NA)   
grade_one(90)   
grade_one(80)   
grade_one(85)  
grade_one(74)   


grades <- rep(NA_character_, length(scores))
for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}
names(grades) <- student_id
grades

# During the loop, i represents the current index/position being processed.

summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  c(
    total   = length(x),
    missing = sum(is.na(x)),
    mean    = round(mean(x, na.rm = na.rm), digits),
    sd      = round(sd(x, na.rm = na.rm), digits),
    min     = round(min(x, na.rm = na.rm), digits),
    max     = round(max(x, na.rm = na.rm), digits)
  )
}

summarize_scores(scores)

summarize_scores(x = scores, na.rm = TRUE, digits = 2)

plot_scores <- function(x, ...) {
  plot(seq_along(x), x, ...)
}

plot_scores(scores, type = "b", pch = 19, xlab = "Position", ylab = "Score",
            main = "Student Scores")

# plot_scores() plots each score against its position in the vector. The "..." lets the caller pass any additional parameters straight through to
# plot(). With type = "b", points are shown with connecting lines, solid circles for score, and the plot has been labeled.
