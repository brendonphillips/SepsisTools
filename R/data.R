#'Class Performance data set
#'
#' Example data set similar to one giving treatment outcomes in the SEPSIS
#' project. For each student/class_teacher combination, the data set gives
#' whether the student has passed Quiz #`test_number` by the second attempt.
#' This data set is used to test the `*_permutation_test*` functions
#' 
#' @format
#' A data frame with 3.618 rows and 6 columns
#' \describe{
#'    \item{student_id}{Unique ID number of the student - similar to the 
#'    staffer IDs generated by the `encode_staffer_ids` function,}
#'    \item{class_teacher}{Name of the faculty member teaching the class,}
#'    \item{quiz_number}{Quiz number N of the term,}
#'    \item{passed_quiz}{Boolean variable telling whether the student passed 
#'    (TRUE) or failed (FALSE) the quiz,}
#'    \item{passed_quiz_numeric}{0/1 whether the student passed or not,}
#'    \item{passed_quiz_string}{"Yes"/"No" string whether the student passed 
#'    the quiz or not.}
#' }
"class_performance"

#' Staffer dictionary
#' 
#' In the SEPSIS project, each staff member in every capacity (field, lab, etc) 
#' was given an ID number, usually of the form Dxxxxx/Nxxxxx (where each x is a 
#' digit 0-9). During QA, we found that these IDs were not unique over the span 
#' of the project and over all encounters, as was presumed. After communication 
#' with the investigative team in Bangladesh, we formulated a lookup table (lut) 
#' that, based on our criteria, maps each data set ID number to a unique ID of 
#' the format `RLxxxxx` (where x is a digit 0-9, with 'RL' an abbreviation for 
#' "Roth Lab"). This lut is used in the function `encode_staffer_ids`.
#' 
#' @format 
#' A data frame with 208 rows and 2 columns
#' \describe{
#'    \item{chrw_id}{The ID of the staffer appearing in the SEPSIS study data 
#'    set [not necessarily a community health research worker (CHRW)],}
#'    \item{encoding}{The new unique Roth Lab ID number that can be used to 
#'    track the worker throughout each encounter of the study.}
#' }
"staffer_dictionary"