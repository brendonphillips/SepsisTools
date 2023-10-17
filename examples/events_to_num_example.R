# example of numerical vector
EX_NUM <- c(1,5,2,2,4,3,3,3,5,2,3,1,4,2,4)

# no zero reference given -> returns the values given
events_to_num(EX_NUM)
# zero reference given (and it's in the vector) -> 0/1 vector
events_to_num(EX_NUM, 4)
# zero reference given (even as a string) -> 0/1 vector
events_to_num(EX_NUM, "4")
# zero reference given, not in the vector -> first element of the vector taken as reference
events_to_num(EX_NUM, 6)
# zero reference given as string, not in the vector -> first element of the vector taken as reference
events_to_num(EX_NUM, "6")

# example of boolean vector
EX_BOOL <- c(TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE)

# # no zero reference given -> original vector returned
# events_to_num(EX_BOOL)
# zero reference 
# events_to_num(EX_BOOL, 4)
# events_to_num(EX_BOOL, "4")
# events_to_num(EX_BOOL, TRUE)
# events_to_num(EX_BOOL, "TRUE")

# paste0("c(", paste(sample(1:5, 15, replace = TRUE), collapse = ","), ")")
# paste0("c(", paste(sample(c(T, F), 15, replace = TRUE), collapse = ","), ")")


# # paste0("c(", paste(sample(1:5, 15, replace = TRUE), collapse = ","), ")")
# haha_num <- c(1,5,2,2,4,3,3,3,5,2,3,1,4,2,4)
# 
# events_to_num(haha_num)
# events_to_num(haha_num, 4)
# events_to_num(haha_num, "4")
# events_to_num(haha_num, 6)
# events_to_num(haha_num, "6")
# 
# # paste0("c(", paste(sample(c(T, F), 15, replace = TRUE), collapse = ","), ")")
# haha_bool <- c(TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE)
# events_to_num(haha_bool)
# events_to_num(haha_bool, 4)
# events_to_num(haha_bool, "4")
# events_to_num(haha_bool, TRUE)
# events_to_num(haha_bool, "TRUE")

# # paste0("c('", paste(sample(1:5, 15, replace = TRUE), collapse = "','"), "')")
# haha_num_string <- c('4','3','2','1','2','5','3','2','4','5','3','1','4','5','3')
# 
# # paste0("c('", paste(sample(letters[1:5], 15, replace = TRUE), collapse = "','"), "')")
# haha_string <- c('d','a','e','e','b','d','e', '6','e','e','a','d','e','a','e','d')

# print(haha_map(haha_num))
# print(haha_map(haha_num, 2))

# print(haha_map(haha_num_string))
# print(haha_map(haha_num_string), 7)


# print(haha_map(haha_string))


