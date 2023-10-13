## Brendon Phillips
#B comment-B is my signature

if (!require("pacman")) install.packages("pacman")
pacman::p_load(doParallel, dplyr)

options(dplyr.summarise.inform = FALSE)

is_systematic <- function(DT) {
    return(DT %>%
               group_by(part_id) %>%
               summarise(split_acros_n_groups = length(unique(group_perm))) %>%
               subset(split_acros_n_groups > 1) %>%
               {nrow(.) == 0})
}

haha_map <- function(list_, zero_reference = NaN, ...) {
    
    zero_ref <- zero_reference
    the_list <- list_
    
    warning_string <-  gsub(
        " +", " ",
        "The given zero-reference member `%s` is not a member of the 
        given list. Using the first element of the list `%s` as the zero"
    )
    
    # fix this function for booleans
    
    if(! any(is.na( as.numeric(the_list) )) ) {
        
        if(is.na(zero_ref)){ 
            return(the_list)
        } else if(! as.numeric(zero_ref) %in% the_list) {
            warning(sprintf(warning_string, zero_ref, the_list[1]))
            return( as.numeric(the_list == the_list[1]) )
        } else {
            return( as.numeric(the_list == zero_ref) )
        }
    }
    
    # # if any element of the list is not castable to a number, create map 
    # # ourselves
    # if( any(is.na( as.numeric(the_list) )) ) {
    #     
    #     the_list <- as.character(list_)
    #     zero_elem <- the_list[1]
    #     
    #     if(is.na(zero_reference)) {
    #         
    #        
    #         
    #     } else if(! toString(zero_reference) %in% the_list) {
    #         warning(sprintf(
    #             "The given zero-reference member %s is not a member of the 
    #             given list. Using the first element of the list %s as the zero",
    #             zero_reference, the_list[1]
    #         ))
    #         zero_elem <- the_list[1]
    #     }
    #     
    #     return( as.numeric(the_list == zero_elem) )
    #     
    # } else {
    #     
    #     return(as.numeric(the_list))
    # }
}

# paste0("c(", paste(sample(1:5, 15, replace = TRUE), collapse = ","), ")")
haha_num <- c(1,5,2,2,4,3,3,3,5,2,3,1,4,2,4)

# haha_map(haha_num)
# haha_map(haha_num, 4)
# haha_map(haha_num, "4")
# haha_map(haha_num, 6)
# haha_map(haha_num, "6")

# # paste0("c(", paste(sample(c(T, F), 15, replace = TRUE), collapse = ","), ")")
# haha_bool <- c(TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE)
# haha_map(haha_bool)
# haha_map(haha_bool, 4)
# haha_map(haha_bool, "4")
# haha_map(haha_bool, TRUE)
# haha_map(haha_bool, "T")
# haha_map(haha_bool, "TRUE")





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


SS_fn <- function(event, group){
    Overall_rate <- mean(event)
    gp_rates <- tapply(event,group,mean)
    SS <- sum((gp_rates-Overall_rate)^2)
    # SS <- sum(abs(gp_rates)>=abs(Overall_rate))/1000
    return(SS)
}

SS_fn_testing <- function(event, group){
    Overall_rate <- mean(event)
    gp_rates <- tapply(event,group,mean)
    SS <- sum((gp_rates-Overall_rate)^2)
    # SS <- sum(abs(gp_rates)>=abs(Overall_rate))/1000
    return(SS)
}


permgp_fn <- function(data_, groupname, idname, eventname, systematic){
    
    if(systematic) {
        
        data_perm <- data_ %>% 
            select(all_of(c(idname, groupname))) %>%
            unique() %>%
            mutate(
                group_perm = sample(
                    x = .data[[groupname]], 
                    size = nrow(.), 
                    replace = FALSE)
            ) %>%
            right_join(
                data_ %>% select(-all_of(c(groupname))),
                by = join_by(!!as.name(idname))
            ) %>%
            mutate()
        
        # print(is_systematic(data_perm))
        
    } else {
        
        data_perm <- data_ %>%
            mutate(group_perm = sample(
                x = .data[[groupname]], 
                size = nrow(.), 
                replace = FALSE)
            )
        
        # print(is_systematic(data_perm))
    }
    
    teststat_perm <- SS_fn(
        data_perm[[eventname]], 
        data_perm$group_perm
    )
    
    return(teststat_perm)
}


perm_test <- function(DF, groupname = "group", idname = "id", eventname = "event", 
                      ntrials = 1000, parallel = TRUE, ranseed = NaN, 
                      systematic = TRUE, ...) {
    
    if(!is.na(ranseed) & is.numeric(ranseed)) {
        set.seed(ranseed)
    } else {
        set.seed(Sys.time())
    }
    
    teststat <- SS_fn(DF[[eventname]], DF[[groupname]])
    
    if(parallel | (ntrials > 1000)) {
        
        permCluster <- makeCluster(max(detectCores()-2, 1), type = "PSOCK")
        registerDoParallel(permCluster)
        
        teststat_null <- tryCatch(
            {
                
                foreach(
                    1:ntrials, 
                    .combine = "c", 
                    .packages = c("dplyr"),
                    .export=c("permgp_fn", "SS_fn", "is_systematic")
                ) %dopar% {
                    
                    permgp_fn(DF, groupname, idname, eventname, systematic)
                }
            },
            
            error = function(e) { 
                print(sprintf("Error encountered during parallel execution: %s", e))
                return(NaN)
            },
            
            warning = function(w) { print("warnings encountered") }
        )
        
        stopCluster(permCluster)
        
    } else {
        
        teststat_null <- array(data = NaN, dim=ntrials)
        
        for(it in 1:ntrials) {
            teststat_null[it] <- permgp_fn(DF, groupname, idname, eventname, systematic)
        }
    }
    
    p_value <- mean(teststat_null>=teststat, na.rm=TRUE) 
    
    return(p_value)
    
}

# library(rstudioapi)
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# 
# library(readr)
# 
# DF <- read_csv("data.csv") %>%
#     select(-any_of(c("...1", "X"))) %>%
#     mutate(across(c(everything(), -event), factor)) %>%
#     mutate()
# 
# group_name = "group"
# id_name = "part_id"
# event_name = "event"
# 
# test_p <- perm_test(
#         DF, 
#         groupname = group_name,
#         idname = id_name,
#         eventname = event_name,
#         parallel = FALSE, 
#         ntrials=1000, 
#         ran_seed = 301031,
#         systematic = TRUE
#     )
# 
# print(test_p)
