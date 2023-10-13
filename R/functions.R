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
        {nrow(.) == 0}
    )
}



#' Test statistic calculation
#'
#' Calculate the test statistic (sum of square differences)
#' @param event The scores to be parsed
#' @param group The groups that the scores belong to
#' @return The temperature in degrees Celsius
#' @examples 
#' "coming soon"
#' @export
SS_fn <- function(event, group){
        Overall_rate <- mean(event)
        gp_rates <- tapply(event,group,mean)
        SS <- sum((gp_rates-Overall_rate)^2)
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
                    .export=c("permgp_fn", "SS_fn")
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

