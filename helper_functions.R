


removeDuplicates <- function(d) {
    
    d <- d %>%
        mutate(code1 = ifelse(ccode1 < ccode2,
                              ccode1,
                              ccode2),
               code2 = ifelse(ccode1 < ccode2,
                              ccode2,
                              ccode1)) %>%
        group_by(code1, code2, year) %>%
        arrange(code1) %>%
        filter(!duplicated(code1)) %>%
        ungroup() %>%
        select(-code1, -code2)
    
    return(d)
    
}

labelBadYears <- function(d) {
    
    d <- d %>%
        mutate(is_bad_transition = year > 1949 & year < 1971)
    
    return(d)
}



addTransition <- function(d) {
    
    d <- d %>%
        group_by(ccode1, ccode2) %>%
        arrange(year) %>%
        mutate(prev_DR_at_1 = lag(DR_at_1),
               prev_DR_at_2 = lag(DR_at_2),
               is_same = prev_DR_at_1 == DR_at_1 &
                   prev_DR_at_2 == DR_at_2,
               is_upgrade = (prev_DR_at_1 < DR_at_1 &
                                 prev_DR_at_2 <= DR_at_2) |
                   (prev_DR_at_1 <= DR_at_1 &
                        prev_DR_at_2 < DR_at_2),
               is_downgrade = (prev_DR_at_1 > DR_at_1 &
                                   prev_DR_at_2 >= DR_at_2) |
                   (prev_DR_at_1 >= DR_at_1 &
                        prev_DR_at_2 > DR_at_2),
               is_mixed = !is.na(is_same) & !is_same & !is_downgrade & !is_upgrade) %>%
        ungroup()
    
    return(d)
}


addTimeInCurrentState <- function(d) {
    
    pairCodes <- paste(d$ccode1, d$ccode2, sep = ",")
    uniqueCodes <- unique(pairCodes)
    d$time_curr <- 0
    
    for (i in 1:length(uniqueCodes)) {
        inds <- which(pairCodes == uniqueCodes[i])
        lenInds <- length(inds)
        
        if (lenInds > 1) {
            for (r in 2:(length(inds))){
                # There was not a state change, so increase time_curState
                if (d$is_same[inds[r]]) {
                    d$time_curr[inds[r]] <- d$time_curr[inds[r - 1]] + 
                        d$year[inds[r]]- d$year[inds[r - 1]]
                    
                # or, there was a state change, so time.curr is zero
                } else {
                    d$time_curr[inds[r]] = 0
                }
            }
        }
    }
    
    return(d)
    
}



addPrevSymStatus <- function(d) {
    
    d <- d %>%
        mutate(is_prev_sym = lag(DR_at_1) == lag(DR_at_2) & 
                   lag(DR_at_1) != 0,
               is_prev_asym = lag(DR_at_1) != lag(DR_at_2),
               is_prev_no_link = lag(DR_at_1) == 0 & 
                   lag(DR_at_2) == 0,
               is_prev_imbal = xor(lag(DR_at_1) == 0, lag(DR_at_2) == 0))
    
    return(d)
    
    
}


addRegions <- function(d, regions) {
    
    d <- d %>%
        inner_join(select(regions, ccode1 = CCode, region1 = Region)) %>%
        inner_join(select(regions, ccode2 = CCode, region2 = Region)) %>%
        mutate(is_same_region = region1 == region2,
               region = case_when(is_same_region ~ region1,
                                  region1 == 1 & region2 == 2 ~ 6,
                                  region1 == 2 & region2 == 1 ~ 6,
                                  region1 == 1 & region2 == 3 ~ 7,
                                  region1 == 3 & region2 == 1 ~ 7,
                                  region1 == 1 & region2 == 4 ~ 8,
                                  region1 == 4 & region2 == 1 ~ 8,
                                  region1 == 1 & region2 == 5 ~ 9,
                                  region1 == 5 & region2 == 1 ~ 9,
                                  region1 == 2 & region2 == 3 ~ 10,
                                  region1 == 3 & region2 == 2 ~ 10,
                                  region1 == 2 & region2 == 4 ~ 11,
                                  region1 == 4 & region2 == 2 ~ 11,
                                  region1 == 2 & region2 == 5 ~ 12,
                                  region1 == 5 & region2 == 2 ~ 12,
                                  region1 == 3 & region2 == 4 ~ 13,
                                  region1 == 4 & region2 == 3 ~ 13,
                                  region1 == 3 & region2 == 5 ~ 14,
                                  region1 == 5 & region2 == 3 ~ 14,
                                  region1 == 4 & region2 == 5 ~ 15,
                                  region1 == 5 & region2 == 4 ~ 15)) %>%
        select(-region1, -region2)
    
    return(d)
    
    
}


addNMC <- function(d, nmc) {
    
    d <- d %>%
        inner_join(select(nmc, ccode1 = ccode, cinc1 = cinc)) %>%
        inner_join(select(nmc, ccode2 = ccode, cinc2 = cinc))
    
    return(d)
    
    
}
