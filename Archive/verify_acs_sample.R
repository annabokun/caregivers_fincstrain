#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin

# "Can I Afford to Support My Aging Parents? Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
# PAA 2025

  # Verify ACS sample construction 

# Script created in RStudio ("Kousa Dogwood")
# updated 1/27/2025
#=======================================================================================================#


  #### Identify households with both adult children (18+) AND aging parents (80+) ---------------------------
  
  
    ## Version #1 --------------------------- 
      v1 <- mexican_hhs %>%
        filter(GQ %in% c(1, 2)) %>%             # Exclude group quarters 
        group_by(SERIAL) %>%                    # Ensure in same HH 
        filter(
          any(RELATE == 3 & AGE >= 18),         # 18-70 (HEPESE)
          any(RELATE == 5 & AGE >= 79),         # 80-102 (HEPESE)
        ) %>%
        ungroup()
    
        # Unweighted n 
          # n = 2,408  (unweighted) 
        
        # Weighted n 
          acs_ma_n <- sum(v1$PERWT) # 266,826 
          
          
  
          
    ## Version #2 ---------------------------
          v2 <- mexican_hhs %>%
            filter(GQ %in% c(1, 2)) %>%                                   # Exclude group quarters
            group_by(SERIAL) %>%                                          # Ensure all in same HH
            filter(
              any(RELATE == 3 & AGE >= 18 & (MOMLOC > 0 | POPLOC > 0)) &  # Adult child with parent link
                any(RELATE == 5 & AGE >= 79) # Aging parent
            ) %>%
            ungroup()
          
          # Unweighted n 
            # n = 2,408  (unweighted) 
          
          # Weighted n 
            acs_ma_n <- sum(v2$PERWT) # 266,826 
          
           
            
            
      ## Version #3 --------------------------- 
          v4 <- mexican_hhs %>%
            filter(GQ %in% c(1, 2)) %>%
            group_by(SERIAL) %>%
            filter(
              any(RELATE == 3 & AGE >= 18),           # At least one adult child
              any(RELATE == 5 & AGE >= 79)            # At least one aging parent
            ) %>%
            mutate(
              has_direct_ties = any(MOMLOC > 0 | POPLOC > 0) # Check for direct ties
            ) %>%
            ungroup()
          
          # Unweighted n 
            # n = 2,408  (unweighted) 
          
          # Weighted n 
            acs_ma_n <- sum(v3$PERWT) # 266,826 