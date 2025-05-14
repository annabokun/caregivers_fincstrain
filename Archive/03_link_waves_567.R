#=======================================================================================================#
# Anna Bokun: bokun001@umn.edu
# Sociology PhD Candidate, University of Minnesota
# Postdoc, Population Research Center, University of Texas-Austin

# ICAA 2024
  # "Living with An Aging Parent in the U.S. and Mexico (or California and Texas): Who benefits, Mom, Dad, (Elderly Parent) or her Children?”  
  # Authors: Anna Bokun, Jacqui Angel, Sunshine Rote, and Phil Cantu

  # Linking waves 5 + 6 + 7 

# Script created in RStudio ("Chocolate Cosmos")
# updated 7/22/2024
#=======================================================================================================#



#==============#
#=== SET-UP ===#
#==============#

#### Load packages 
  library(tidyverse)
  library(vtable)
  library(tabulator)



#### Load data 
  # HEPESE, wave 7 (2010-2011): https://www.icpsr.umich.edu/web/NACDA/studies/36537  
  # Google Drive --> UT-Austin --> ICAA 2024 --> Caregivers - Housing Health Paradox --> Data --> Raw Data
  
    # WAVE 5 
      wave5 <- read_dta("Data/Raw Data/W5 (2).dta")
        length(unique(wave5$Q_NO)) # n = 2069
      
    # WAVE 6 
      wave6 <- read_dta("Data/Raw Data/W6 (2).dta")
        length(unique(wave6$Q_NO)) # n = 1542
    
    # WAVE 7 
      wave7 <- read_csv("Data/samp.csv", col_types = cols(...1 = col_skip()))
        length(unique(wave7$Q_NO)) # n = 1078
      
    
    
  
    
    
    
  #================#
  #=== CLEANING ===#
  #================#   


  #### 1.) Check variables ---------------------------
    vtable(wave5)
    vtable(wave6)
    
    
    
  #### 2.) Make sure all variables end in either a 5,6,7 (to indicate wave) --> if not, rename
    # Most already have that, but some don't 
    # Need this to later reshape from wide to long 
      wave5 <- wave5 %>%
        rename_with(~ paste0(., "5"), -Q_NO)
      
      wave6 <- wave6 %>%
        rename_with(~ paste0(., "6"), -Q_NO)
      
      wave7 <- wave7 %>%
        rename_with(~ paste0(., "7"), -Q_NO)  
    
    
    
  #### 2.) Merge waves 5 + 6  ---------------------------
    waves5_6 <- merge(wave5, wave6, by = "Q_NO", all = TRUE)
    
    
    
  #### 3.) Merge waves5_6 + wave7  ---------------------------
    waves_567 <- merge(waves5_6, wave7, by = "Q_NO", all = TRUE)
    
        # See variables 
        vtable(waves_567)
        
        # How many unique individuals?
        length(unique(waves_567$Q_NO)) # n = 2181
    
        
    
    
#### SAVE ---------------------------
write.csv(waves5_6, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/ICAA 2024/Caregivers - Housing Health Paradox/Replication Files/Data/waves5_6.csv")
write.csv(waves_567, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/ICAA 2024/Caregivers - Housing Health Paradox/Replication Files/Data/waves_567.csv")
    





  #### Relevant "movement" variables --------------------------- 
      ## Wave 5
        # WHOMOV51:WHOMOV54 == 3 (Son/daughter)
        # TAKECARE5: I needed help taking care of myself or the house
        # PLASTAY5: (Person who moved in) needed a place to stay
        # KIDSHME5: Children live in household
    
      ## Wave 6 
        # WHOMOV61:WHOMOV64 == 3 (Son/daughter)
          # TAKECARE6: I needed help taking care of myself or the house
          # PLASTAY6: (Person who moved in) needed a place to stay
          # KIDSHME6: Children live in household
      
      ## Wave 7 
        # WHOMOV71:WHOMOV74 == 3 (Son/daughter)
        # TAKECARE7: I needed help taking care of myself or the house
        # PLASTAY7: (Person who moved in) needed a place to stay
        # KIDSHME7: Children live in household





  #### 4.) Generate a dummy: an adult child moved in with the care recipient over the last 6 years ---------------------------
    
      # WHOMOV51 --> https://www.icpsr.umich.edu/web/NACDA/studies/25041/datasets/0001/variables/WHOMOV51?archive=nacda
      # WHOMOV61 --> https://www.icpsr.umich.edu/web/NACDA/studies/29654/datasets/0001/variables/WHOMOV61?archive=NACDA
      # WHOMOV71 --> https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0001/variables/WHOMOV71?archive=NACDA
          # == 3 (son/daughter)


          ## Did your adult child move in with you?
          
                # Wave 5 
                    waves_567 <- waves_567 %>%
                          mutate(kids_move_in5 = if_else(
                            WHOMOV515 == 3 | WHOMOV525 == 3 | WHOMOV535 == 3 | WHOMOV545 == 3,
                            1, 0
                          ))
    
                # Wave 6 
                    waves_567 <- waves_567 %>%
                          mutate(kids_move_in6 = if_else(
                            WHOMOV616 == 3 | WHOMOV626 == 3 | WHOMOV636 == 3 | WHOMOV646 == 3,
                            1, 0
                          ))
                    
                # Wave 7 
                    waves_567 <- waves_567 %>%
                          mutate(kids_move_in7 = if_else(
                            WHOMOV717 == 3 | WHOMOV727 == 3 | WHOMOV737 == 3 | WHOMOV747 == 3,
                            1, 0
                          ))


            ## Check kids_move_in
                kids_move_in_count <- waves_567 %>%
                  summarize(
                    total_move_in_5 = sum(kids_move_in5 == 1, na.rm = TRUE),
                    total_move_in_6 = sum(kids_move_in6 == 1, na.rm = TRUE),
                    total_move_in_7 = sum(kids_move_in7 == 1, na.rm = TRUE)
                  )
                        # wave 5: 115
                        # wave 6: 84
                        # wave 7: 88
                          # = 287
                
                
                
                
                
                
                
                
  #### 5.) Generate a dummy: adult child moved in because they needed a place to stay ---------------------------
                
      # PLASTAY55 --> https://www.icpsr.umich.edu/web/NACDA/studies/25041/datasets/0001/variables/PLASTAY5?archive=NACDA
            # == 1 (yes)
      # PLASTAY66 --> https://www.icpsr.umich.edu/web/NACDA/studies/29654/datasets/0001/variables/PLASTAY6?archive=NACDA
            # == 1 (yes)
      # PLASTAY77 --> https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0001/variables/PLASTAY7?archive=NACDA
            # == 2 (yes) 
                
      # ^^^ coding scheme changes for wave 7!!! 
                
                
                # Wave 5 
                    waves_567 <- waves_567 %>%
                          mutate(kids_move_in_playstay5 = if_else(
                            WHOMOV515 == 3 & PLASTAY55 == 1 | 
                              WHOMOV525 == 3 & PLASTAY55 == 1 | 
                              WHOMOV535 == 3 & PLASTAY55 == 1 | 
                              WHOMOV545 == 3 & PLASTAY55 == 1,
                            1, 0
                          ))
    
                # Wave 6 
                    waves_567 <- waves_567 %>%
                          mutate(kids_move_in_playstay6 = if_else(
                            WHOMOV616 == 3 & PLASTAY66 == 1 | 
                              WHOMOV626 == 3 & PLASTAY66 == 1 | 
                              WHOMOV636 == 3 & PLASTAY66 == 1 |
                              WHOMOV646 == 3 & PLASTAY66 == 1,
                            1, 0
                          ))
                    
                # Wave 7 
                    waves_567 <- waves_567 %>%
                          mutate(kids_move_in_playstay7 = if_else(
                            WHOMOV717 == 3 & PLASTAY77 == 2 | 
                              WHOMOV727 == 3 & PLASTAY77 == 2 | 
                              WHOMOV737 == 3 & PLASTAY77 == 2 | 
                              WHOMOV747 == 3 & PLASTAY77 == 2,
                            1, 0
                          ))


            ## Check kids_move_in_playstay 
                kids_move_in_playstay_count <- waves_567 %>%
                  summarize(
                    total_playstay5 = sum(kids_move_in_playstay5 == 1, na.rm = TRUE),
                    total_playstay6 = sum(kids_move_in_playstay6 == 1, na.rm = TRUE),
                    total_playstay7 = sum(kids_move_in_playstay7 == 1, na.rm = TRUE)
                  )
                            # wave 5: 37
                            # wave 6: 37
                            # wave 7: 35 
                              # = 109 
                
                

                
    
   
                           
  #### 6.) Generate a dummy: adult child moved in because because I (care recipient) needed help ---------------------------
             
      # TAKECARE55 --> https://www.icpsr.umich.edu/web/NACDA/studies/25041/datasets/0001/variables/TAKECARE5?archive=NACDA
                # == 1 (yes)
      # TAKECARE66 --> https://www.icpsr.umich.edu/web/NACDA/studies/29654/datasets/0001/variables/TAKECARE6?archive=NACDA
                # == 1 (yes)
      # TAKECARE77 --> https://www.icpsr.umich.edu/web/NACDA/studies/36537/datasets/0001/variables/TAKECARE7?archive=NACDA
                # == 1 (yes)
                
                # Wave 5 
                    waves_567 <- waves_567 %>%
                          mutate(kids_move_in_take_care5 = if_else(
                            WHOMOV515 == 3 & TAKECARE55 == 1 | 
                              WHOMOV525 == 3 & TAKECARE55 == 1 | 
                              WHOMOV535 == 3 & TAKECARE55 == 1 | 
                              WHOMOV545 == 3 & TAKECARE55 == 1,
                            1, 0
                          ))
    
                # Wave 6 
                    waves_567 <- waves_567 %>%
                          mutate(kids_move_in_take_care6 = if_else(
                            WHOMOV616 == 3 & TAKECARE66 == 1 | 
                              WHOMOV626 == 3 & TAKECARE66 == 1 | 
                              WHOMOV636 == 3 & TAKECARE66 == 1 |
                              WHOMOV646 == 3 & TAKECARE66 == 1,
                            1, 0
                          ))
                    
                # Wave 7 
                    waves_567 <- waves_567 %>%
                          mutate(kids_move_in_take_care7 = if_else(
                            WHOMOV717 == 3 & TAKECARE77 == 1 | 
                              WHOMOV727 == 3 & TAKECARE77 == 1 | 
                              WHOMOV737 == 3 & TAKECARE77 == 1 | 
                              WHOMOV747 == 3 & TAKECARE77 == 1,
                            1, 0
                          ))


            ## Check kids_move_in_take_care
              kids_move_in_take_carecount <- waves_567 %>%
                  summarize(
                    total_takecare5 = sum(kids_move_in_take_care5 == 1, na.rm = TRUE),
                    total_takecare6 = sum(kids_move_in_take_care6 == 1, na.rm = TRUE),
                    total_takecare7 = sum(kids_move_in_take_care7 == 1, na.rm = TRUE)
                  )
                            # wave 5: 53
                            # wave 6: 40
                            # wave 7: 47 
                              # =  140
                
                
                
            


              
              
              
              
  #### 7.) Reshape to long ---------------------------
    long_567 <- waves_567 %>%
      pivot_longer(
        cols = ends_with(c("5", "6", "7")), # ID columns that end with 5, 6, or 7
        names_to = c(".value", "wave"),
        names_pattern = "(.+)([567])$",
        values_to = "value"
      ) %>%
      mutate(wave = paste0("wave_", wave)) %>%
      arrange(Q_NO, wave)
              
              
    ## Check that each Q_NO has 3 rows (waves 5, 6, 7)
      row_counts <- long_567 %>%
        group_by(Q_NO) %>%
        summarize(count = n())
      
      does_not_have3 <- row_counts %>%
        filter(count !=3) # everyone has 3 rows. yaay!
               
      
    ## Remove "wave" in wave_5, wave_6, wave_7 in the "wave column"
      long_567 <- long_567 %>%
        mutate(wave = gsub("wave_", "", wave))     
      
      
    ## Move "movement" variables up 
      long_567 <- long_567 %>%   
        relocate(kids_move_in, .after = wave) %>%
        relocate(kids_move_in_playstay, .after = kids_move_in) %>%
        relocate(kids_move_in_take_care, .after = kids_move_in_playstay) %>%
        relocate(WHOMOV51, .after = kids_move_in_take_care) %>%
        relocate(WHOMOV52, .after = WHOMOV51) %>%
        relocate(WHOMOV53, .after = WHOMOV52) %>%
        relocate(WHOMOV54, .after = WHOMOV53) %>%
        relocate(WHOMOV61, .after = WHOMOV54) %>%
        relocate(WHOMOV62, .after = WHOMOV61) %>%
        relocate(WHOMOV63, .after = WHOMOV62) %>%
        relocate(WHOMOV64, .after = WHOMOV63) %>%
        relocate(WHOMOV71, .after = WHOMOV64) %>%
        relocate(WHOMOV72, .after = WHOMOV71) %>%
        relocate(WHOMOV73, .after = WHOMOV72) %>%
        relocate(WHOMOV74, .after = WHOMOV73) %>%
        relocate(PLASTAY5, .after = WHOMOV74) %>%
        relocate(PLASTAY6, .after = PLASTAY5) %>%
        relocate(PLASTAY7, .after = PLASTAY6) %>%
        relocate(TAKECARE5, .after = PLASTAY7) %>%
        relocate(TAKECARE6, .after = TAKECARE5) %>%
        relocate(TAKECARE7, .after = TAKECARE6) 
      
      

      
      
  ## Count adult children "movement" variables  
      kids_move_in_unique <- long_567 %>%
        filter(kids_move_in == 1) %>%
        summarize(count = n_distinct(Q_NO))
      
      
      kids_move_in_playstay_unique <- long_567 %>%
        filter(kids_move_in_playstay == 1) %>%
        summarize(count = n_distinct(Q_NO))
      
      
      kids_move_in_take_care_unique<- long_567 %>%
        filter(kids_move_in_take_care == 1) %>%
        summarize(count = n_distinct(Q_NO))

        
      
      
      
      
#### SAVE --------------------------- 
write.csv(long_567, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/ICAA 2024/Caregivers - Housing Health Paradox/Replication Files/Data/long_567.csv")
      
    
      
      
      
      
      
      