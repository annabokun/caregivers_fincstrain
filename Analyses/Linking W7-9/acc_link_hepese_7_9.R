#=======================================================================================================#
# Anna Bokun 
# Postdoc, Population Research Center, University of Texas at Austin
# anna.bokun@austin.utexas.edu

  # "Can I Afford to Support My Aging Parents? Financial Challenges of Adult Children Caregivers: The Cost of Living Together with an Aging Parent"
  # PAA 2025

# Script created in RStudio ("Cranberry Hibiscus")
# updated 12/4/2024
#=======================================================================================================#


  #==============#
  #=== SET-UP ===#
  #==============#

  #### 1.) Set working directory --------------------------- 
    setwd("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files")

    
    
  #### 2.) Download HEPESE waves 7 and 9 ---------------------------
    ## Source: https://www.icpsr.umich.edu/web/NACDA/series/546

    

  #### 3.) Import files ---------------------------
    ## Wave 7 
      load("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Raw Data/Wave 7/Caregivers/36537-0002-Data.rda")
      load("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Raw Data/Wave 7/Care Recipients/36537-0001-Data.rda")
      
    ## Wave 9 
      load("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Raw Data/Wave 9/Caregiver/39038-0002-Data.rda")
      load("~/Library/CloudStorage/GoogleDrive-bokun001@umn.edu/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Raw Data/Wave 9/Care Recipient/39038-0001-Data.rda")
      
    ## Rename files 
      w7_cr <- da36537.0001
      w7_cg <- da36537.0002
      w9_cr <- da39038.0001
      w9_cg <- da39038.0002
      
      
    ## Save new dfs
      write.csv(w7_cr, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/w7_cr.csv")
      write.csv(w7_cg, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/w7_cg.csv")
      write.csv(w9_cr, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/w9_cr.csv")
      write.csv(w9_cg, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/w9_cg.csv")
      
      
      
      
  #============================#
  #=== LINKING ACROSS WAVES ===#
  #============================#

  #### 4.) CAREGIVERS: Merge W7 + W9 by Q_NO ---------------------------
      
      ## 4a.) Restrict W9 caregivers to same as in W7, using the SAME variable 
          # Source: https://www.icpsr.umich.edu/web/NACDA/studies/39038/datasets/0002/variables/SAME?archive=NACDA
        table(w9_cg$SAME) # yes = 218; no = 242
        
        w9_cg_same <- w9_cg %>%
          filter(SAME == "(1) Yes") # = 218
       
         
      ## 4b.) Merge W7 + W9 
          cg_w79 <- merge(w7_cg, w9_cg_same, by = "Q_NO")
          
          
      ## 4c.) Relocate SAME variable to front for easier visibility
          cg_w79 <-  cg_w79 %>%   
            relocate(SAME, .after = Q_NO)
        
        
      ## 4d.) Rename columns that don't ID the wave (i.e., without 7I or 9I) to avoid errors when reshaping
        cg_w79_rename <- cg_w79 %>%
          rename_with(~paste0(.x, "_standalone"), 
                      matches("^(II1[A-D]|JJ[12]|PSS[1-4]I)$"))
        
        
      ## 4e.) Reshape to long
        cg_w79_long <- cg_w79_rename %>%
          pivot_longer(
            cols = matches("(7I|9I)$"), # ID the matching variables between waves
            names_to = c(".value", "wave"),
            names_pattern = "(.+?)(7I|9I)$",
            values_drop_na = TRUE
          ) %>%
          mutate(wave = as.numeric(str_replace(wave, "I", ""))) %>% # Convert wave identifiers to numbers
          select(Q_NO, wave, everything()) # Add the standalone variables back
        
        length(unique(cg_w79_long$Q_NO)) ## 218 UNIQUE CAREGIVERS LINKED ACROSS WAVES 7 + 9 (N = 436)

        
        ## See vars 
          vtable(cg_w79_long)
        
        ## Arrange vars alphabetically
          cg_w79_long <- cg_w79_long[, order(names(cg_w79_long))]
        
        ## Move ID and wave back to the front  
          cg_w79_long <- cg_w79_long %>%   
            relocate(Q_NO, .before = AGE)
          
          cg_w79_long <- cg_w79_long %>%   
            relocate(wave, .after = Q_NO)
   
          
      ## 4f.) Save new dfs
        write.csv(w9_cg_same, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/w9_cg_same.csv")
        write.csv(cg_w79, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/cg_w79.csv")
        write.csv(cg_w79_rename, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/cg_w79_rename.csv")
        write.csv(cg_w79_long, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/cg_w79_long.csv")
        
    

        
                
  #### 5.) CARE RECIPIENTS: Merge W7 + W9 by Q_NO ---------------------------
        
      ## 5a.) Merge W7 + W9 
        cr_w79 <- merge(w7_cr, w9_cr, by = "Q_NO")
        
        
      ## 5b.) Identify variables that end with 7 or 9
        vars7 <- names(cr_w79)[str_detect(names(cr_w79), "7$")]
        vars9 <- names(cr_w79)[str_detect(names(cr_w79), "9$")]
        
        
      ## 5c.) Convert all wave variables to character type first
        cr_w79_long <- cr_w79 %>%
          # First, select only the ID column and variables ending in 7 or 9
          select(Q_NO, matches("7$|9$")) %>%
          # Convert all columns (except ID) to character
          mutate(across(-Q_NO, as.character)) %>%
          # Reshape to long format
          pivot_longer(
            cols = -Q_NO,
            names_to = c(".value", "wave"),
            names_pattern = "(.+?)(7|9)$"
          ) %>%
          # Convert wave to numeric
          mutate(wave = as.numeric(wave))
        
        length(unique(cr_w79_long$Q_NO)) ## 461 UNIQUE CARE RECIPIENTS LINKED ACROSS WAVES 7 + 9 (n = 922)
        

        ## See vars 
          vtable(cr_w79_long)
        
        ## Arrange vars alphabetically
          cr_w79_long <- cr_w79_long[, order(names(cr_w79_long))]
        
        ## Move ID and wave back to the front  
          cr_w79_long <- cr_w79_long %>%   
            relocate(Q_NO, .before = AGE)
          
          cr_w79_long <- cr_w79_long %>%   
            relocate(wave, .after = Q_NO)
        
        
      ## 5d.) Save new dfs
        write.csv(cr_w79, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/Temp/cr_w79.csv")
        write.csv(cr_w79_long, "/Users/annabokun/Google Drive/My Drive/UT-Austin Postdoc/Projects/Caregivers - Housing Health Paradox/Replication Files/Data/cr_w79_long.csv")
    
    
        
  #=================#
  #=== STUDY POP ===#
  #=================#      
          
  #### 1.) ADULT CHILDREN AND STEP-CHILDREN CAREGIVERS (18+)
    ## W7:  
    ## W9:    
        
        
  #### 2.) AGING PARENTS CARE RECIPIENTS 
    ## W7:  
    ## W9:    
        

        
        
    #### SAMPLE SIZES

        
    
        
    
    
    
    
    
    
    
    
    
      
      
      