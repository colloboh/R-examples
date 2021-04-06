#This code creates 4 dataframes for grade 3, 6 ,8, & 9 students to aggregate absentee sum based on specific demographics


library(tidyverse)
library(fuzzyjoin)
library(janitor)

afile1920 <- read.csv("SY1920Attendance.csv")

afile1920 %>% 
        #rename GENDER values
        mutate(GENDER = case_when(
                GENDER == 1 ~ "Male",
                GENDER == 2 ~ "Female")) %>% 
        #combine values from 5 race columns then store as a string in new column(Race) 
        mutate(Race = paste0(
                RACE_AMERICAN_INDIAN_OR_ALASKA_NATIVE,
                RACE_ASIAN,
                RACE_BLACK_OR_AFRICAN_AMERICAN,
                RACE_NATIVE_HAWAIIAN_OR_OTHER_PACIFIC_ISLANDER,
                RACE_WHITE)) %>% 
        #rename generated string values in new column to respective race category
        mutate(Race = case_when(
                Race == "10000" ~ "American Indian - Alaska Native",
                Race == "02000" ~ "Asian",
                Race == "00300" ~ "Black - African American",
                Race == "00040" ~ "Native Hawaiian - Pacific Islander",
                Race == "00005" ~ "White",
                TRUE ~ "Two or More")) %>%
        mutate(DAYS_ATTENDING = DAYS_ATTENDING / 10,
               DAYS_ABSENT = DAYS_ABSENT / 10) %>% 
        select(-starts_with("RACE_"))-> afile1920  


#remove afile duplicates & filter to find students in HS for 4 & 5 years
filterAndRemove <- function(df, num, col1, col2, col3) {
        df %>% 
                filter({{col1}} == num) %>% 
                group_by({{col2}}) %>% 
                arrange({{col3}}) %>% 
                filter({{col3}} == last({{col3}})) %>% 
                ungroup()     
}

filterAndRemove(afile1920, 3, GRADE, LOCAL_STUDENT_ID_NUMBER, ENTRY_DATE) ->gd3afile1920


###############################################################################
#group by for race & gender

absRaceGender <- function(df,colName, num, newdf) {
        df %>% 
                group_by(Race, GENDER) %>% 
                summarise(!!(colName) := sum(DAYS_ABSENT %in% num)) %>% 
                adorn_totals("row")->newdf
        #moves total row (at bottom row#13) to the top
        newdf <- newdf[c(nrow(newdf),1:nrow(newdf)-1),]
        #concatenate the race and gender (name) columns into one
        ##then change row value from "Total, -" to "All Students"
        newdf %>% 
                unite("Variables", Race:GENDER, sep = ", ") %>% 
                mutate(Variables = if_else(Variables == "Total, -",
                                           "All Students", Variables))
}

absRaceGender(gd3afile1920, "grade3_abs_5to9days", 5:9, sumdf) -> t1c1.1


####group by for other variables

absOther <- function(df, col1, col1value, colName, num, newValue) {
        df %>% 
                filter({{col1}} == col1value) %>% 
                group_by({{col1}}) %>% 
                summarise(!!(colName) := sum(DAYS_ABSENT %in% num)) %>% 
                #rename 1st column to match other groupby (to make row binding easier)
                rename(Variables = {{col1}}) %>% 
                mutate(Variables = if_else(Variables == col1value,
                                           newValue, Variables))
}

absOther(gd3afile1920, SPECIAL_EDUCATION_SERVICES, "Y", "grade3_abs_5to9days", 5:9, "Student With Disabilities") -> t1c1.2
absOther(gd3afile1920, EL_STATUS, "Y", "grade3_abs_5to9days", 5:9, "English Language Learners") -> t1c1.3
absOther(gd3afile1920, EL_STATUS, "E", "grade3_abs_5to9days", 5:9, "Former ELLs") -> t1c1.4
absOther(gd3afile1920, FREE_REDUCED_PRICE_MEALS, "F", "grade3_abs_5to9days", 5:9, "Eligible for Free/Reduced-Price Lunch") -> t1c1.5

#bind all rows to create complete column
bind_rows(t1c1.1, t1c1.2, t1c1.3, t1c1.4, t1c1.5) ->table1col1



###############################################################################
#use functions fot other columns


#column 2
absRaceGender(gd3afile1920, "grade3_abs_10to19days", 10:19, sumdf) -> t1c2.1

absOther(gd3afile1920, SPECIAL_EDUCATION_SERVICES, "Y", "grade3_abs_10to19days", 10:19, "Student With Disabilities") -> t1c2.2
absOther(gd3afile1920, EL_STATUS, "Y", "grade3_abs_10to19days", 10:19, "English Language Learners") -> t1c2.3
absOther(gd3afile1920, EL_STATUS, "E", "grade3_abs_10to19days", 10:19, "Former ELLs") -> t1c2.4
absOther(gd3afile1920, FREE_REDUCED_PRICE_MEALS, "F", "grade3_abs_10to19days", 10:19, "Eligible for Free/Reduced-Price Lunch") -> t1c2.5

bind_rows(t1c2.1, t1c2.2, t1c2.3, t1c2.4, t1c2.5) ->table1col2


#column 3
##!note 200 is an arbitrary number (greater than the number of school days in a year) - used because i'm lazy and dont want to rewrite the function
absRaceGender(gd3afile1920, "grade3_abs_20plusDays", 20:200, sumdf) -> t1c3.1

absOther(gd3afile1920, SPECIAL_EDUCATION_SERVICES, "Y", "grade3_abs_20plusDays", 20:200, "Student With Disabilities") -> t1c3.2
absOther(gd3afile1920, EL_STATUS, "Y", "grade3_abs_20plusDays", 20:200, "English Language Learners") -> t1c3.3
absOther(gd3afile1920, EL_STATUS, "E", "grade3_abs_20plusDays", 20:200, "Former ELLs") -> t1c3.4
absOther(gd3afile1920, FREE_REDUCED_PRICE_MEALS, "F", "grade3_abs_20plusDays", 20:200, "Eligible for Free/Reduced-Price Lunch") -> t1c3.5

bind_rows(t1c3.1, t1c3.2, t1c3.3, t1c3.4, t1c3.5) ->table1col3

####
####join all 3 columns

left_join(table1col1, table1col2, by = "Variables") -> absesenceTable1
left_join(absesenceTable1, table1col3, by = "Variables") ->absesenceTable1




################################################################################
#table 2
################################################################################
#create a function to reuse prior functions for each grade/table requested

reuseStuff <- function(grade, newcolName, gradeRange){
        #re-use afile function
        filterAndRemove(afile1920, grade, GRADE, LOCAL_STUDENT_ID_NUMBER, ENTRY_DATE) ->filteredGrade
        
        #re-use group/summarize functions (changing just colName argument and output df name)
        absRaceGender(filteredGrade, newcolName, gradeRange, sumdf) -> tc1.1
        
        absOther(filteredGrade, SPECIAL_EDUCATION_SERVICES, "Y", newcolName, gradeRange, "Student With Disabilities") -> tc1.2
        absOther(filteredGrade, EL_STATUS, "Y", newcolName, gradeRange, "English Language Learners") -> tc1.3
        absOther(filteredGrade, EL_STATUS, "E", newcolName, gradeRange, "Former ELLs") -> tc1.4
        absOther(filteredGrade, FREE_REDUCED_PRICE_MEALS, "F", newcolName, gradeRange, "Eligible for Free/Reduced-Price Lunch") -> tc1.5
        
        #bind all rows to create complete column
        bind_rows(tc1.1, tc1.2, tc1.3, tc1.4, tc1.5)
}

###############################################################################
reuseStuff(6, "grade6_abs_5to9days", 5:9) -> table2col1
reuseStuff(6, "grade6_abs_10to19days", 10:19) -> table2col2
reuseStuff(6, "grade6_abs_20plusDays", 20:200) -> table2col3

####join all 3 columns
left_join(table2col1, table2col2, by = "Variables") -> absesenceTable2
left_join(absesenceTable2, table2col3, by = "Variables") ->absesenceTable2



################################################################################
#table 3
################################################################################

reuseStuff(8, "grade8_abs_5to9days", 5:9) -> table3col1
reuseStuff(8, "grade8_abs_10to19days", 10:19) -> table3col2
reuseStuff(8, "grade8_abs_20plusDays", 20:200) -> table3col3

####join all 3 columns
left_join(table3col1, table3col2, by = "Variables") -> absesenceTable3
left_join(absesenceTable3, table3col3, by = "Variables") ->absesenceTable3


################################################################################
#table 4
################################################################################

reuseStuff(9, "grade9_abs_5to9days", 5:9) -> table4col1
reuseStuff(9, "grade9_abs_10to19days", 10:19) -> table4col2
reuseStuff(9, "grade9_abs_20plusDays", 20:200) -> table4col3

####join all 3 columns
left_join(table4col1, table4col2, by = "Variables") -> absesenceTable4
left_join(absesenceTable4, table4col3, by = "Variables") ->absesenceTable4