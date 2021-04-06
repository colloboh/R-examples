#this script uses the fuzzyjoin package to match misspelled names from one dataframe (consent) to another
#official data frame with correct spelling. when matched, the individual's unique ID can be used to extract
#necessary variables


library(tidyverse)
library(fuzzyjoin)
library(xlsx)


afile1920 <- read.csv("1920attendance.csv")
consent <- read.csv("consent.csv")

#remove afile duplicates
afile1920 %>% 
        group_by(LOCAL_STUDENT_ID_NUMBER) %>% 
        arrange(ENTRY_DATE) %>% 
        filter(ENTRY_DATE == last(ENTRY_DATE)) %>% 
        ungroup()->afile1920

#remove whitespace from afile name columns
afile1920$FIRST_NAME <- gsub('\\s+', '', afile1920$FIRST_NAME)
afile1920$LAST_NAME <- gsub('\\s+', '', afile1920$LAST_NAME)

afile1920 %>% 
        select(LOCAL_STUDENT_ID_NUMBER,
               FIRST_NAME,
               LAST_NAME,
               GRADE,
               SCHOOL_NUMBER) -> afile

consent %>% 
        mutate(grade1920 = Grade -1) -> consent

#When/before using stringdist, NA's in df may need to be replaced with and empty string
consent[is.na(consent)] <- "empty_string"

stringdist_left_join(consent, afile, by = c("First_Name" = "FIRST_NAME",
                                            "Last_Name" = "LAST_NAME",
                                            "School_Number" = "SCHOOL_NUMBER"),
                     max_dist = 1, distance_col = "difference") -> join1


join1 %>% 
        filter(!is.na(GRADE)) -> match1

join1 %>% 
        filter(is.na(GRADE)) -> noMatch1

noMatch1 %>% 
        select(1:9) %>% 
        mutate(first.nameSUBStr = substr(First_Name, 1,2),
               last.nameSUBStr = substr(Last_Name, 1,2))-> noMatch1

afile %>% 
        mutate(first.nameSUBStr = substr(FIRST_NAME, 1,2),
               last.nameSUBStr = substr(LAST_NAME, 1,2))-> afile

afile %>% 
        filter(SCHOOL_NUMBER == 382,
               GRADE %in% 7:11) -> afile



stringdist_left_join(noMatch1, afile, by = c("first.nameSUBStr",
                                             "Last_Name" = "LAST_NAME",
                                             "grade1920" = "GRADE"),
                     max_dist = 1, distance_col = "difference") -> join2

match2 <- join2[c(1,2,8,12,13),]
noMatch2 <- join2[c(3:7, 9:11, 14:nrow(join2)),]

noMatch2 %>% 
        select(1:8)-> noMatch2


#remove duplicate rows
noMatch2 %>% distinct(First_Name, Last_Name, .keep_all = TRUE) %>% 
        filter(Last_Name != "Brown")-> noMatch2



match1 %>% 
        select(10:12,
               3:8,
               13) %>% 
        rename(GRADE_SY1920 = GRADE) -> match1

match2 %>% 
        select(12:14,
               3:8,
               15) %>% 
        rename(GRADE_SY1920 = GRADE) -> match2

bind_rows(match1, match2) -> fullmatch
################################################################################

write.xlsx(as.data.frame(fullmatch), file = "consentNo2.xlsx",
           sheetName = "match", append = FALSE)

write.xlsx(as.data.frame(noMatch2), file = "consentNo2.xlsx",
           sheetName = "no match", append = TRUE)
