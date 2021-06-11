#ctrl+alt+C to comment out highlighted lines!


#in filter(), == is used for a specific value, %in% is used for multiple values

#filter rows that have NA (choose specific column)
##ususally done after a join produces NA's
df %>% 
        filter(!is.na(specificColumn)) -> newdf

#search for unique values in one column based on NA's from other column

testtable <- subset(tablename, is.na(tablename$columnname))

#now a unique() can be used to find the unique values in a column


#convert every 0 (or any specific value) in a column to NA
df$col[df$col == 0] <- NA

#remove duplicated rows (keeps all the columns)
distinct(colname, .keep_all = TRUE)


#find # of NA's in a column

sum(is.na(df$col))


#replace character in string with something else (or a blank)

mutate(colname = str_replace_all(colname, "[character]", "replacement"))

#find values in a column that appear more than once

extra <- df %>% group_by(col) %>%  filter(n()>1)


#split name column into 2 columns (for first and last name), seperate by comma and space

df %>% 
        separate(col, c("LAST_NAME", "FIRST_NAME"), sep = ", ") -> df

#remove whitespace from column
df$col <- gsub('\\s+', '', df$col)

#relocate columns
relocate(col1, .after = col2)


#remove any charater in a string (this example removes the -,/ , and ' characters)
mutate(col = str_replace_all(col, "[-/']", ""))


#convert m/d/y to yyyymmdd
mutate(newCol = as.Date(ogCol, format="%m/%d/%Y"))


#extract # of observations from a group

df %>% 
        group_by(cols...) %>%
        summarise(col1 = n())-> df

#see values in a column and howmany times they appear
table(df$col)

#count number of times value shows up in column 
summarise(newcol = sum(col == "value"))
#see percentage of times that value occurs
summarise(newcolpct = newcol/n())


#When/before using stringdist, NA's in df may need to be replaced with and empty string
df[is.na(df)] <- "empty_string"


#SPLIT column into 2 different columns (useful if full name is in 1 column). Separate by any character
separate(colname, c("newcol1", "newcol2"), sep = ",")

#opposite of seperate. concatenates 2 or more columns into one
unite("new col name", colA:colB, sep = ", ") 

#
##For help adding a "total" row for grouped table open "FL2021" in FamilyLeague folder. line 59-125



#FILTER multiple values

filter(col %in% c(value1, value2, value3))


#generate random numbers in column between 0 - 1000
##nrow(df) counts the number of rows in the df so the column has the same # of values as the others in the df
df$col <- runif(nrow(df), min = 0, max = 1000)


#replace NA in column with 0

df$col[is.na(df$col)] <- 0


#remove afile duplicates
afile1920 %>% 
        group_by(LOCAL_STUDENT_ID_NUMBER) %>% 
        arrange(ENTRY_DATE) %>% 
        filter(ENTRY_DATE == last(ENTRY_DATE)) %>% 
        ungroup()->afile1920

#convert value/list to df
data.frame(Reduce(rbind, list/valueName))


#moves row (based on specific row #) to the top and brings back the remaining rows
##this example is specific to CGCS_HSdata.R project 
##nrow(p1) - 1 is the total number of row in df minus the last row (which is the one we moved up)
df <- df[c(rownumber,1:nrow(df) - 1,]




#use "n()" when counting the number of times a group_by variable occurs
##ex -
afile1920 %>% 
        group_by(GRADE) %>% 
        summarise(total_students = n()) -> gradecount
##this gives the amount of students in each grade



getAge <- function(from, to) {
        # Calculates the age of a person born on date "from" as of date "to". Use with lubridate to parse
        # date formats correctly - functions ymd, mdy, etc. Returns an integer vector.
        from_lt = as.POSIXlt(from)
        to_lt = as.POSIXlt(to)
        age = to_lt$year - from_lt$year
        ifelse(to_lt$mon < from_lt$mon |
                       (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
               age - 1, age)
}        



#filtering in a summarize
#getting the sum(or other stat like median, mean...) of a column (col1)
#that align with specific values in other column (col2)

summarise(
newcol = sum(col1[col2 %in% c(value1, value2,...)]),
)
##can also be more specific and filter values in col1 
summarise(
        newcol = sum(col1[col2 %in% c(value1, value2,...)] == col1value),
)


#select specific rows in df

newdf <- df[1:5,] #selects rows 1-5
newdf <- df[3:nrow(df),] #selects all rows after row 3


#remove duplicate rows/observation
df %>% distinct(col1, col2, .keep_all = TRUE) #col1 & 2 have the same values in each row/observation



#passing column name into function - use {{colname}}
Fun3 <- function(df, column) {
        
        df %>%
                select(Reference, {{column}}, Var3) %>%
                group_by(Var3) %>%
                count({{column}})
}



#extract first n characters in a string



#extract the last characters in a string (n == the number of characters you want(ex last 4 characters))

mutate(newCol = substr(ogCol, nchar(ogCol)-n+1, nchar(ogCol)))


#filter specific rows in column based on values

filter(colName %in% c(val1, val2, val3...)) 

#create a function that is opposite of %in%

'%ni%' <- Negate('%in%')


#filter based on another dataframe
filter(col1 %in% df$col)


#round column to n decimal place
mutate(col = round(col, n)
       
       
       
       
       
       
       
#pass column name to summarize inside a function using-   !!(...) :=
absRaceGender <- function(df,colName, num) {
        df %>% 
                group_by(Race, GENDER) %>% 
                summarise(!!(colName) := sum(DAYS_ABSENT %in% num))
##!!!dont forget to use quotations when passing the column name as an argument
absRaceGender(afile, "Abesent_5_days", 5) 
##!!Also, equations can be passed as an argument in a function using same "!!()" technique
absRaceGender <- function(df,colname, x) {
        df %>% 
                summarise(!!(colname) := sum(!!(x))))
#"x" must be written with a quo() when being passed
absRaceGender("afile", "all students", quo(n())) 





        
        
#pass an operator in a function
        
 test <- function(a, op, b) {
                op(a,b)
 }
 
 
 #using the spread function to morph a  group by summarise (long df) into a wider df 
 
 afile1920 %>%
         mutate(GRADE = ifelse(GRADE %in% 92:96, "PK", GRADE)) %>%
         group_by(Race, GENDER, GRADE) %>%
         summarise(total_students_enrolled = n()) %>%
#spread creates a df by using the values in GRADE as cols and "N"total_students_enrolled" are the values
#rows are for each race and gender
         spread(GRADE, total_students_enrolled) -> tic2

 
#sum numeric columns in row (creating a column that has the total of each values in the row)

 mutate(Total = rowSums(select_if(., is.numeric), na.rm = T)) 
 
 
 #find and filter rows containing a specific character or string
 scgt1920 %>% 
         filter(str_detect(SECTION_TITLE, "AP")) -> scgtAP
 #can also use str_start and str_end to find characters in beginning or end of string. can also use operators to find multiple 
 filter(str_starts(SECTION_TITLE, "AP") | str_starts(SECTION_TITLE, "IB")) -> scgtAP
 
 
 
 
 
 
 
 ##!!!using conditionals in (tidyverse) function
 #using missing() to check if function argument is missing
 #conditional CANNOT be used inside tidyverse pipe like regular if_else(), should wrap around it!
 newfunction <- function(x) {
         if (missing(x)) {
                 df %>%  
                         filter(...)}
         else {df %>% 
                         filter(...)}
         
#if you want to continue the dplyr pipe inside the function you need to create a df inside with the same name for both
#conditional outcomes - in this example, the internal df is called "func1"
#if you dont do this the remaining pipe will only affect the "else" statement of the conditionl
         
         newfunction <- function(x) {
                 if (missing(x)) {
                         df %>%  
                                 filter(str_starts(SECTION_TITLE, "AP"))->func1}
                 else {scgt1920 %>% 
                                 filter(LEA_COURSE_CODE %in% x$Course.Number) -> func1} 
                 func1%>%
                         mutate(...) %>%
                         select(...)...


                

                 
                                  
#!Pivot df's to be longer or wider!
newpivot <- pivot_longer(df, c(col1,col2...), names_to = "newcolname", values_to = "valuecolname")
#c() is used to list columns that you want to combine into one jumbo column
##can also use starts_with(), ends_with(), etc
#names_to is what you will call the newly generated jumbo column(defaults to "name")
#values_to is what you will call the column that has the values from all the combined columns (defaults to "value")
##note: the names_to column is just a list of the initial column NAMES in order to match with the values column   



#fuzzyjoin stringdist

stringdist_left_join(df1, df2, by = c("col1x" = "col1y",
                                                "col2x" = "col2y"),
                     max_dist = 1, distance_col = "difference") -> join1


#manually input column values based on value of other column 
nomatch5 %>% 
        mutate(LOCAL_STUDENT_ID_NUMBER = case_when(
                Student.First.Name == "Andrea" ~ 1302569,
                Student.First.Name == "A'niyah" ~ 1302303,
                Student.First.Name == "Jeannette" ~ 1320723,
                Student.First.Name == "O'Mari" ~ 1300967,
                Student.First.Name == "Tiffany" ~ 1315748,
                Student.First.Name == "A'nijah" ~ 1307185,
                Student.First.Name == "Michelle" ~ 1300799)) -> match6



#change a list of columns to numeric

numlist <- c("col1", "col2", "col3", "col4"...)

df[numlist] <- sapply(df[numlist], as.integer)



