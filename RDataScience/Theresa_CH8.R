#####Getting Started#####
#1. read_delim(file, delim = "|")

#2. col_names, col_types,locale, na, quoted_na, trim_ws, n_max, guess_max, progress

#3. most important: col_positions (where cols begin and end)

#4. read_delim(...quote = "'")

#5. 
read_csv("a,b\n1,2,3\n4,5,6") #the first line specifies 2 col, but the rows actually have 3 cols
read_csv("a,b,c\n1,2\n1,2,3,4") #same as above
read_csv("a,b\n\"1")#the first quote is ignored bc of the backslash before it
read_csv("a,b\n1,2\na,b") #both cols are considered char because a and b are not num (was it intended for 1 and 2 to be in the first column, not first row?)
read_csv("a;b\n1;3") #values are seperated by ; instead of ,

#####Parsing a Vector#####
#1. most important: decimal and grouping mark

#2. 
num <- "1.234.567,89"
parse_double(num, locale(decimal_mark = ".", grouping_mark = ".")) #doesn't work because they have to be different
parse_number("1.234.567", locale = locale(grouping_mark = ".")) #the defaults automatically switch

#3.they set what format to use for date and time
#example from the readr vignette:
parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))

#4. I live in the U.S.

#5. read_csv uses delim = ",", read_csv2 uses delim = ";"

#6.Chinese uses: GB 2312, GBK, GB 18030
# Japanese uses: JIS X 0208, Shift JIS, ISO-2022-JP
# Greek uses: ISO-8859-7
#etc.

#7. 
d1 <- "January 1, 2010"
parse_date(d1, format = "%B %d, %Y")

d2 <- "2015-Mar-07"
parse_date(d2, format = "%Y-%b-%d")

d3 <- "06-Jun-2017"
parse_date(d3, format = "%d-%b-%Y")

d4 <- c("August 19 (2015)", "July 1 (2015)")
parse_date(d4, format = "%B %d (%Y)")

d5 <- "12/30/14" # Dec 30, 2014
parse_date(d5, "%m/%d/%y")

t1 <- "1705"
parse_time(t1, "%H%M")

t2 <- "11:15:10.12 PM"
parse_time(t2, "%I:%M:%OS %p")
