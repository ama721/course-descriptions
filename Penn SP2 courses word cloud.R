#### PACKAGES ####
# create notin operator
`%notin%` <- Negate(`%in%`)

# Download packages if not available
pckgs <- c("tidyverse","readxl","rvest", "tm", "wordcloud","stringr","mgsub")

if (any(pckgs %notin% rownames(installed.packages())==TRUE)){
  install.packages(pckgs, repos = c(CRAN = "http://cloud.r-project.org"))}

# Load packages
sapply(pckgs, FUN = require, character.only = TRUE)


##### FUNCTIONS ####
# function to clean the text up
clean.text <- function(x)
{
	# replace various characters and spaces
   x = mgsub(x, c("\n","\t", "\r"), c(" ", " ", " "))
   # remove blank spaces at the beginning
   x = gsub("^ ", "", x)
   # remove blank spaces at the end
   x = gsub(" $", "", x)
  # double check on the blank spaces
  x = trimws(x, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
}

# function to scrape data		
do_smthn <- function(x){
	
text_html <- read_html(x)

dd <- text_html %>%
			html_nodes("p") %>%
			html_text() %>%
			unlist() %>%
			clean.text() %>%
			tolower() %>%
			paste(., collapse=" ")
}


#### IMPORT + SCRAPE DATA ####
# create data frame to store data
courses <- read_excel("Penn-SP2-courses.xlsx", sheet="courses", col_types = rep("text", times = 3))%>%
rowwise() %>%
# create data frame to store data
mutate(Courses.Text = do_smthn(Courses)) %>%
ungroup() %>%
data.frame()


#### MANIPULATE CORPUS ####
# cull corpus of data				
all <- c(swrk = courses[grep("swrk",tolower(courses$Program)),3], 
			mssp = courses[grep("mssp",tolower(courses $Program)),3],
			npl = courses[grep("npl",tolower(courses $Program)),3])
# create Corpora
all_docs <- corpus <- Corpus(VectorSource(all))
# remove certain strings/numbers
all_docs <- all_docs  %>%
tm_map(., content_transformer(tolower)) %>%
tm_map(., removeNumbers) %>%
tm_map(., removeWords, c(stopwords("english"), 
"sp2","mssp","swrk","npld", "penn", "university","courses","faculty","will",
"requirements", "will", "course", "grade", "students", "student", "week", "activity",
"link", "links", "readings", "literature","university", "press", "also","assignment",
"assignments", "reading", "class", "absenteeism", "ability", "expectations", "able", 
"using","book", "identify","understand", "understanding", "philadelphia", "expect", 
"expected", "possible", "syllabus", "canvas", "way", "ways", "required", "requires", 
"seminar", "academic", "access", "york", "cambridge", "paper", "project", "seminar", 
"addresses", "address", "journal", "chapter", "nonprofit","pennsylvania","book", "books",
"term","fall", "spring","catalog","unit","offered","lecture", "usually","also", "including",
"include", "taught", "can","units","learn", "either","prerequisite","throughout", "summer",
"well","website", "html",".com", "catalog", "2020-21","2020", "2021")) %>%
tm_map(., removePunctuation) %>%
tm_map(., stripWhitespace)

# create term-document matrix
tdm <- TermDocumentMatrix(all_docs)
# convert to matrix
tdm <- as.matrix(tdm) %>% data.frame()


#### VISUALIZE DATA ####
#  separate clouds same plot
dev.new(width=12, height=6, unit="in")
par(mfrow=c(1,3))
#Create word cloud of mssp courses
set.seed(0721)
wordcloud(rownames(tdm), tdm$mssp, min.freq = 3, scale=c(7, .2), random.order = FALSE, 
rot.per = 0, max.words = 50,  random.color = FALSE, colors= c("indianred"))
#Create word cloud of social work courses
wordcloud(rownames(tdm), tdm$swrk, min.freq = 3, scale=c(7, .2), random.order = FALSE, 
rot.per = 0, max.words = 50, random.color = FALSE, colors= c("#405AAF"))
#Create word cloud of nonprofit leadership courses
wordcloud(rownames(tdm), tdm$npl, min.freq = 3, scale=c(7, .2), random.order = FALSE, 
rot.per = 0, max.words = 50, random.color = FALSE, colors= c("#595959"))