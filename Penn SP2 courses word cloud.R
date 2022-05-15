#### PACKAGES ####
# create notin operator
`%notin%` <- Negate(`%in%`)

# Download packages if not available
pckgs <- c("tidyverse","readxl","rvest", "tm", "wordcloud","stringr","mgsub")

if (any(pckgs %notin% rownames(installed.packages())==TRUE)){
  install.packages(pckgs, repos = c(CRAN = "http://cloud.r-project.org"))}

# Load packages
sapply(pckgs, FUN = require, character.only = TRUE)


#### Set seed ####
set.seed(0721)


##### FUNCTIONS ####
# function to clean the text up
clean.text <- function(x)
{# replace various characters and spaces
x <- gsub(pattern = "[[:space:]]+", replacement = " ", x = x)
}

# function to scrape data		
do_smthn <- function(x){

key_omit <- paste(c("Activity:","Course Unit", "Course usually offered", 
"Taught by:", "Prerequisite:", "course offered"), collapse = "|")
	
text_html <- read_html(x)

dd <- 
  text_html %>%
	html_nodes(".courseblockextra") %>%
	html_text() %>%
	data.frame() %>%
	filter(!str_detect(tolower(.), tolower(key_omit))) %>%
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
all <- 
  c(swrk = courses[grep("swrk",tolower(courses$Program)),3], 
	mssp = courses[grep("mssp",tolower(courses $Program)),3],
	npl = courses[grep("npl",tolower(courses $Program)),3])

# create corpus
all_docs <- Corpus(VectorSource(all))
all_docs <- all_docs  %>%
# make sure text is lower case
tm_map(., content_transformer(tolower)) %>%
# remove numbers
tm_map(., removeNumbers) %>%
# remove certain words/string
tm_map(., removeWords, c(stopwords("english"), 
"sp2","mssp","swrk","npld", "penn", "university","courses","faculty","will", "advisor",
"requirements", "will", "course", "grade", "students", "student", "week", "activity", "link", 
"links", "readings", "literature","university", "press", "also","assignment", "independent" ,
"assignments", "reading", "class", "absenteeism", "ability", "expectations", "able", 
"using","book", "identify","understand", "understanding", "philadelphia", "expect", 
"expected", "possible", "syllabus", "canvas", "way", "ways", "required", "requires", 
"seminar", "academic", "access", "york", "cambridge", "paper", "project", "seminar", 
"addresses", "address", "journal", "chapter", "pennsylvania","book", "books",
"term","fall", "spring","catalog","unit","offered","lecture", "usually","also", "including",
"include", "taught", "can","units","learn", "either","prerequisite","throughout", "summer",
"well","website", "html",".com", "catalog", "2020-21","2020", "2021")) %>%
# remove punctuation
tm_map(., removePunctuation) %>%
# remove whitespace
tm_map(., stripWhitespace)

# create term-document matrix
tdm <- TermDocumentMatrix(all_docs)
# convert to matrix then data frame
tdm <- as.matrix(tdm) %>% data.frame()


#### VISUALIZE DATA ####
#  three separate clouds on the same page
dev.new(width=10, height=6, unit="in")
png(file="SP2 Course Descriptions2.png", width = 12, height = 6, units = 'in', res = 300, bg = "transparent")
par(mfrow=c(1,3))
#Create word cloud of mssp courses
wordcloud(rownames(tdm), tdm$mssp, min.freq = 3, scale=c(5, .08), random.order = FALSE, 
rot.per = 0, max.words = 50,  random.color = FALSE, colors= c("#c31f2b"))
#Create word cloud of social work courses
wordcloud(rownames(tdm), tdm$swrk, min.freq = 3, scale=c(5, .08), random.order = FALSE, 
rot.per = 0, max.words = 50, random.color = FALSE, colors= c("#004685"))
#Create word cloud of nonprofit leadership courses
wordcloud(rownames(tdm), tdm$npl, min.freq = 3, scale=c(5, .08), random.order = FALSE, 
rot.per = 0, max.words = 50, random.color = FALSE, colors= c("#404041"))
dev.off(); dev.off()
