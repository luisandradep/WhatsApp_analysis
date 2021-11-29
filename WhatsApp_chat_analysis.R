#R SCRIPT TO ANALYZE WHATSAPP CHATS USING RWHATSAPP PACKAGE
#MODIFIED FROM J.B. GRUBER ET AL DEVELOPMENT AND TUTORIAL
#FULLY RECOMENDED MATERIAL: https://github.com/JBGruber/rwhatsapp

#REQUIRED PACKAGES: rwhatsapp, dplyr, ggplot2, lubridate, tidytext, stopwords, TM, wordcloud, wordcloud2
#IF YOU DON'T HAVE ANY OF THESE PACKAGES, INSTALL THEM WITH: install.packages("")
#install.packages("rwhatsapp")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("tidytext")
#install.packages("stopwords")
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("wordcloud2")

library("rwhatsapp")
library("dplyr")
#Import chat file, exported as txt file from a particular folder (edit your actual file route!)
#It may be necessary to indicate actual file time format
#In my case the file's date format was the type "14/04/20, 10:20 p. m."
chat = rwa_read("C:/User/Documents/WhatsappChatfile.txt", format = "dd/MM/yy, hh:mm a") %>%
  filter(!is.na(author))  %>% #remove mensages withut any author, usually 1st row
  filter(time > "2021-03-31") #Only if you want a date cutoff, otherwise omit step

#Often there are chat members out of our personalcontact lists that simply appear as telephone numbers
#We don't necessarily want to include them in our personal contact list
#but we want to know who they are in our chat analysis
#Recode comand allows us to replace the author's names.
#In this case, we replace author's phone number by his or her name
chat = chat %>%
  mutate(author = recode(author, "+1 (111) 222-3344" = "Name Lastname",
                         "+66 444 555 666" = "Name2 Lastname2",
                         "+33 777 88 88 99" = "Name3 Lastname3"))

paste(c("The loaded chat has: ", dim(chat)[1], " messages and ", dim(chat)[2], " attributes"), collapse=" ")
paste(c("The chat attributes are: ", colnames(chat)))

#Select chosen attributes or remove discarded attributes
chat = select(chat, c(time, author, text)) #Select chosen attributes
#chat = select(chat, -c(source, id, emoji, emoji_name)) #Remove discarded attributes
colnames(chat)

#Add a feature column from external source and merge to main database
#In this example the feature is called "group", but it could be any other name
#External table with group assignment to each chat user (author)
groups = read.csv("C:/.../Demo_groups.csv")
#Table join or merge based on common 'author', keeping the main database on the left (identified as x)
chat = merge(x=chat, y=groups, by="author", all.x=TRUE)
paste(c("The updated chat attributes are: ", colnames(chat)))

#Rename feature names if you are not happy with the actual ones
chat = chat %>%
  mutate(group = recode(group,"OldName1" = "NewName1",
                        "OldName2" = "NewName2",
                        "OldName3" = "NewName3",
                        "OldName4" = "NewName4",
                        "OldName5" = "NewName5"))
list(unique(chat$group))

##    GRAPHIC ANALYSIS  ##
library("ggplot2"); theme_set(theme_minimal())
library("lubridate")

#OPTIONAL: Fix colors for each group value or name
# This is a manual scale based on set1 palette, but limited from 1 to 5 levels
colors5 = c("#377EB8","#4DAF4A","#984EA3","#A65628","#FF7F00")
names(colors5) = unique(chat$group)


#MESSAGES PER DATE
chat %>%
  #filter(date(time) > "2016-11-01") %>%
  mutate(day = date(time)) %>%
  count(day, group) %>%
  ggplot(aes(x = day, y = n, fill = group)) +
  geom_bar(stat = "identity") +
  #scale_fill_manual(values=colors5) +
  theme(legend.position = "top") +
  ylab("") + xlab("") + ggtitle("Messages per date") 
#table(date(chat$time)) #Optional if you want to directly read the number of messages per date, not advised with too many dates
mean(table(date(chat$time))) #Average messages per day

#MESSAGES PER YEAR
chat %>%
  mutate(year = year(time)) %>%
  count(year, group) %>%
  ggplot(aes(x = year, y = n, fill = group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=colors5, name = "Group:") +
  #scale_fill_brewer(palette = "Set3",name = "Group:") +
  theme(legend.position = "top") +
  ylab("") + xlab("") + ggtitle("Messages per year") 
table(year(chat$time))
mean(table(year(chat$time)))

#MESSAGES PER MONTH
chat %>%
  mutate(Month = factor(month(time),ordered = TRUE)) %>%
  count(Month, author) %>%
  ggplot(aes(x = Month, y = n, fill=author)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3",name = "Author:") +
  #scale_fill_manual(values=colors5) +
  scale_x_discrete(labels=c("1"="Jan","2"="Feb","3"="Mar","4"="Apr","5"="May","6"="Jun",
                            "7"="Jul","8"="Aug","9"="Sep","10"="Oct","11"="Nov","12"="Dec")) +
  theme(legend.position = "right") +
  ylab("") + xlab("") + ggtitle("Messages per month") 

#MESSAGES PER HOUR
chat %>%
  mutate(Hour = hour(time)) %>%
  count(Hour, group) %>%
  ggplot(aes(x = Hour, y = n, fill = group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=colors5) +
  theme(legend.position = "top") +
  ylab("") + xlab("") + ggtitle("Messages per hour UTC -05")
table(hour(chat$time)) #Optional if you want to directly read the number of messages per hour
mean(table(hour(chat$time))) #Average messages per hour

#MESSAGES PER WEEKDAY
#It's necessary to arrange the weekdays per chronological and not alphabetical order
#I assumed Monday as 1st weekday. In order to make Sunday the 1st day, just type it before "Monday"
chat %>%
  mutate(wkd = factor(weekdays(time),ordered = TRUE,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))) %>%
  count(wkd, group) %>%
  ggplot(aes(x = wkd, y = n, fill = group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=colors5, name = "Group: ") +
  theme(legend.position = "top") +
  ylab("") + xlab("") + ggtitle("Messages per weekday")

#MESSAGES PER HOUR AND WEEKDAY COUNT
chat %>%
  mutate(Hour = hour(time)) %>%
  mutate(wkd = factor(weekdays(time),ordered = TRUE,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))) %>%
  count(Hour, wkd) %>%
  ggplot(aes(x = Hour, y = n, fill = wkd)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1",name = "Weekday:") +
  theme(legend.position = "top") +
  ylab("") + xlab("") + ggtitle("Messages per hour, local time")

#MESSAGES PER FEATURE
chat %>%
  filter(date(time) > "2016-10-31") %>%
  mutate(yr = factor(year(time), ordered = TRUE)) %>%
  count(group, yr) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(group, n), y = n, fill=yr)) +
  geom_bar(stat = 'identity') +
  scale_fill_brewer(palette = "Set1",name = "Year:") +
  theme(legend.position = "right") +
  ylab("# Messages") + xlab("") +
  ggtitle("Messages per Group")

#MESSAGES PER USER / CHAT MEMBER
chat %>%
  count(author) %>%
  slice_max(n, n = 10) %>% #Filter the n authors with more contributions
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(author, n), y = n, fill = author)) +
  geom_bar(stat = "identity") +
  #scale_fill_manual(values=colors5) +
  geom_text(aes(label = n), hjust = 1.2, color = 'white') +
  theme(legend.position = "") +
  ylab("") + xlab("") +
  coord_flip() + ggtitle("Messages per user")

#TEXT ANALYSIS
library("tidytext")
library("stopwords")

#Extract text attribute
text = chat$text
#Remove emojis
text = iconv(text, 'UTF-8', 'ASCII')
#Stopwords has a list of connecting words in different languages
#By removing stopwords we pretend to have a cleaner word analysis
#We create a word vector that contains a list of unnecesary or noisy words for the word analysis
#You should include not only the listed stopword but also any other words or list of words
#you consider that don't give added value to the word analysis
exclude = c(stopwords("es"),"multimedia","omitido",
            "common1","common2","common3","common4")


#TOTAL WORD COUNT PER USER
chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% exclude) %>%
  group_by(author) %>%
  summarise(sumwords = length(word)) %>%
  slice_max(sumwords, n = 20) %>%
  arrange(desc(sumwords)) %>%
  ggplot(aes(x = reorder(author, sumwords),
             y = sumwords,
             fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 300))) +
  geom_text(aes(label = sumwords), hjust = -0.1) +
  ylab("") +
  xlab("") +
  ggtitle("Total word count per user excluding stop and other common words") +
  coord_flip()

#DIFFERENT WORD COUNT PER USER
chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% exclude) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  slice_max(lex_diversity, n = 20) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(x = reorder(author, lex_diversity),
             y = lex_diversity,
             fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 300))) +
  geom_text(aes(label = lex_diversity), hjust = -0.1) +
  ylab("") +
  xlab("") +
  ggtitle("Total disctinct word count per user excluding stop and other common words") +
  coord_flip()

#CLOUD OF WORDS
#Word Preprocessing:
library("tm")
corpus = VCorpus(VectorSource(text)) #Text formated as corpus
corpus = tm_map(corpus, content_transformer(tolower)) #put all words in lower case letters
corpus = tm_map(corpus, removePunctuation) #remove punctuation marks
corpus = tm_map(corpus, removeWords, exclude) #remove the listed unnecesary words
corpus = tm_map(corpus, removeNumbers) #remove numbers
Matrix = as.matrix(TermDocumentMatrix(corpus)) #count repeated words
words = sort(rowSums(Matrix), decreasing = TRUE)
matrixdf = data.frame(word = names(words), freq=words) #frequency info converted to matrix

#Word cloud execution
library("wordcloud")
library("wordcloud2")

#Adjust parameters as convenient. If necessary, repeat preprocessing steps
wordcloud(words = matrixdf$word, freq = matrixdf$freq, min.freq = 2,
          max.word = 200, scale = c(1.65, 0.65),rot.per = 0.15, 
          random.order = FALSE, colors = brewer.pal(9, "Blues")[5:9])

#Alternative more fashionable wordcloud2. Adjust minimum freq, sizes, colors, ellipticity, etc
wordcloud2(matrixdf[matrixdf$freq > 1,], size=0.3, gridSize = 1, color='random-dark',
           backgroundColor = "white", ellipticity = 0.8, shuffle = TRUE)


#Word repeated by users
Word = 'ENTER YOUR WORD'
chat %>%
  unnest_tokens(input = text, 
                output = word) %>%
  filter(tolower(word) %in% tolower(Word)) %>%
  group_by(author) %>%
  summarise(sumwords = length(word)) %>%
  #slice_max(sumwords, n = 3) %>%  #uncomment if you want to constrain the nomber of listed users
  arrange(desc(sumwords)) %>%
  ggplot(aes(x = reorder(author, sumwords),
             y = sumwords,
             fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 10))) +
  geom_text(aes(label = sumwords), hjust = -0.1) +
  ylab("") +
  xlab("") +
  ggtitle("Word repeated by user:",Word) +
  coord_flip()

#SUBSET ANALYSIS
#Feel free to create subsets of the original database by constraining it to particular conditions

#For example, you can constrain by considering working days (Monday to Friday)
chat_mf = chat %>%
  filter(weekdays(time) != "Saturday" & weekdays(time) != "Sunday") #Exclude Saturdays and Sundays
#Also you could only consider the Weekend
chat_ss = chat %>%
  filter(weekdays(time) == "Saturday" | weekdays(time) == "Sunday") #Only include messages posted on Saturdays OR Sundays 

#Next you can copy any of the previous plotting or text analysis codes, update the variables (not "chat" anymore) and execute
#You could also constrain by any other feature such as "author" or group