############################################################################################################
## 2019. 6. 17
## english text analysis
## Training Text data analysis
############################################################################################################

rm(list=ls())
# rm(list=ls())은 모든 변수를 삭제하여 R을 새로 시작하였을 경우처럼 초기화를 해주는 명령입니다, rm (list = ls ()) is a command to initialize R as if you deleted all variables and started R again.

gc()
# 메모리를 비워주는 명령어, Garbage Collection = Clear memory Usage

options(mc.cores=1) 
# 코어를 한개만 쓰는 명령어, tm패키지는 다중코어기능을 사용을 하면 충돌이 일어나서 오류가 발생함, command that uses only one core, tm package crashes and errors occur when using multicore functionality

setwd("C:/Users/5804313/Desktop/출장/english")
# 실행 파일 위치 지정, Command that set working directory
getwd()
# 디렉토리 경로 정보, You can find out which directory by running the getwd (get working directory) function

ls()
# ls()를 타이핑하면 현재 어떤 변수들이 존재하는 지 알 수 있습니다., ls () is a command to see what variables are currently available.
# rm(a) # 특정 변수 제거, remove specific variables

## 01. 패키지 설치, Package Setting

# install.packages(c("data.table","tm","wordcloud","ggplot2","qgraph","slam","topicmodels","lad","LDAvis","servr"))
library(readxl)
library(data.table)
library(tm) # tm package version 0.6-2
library(SnowballC) # stopword function

## 02. 데이터 로딩 & 전처리, data loading & cleaning

# 데이터 로딩, data loading
data <- read.csv('data.csv')

writeLines(as.character(data$contents[[1]]))
dim(data)

# 데이터 전처리, data cleaning
data = data[!duplicated(data$contents),] # 중복된 데이터 제거, remove duplicate data
data = data[!apply(is.na(data) | data == "", 1, all), ] # NA 데이터 제거, Remove NA data
data = as.data.frame(data$contents)
colnames(data) <- c('data')

dim(data)
nrow(data)
nchar(as.vector(data[1,])) # 관측치별 문장 길이, sentence length by observation

write.csv(data,'verna review_Anwer.csv') # 데이터 저장, save data

d_length =c()

# 길이 100 이상인 값만 취하기, only take observations that are 100 or greater in length
for (i in 1:nrow(data)){
  d_length_t = nchar(as.vector(data[i,]))
  d_length  = c(d_length, d_length_t)
}
data <- cbind(data,d_length)
data <- subset(data, d_length > 100)
data <- data.frame(data=data$data)

ex <- Corpus(DataframeSource(data))
# get corpus, DataframeSource는 data.frame만 취급합니다, get corpus, DataframeSource is only handles data.frame.

writeLines(as.character(ex[[1]])) # 관측치별 문장 검색, search observation sentence

# 문장 정리, sentence organization
ex <- tm_map(ex, removeNumbers) # 숫자 제거, remove number
ex <- tm_map(ex, removePunctuation) # 구두점 제거, remove punctuation
ex <- tm_map(ex, content_transformer(tolower)) # 대문자 → 소문자, capital letter → small letter
ex <- tm_map(ex, removeWords, stopwords("english")) # 띄어쓰기와 시제 등의 내용 제거, Removing contents such as spaces and tenses
ex <- tm_map(ex, stripWhitespace) # 중간의 빈칸 제거, Remove blank
ex = tm_map(ex, stemDocument) ## ed, es, s등의 단어 끝부분을 지워줌, deletes the end of a word such as ed, es, s, etc.
ex <- tm_map(ex, removeWords, c("cng","vehicl","can","will","car", "problem", "time", "servic", "hyundai", "day", "also", "got", "month")) # 특정단어 삭제, delete a specific word

## 03. 주요키워드 발췌, extract keyword

# TDM (Term Document Matrix) 변환, TDM (Term Document Matrix) conversion
tdm <- TermDocumentMatrix(ex) # 행(row)은 단어(term), 열(column)은 각 문서 형태로 변환하는 함수가 TDM임, A row is a term, a column is a document that converts each document type into a TDM
tdm <- removeSparseTerms(tdm, 0.99) # sparsity값으로 해당 값까지는 허용된다. 즉 0.95인 경우 0.95를 넘는 경우는 삭제됨, a value of sparsity is allowed up to that value. in other words, if 0.95, exceeding 0.95 is deleted.
tdm
dim(tdm)
inspect(tdm[1:10, 1:10])

library(slam)
word.count = as.array(rollup(tdm, 2)) # 단어별 전체 문서에서 쓰인 개수, number of each words written in the entire document
word.order = order(word.count, decreasing = T) # 단어별 전체 문서에서 쓰인 개수 순위, rank of each words's number written in the entire document frequent used word
freq.word = word.order[1:200]

# csv 파일로 주요 키워드 횟수 저장
# method 1
top.freq = data.frame(word=tdm$dimnames$Terms[freq.word], freq=word.count[freq.word])
write.csv(top.freq, file = "1.1 word_freq.csv") # 데이터 저장, save data
# method 2
#freq=sort(rowSums(as.matrix(tdm)), decreasing = T)
#wf=data.frame(word=names(freq), freq=freq)

## 04. 연관분석, association analysis

# 1) bigram(co-occurrence Matrix) analysis
tdm_m <- TermDocumentMatrix(ex, control = list(weighting = weightBin)) # binary 형태 생성, convert binary type

word.count = as.array(rollup(tdm_m, 2)) # 단어별 전체 문서에서 쓰인 개수, number of each words written in the entire document
word.order = order(word.count, decreasing = T) # 단어별 전체 문서에서 쓰인 개수 순위, rank of each words's number written in the entire document frequent used word
freq.word = word.order[1:200]

library(slam)
top.freq_binary = data.frame(word=tdm_m$dimnames$Terms[freq.word], freq=word.count[freq.word])
write.csv(top.freq_binary, file = "1.2 word_freq_binary.csv")

findFreqTerms(tdm_m, lowfreq=1000) # 1,000회이상 빈도 단어 찾기, find words more than 1000 times
head(findAssocs(tdm_m, "bad", 0.2)) # 연관단어 찾기, find related words

tdm_m <- as.matrix(tdm_m)
dim(tdm_m)

library(stringr)
rownames(tdm_m) <- str_trim(rownames(tdm_m))  # 윈도우용, for windows
# 윈도우의 경우 버그로 인해 단어들 뒤에 스페이스(빈칸) 가 들어간 경우가 있습니다, in Windows, bugs may cause words to be followed by spaces.
# str_trim을 이용하면 자동으로 빈칸을 없애줍니다, using 'str_trim' automatically removes the blank space.
# 단어는 총 14,628개 문서는 총 5,089개입니다, there are 14,628 words and 5,089 documents.
# 단어*단어 (Co-occurrence Matrix)는 14,628 * 14,628으로 비교적 큰 Matrix입니다, co-occurrence matrix is a relatively large matrix with 14,628 * 14,628.
# 따라서, 단어 빈도가 높은 상위 20개만 뽑아 50 * 50 Matrix 형태로 만듭니다, therefore, only the top 20 words with the highest frequency are extracted to form a 50 * 50 matrix.
tdm_m <- tdm_m[order(rowSums(tdm_m), decreasing = T),]
tdm_m <- tdm_m[1:100, ]

co_matrix <- tdm_m %*% t(tdm_m)
# 동시 발생 메트릭스 생성, generate co-occurrence Matrix
# (200*14,628) %*% (14,628*200) 매트릭스 연산을 통해 200*200 형태로 만듭니다. (200*14,628) %*% (14,628*200) convert 200*200 form through matrix operation.

write.csv(co_matrix, file = "2.bigram_matrix.csv") # 데이터 저장, save data

# 2) trigram analysis
# 단어가 공통적으로 나오는 행수 찾기, find the number of document which two terms emerge at the same time
inter_doc_name <- intersect(names(which(tdm_m[rownames(tdm_m)=="verna",]==1)),
                            names(which(tdm_m[rownames(tdm_m)=="bad",]==1)))
inter_doc_name <- as.numeric(inter_doc_name)
common <- as.data.frame(data[inter_doc_name,])
write.csv(common, file = "3.common_sentence.csv")

ex2 <- Corpus(DataframeSource(data))
# get corpus, DataframeSource는 data.frame만 취급합니다, get corpus, DataframeSource is only handles data.frame.

# 문장 정리
ex2 <- tm_map(ex2, removeNumbers) # 숫자 제거, remove number
ex2 <- tm_map(ex2, removePunctuation) # 구두점 제거, remove punctuation
ex2 <- tm_map(ex2, content_transformer(tolower)) # 대문자 → 소문자, capital letter → small letter
ex2 <- tm_map(ex2, removeWords, stopwords("english")) # 띄어쓰기와 시제 등의 내용 제거, Removing contents such as spaces and tenses
ex2 <- tm_map(ex2, stripWhitespace) # 중간의 빈칸 제거, Remove blank
ex2 = tm_map(ex2, stemDocument) ## ed, es, s등의 단어 끝부분을 지워줌, deletes the end of a word such as ed, es, s, etc.
ex2<- tm_map(ex, removeWords, c("car", "problem", "time", "servic", "hyundai", "day", "also", "got", "month"))

tdm_m2 = TermDocumentMatrix(ex2, control = list(weighting = weightBin)) # binary 형태 생성
tdm_m2 <- as.matrix(tdm_m2)
tdm_m2 <- tdm_m2[order(rowSums(tdm_m2), decreasing = T),]
tdm_m2 <- tdm_m2[1:50, ]
dim(tdm_m2)

co_matrix2 <- tdm_m2 %*% t(tdm_m2)
# 동시 발생 메트릭스 생성, generate co-occurrence Matrix
# (50*14,628) %*% (14,628*50) 매트릭스 연산을 통해 50*50 형태로 만듭니다. (50*14,628) %*% (14,628*50) convert 50*50 form through matrix operation.

write.csv(co_matrix2, file = "4.trigram_matrix.csv")

## 05. 그래프 생성, plot graph

# bargraph
library(ggplot2)
k = data.frame(word=factor(top.freq$word, levels=top.freq$word), freq=top.freq$freq)
p=ggplot(subset(k, freq>1500), aes(word, freq))
p=p+geom_bar(stat="identity")
p=p+theme(axis.text.x=element_text(angle=45, hjust=1, size=20), axis.text.y=element_text(size=20))
p
png("bargraph.png", width=36, height=24, units="in", res=300)
#png(filename="D:/HKM/work/2017/빅데이터/엔지니어링분석툴개발_연구개발정보기술팀/Rcode/bargraph.png", width=800, height=600)
p
dev.off()

# wordcloud
library(wordcloud)
freq=sort(rowSums(as.matrix(tdm)), decreasing = T)
wordcloud(names(freq), freq, random.order=FALSE, min.freq=5, color=brewer.pal(8, "Dark2"))
png("wordcloud.png", width=36, height=24, units="in", res=300)
wordcloud(names(freq), freq, random.order=FALSE, min.freq=1000, color=brewer.pal(8, "Dark2"))
dev.off()

# qgraph
# Network 그래프를 위해서는 diag=F, layout="spring" 을합니다.
##edge 색은 darkblue로 해보겠습니다, for the network graph, set diag = F, layout = "spring".
# vsize는 log(diag(co_matrix)) 로 합니다, vsize is log (diag (co_matrix)).
library(qgraph)
qgraph(co_matrix2, labels = rownames(co_matrix2), diag=F,
       layout = "spring",
       edge.color = "darkblue",
       vsize = log(diag(co_matrix2)),
       legend.cex = .7)

# 그래프의 타이틀을 붙입니다. line은 그래프와 타이틀의 거리입니다, attaches the title of the graph. line is the distance between the graph and the title.
title("correlation graph", line = 3)
png("correlation graph.png", width=36, height=24, units="in", res=300)
dev.off()

## 06. 토픽모델 분석, topic model analysis

# DTM (Document Term Matrix) 변환, DTM (Document Term Matrix) conversion
dtm = DocumentTermMatrix(ex)
dim(dtm)
dtm <- removeSparseTerms(dtm, 0.99) # sparsity값으로 해당 값까지는 허용된다. 즉 0.95인 경우 0.95를 넘는 경우는 삭제됨, a value of sparsity is allowed up to that value. in other words, if 0.95, exceeding 0.95 is deleted.
dim(dtm)

library(topicmodels)
ldaform = dtm2ldaformat(dtm, omit_empty = F) # omit_empty = T: 상위 300개 단어가 하나도 안들어간 문서 제거, Remove the top 300 frequency words

attributes(ldaform)
ldaform$documents[1]

library(lda)
eta = 0.01
alpha = 0.01

# 모델 적합, Fit the model
t1 <- Sys.time()
set.seed(nrow(data))
result.lda = lda.collapsed.gibbs.sampler(documents = ldaform$documents,
                                         K = 10,
                                         vocab = ldaform$vocab,
                                         num.iterations = 5000, # 경험적 반복수
                                         burnin = 1000,  # 앞부분의 계산 결과를 버려서 성능을 높히는 것
                                         alpha = 0.01,
                                         eta = 0.01)
t2 <- Sys.time()
t2 - t1  # 작동시간 체크, running time check

attributes(result.lda)
dim(result.lda$topics)

top.topic.words = top.topic.words(result.lda$topics, num.words = 10) # 토픽별 상위 10개 단어 보기, see top 10 words by topic
top.topic.words <- as.data.frame(top.topic.words)
names(top.topic.words) <- c("Topic1", "Topic2", "Topic3", "Topic4", "Topic5", "Topic6", "Topic7", "Topic8", "Topic9", "Topic10")
write.csv(top.topic.words, file = "5.top.topic.words.csv") # 데이터 저장, save data

result.lda$topics

# 각 토픽에 할당된 단어수, number of words assigned to each topic
count.topic.words <- result.lda$topic_sums
count.topic.words <- as.data.frame(count.topic.words)
rownames(count.topic.words) <- c("Topic1", "Topic2", "Topic3", "Topic4", "Topic5", "Topic6", "Topic7", "Topic8", "Topic9", "Topic10")
names(count.topic.words) <- c("number of word")
write.csv(count.topic.words, file = "6.count.topic.words.csv") # 데이터 저장

# 문서별 각 토픽에 할당된 단어수, number of words assigned to each topic per document
doc.topic = result.lda$document_sums

doc.topic.trans <- as.data.frame(t(doc.topic))
names(doc.topic.trans) <- c("Topic1", "Topic2", "Topic3", "Topic4", "Topic5", "Topic6", "Topic7", "Topic8", "Topic9", "Topic10")

# 문서별 각 토픽에 할당된 단어 비율, percentage of words assigned to each topic by document
doc.topic.ratio = scale(doc.topic, center=F, scale=colSums(doc.topic))
doc.topic.ratio.t <- t(doc.topic.ratio)
doc.cnt <- nrow(doc.topic.ratio.t)

# 토픽 분류 결과 저장, save Topic Classification Results
topic.result = c()
for (i in 1:doc.cnt) {
  #print(which.max(doc.topic.ratio[,i]))
  #temp.topic.result <- which.max(doc.topic.ratio[,i])
  temp.topic.result  <-  if (length(which.max(doc.topic.ratio[,i])) > 0 ) { which.max(doc.topic.ratio[,i])} else {999}
  topic.result <- c(topic.result, temp.topic.result)
}

# 토픽 분류 값 저장, save Topic Classification Values
topic.result.value <- paste(round(apply(doc.topic.ratio, 2, function(x) max(x, na.rm = FALSE))*100,0),"%")
doc.topic.ratio.t <- cbind(as.data.frame(doc.topic.ratio.t),topic.result,topic.result.value,data)
names(doc.topic.ratio.t) <- c("Topic1", "Topic2", "Topic3", "Topic4", "Topic5", "Topic6", "Topic7", "Topic8", "Topic9", "Topic10")
write.csv(doc.topic.ratio.t, file = "7.doc.topic.ratio.csv")

# JSON 파일 생성, create the JSON object to feed the visualization
library(LDAvis)
theta <- t(apply(result.lda$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(result.lda$topics) + eta, 2, function(x) x/sum(x)))

term.frequency <- slam::col_sums(dtm)

doc.length <- sapply(ldaform$documents, function(x) sum(x[2,]))

Reviews <- list(phi = phi,
                theta = theta,
                doc.length = doc.length,
                vocab = ldaform$vocab,
                term.frequency = term.frequency)

json <- createJSON(phi = Reviews$phi,
                   theta = Reviews$theta,
                   doc.length = Reviews$doc.length,
                   vocab = Reviews$vocab,
                   term.frequency = Reviews$term.frequency,
                   R=10)

library(servr)
serVis(json, out.dir = 'vis', open.browser = F) # firefox 이용, using firefox
