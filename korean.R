############################################################################################################
## 2019. 6. 17
## korea text analysis
## Training Text data analysis
############################################################################################################

## rm(list=ls())은 모든 변수를 삭제하여 R을 새로 시작하였을 경우처럼 초기화를 해주는 명령입니다.
## ls()를 타이핑하면 현재 어떤 변수들이 존재하는 지 알 수 있습니다.
rm(list=ls())

## 01. Package Setting

setwd("C:/Users/5804313/Desktop/출장/korea")
getwd()

# install.packages(c("rvest","plyr","tm","KoNLP","qgraph","stringr","slam","xlsx","wordcloud","ggplot2"))

library(rvest)
library(plyr)
library(tm)
library(KoNLP)
library(qgraph)
library(stringr)
library(xlsx)
library(wordcloud)
library(ggplot2)

## 02. data loading & cleaning

library(data.table)
data = fread('data.csv')

data = data[!duplicated(data$특이사항),] # 중복된 데이터 제거
data = as.matrix(data$특이사항) # chr 형태로 변경
data = data[!apply(is.na(data) | data == "", 1, all), ] # NA 데이터 제거

write.csv(data,'noise claims_특이사항.csv') # 데이터 저장

### 차후의 분석 편의를 위해 character vector 형태로 변환해 줍니다.
### character vector는 행(row)과 열(col)이 없습니다. character vector는 문자로 구성된 덩어리입니다.
### 예를 들어, "와와와 대박~" 등이 character vector입니다.
### 왜 이렇게 하냐고 물어보시면 extractNoun이 data.frame 형태를 처리하지 못하기 때문입니다.
### extractNoun에는 character vector 형태로 들어가야합니다.

## 03. 키워드 추출

key_vec_sum <- c();

for (i in 1:length(data))
{
    ### 명사를 추출합니다.
    key_vec <- extractNoun(data[i])
    
    ### 맞춤법 교정을 합니다.
    key_vec <- revalue(key_vec, c("휠밸런스" = "밸런스",
                                  "휠밸런스조정" = "밸런스",
                                  "발란스" = "밸런스",
                                  "휠바란스" = "밸런스",
                                  "휠발란스" = "밸런스"),
                       warn_missing = F)
    
    ### 2 글자 이상만 추출합니다.
    key_vec <- key_vec[nchar(key_vec) > 1]
    
    ### 윈도우 tm 버그 때문에 합니다. 맥은 안하셔도 됩니다.
    key_vec <- c(key_vec, ' ')  
    ###  collapse 뒤에 두 칸. 윈도우 tm 버그 때문에 두 칸 입니다.
    key_vec_sum[i] <- paste(key_vec, collapse='  ')  
}

## 04. TDM 만들기

options(mc.cores=1) # 코어를 한개만 쓰는 명령어

### Corpus 를 왜쓰냐고 물어보시면 TermDocumentMatrix를 넣을 때 이 형태로 넣게 되어 있습니다. 
### DataframeSource는 data.frame만 취급합니다. 
### 따라서, key_vec_sum은 character vector인데, as.data.frame을 사용하여 data.frame 형태로 변환합니다.
key_corpus <- Corpus(DataframeSource(as.data.frame(key_vec_sum)))
key_corpus

### 행(row)은 단어(term), 열(column)은 각 문서 형태로 변환하는 함수가 TermDocumentMatrix입니다.

key_tdm <- TermDocumentMatrix(key_corpus,
                              control = list(
                              removeNumbers = T, ## 숫자 제거
                              removePunctuation = T, ## 특수문자 제거
                              wordLengths = c(2,6),## 단어 글자가 2글자 이상부터 무한대(Inf)까지
                              stopwords = c("[c]","원인","현상","점검","조치"))) ## 특정단어 제거

## 05. count words

library(slam)
word.count = as.array(rollup(key_tdm, 2)) # 단어가 전체 문서에서 쓰인 개수

# frequently used words
word.order = order(word.count, decreasing = T)
freq.word = word.order[1:100]
row.names(key_tdm[freq.word,]) # 동일한 코드 : key_tdm$dimnames$Terms[freq.word]

# save word frequencies in the xlsx format
top.freq = data.frame(word=key_tdm$dimnames$Terms[freq.word], freq=word.count[freq.word])
write.xlsx(top.freq, "word_freq.xlsx")

## 06. Co-Occurrence Matrix 만들기

### matrix 형태로 변환합니다. 왜냐하면 TermDocumetMatrix 형태는 R에서 다루기가 힘들기 때문입니다.
### 좀 더 추가설명을 하자면 TermDocumentMatrix 함수는 R에서 만들어진 함수가 아닙니다.
### R은 메모리 등의 문제로 큰 데이터를 다루기 힘들고 속도도 느립니다. 그러나, 사용이 간편합니다.
### 따라서, 외부에서 만들어진 함수(예, C, JAVA 등)를 불러와서 사용합니다.

key_tdm_m <- as.matrix(key_tdm)

### 윈도우의 경우 버그로 인해 단어들 뒤에 스페이스(빈칸) 가 들어간 경우가 있습니다.
### str_trim을 이용하면 자동으로 빈칸을 없애줍니다.

rownames(key_tdm_m) <- str_trim(rownames(key_tdm_m))  ##  for windows

dim(key_tdm_m) ## 단어는 총 386개 문서는 총 90개입니다.

### 단어 * 단어 (Co-occurrence Matrix)는 386 * 386으로 비교적 큰 Matrix입니다.
### 따라서, 단어 빈도가 높은 상위 20개만 뽑아 20 * 20 Matrix 형태로 만듭니다.
### 단어 빈도가 높은 상위 20개 뽑는법은 
### Step 1) 단어별로 빈도를 셉니다. 
### rowSums(key_tdm_m)
### Step 2) 빈도가 높은 순부터 낮은 순대로 순서를 매깁니다.
### order(rowSums(key_tdm_m), decreasing = T)
### Step 3) key_tdm_m을 재정렬합니다.
### key_tdm_m[order(rowSums(key_tdm_m), decreasing = T),]

key_tdm_m <- key_tdm_m[order(rowSums(key_tdm_m), decreasing = T),]

### Step 4) 상위 20개(1부터 20, 1:20)를 추립니다.
key_tdm_m <- key_tdm_m[1:20, ]
dim(key_tdm_m)

### (20 * 90) %*% (90 * 20) 매트릭스 연산을 통해 20 * 20 형태로 만듭니다.
co_matrix <- key_tdm_m %*% t(key_tdm_m)

write.csv(co_matrix, file = "co_matrix.csv")

## 07. 그래프

## wordcloud
wordcloud(key_tdm$dimnames$Terms[freq.word], word.count[freq.word], min.freq=2, color=brewer.pal(6, "Dark2"))
rownames(co_matrix)  ## 상위 20개 단어입니다.

## bar graph
p=ggplot(subset(top.freq, freq>5), aes(word, freq))
p=p+geom_bar(stat="identity")
p=p+theme(axis.text.x=element_text(angle=45, hjust=1))
p

### qgraph
### labels Node의 이름입니다. 
### Network 그래프를 위해서는 diag=F, layout="spring" 을합니다.
### edge 색은 darkblue로 해보겠습니다.
### groups는 groups_list(호감단어/비호감단어)를 넣습니다.
### vsize는 log(diag(co_matrix)) 로 합니다.
### legend 그래프 우측의 비호감단어 호감단어 legend 크기를 .7로
### 합니다. default 는 1입니다.

qgraph(co_matrix, labels = rownames(co_matrix), diag=F,
       layout = "spring", 
       edge.color = "darkblue",
       vsize = log(diag(co_matrix)),
       legend.cex = .7)

## movie_name은 위에서 "어벤져스"로 넣어두었습니다.
### 그래프의 타이틀을 붙입니다. line은 그래프와 타이틀의 거리입니다.
title("휠 떨림 클레임", line = 3)

## 09. 상관관계

head(findAssocs(key_tdm, "시트", 0.3))
### findAssocs(key_tdm, "타이어", 0)[1:5,]와 동일합니다.

## 10. LSA, (Latent Semantic Analysis)

library(lsa)
tdm.mat = as.matrix(key_tdm[freq.word,],)
tdm.mat = lw_bintf(tdm.mat) * gw_idf(tdm.mat) # Local & Gloabe weighting 주는 코드

tdm.mat = tdm.mat[,colSums(tdm.mat) > 5]
news.lsa = lsa(tdm.mat, 5)

library(GPArotation) # 결과의 정확도를 높히기 위해 tk를 로테이션 시키는 것
tk = Varimax(news.lsa$tk)$loadings

# 30차원중 첫번째 차원의 값이 +로 갈수록 변하는 값의 변화량
for(i in 1:5){
  print(i)
  importance = order(abs(tk[,i]), decreasing = T) # abs를 사용하는 이유는 +-보다는 값이 크기가 중요하므로
  print(tk[importance[1:10],i])
}

# 각 문서의 좌표를 구하는 방법
dim(tdm.mat)
doc.space = t(tk) %*% tdm.mat
dim(doc.space)
doc.space[,1]

# 각 문서의 유사도 구하는 방법
norm = sqrt(colSums(doc.space^2))
norm.space = sweep(doc.space, 2, norm, '/') # 열 단위로 doc.space를 norm으로 나누는 것
norm.space[,1]
sum(norm.space[,1]^2)

cosine(norm.space[,1], norm.space[,2]) # 문서끼리 유사도 값

## 11. Topic Model

install.packages(c("RTextTools", "topicmodels"))

library(RTextTools) 
library(topicmodels)

key_dtm = as.DocumentTermMatrix(key_tdm[freq.word,])

key_dtm <- DocumentTermMatrix(key_corpus,
                              control = list(
                                removeNumbers = T, ## 숫자 제거
                                removePunctuation = T, ## 특수문자 제거
                                wordLengths = c(2,6),## 단어 글자가 2글자 이상부터 무한대(Inf)까지
                                stopwords = c("[c]","원인","현상","점검","조치"))) ## 특정단어 제거

k = 5; 

rowTotals <- apply(key_dtm , 1, sum) #Find the sum of words in each Document
key_dtm <- key_dtm[rowTotals> 0, ] #remove all docs without words
lda <- LDA(key_dtm, k)

terms(lda)
topics(lda)

## 12. Latent Dirichlet Allocation (LDA)

install.packages(c("LDAvis","lda","servr"))

library(LDAvis)
library(lda)
library(servr)

# weighting =function(x)
# weightTfIdf(x, normalize =FALSE),
dtm.r <- dtm2ldaformat(key_dtm)

eta = 0.02
alpha = 0.02

# Fit the model:
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(dtm.r$documents, K = 5, dtm.r$vocab, 
                                   num.iterations = 5000, alpha = 0.02, 
                                   eta = 0.02, initial = NULL, burnin = 1000,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 24 minutes on laptop

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

term.frequency <- slam::col_sums(key_dtm)

doc.length <- sapply(dtm.r$documents, function(x) sum(x[2,]))

Reviews <- list(phi = phi,
                theta = theta,
                doc.length = doc.length,
                vocab = dtm.r$vocab,
                term.frequency = term.frequency)

# create the JSON object to feed the visualization:
json <- createJSON(phi = Reviews$phi, 
                   theta = Reviews$theta, 
                   doc.length = Reviews$doc.length, 
                   vocab = Reviews$vocab, 
                   term.frequency = Reviews$term.frequency, 
                   R=10)


serVis(json, out.dir = 'vis', open.browser = F) # Firefox 이용
cat(json, file=file('vis/lda.json', encoding='utf8'))

# serVis(json, out.dir = 'vis', open.browser = T)

# http://cpsievert.github.io/LDAvis/reviews/reviews.html ## 


