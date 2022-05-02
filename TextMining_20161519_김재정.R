# 라이브러리 정리
library(readr)
library(KoNLP)
library(tidyr)
library(tidytext)
library(dplyr)
library(textclean)
library(stringr)
library(ggplot2)

# 2. 가장 자주 사용된 단어 추출 및 빈도 그래프 그리기
### 데이터 구조 확인확인
raw_youtube_comment <- readLines("youtube_comment.txt", encoding = "UTF-8")
str(raw_youtube_comment)
head(raw_youtube_comment)

### 전처리 수행
### Pipeline 연산자를 활용하여 전처리 작업 한번에 수행
### str_replace_all("[^가-힣]", replacement = " ") : 한글을 제외한 나머지 공백으로 변환
### str_squish() : 연속된 공백 제거
### as_tibble() : 데이터 구조 파악과 텍스트 관련 함수 적용에 용이한 tibble 구조로 변환
youtube_comment <- raw_youtube_comment %>% 
    str_replace_all("[^가-힣]", replacement = " ") %>%
    str_squish() %>%
    as_tibble()
youtube_comment


### 단어 빈도를 확인하기 위한 토큰화
### 단어 빈도를 분석하기 위해 token = "words" 옵션 사용
word_space <- youtube_comment %>%
    unnest_tokens(input = value,
                  output = word,
                  token = "words")
word_space


### count() 함수를 통해 단어 빈도 추출
word_space <- word_space %>% count(word, sort = T)
word_space

### 한글자 단어 제거
### str_count() : 문자열의 글자 수 return
### filter() : ()안의 조건에 맞는 행만 추출
word_space <- word_space %>% filter(str_count(word)>1)
word_space

### 자주 사용된 단어 추출
### 가장 많이 사용된 단어가 필요하므로 head(1)
top1 <- word_space %>% head(1)
top1

### 빈도 그래프 그리기(가독성을 위해 상위 10개만 추출)
### ggplot2 패키지의 geom_col()함수를 이용하여 막대그래프 생성
### 
top10 <- word_space %>% head(10)
ggplot(top10, aes(x = reorder(word, n), y = n)) +
    geom_col() +
    coord_flip()

### 단어 빈도를 확인하기 위한 토큰화
### 제대로된 단어 분석을 위해 KoNLP 패키지를 이용(형태소 분석)
### 명사 기준 토큰화
word_space <- youtube_comment %>% unnest_tokens(input = value,
                                                output = word,
                                                token = extractNoun)
word_space

### count() 함수를 통해 단어 빈도 추출
### str_count() : 문자열의 글자 수 return
### filter() : ()안의 조건에 맞는 행만 추출
word_space <- word_space %>% count(word, sort = T) %>% filter(str_count(word)>1)
word_space

### 자주 사용된 단어 추출
### 가장 많이 사용된 단어가 필요하므로 head(1)
top1 <- word_space %>% head(1)
top1

### 빈도 그래프 그리기(가독성을 위해 상위 10개만 추출)
### ggplot2 패키지의 geom_col()함수를 이용하여 막대그래프 생성
### 
top10 <- word_space %>% head(10)
ggplot(top10, aes(x = reorder(word, n), y = n)) +
    geom_col() +
    coord_flip()

# 3. 비교 분석(오즈비)
### 두 개의 데이터셋 불러오기
raw_crime_youtube_comment <- readLines("youtube_comment.txt", encoding = "UTF-8")
raw_wind_youtube_comment <- readLines("youtube_comment2.txt", encoding = "UTF-8")

### 구분을 위해 각자의 이름 추가
crime_comment <- raw_crime_youtube_comment %>% as_tibble() %>% mutate(movie = "crime")
wind_comment <- raw_wind_youtube_comment %>% as_tibble() %>% mutate(movie = "wind")

### 데이터 합치기
### 두 데이터를 행(세로) 방향으로 결합
### 출력 결과를 보기 편하게 select()로 변수 순서 변경
bind_youtube_comment <- bind_rows(crime_comment, wind_comment) %>% select(movie, value)
head(bind_youtube_comment)
tail(bind_youtube_comment)

### 기본적인 전처리 및 토큰화
youtube_comment <- bind_youtube_comment %>% mutate(value = str_replace_all(value, "[^가-힣]", " "), value = str_squish(value))
youtube_comment <- youtube_comment %>% unnest_tokens(input = value,
                                                     output = word,
                                                     token = extractNoun)
youtube_comment

### 집단별 단어 빈도 구하기
### count 함수를 사용하여 각각의 단어빈도 구하기(2글자 이상)
freq <- youtube_comment %>% count(movie, word) %>% filter(str_count(word)>1)
freq

### 오즈비 구하기 전 전처리
### 데이터 전체를 wide form으로 변환
freq_wide <- freq %>% pivot_wider(names_from = movie, values_from = n, values_fill = list(n=0))
freq_wide

### 오즈비 구하기
### 계산 -> '각 단어의 빈도'/'모든 단어 빈도의 합', 일괄 + 1
### 일괄 +1은 향후 계산시 0으로 나누는 경우를 방지하기 위함
freq_wide <- freq_wide %>% mutate(ratio_crime = ((crime+1)/(sum(crime+1))), ratio_wind = ((wind+1)/(sum(wind+1))))
freq_wide

### 최종 계산 한 텍스트의 단어 비중을 다른 텍스트의 단어 비중으로 나눔
freq_wide <- freq_wide %>% mutate(odds_ratio = ratio_crime/ratio_wind)
freq_wide

### 해석
### 1보다 큰 값 = 범죄와의 전쟁에서 비중이 큰 단어
### 1보다 작은 값 = 바람에서 비중이 큰 단어
freq_wide %>% arrange(-odds_ratio)
freq_wide %>% arrange(odds_ratio)

# 4. 감정사전을 적용하여, 텍스트의 감정 경향을 분석하기
### 똑같이 기본적인 전처리 수행
raw_youtube_comment <- readLines("youtube_comment.txt", encoding = "UTF-8") 

### 고유번호 부착
youtube_comment <- raw_youtube_comment %>% as_tibble() %>% mutate(id = row_number()) 

### 단어기준 토큰화하고 부착
word_comment <- youtube_comment %>% unnest_tokens(input = value, output = word, token = "words", drop = F)
word_comment %>% select(word, value)

### 감정사전 불러오기
dic = read_csv("knu_sentiment_lexicon.csv")

### 감정사전을 통해 감정점수 부여
word_comment <- word_comment %>%
    left_join(dic, by = "word") %>%
    mutate(polarity = ifelse(is.na(polarity), 0, polarity))
word_comment %>% select(word, polarity)

### 감정 분류하기
word_comment <- word_comment %>%
    mutate(sentiment = ifelse(polarity == 2, "pos", ifelse(polarity == -2, "neg", "neu")))
word_comment %>% count(sentiment)

### 자주 사용된 감정단어 살피기
### 긍정 상위 10개, 부정 상위 10개 추출하여 막대 그래프
top10_sentiment <- word_comment %>%
    filter(sentiment != "neu") %>%
    count(sentiment, word) %>%
    group_by(sentiment) %>% 
    slice_max(n, n = 10)

### 댓글별 감정 점수 계산
score_comment <- word_comment %>%
    group_by(id, value) %>%
    summarise(score = sum(polarity)) %>% ungroup()
score_comment %>% select(score, value)

### 긍정적인 댓글 찾기
score_comment %>% select(score, value) %>% arrange(-score)
### 부정적인 댓글 찾기
score_comment %>% select(score, value) %>% arrange(score)

### 댓글 감정경향 비교분석
score_comment <- score_comment %>% 
    mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))
freq_score <- score_comment %>%
    count(sentiment) %>% 
    mutate(ratio = n/sum(n)*100)
freq_score

# 5. 감정사전 수정 후 비교분석
### 맥락에 안맞는것으로 추정되는 댓글 조회
new_dic %>% filter(word %in% c("소름", "소름이", "미친", "ㅋㅋ", "쓸데없이", "미치겠다", "쩐다"))

### 감정사전 수정
new_dic <- dic %>% mutate(polarity = ifelse(word %in% c("소름", "소름이", "미친", "미치겠다"), 2, ifelse(word %in% "쓸데없이", 1, polarity)))

### 감정사전 추가
new_word <- tibble(word = c("ㅋㅋ", "쩐다", "개쩐다"), polarity = c(1, 2, 2))
new_word_dic <- bind_rows(new_dic, new_word)
new_word_dic %>% filter(word %in% c("소름", "소름이", "미친", "미치겠다", "ㅋㅋ", "쓸데없이", "쩐다", "개쩐다"))

### 수정된 사전으로 다시 감정점수 계산
new_word_comment <- word_comment %>% select(-polarity) %>% left_join(new_word_dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity), 0, polarity))
new_word_comment

# 비교분석
new_score_comment <- new_word_comment %>%
    group_by(id, value) %>%
    summarise(score = sum(polarity)) %>% ungroup()
new_score_comment %>% select(score, value) %>% arrange(-score)
new_score_comment <- new_score_comment %>% mutate(sentiment = ifelse(score >= 1, "pos", ifelse(score <= -1, "neg", "neu")))
new_score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
score_comment %>% count(sentiment) %>% mutate(ratio = n/sum(n)*100)
### 향상된 모습을 알 수 있음
