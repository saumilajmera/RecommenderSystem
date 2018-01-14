# CS422 Data Mining
# Saumil Ajmera A20397303
# Illinois Institute of Technology

rm(list=ls())
setwd("D:/iit Chicago/Data Mining/Assignment 4/ml-100K/ml-100K")

userRating <- read.csv(file = "u.data",sep = "",col.names=c("user id","movie id","rating","timestamp"), 
header= F)

movieType <- read.csv(file = "u.item",sep = "|",col.names=c( "movie id"," movie title", "release date", "video release date",
"IMDb URL","unknown","Action","Adventure","Animation","Children's","Comedy","Crime","Documentary","Drama","Fantasy", 
"Film-Noir","Horror","Musical","Mystery","Romance","Sci-Fi","Thriller","War","Western"), header= F)                                                                 

#(i)
user200 <- subset(userRating, userRating$user.id == 200) 
movies200 <- movieType[which(movieType$movie.id %in% user200$movie.id),] 
movie.matrix <- movies200[,6:24]
genreUser200 <- apply(movie.matrix, 2, mean)   

user50 <- subset(userRating, userRating$user.id == 50) 
movies50 <- movieType[which(movieType$movie.id %in% user50$movie.id),] 
movie.matrix <- movies50[,6:24]
genreUser50 <- apply(movie.matrix, 2, mean)  

movieid.matrix <-subset(movieType[127,6:24])

cosine <- function(x, y) {
  # Need to do error checking:
  # 1) Ensure x and y are vectors.
  
  sum(x*y)/(norm(x, type="2") * norm(y, type="2"))
}
cosine(genreUser200,genreUser50)
#(i) user-user similarity of user with ID 200 and user with ID 50 is 0.54825
cosine(genreUser200,movieid.matrix)
#(ii) the user-item similarity of movie with ID 127 to user 200 is 0.5533
cosine(genreUser50,movieid.matrix)
#(iii) the user-item similarity of movie with ID 127 to user 50 is 0.6235 

#(iv) Based on above value user 50 would be recommended movie 127 has it has higher cosine value

#(II) Second Part 
a <- merge(x= movieType,y=userRating,by="movie.id",all.y = TRUE)

#(b) Collaborative Filtering
utilityMatrix  <- matrix(NA, nrow=6,ncol=(11))
colnames(utilityMatrix) <- c("1","21","44","59","72","82","102","234","268", "409","486")
rownames(utilityMatrix) <- c("1","2","3","4","5","6")

# Loops through movie ids
for(i in 1:nrow(utilityMatrix)) 
{
  # Loops through user ids 
  for(j in 1:ncol(utilityMatrix)) 
  {
    k = as.numeric(colnames(utilityMatrix)[j]) 
    if(length(which(userRating$user.id %in% k & userRating$movie.id %in% i)) != 0)
    {
      index <- which(userRating$user.id %in% k & userRating$movie.id %in% i)
      utilityMatrix[i,j] <- userRating$rating[index]
    }
  }
}

data <- utilityMatrix
temp <- subset(userRating, userRating$user.id %in% c( 1, 21, 44, 59, 72, 82, 102, 234, 268,409, 486) & userRating$movie.id %in% c(1,2,3,4,5,6)) 

rMean <- rowMeans(data,na.rm = TRUE)

for(i in 1:nrow(data)) 
{
  for(j in 1:ncol(data)) 
  {
    if(!is.na(data[i,j]))
    {
      data[i,j] <- data[i,j] - rMean[[i]]
    } 
    else
    {
      data[i,j] <- 0 
    }
  }
}


sim15 <- cosine(as.numeric(data[1,]),as.numeric(data[5,]))
sim15
sim25 <- cosine(as.numeric(data[2,]),as.numeric(data[5,]))
sim25
sim35 <- cosine(as.numeric(data[3,]),as.numeric(data[5,]))
sim35
sim45 <- cosine(as.numeric(data[4,]),as.numeric(data[5,]))
sim45
sim65 <- cosine(as.numeric(data[6,]),as.numeric(data[5,]))
sim65

# sim35, sim25 and sim45 has the highest cosine similarity w.r.t 5 movie for N=3
# So now taking their weighted average gives 
c((utilityMatrix[3,9]*sim35 + utilityMatrix[2,9]*sim25 + utilityMatrix[4,9]*sim45)/(sim35+sim25+sim45))

utilityMatrix[5,9] = c((utilityMatrix[3,9]*sim35 + utilityMatrix[2,9]*sim25 + utilityMatrix[4,9]*sim45)/(sim35+sim25+sim45))

utilityMatrix
#Answer - So rating user 268 will give to movie 5 is 2.6


cor15 <- cor(as.numeric(data[1,]),as.numeric(data[5,]))
cor25 <- cor(as.numeric(data[2,]),as.numeric(data[5,]))
cor35 <- cor(as.numeric(data[3,]),as.numeric(data[5,]))
cor45 <- cor(as.numeric(data[4,]),as.numeric(data[5,]))
cor65 <- cor(as.numeric(data[6,]),as.numeric(data[5,]))

