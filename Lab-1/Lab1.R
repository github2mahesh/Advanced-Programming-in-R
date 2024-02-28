name <- "UMAMAHESWARABABU MADDELA"
liuid <- "umama339"

my_num_vector <- function(){
  a<-log10(11)
  b<-cos(pi/5)
  c<-exp(pi/3)
  d<-(1173%%7)/19
  x<-c(a,b,c,d)
  return(x)
}


filter_my_vector <- function(x, leq){
  for (i in 1:length(x)){
    if (x[i]>=leq){
        x[i]<-NA
    }
  }
  return(x)
}

dot_prod <- function(a,b){
  x=0
  for (i in 1:length(a)){
    x=x+a[i]*b[i]
  }
  return(x)
}

approx_e <- function(N){
  e=0
  for(i in 0:N){
    e=e+1/factorial(i) 
  }
  return(e)
}

my_magic_matrix<-function(){
  m<-matrix(c(4,9,2,3,5,7,8,1,6),nrow = 3, ncol = 3, byrow = TRUE)
  return(m)
}

calculate_elements<-function(A){
  return(length(A))
}

row_to_zero=function(A,i){
  A[i,]<-0
  return(A)
}

add_elements_to_matrix<-function(A,x,i,j){
  for (p in i){
    for (q in j) {
      A[p,q]=A[p,q]+x
    }
  }
  return(A)
}

my_magic_list=function(){
  l<-list('info'="my own list",my_num_vector(),my_magic_matrix())
  return(l)
}

change_info<-function(x,text){
  x$info<-text
  return(x)
}

add_note<-function(x,note){
  l<-list('note'=note)  
  x<-c(x,l)
  return(x)
}

sum_numeric_parts=function(x){
  s=0
  for (i in 1:length(x)){
    if (is.numeric(x[[i]])==TRUE){
      s=s+sum(x[[i]])
    }
  }
  return(s)
}

my_data.frame=function(){
  id<-c(1,2,3)
  name<-c('John','Lisa','Azra')
  income<-c(7.30, 0.00, 15.21)
  rich<-c(FALSE, FALSE, TRUE)
  df<-data.frame(id,name,income,rich)
  return(df)
}

sort_head=function(df, var.name, n){
  ndf<- df[order(df[,var.name], decreasing = TRUE),]
  sorted_df<- head(ndf,n)
  return(sorted_df)
}

add_median_variable=function(df,j){
  m=median(df[[j]])
  cmp<-c()
  for (i in 1:length(df[[j]])){
    if (df[[j]][i]>m){
      cmp<-append(cmp,'Greater')
    }else if(df[[j]][i]<m){
      cmp<-append(cmp,'Smaller')
    }else {
      cmp<-append(cmp,'Median')
    }
  }
  df['compared_to_median']<-cmp
  return(df)
}

analyze_columns<-function(df,j){
  anl_col<- list()
  k<-1
  for (i in j){
    vec <- c(mean(df[,i]), median(df[,i]), sd(df[,i]))
    names(vec)<- c("mean","median","sd")
    anl_col[[k]]<-vec
    k=k+1
  }
  correlation_matrix <- cor(df[,j])
  anl_col[[3]]<-correlation_matrix
  names(anl_col)<-c(colnames(df[j]),"correlation_matrix")
  return(anl_col)
}  

