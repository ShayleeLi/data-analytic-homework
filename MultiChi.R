# save functions for pairwise chi-square test
# to test independence between categorical variables

chiMatrix = function(dt) # input a data frame or matrix
{
  n=ncol(dt)
  c=rep(1,(n*n))
  rec=matrix(c,n)
  
  for (i in 1:n)
  {
    for (j in i:n)
    {
      rec[i,j]=chisq.test(dt[,i],dt[,j])$p.value
      rec[j,i]=rec[i,j]
    }
  }
  colnames(rec)=colnames(dt)
  rownames(rec)=colnames(dt)
  return(rec)
}

pickChi=function(dt,p=0.01) # input a data frame or matrix and p-value level
{
  data=chiMatrix(dt)
  n=ncol(data)
  a=colnames(data)
  for (i in 1:(n-1))
  {
    for (j in (i+1):n)
    {
      if(data[i,j]<=p)
        cat("Chi-square test [",a[i],", ",a[j],"] <=",p,"\n")
    }
  }
  return(data)
}


