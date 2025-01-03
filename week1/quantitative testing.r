#function for original code
original=function(){
  
  require(moments)
  
  year_edu = c(14,11,10,15,7,8,12,13,16,12,12,11,9,12,13,11,12,11,12,11,18,8,9,12,13,13,10,14,12,14,10,11,12,14,13,13)
  job_sat = c(36,38,42,51,30,37,40,43,47,44,37,40,32,42,45,38,42,42,43,46,53,30,35,40,40,41,32,50,33,47,38,37,40,50,42,45)
  
  mean(year_edu)
  median(year_edu)
  min(year_edu)
  max(year_edu)
  sd(year_edu)
  skewness(year_edu)
  kurtosis(year_edu)
  
  mean(job_sat)
  median(job_sat)
  min(job_sat)
  max(job_sat)
  sd(job_sat)
  skewness(job_sat)
  kurtosis(job_sat)

}

#function for updated code (version 1)
update1=function(){
  
  require(psych)
  
  my_df=data.frame(year_edu = c(14,11,10,15,7,8,12,13,16,12,12,11,9,12,13,11,12,11,12,11,18,8,9,12,13,13,10,14,12,14,10,11,12,14,13,13),
                   job_sat = c(36,38,42,51,30,37,40,43,47,44,37,40,32,42,45,38,42,42,43,46,53,30,35,40,40,41,32,50,33,47,38,37,40,50,42,45))
  
  describe(my_df)

}

#function ofr updated code (version 2)
update2=function(){
  
  my_moments=function(x){
    
    out=numeric(8)
    my_sort=sort(x)
    out[1]=length(x)
    out[2]=sum(x)/(out[1])
    out[3]=ifelse(out[1]%%2==1,
                  my_sort[(out[1]+1)/2],
                  (my_sort[(out[1])/2]+my_sort[(out[1])/2+1])/2)
    out[4]=my_sort[1]
    out[5]=my_sort[out[1]]
    out[6]=sqrt(sum((x-out[2])^2)/(out[1]))
    out[7]=(sum((x-out[2])^3)/out[1])/(sum((x-out[2])^2)/out[1])^(3/2)
    out[8]=out[1]*sum((x-out[2])^4)/(sum((x-out[2])^2)^2)
    
    names(out)=c('n','mean','median','min','max','sd','skew','kurt')
    
    return(out)
  }
  
  my_mat=matrix(c(14,11,10,15,7,8,12,13,16,12,12,11,9,12,13,11,12,11,12,11,18,8,9,12,13,13,10,14,12,14,10,11,12,14,13,13,
                  36,38,42,51,30,37,40,43,47,44,37,40,32,42,45,38,42,42,43,46,53,30,35,40,40,41,32,50,33,47,38,37,40,50,42,45),
                nrow=36,dimnames=list(c(),c('year_edu','job_sat')))
  
  apply(my_mat,2,my_moments)
  
}

#get benchmarks and arrange data
my_dat=data.frame(times=c(my_benchmark=microbenchmark(original())$time,
                    u1results=microbenchmark(update1())$time,
                    u2results=microbenchmark(update2())$time),
                  model=factor(c(rep(1,100),rep(2,100),rep(3,100))))

#describe benchmarks
describeBy(my_dat,group=my_dat$model)

#run a one-way ANOVA on benchmarks, note the overall model is statistically significant
my_aov=aov(times~model,data=my_dat)
summary(my_aov)

#run post-hocs, note the original code and the updated code (version 2) are not statistically significantly differnet while hte updated code (version 2) takes statistically significantly longer
TukeyHSD(my_aov)
