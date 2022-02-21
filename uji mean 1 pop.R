x=ChickWeight$weight

#function rata-rata
ratarata=function(data)
{
  ratarata=0
  n=length(data)
  for(i in 1:n)
  {
    ratarata=ratarata+data[i]/n
  }
  return(ratarata)
}
ratarata(x)
mean(x)

#function varians
varians=function(data)
{
  varians=0
  n=length(data)
  for(i in 1:n)
  {
    varians=varians+((data[i]-ratarata(data))^2)/(n-1)
  }
  return(varians)
}
varians(x)
var(x)

#menghitung t test
print("H0 : miu sama dengan miu0")
print("H1 : miu tidak sama dengan miu0")
miu0=100
t=function(data)
{
  n=length(data)
  t=(ratarata(data)-miu0)*sqrt(n)/sqrt(varians(data))
  return(t)
}

t(x)

t.test(x,y=NULL,alternative = "two.sided", mu=100, paired = FALSE,var.equal = FALSE, conf.level = 0.95)
n=length(x)
df=n-1
ttab=qt(1-0.05,df)
ttab

if(abs(t(x))>ttab){
  print("keputusan : Tolak H0")
  print("kesimpulan : miu tidak sama dengan miu0")
}else{
  print("keputusan : Gagal Tolak H0")
  print("kesimpulan : miu sama dengan miu0")
}

pvalue=2*(1-pt(abs(t(x)),df))
