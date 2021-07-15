x1=12323
y1=12343
f1=function(x){
  return(2*sin(pi*x)^2)  
}
g1=function(x,p){
  return(2*pi*(1-x1*y1/x^2)*(x-p))  
}

for(j in (1:100)*150){
  upper2=j  
  image2=rep(0,1000)
  for(i in 1:1000){
    image2[i]=(f1(upper2+0.00005*(i+1))-f1(upper2+0.00005*i))^2+(g1(upper2+0.00005*(i+1),j)-g1(upper2+0.00005*i,j))^2
    image2[i]=image2[i]-f1(upper2+0.00005*(i+1))^2-(g1(upper2+0.00005*(i+1),j))^2-f1(upper2+0.00005*i)^2-g1(upper2+0.00005*i,j)^2
    image2[i]=image2[i]/((-2)*(sqrt(f1(upper2+0.00005*i)^2+g1(upper2+0.00005*i,j)^2)))
    image2[i]=image2[i]/sqrt(f1(upper2+0.00005*(i+1))^2+g1(upper2+0.00005*(i+1),j)^2)
    
    image2[i]=abs(acos(image2[i]))          
    
    
  }
  print(c(sum(image2[2]),j)) 
}
