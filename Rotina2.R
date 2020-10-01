remove(list=ls())
Endereco="D:/Backup Pendrive/IF Guanambi/Bruno Vinicius/Palma/Area Foliar"
setwd(Endereco)



####################################################################################
PreencherMascaraNegra =function(img2,perc,imagem=T,individual=F){
  if(imagem==T){t=img2@.Data[,,1]}
  if(imagem==F){t=img2}
  
  if(individual==F){
    
    
    n=round(perc*min(c(ncol(t),nrow(t))),0)
    
    p1=function(t){
      t2=t
      for( i in 2:(nrow(t)-n-1)){
        for(j in 1:ncol(t)){
          if(t[i,j]==1){
            if(t[i-1,j]==0){
              
              a=0
              while(a<n){
                a=a+1
                
                if(sum(t[i:(i+a),j]==1)<a){t2[i:(i+a),j]=0;a=n}
                
              }
              
            }
          }
        }
      }
      return(t2)
    }
    
    Pp=p1(t)
    Pp=p1(t(Pp))
    return(t(Pp))
  }
  
  if(individual==T){
    
    t2=Contorno(t,imagem = F)
    display(t2)
    m=cbind(expand.grid(1:nrow(t2),1:ncol(t2)),c(t2))
    m=as.matrix(m[m[,3]<1,])
    
    ind=unique(m[,1])
    for(y in 1:length(ind)){
      t2[ind[y],min(m[m[,1]==ind[y],2]):max(m[m[,1]==ind[y],2])]=0
    }
    
    
    return(t2)
    
  }
}
######################################################################################

require(EBImage)
fundo=readImage("fundo0.jpg")
palma=readImage("palma.jpg")
ref=readImage("ref.jpg")

Mfundo=cbind(c(fundo@.Data[,,1]),c(fundo@.Data[,,2]),c(fundo@.Data[,,3]))
Mfundo=Mfundo[sample(1:nrow(Mfundo)),]
Mfundo=Mfundo[1:5000,]


Mpalma=cbind(c(palma@.Data[,,1]),c(palma@.Data[,,2]),c(palma@.Data[,,3]))
Mpalma=Mpalma[sample(1:nrow(Mpalma)),]
Mpalma=Mpalma[1:5000,]

Mref=cbind(c(ref@.Data[,,1]),c(ref@.Data[,,2]),c(ref@.Data[,,3]))
Mref=Mref[sample(1:nrow(Mref)),]
Mref=Mref[1:5000,]

mat=data.frame(rbind(cbind(Mfundo,"F"),cbind(Mref,"R"),cbind(Mpalma,"P")))
colnames(mat)=c("R","G","B","Y")
for(i in 1:3){mat[,i]=as.numeric(as.character(mat[,i]))}
n=nrow(mat)
mat=mat[sample(1:n),]

trein=mat[1:round(n*.7),]
val=mat[round(n*.7+1):n,]


library(nnet)
model=multinom(Y~.,data=trein)
mean(predict(model)==trein$Y)
mean(predict(model,newdata=val)==val$Y)


nomeFoto="IMG_0003.JPG"
    Im=readImage(nomeFoto)  
    im2=EBImage::resize(Im, 384)
    #EBImage::display(im2)
    
    im3=data.frame(R=c(im2@.Data[,,1]),G=c(im2@.Data[,,2]),B=c(im2@.Data[,,3]))
    imp3=predict(model,newdata=im3)
    im2b=matrix(imp3!="P",nrow=nrow(im2@.Data[,,1]),byrow = F)
    im2palma=PreencherMascaraNegra(im2b,perc = 0.02,imagem=F,individual=F)
    #EBImage::display(im2palma)
    
    im2b=matrix(imp3!="R",nrow=nrow(im2@.Data[,,1]),byrow = F)
    im2ref=PreencherMascaraNegra(im2b,perc = 0.02,imagem=F,individual=F)
    #EBImage::display(im2ref==F)
    
    NumPixelRef=sum(im2ref==0)
    
    MPred=bwlabel(im2palma==0)
    Shape=computeFeatures.shape(MPred)
    ID2=Shape[,1]>200
    Medidas=matrix(Shape[ID2,-4],ncol=5)
    AreaConhecida=46.75
    Medidas2=cbind(AreaCor=Medidas[,1]*AreaConhecida/NumPixelRef,Medidas)
    
    moment=computeFeatures.moment(MPred)
    moment=matrix(moment[ID2,],ncol=5)
    
    #Medidas2=cbind(Planta=nomePasta,Foto=nomeFotos[j],raque=1:nrow(Medidas2),Medidas2)
    
    writeImage(MPred,file="aa.jpeg")
    im4=load.image("aa.jpeg")
    print("Area foliar em cm²")
    Medidas2[,1]
   
    
    for (k in 1:sum(ID2)){
      pf=px.flood(im4,moment[k,1],moment[k,2],sigma=.21) #%>% highlight
      #sp=split_connected(im)
      #plot(pf)
      ima=im2
      ima@.Data[,,1]=ima@.Data[,,1]*as.matrix(pf)
      ima@.Data[,,2]=ima@.Data[,,2]*as.matrix(pf)
      ima@.Data[,,3]=ima@.Data[,,3]*as.matrix(pf)
     
      jpeg(file=paste(nomeFoto,paste("Raque",k,sep="."),".jpeg",sep="_"))
      plot(ima)
    text(nrow(ima@.Data[,,1])/2,ncol(ima@.Data[,,1])/2,paste(round(Medidas2[k,1],2),"cm²"),col="red",cex = 3)
      
    dev.off()
     
  }
  
  
