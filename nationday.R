starplot <- function(pointcenter,Ridiu=10,interNm=10){
  getpoint <- function(pointcenter,R=Ridiu,interNum=interNm){
    
    #pointcenter = c(0,0)
    x=pointcenter[1]
    y=pointcenter[2]
    #R = 10
    points = seq(1,36)
    h = R*tan(18/180*pi)*tan(points/180*pi)/(tan(points/180*pi)+tan(18/180*pi))
    pointxy <- matrix(c(x,y+R),ncol=2)
    pointx = x+h
    #pointxInter = runif(10,x,pointx)
    pointy = y+h/tan(points/180*pi)
    #pointyInter = runif(10,y,pointy)
    pointxy <- rbind(pointxy,cbind(pointx,pointy))
    
    pointx_sym = x-h
    #pointx_symInter = runif(10,pointx_sym,x)
    pointy_sym = pointy
    pointxy_sym = cbind(pointx_sym,pointy_sym)
    #pointy_symInter = runif(10)
    pointxy <- rbind(pointxy,pointxy_sym)
    
    getinterpoints_XY <- function(point,n=interNum){
      tem <- point
      for(i in 1:nrow(point)){
        if(x < point[i,1]){
          pointxInter = runif(n,x,point[i,1])
          pointyInter = runif(n,y,point[i,2])
          tem <- rbind(tem,cbind(pointxInter,pointyInter))
        } else{
          pointxInter = runif(n,point[i,1],x)
          pointyInter = runif(n,y,point[i,2])
          tem <- rbind(tem,cbind(pointxInter,pointyInter))
        }}
      return(tem)
    }
    return(getinterpoints_XY(pointxy))
  }
  ###########对点进行翻转72度
  rotate <- function(poinpos,pointcenter,angle=72){
    #b.x = poinpos[,1]*cos(angle)  - poinpos[,2]*sin(angle)
    b.x = ( poinpos[,1] - pointcenter[1])*cos(angle/180*pi) - (poinpos[,2] - pointcenter[2])*sin(angle/180*pi) + pointcenter[1]
    
    b.y = (poinpos[,1] - pointcenter[1])*sin(angle/180*pi) + (poinpos[,2] - pointcenter[2])*cos(angle/180*pi) + pointcenter[2]
    
    #b.y = poinpos[,1]*sin(angle) + poinpos[,1]*cos(angle)
    poinpos_rotate = cbind(b.x,b.y)
    #return(rbind(poinpos,poinpos_rotate))
    return(poinpos_rotate)
  }
  #centerpoint = c(5,10)
  points = getpoint(pointcenter)
  for( i in seq(72,288,72)){
    points<- rbind(points,rotate(points,pointcenter))
    
  }
  return(points)
}

#####################
Bigstar_centerpoint = c(10,20)
littelstar1_centerpoint = c(Bigstar_centerpoint[1]+30*sin(pi/4),Bigstar_centerpoint[2]+30*cos(pi/4))
rotate2 <- function(poinpos,pointcenter,angle=20){
  b.x = ( poinpos[1] - pointcenter[1])*cos(angle/180*pi) - (poinpos[2] - pointcenter[2])*sin(angle/180*pi) + pointcenter[1]
  
  b.y = (poinpos[1] - pointcenter[1])*sin(angle/180*pi) + (poinpos[2] - pointcenter[2])*cos(angle/180*pi) + pointcenter[2]
  return(c(b.x,b.y))
  
}
littelstar2_centerpoint = rotate2(littelstar1_centerpoint,Bigstar_centerpoint,-30)
littelstar3_centerpoint = rotate2(littelstar1_centerpoint,Bigstar_centerpoint,-60)
littelstar4_centerpoint = rotate2(littelstar1_centerpoint,Bigstar_centerpoint,-90)
Bigstar_points=as.data.frame(starplot(Bigstar_centerpoint))
littelstar1_points = as.data.frame(starplot(littelstar1_centerpoint,Ridiu = 5,interNm = 2))
littelstar2_points =as.data.frame( starplot(littelstar2_centerpoint,Ridiu = 5,interNm = 2))
littelstar3_points = as.data.frame(starplot(littelstar3_centerpoint,Ridiu = 5,interNm = 2))
littelstar4_points = as.data.frame(starplot(littelstar4_centerpoint,Ridiu = 5,interNm = 2))
Bigstar_points=cbind(Bigstar_points,label="A")
littelstar1_points= cbind(littelstar1_points,label="B")
littelstar2_points= cbind(littelstar2_points,label="C")
littelstar3_points= cbind(littelstar3_points,label="D")
littelstar4_points= cbind(littelstar4_points,label="E")
points_all <- rbind(Bigstar_points,littelstar1_points,littelstar2_points,littelstar3_points,littelstar4_points)
library(ggplot2)
library(plotly)
#ggplot(as.data.frame(points_all),aes(x=pointx,y=pointy,color=label))+geom_point()
#ggplot(as.data.frame(points_all),aes(x=pointx,y=pointy,color=label))+geom_point(size=0.5)+xlab("PC1")+ylab("PC2")
#p=ggplot(as.data.frame(points_all),aes(x=pointx,y=pointy,color=label))+geom_point(size=0.5)+xlab("PC1(人民主人翁)")+ylab("PC2(人民主人翁)") +annotate("text",x=25,y=25,label="70",size=100,colour="red",fontface="italic")+labs(color="百花齐放")
ggplot(as.data.frame(points_all),aes(x=pointx,y=pointy,color=label))+geom_point(size=0.5)+xlab("PC1(人民主人翁)")+ylab("PC2(人民主人翁)") +annotate("text",x=25,y=25,label="70",size=100,colour="red",fontface="italic")+labs(color="百花齐放")+ggtitle("祖国母亲细胞群")+ theme(plot.title = element_text(hjust = 0.5,colour = "red",size=20))
ggplotly(p)
