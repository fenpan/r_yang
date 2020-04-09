getwd() 
workL <- "/Users/sunxy/Desktop/全国脂肪肝投稿/全国脂肪肝-R语言新"
setwd(workL) 
library(foreign)
mydata<-read.spss('全国调研加地区-0824xmf_1-0107加烟酒.sav')
mydata2<-as.data.frame(mydata)
mydata2<-mydata2[,-c(70,71)]
mydata2$编号<-as.character(mydata2$编号)
mydata2$编号<-trimws(mydata2$编号, which = c("both", "left", "right") )  #去除字符串里的空格
mydata2[c(305,690),]   #编号重复
mydata2[305,1]<-"D03-088-2"

library(readxl)
#data <- read.table("name.txt",header = T,sep = "")
data <- read_excel("全国调研 给潇泱.xlsx",sheet=1,col_names = T,col_types = NULL ,na="", skip=0)
data[823,1]<-"D03-088-2"   #编号重复

grep("他汀",colnames(data))    #include string
tatin_data<-data[,c(1,2,159:162)]
head(tatin_data)
tatin_data<-as.data.frame(tatin_data)
colnames(tatin_data)[1]<-"编号"

tatin_data[,7]<-tatin_data[,1]  #复制一列编号用于后续比较
#colnames(mydata2)
mydata3 <- merge(mydata2,tatin_data,by.tatin_data = "编号",by.mydata2 = "编号",all.x=TRUE)  #前一个小名单，后一个大名单，all.x=true 保留第一个矩阵的结构
colnames(mydata3)
# table(tatin_data$他汀类药物)
# table(mydata3$他汀类药物)
# l1<-tatin_data$编号
# l2<-mydata2$编号
# #l3<-trimws(l2, which = c("both", "left", "right") )
# l4<-intersect(l1,l2)
#setequal(l3, l4)   #比较两个集合中的元素是否完全相同 但向量里有重复元素检测不出来
#which(as.data.frame(table(l3))[,2]==2)  #把table结果变成matrix
mydata3<-mydata3[,-dim(mydata3)[2]]
mydata3$他汀类药物二分类<-mydata3$他汀类药物
mydata3$他汀类药物二分类[which(mydata3$他汀类药物!=1)]<-0
mydata3$他汀类药物二分类[which(is.na(mydata3$他汀类药物))]<-0


colnames(mydata3)
#mydata3是合并完他汀数据的n=2672

colnames(mydata3)[10]<-"调查前1个月饮食控制0无1有"
colnames(mydata3)[11]<-"调查前1个月运动治疗0无1有" 
colnames(mydata3)[16]<-"B超脂肪肝有无0无1有" 
colnames(mydata3)[67]<-"new每周酒精量g"
colnames(mydata3)[9]<-"有无降糖药物干预0无1有"
colnames(mydata3)

asfactor<-function(a,b){         #transfer v as factor
  num<-which(colnames(a)==b)
  #print(class(a))
  #print(num)
  a[,num]<-as.factor(as.character(a[,num]))
  #print(class(a[,num]))
  return(a)
}

asnum<-function(a,b){      #transfer v as numeric
  num<-which(colnames(a)==b)
  a[,num]<-as.numeric(a[,num])
  return(a)
}

listf<-c(colnames(mydata3)[c(4:16,19,20,29,51,55,56,58,69:71,73,74,76,77)])  
# add need to be factor
for (i in c(1:length(listf))){
  mydata3<-asfactor(mydata3,listf[i])
}

# listn<-c("bmr","dmperiod","obesitydegree","flhis","ppg120","apoa","rimt","tpoab","tgab",   # add need to be num
#          "fat","necro","ballooning","nas","saf","uwbc","e2")
# for (j in c(1:length(listn))){
#   liverdata2<-asnum(liverdata2,listn[j])
# }

mydata4<-subset(mydata3,省份1上海2天津3宁夏4江苏5河南6云南7山东8黑龙江9湖南!=3 &省份1上海2天津3宁夏4江苏5河南6云南7山东8黑龙江9湖南!=6)
#mydata4是合并完他汀数据的n=2420


crosstab<-table(mydata4[,c("有无降糖药物干预0无1有","地理分区1南方2北方")]) #分组变量写后面
apply(crosstab,2,sum)  #纵向求和
colnum<-which(crosstab==622,arr.ind = TRUE)[2]    #获取元素在表中的行列号 [2]为在第几列
prop<-622/apply(crosstab,2,sum)[colnum]
n=paste(622,"(",round(prop*100,2), "%",")",sep='')

my_crosstab<-function(dataname,vname,groupname){    #实现2X2的百分比显示
  crosstab<-table(dataname[,c(vname,groupname)]) #分组变量写后面
  crosstab<-as.matrix(crosstab)
  chi2_p<-round(chisq.test(crosstab)$p.value,3)
  l=length(crosstab)
  crosstab2<-crosstab
  for (n in c(1:l)){
    #print(n)
    x<-crosstab[n]
    colnum<-which(crosstab==x,arr.ind = TRUE)[2]#获取元素x在表中的行列号 [2]为在第几列
    #print(colnum)
    prop<-x/apply(crosstab,2,sum)[colnum]
    #print(prop)
    y=paste(x,"(",round(prop*100,2), "%",")",sep='')
    #print(y)
    crosstab2[n]<-y
    #print(y)
  }
  #print(crosstab2)
  return(crosstab2)
  #return(chi2_p)
  #print(chi2_p)
}             # NXN 二级列联表及
my_crosstab_p<-function(dataname,vname,groupname){
  crosstab<-table(dataname[,c(vname,groupname)]) #分组变量写后面
  chi2_p<-round(chisq.test(crosstab)$p.value,3)
  return(chi2_p)
}

my_crosstab(mydata4,"他汀类药物二分类","地理分区1南方2北方")
my_crosstab_p(mydata4,"他汀类药物二分类","地理分区1南方2北方")
my_crosstab(mydata4,"入组受试者类型1正常2IGR3DM","地理分区1南方2北方")


#############
my_crosstab_plus<-function(dataname,vname,groupname){    #实现含sum的百分比显示
  crosstab<-table(dataname[,c(vname,groupname)]) 
  crosstab2<-addmargins(table(dataname[,c(vname,groupname)])) #分组变量写后面
  crosstab<-as.matrix(crosstab)
  chi2_p<-round(chisq.test(crosstab)$p.value,3)
  row=dim(crosstab2)[1]
  col=dim(crosstab2)[2]
  crosstab3<-crosstab2
  for (c in c(1:col)){
    for (r in c(1:row)){
      prop<-crosstab2[r,c]/crosstab2[row,c]
      x<-crosstab2[r,c]
      y<-paste(x,"(",round(prop*100,2), "%",")",sep='')
      crosstab3[r,c]<-y
      
    }
  }
  return(crosstab3)
}


my_crosstab_p<-function(dataname,vname,groupname){    #输出卡方p和校正性别年龄的p
  crosstab<-table(dataname[,c(vname,groupname)]) #分组变量写后面
  chi2_p<-round(chisq.test(crosstab)$p.value,3)
  i<-which(colnames(dataname)==vname)
  j<-which(colnames(dataname)==groupname)
  test_df <- dataname
  test_df$level <- test_df[,i] == 1 #因变量设置，转换为布尔型
  glm_ploy0 <- glm(level ~ test_df[,j] +test_df[,19] +test_df[,21], data = test_df,family=binomial(link='logit'))  #test_df[,19] +test_df[,21]为性别年龄
  p_adjust<-summary(glm_ploy0)$coefficients[2,4]  #校正了性别年龄的p
  return(c(chi2_p,p_adjust))
}

my_crosstab_plus(mydata4,"B超脂肪肝有无0无1有","地理分区1南方2北方") 
my_crosstab_p(mydata4,"他汀类药物二分类","地理分区1南方2北方") 

# t<-my_crosstab_plus(mydata4,"B超脂肪肝有无0无1有","地理分区1南方2北方") 
# t2<-t[2,]
# c(t2[length(t2)],t2[-length(t2)],my_crosstab_p(mydata4,"他汀类药物二分类","地理分区1南方2北方") )











library(xlsx)
write.xlsx(my_crosstab(mydata4,"有无降糖药物干预0无1有","地理分区1南方2北方"),"table1.xlsx",sheetName="Sheet2",append=TRUE)  #同时改sheetname和append true
write.xlsx(my_crosstab(mydata4,"他汀类药物二分类","地理分区1南方2北方"),"table1.xlsx",append=TRUE)


mydata4_noanti<-subset(mydata4,有无降糖药物干预0无1有==0)

#两组回归直线的比较
fit1 <- aov(lgLFC ~ lgfbg * 地理分区1南方2北方, data = mydata4_noanti)
summary(fit1)   #看回归线斜率是否相等
summary(aov(lgLFC ~ lgfbg + 地理分区1南方2北方, data = mydata4_noanti))  #协方差，看分组是否仍有差异

#分组求pearson相关系数和p
cordata=mydata4_noanti[mydata4_noanti$地理分区1南方2北方==1,]
cor.test(cordata$lgLFC,cordata$lgfbg,method = "pearson")$p.value
cor.test(cordata$lgLFC,cordata$lgfbg,method = "pearson")$estimate
cordata=mydata4_noanti[mydata4_noanti$地理分区1南方2北方==2,]
cor.test(cordata$lgLFC,cordata$lgfbg,method = "pearson")$p.value
cor.test(cordata$lgLFC,cordata$lgfbg,method = "pearson")$estimate



##########################
get_or_withci<-function(glmmodel){     #logistc回归中得到or ci 和p
  p1<-round(summary(glmmodel)$coefficient[2,4],3)  #回归模型中地域的p
  or1<-exp(coef(glmmodel))[2]  #OR值
  #exp(confint(glmmodel)) #95%的置信区间
  or_withci<-paste(round(or1,2),"(",round(exp(confint(glmmodel))[2,1],2),"-",round(exp(confint(glmmodel))[2,2],2),")",sep="")
  l<-c(or_withci,p1)
  return(l)
}

####### 校正性别年龄后的p 他汀使用差异
test_df <- mydata4
#colnames(mydata4)
test_df$level <- test_df$他汀类药物二分类 == 1 #因变量设置，转换为布尔型
glm_ploy0 <- glm(level ~ 地理分区1南方2北方+性别1男2女+年龄, data = test_df,family=binomial(link='logit'))
result1<-get_or_withci(glm_ploy0)



#colnames(mydata4)


#############  总人群回归
test_df$level <- test_df$B超脂肪肝有无0无1有 == 1 #因变量设置，转换为布尔型
glm_ploy1 <- glm(level ~ 地理分区1南方2北方, data = test_df,family=binomial(link='logit'))
result1<-get_or_withci(glm_ploy1)

glm_ploy2 <- glm(level ~ 地理分区1南方2北方+性别1男2女+年龄+饮酒史二分类+吸烟史二分类, data = test_df,family=binomial(link='logit'))
result2<-get_or_withci(glm_ploy2)

glm_ploy3 <- glm(level ~ 地理分区1南方2北方+性别1男2女+年龄+饮酒史二分类+吸烟史二分类
                 +BMI+腰围+ogtt血糖0+收缩压+胆固醇+甘油三酯+hdlc+ldlc+他汀类药物二分类, data = test_df,family=binomial(link='logit'))
result3<-get_or_withci(glm_ploy3)

glm_ploy4 <- glm(level ~ 地理分区1南方2北方+性别1男2女+年龄+饮酒史二分类+吸烟史二分类
                 +BMI+腰围+ogtt血糖0+收缩压+胆固醇+甘油三酯+hdlc+ldlc+他汀类药物二分类
                 +alt+ast+ggt, data = test_df,family=binomial(link='logit'))
result4<-get_or_withci(glm_ploy4)

or_matrix_all<-rbind(result1,result2,result3,result4)
library(xlsx)
write.xlsx(or_matrix_all,"or.xlsx",sheetName="Sheet1",append=TRUE)  #同时改sheetname和append true

###
for (g in 1:3){
test_df <- mydata4[mydata4$入组受试者类型1正常2IGR3DM==g,]
#colnames(mydata4)
test_df$level <- test_df$B超脂肪肝有无0无1有 == 1 #因变量设置，转换为布尔型
glm_ploy1 <- glm(level ~ 地理分区1南方2北方, data = test_df,family=binomial(link='logit'))
result1<-get_or_withci(glm_ploy1)

glm_ploy2 <- glm(level ~ 地理分区1南方2北方+性别1男2女+年龄+饮酒史二分类+吸烟史二分类, data = test_df,family=binomial(link='logit'))
result2<-get_or_withci(glm_ploy2)

glm_ploy3 <- glm(level ~ 地理分区1南方2北方+性别1男2女+年龄+饮酒史二分类+吸烟史二分类
                 +BMI+腰围+ogtt血糖0+收缩压+胆固醇+甘油三酯+hdlc+ldlc+他汀类药物二分类, data = test_df,family=binomial(link='logit'))
result3<-get_or_withci(glm_ploy3)

glm_ploy4 <- glm(level ~ 地理分区1南方2北方+性别1男2女+年龄+饮酒史二分类+吸烟史二分类
                 +BMI+腰围+ogtt血糖0+收缩压+胆固醇+甘油三酯+hdlc+ldlc+他汀类药物二分类
                 +alt+ast+ggt, data = test_df,family=binomial(link='logit'))
result4<-get_or_withci(glm_ploy4)

or_matrix_all<-rbind(result1,result2,result3,result4)

write.xlsx(or_matrix_all,"or.xlsx",sheetName=paste("Sheet",(g+1),sep=""),append=TRUE)  #同时改sheetname和append true
}
########


############# 总人群饮酒=0
test_df <- subset(mydata4,饮酒史二分类==0)
#colnames(mydata4)

test_df$level <- test_df$B超脂肪肝有无0无1有 == 1 #因变量设置，转换为布尔型
glm_ploy1 <- glm(level ~ 地理分区1南方2北方, data = test_df,family=binomial(link='logit'))
result1<-get_or_withci(glm_ploy1)

glm_ploy2 <- glm(level ~ 地理分区1南方2北方+性别1男2女+年龄+吸烟史二分类, data = test_df,family=binomial(link='logit'))
result2<-get_or_withci(glm_ploy2)

glm_ploy3 <- glm(level ~ 地理分区1南方2北方+性别1男2女+年龄+吸烟史二分类
                 +BMI+腰围+ogtt血糖0+收缩压+胆固醇+甘油三酯+hdlc+ldlc+他汀类药物二分类, data = test_df,family=binomial(link='logit'))
result3<-get_or_withci(glm_ploy3)

glm_ploy4 <- glm(level ~ 地理分区1南方2北方+性别1男2女+年龄+吸烟史二分类
                 +BMI+腰围+ogtt血糖0+收缩压+胆固醇+甘油三酯+hdlc+ldlc+他汀类药物二分类
                 +alt+ast+ggt, data = test_df,family=binomial(link='logit'))
result4<-get_or_withci(glm_ploy4)

or_matrix_all<-rbind(result1,result2,result3,result4)
library(xlsx)
write.xlsx(or_matrix_all,"or2.xlsx",sheetName="Sheet1",append=TRUE)  #同时改sheetname和append true

###
for (g in 1:3){
  mydata42<-subset(mydata4,饮酒史二分类==0)
  test_df <- mydata42[mydata42$入组受试者类型1正常2IGR3DM==g,]
  #colnames(mydata4)
  test_df$level <- test_df$B超脂肪肝有无0无1有 == 1 #因变量设置，转换为布尔型
  glm_ploy1 <- glm(level ~ 地理分区1南方2北方, data = test_df,family=binomial(link='logit'))
  result1<-get_or_withci(glm_ploy1)
  print(result1)
  glm_ploy2 <- glm(level ~ 地理分区1南方2北方+性别1男2女+年龄+吸烟史二分类, data = test_df,family=binomial(link='logit'))
  result2<-get_or_withci(glm_ploy2)
  print(result2)
  glm_ploy3 <- glm(level ~ 地理分区1南方2北方+性别1男2女+年龄+吸烟史二分类
                   +BMI+腰围+ogtt血糖0+收缩压+胆固醇+甘油三酯+hdlc+ldlc+他汀类药物二分类, data = test_df,family=binomial(link='logit'))
  result3<-get_or_withci(glm_ploy3)
  print(result3)
  glm_ploy4 <- glm(level ~ 地理分区1南方2北方+性别1男2女+年龄+吸烟史二分类
                   +BMI+腰围+ogtt血糖0+收缩压+胆固醇+甘油三酯+hdlc+ldlc+他汀类药物二分类
                   +alt+ast+ggt, data = test_df,family=binomial(link='logit'))
  result4<-get_or_withci(glm_ploy4)
  print(result4)
  or_matrix_all<-rbind(result1,result2,result3,result4)
  print("ok")
  write.xlsx(or_matrix_all,"or2.xlsx",sheetName=paste("Sheet",(g+1),sep=""),append=TRUE)  #同时改sheetname和append true
}

########
mydata4_nodrink<-subset(mydata4,饮酒史二分类==0)
t1<-addmargins(table(mydata4_nodrink[,c("B超脂肪肝有无0无1有","地理分区1南方2北方")]))
t2<-addmargins(table(mydata4_nodrink[,c("B超脂肪肝有无0无1有","地理分区1南方2北方","入组受试者类型1正常2IGR3DM")]))


my_crosstab(mydata4_nodrink,"B超脂肪肝有无0无1有","地理分区1南方2北方")
my_crosstab(mydata4_nodrink[mydata4_nodrink$入组受试者类型1正常2IGR3DM==3,],"B超脂肪肝有无0无1有","地理分区1南方2北方")

colnames(mydata4_nodrink)
table_for_figure_nodrink<-subset(mydata4_nodrink,入组受试者类型1正常4IGR5NT2DM6KT2DM!=6&测过超声定量LFC==1)[,c(1,6,7,8,9,18,28,30,36,37,41,48,49,50,70,71,77)]
write.xlsx(table_for_figure_nodrink,"table_for_figure_nodrink.xlsx")

table_for_figure_withdrink<-subset(mydata4,入组受试者类型1正常4IGR5NT2DM6KT2DM!=6&测过超声定量LFC==1)[,c(1,6,7,8,9,18,28,30,36,37,41,48,49,50,70,71,77)]
write.xlsx(table_for_figure_withdrink,"table_for_figure_withdrink.xlsx")


#分组求pearson相关系数和p
lfc_north_statin<-subset(mydata4,入组受试者类型1正常4IGR5NT2DM6KT2DM!=6&测过超声定量LFC==1&地理分区1南方2北方==2)
cordata=lfc_north_statin[lfc_north_statin$他汀类药物二分类==0,]
cor.test(cordata$lgLFC,cordata$lgfbg,method = "pearson")$p.value
cor.test(cordata$lgLFC,cordata$lgfbg,method = "pearson")$estimate
cordata=lfc_north_statin[lfc_north_statin$他汀类药物二分类==1,]
cor.test(cordata$lgLFC,cordata$lgfbg,method = "pearson")$p.value
cor.test(cordata$lgLFC,cordata$lgfbg,method = "pearson")$estimate
#两组回归直线的比较
fit1 <- aov(lgLFC ~ lgfbg * 他汀类药物二分类, data = lfc_north_statin)
summary(fit1)   #看回归线斜率是否相等
summary(aov(lgLFC ~ lgfbg + 他汀类药物二分类, data = lfc_north_statin))  #协方差，看分组是否仍有差异

cordata=lfc_north_statin[lfc_north_statin$他汀类药物二分类==0,]
cor.test(cordata$lgLFC,cordata$lg2hpg,method = "pearson")$p.value
cor.test(cordata$lgLFC,cordata$lg2hpg,method = "pearson")$estimate
cordata=lfc_north_statin[lfc_north_statin$他汀类药物二分类==1,]
cor.test(cordata$lgLFC,cordata$lg2hpg,method = "pearson")$p.value
cor.test(cordata$lgLFC,cordata$lg2hpg,method = "pearson")$estimate
#两组回归直线的比较
fit1 <- aov(lgLFC ~ lg2hpg * 他汀类药物二分类, data = lfc_north_statin)
summary(fit1)   #看回归线斜率是否相等
summary(aov(lgLFC ~ lg2hpg + 他汀类药物二分类, data = lfc_north_statin))  #协方差，看分组是否仍有差异

cordata=lfc_north_statin[lfc_north_statin$他汀类药物二分类==0,]
cor.test(cordata$lgLFC,cordata$BMI,method = "pearson")$p.value
cor.test(cordata$lgLFC,cordata$BMI,method = "pearson")$estimate
cordata=lfc_north_statin[lfc_north_statin$他汀类药物二分类==1,]
cor.test(cordata$lgLFC,cordata$BMI,method = "pearson")$p.value
cor.test(cordata$lgLFC,cordata$BMI,method = "pearson")$estimate
#两组回归直线的比较
fit1 <- aov(lgLFC ~ BMI * 他汀类药物二分类, data = lfc_north_statin)
summary(fit1)   #看回归线斜率是否相等
summary(aov(lgLFC ~ BMI + 他汀类药物二分类, data = lfc_north_statin))  #协方差，看分组是否仍有差异

cordata=lfc_north_statin[lfc_north_statin$他汀类药物二分类==0,]
cor.test(cordata$lgLFC,cordata$腰围,method = "pearson")$p.value
cor.test(cordata$lgLFC,cordata$腰围,method = "pearson")$estimate
cordata=lfc_north_statin[lfc_north_statin$他汀类药物二分类==1,]
cor.test(cordata$lgLFC,cordata$腰围,method = "pearson")$p.value
cor.test(cordata$lgLFC,cordata$腰围,method = "pearson")$estimate
#两组回归直线的比较
fit1 <- aov(lgLFC ~ 腰围 * 他汀类药物二分类, data = lfc_north_statin)
summary(fit1)   #看回归线斜率是否相等
summary(aov(lgLFC ~ 腰围 + 他汀类药物二分类, data = lfc_north_statin))  #协方差，看分组是否仍有差异

############
#分组求pearson相关系数和p
lfc_nondrinker<-subset(mydata4_nodrink,入组受试者类型1正常4IGR5NT2DM6KT2DM!=6&测过超声定量LFC==1)
cordata=lfc_nondrinker[lfc_nondrinker$地理分区1南方2北方==1,]
cor.test(cordata$lgLFC,cordata$lgfbg,method = "pearson")$p.value
cor.test(cordata$lgLFC,cordata$lgfbg,method = "pearson")$estimate
cordata=lfc_nondrinker[lfc_nondrinker$地理分区1南方2北方==2,]
cor.test(cordata$lgLFC,cordata$lgfbg,method = "pearson")$p.value
cor.test(cordata$lgLFC,cordata$lgfbg,method = "pearson")$estimate
#两组回归直线的比较
fit1 <- aov(lgLFC ~ lgfbg * 地理分区1南方2北方, data = lfc_nondrinker)
summary(fit1)   #看回归线斜率是否相等
summary(aov(lgLFC ~ lgfbg + 地理分区1南方2北方, data = lfc_nondrinker))  #协方差，看分组是否仍有差异

cordata=lfc_nondrinker[lfc_nondrinker$地理分区1南方2北方==1,]
cor.test(cordata$lgLFC,cordata$lg2hpg,method = "pearson")$p.value
cor.test(cordata$lgLFC,cordata$lg2hpg,method = "pearson")$estimate
cordata=lfc_nondrinker[lfc_nondrinker$地理分区1南方2北方==2,]
cor.test(cordata$lgLFC,cordata$lg2hpg,method = "pearson")$p.value
cor.test(cordata$lgLFC,cordata$lg2hpg,method = "pearson")$estimate
#两组回归直线的比较
fit1 <- aov(lgLFC ~ lg2hpg * 地理分区1南方2北方, data = lfc_nondrinker)
summary(fit1)   #看回归线斜率是否相等
summary(aov(lgLFC ~ lg2hpg + 地理分区1南方2北方, data = lfc_nondrinker))  #协方差，看分组是否仍有差异

cordata=lfc_nondrinker[lfc_nondrinker$地理分区1南方2北方==1,]
cor.test(cordata$lgLFC,cordata$BMI,method = "pearson")$p.value
cor.test(cordata$lgLFC,cordata$BMI,method = "pearson")$estimate
cordata=lfc_nondrinker[lfc_nondrinker$地理分区1南方2北方==2,]
cor.test(cordata$lgLFC,cordata$BMI,method = "pearson")$p.value
cor.test(cordata$lgLFC,cordata$BMI,method = "pearson")$estimate

cordata=lfc_nondrinker[lfc_nondrinker$地理分区1南方2北方==1,]
cor.test(cordata$lgLFC,cordata$腰围,method = "pearson")$p.value
cor.test(cordata$lgLFC,cordata$腰围,method = "pearson")$estimate
cordata=lfc_nondrinker[lfc_nondrinker$地理分区1南方2北方==2,]
cor.test(cordata$lgLFC,cordata$腰围,method = "pearson")$p.value
cor.test(cordata$lgLFC,cordata$腰围,method = "pearson")$estimate

colnames(lfc_nondrinker)
lfc_nondrinker[,c(6,7,8,16,17)]
write.xlsx(lfc_nondrinker[,c(6,7,8,16,17)],"table_for_figure_withoutdrink.xlsx",sheetName="Sheet2",append=TRUE)

###############
## table 1
#total
write_meansd<-function(vname,n,dataname){ 
  i<-which(colnames(dataname)==vname)
  if (n==1){
    v<-c(vname,paste(round(mean(dataname[,i], na.rm = TRUE),2),"±",round(sd(dataname[,i], na.rm = TRUE),2),sep=""))
  }else{
    v<-c(vname,paste(round(quantile(dataname[,i], na.rm = TRUE)[3],2),"(",round(quantile(dataname[,i], na.rm = TRUE)[2],2),"-",round(quantile(dataname[,i], na.rm = TRUE)[4],2),")",sep=""))
  }  
  return(v)
}
write_meansd("年龄",1,mydata4_nodrink)

#### 大于2组的table1
write_v_table1_anova<-function(vname,groupname,n,groupcount,data){
  library(agricolae)
  i<-which(colnames(data)==vname)
  group<-data[,which(colnames(data)==groupname)]  #####新加 把分组变量填在这里
  model <- aov(data[,i] ~ group, data)
  p.anova<-summary(model)[[1]][1,5]  # anova p
  out <- LSD.test(model, "group", p.adj = "bonferroni" ) # multiple comparision
  #out$groups$groups    #group difference a b c ab
  v=c(vname)
  for (gn in c(1:groupcount)){
    if (n==1){
      v<-append(v,paste(round(out$means[gn,1],2),"±",round(out$means[gn,2],2)))
      
    }else{
      v<-append(v,paste(round(out$means[gn,9],2),"(",round(out$means[gn,8],2),"-",round(out$means[gn,10],2),")"))
    }  
    v<-append(v,paste(out$groups$groups[gn]))   #group difference a b c ab
  }
  v<-append(v,round(p.anova,3))    # anova p
  v<-append(v,write_meansd(vname,n,data))  #total count
  return(v)
}
# example
#write_v_table1("年龄","入组受试者类型1正常2IGR3DM",1,3,mydata4)  #vname, #groupname  #1=mean sd,2=(25~75)   #how many groups   #dataframe
# rbind(write_v_table1("bmi2",1,3,data3),write_v_table1("age",1,3,data3))

#### =2组的table1
write_v_table1_ttest<-function(vname,groupname,n,data){
  #library(agricolae)
  i<-which(colnames(data)==vname)
  j<-which(colnames(data)==groupname)
  p1<-t.test(data[,i]~data[,j])$p.value   #普通t检验p
  p2<-wilcox.test(data[,i]~data[,j])$p.value   #wilcox检验p
  glm_ploy0 <- glm(data[,i] ~ data[,j] +data[,19] +data[,21],gaussian(link = "identity"))  
  #连续变量用这个family，二分类变量用binomial(link = "logit"),要验证的分组写第一个。data[,19] +data[,21]为要校正的性别年龄
  p_adjust<-summary(glm_ploy0)$coefficients[2,4]  #校正了性别年龄的p

  v=c()
  for (gn in c(1:2)){
    if (n==1){
      v<-append(v,paste(round(tapply(data[,i],data[,j],mean,na.rm=TRUE)[gn],2),"±",round(tapply(data[,i],data[,j],sd,na.rm=TRUE)[gn],2),sep=""))
      #v<-append(v,round(p1,3)) 
      }else{
      q25<-tapply(data[,i],data[,j],quantile,na.rm=TRUE)[[gn]][2]
      q50<-tapply(data[,i],data[,j],quantile,na.rm=TRUE)[[gn]][3]
      q75<-tapply(data[,i],data[,j],quantile,na.rm=TRUE)[[gn]][4]
      v<-append(v,paste(round(q50,2),"(",round(q25,2),"-",round(q75,2),")",sep=""))
      #v<-append(v,round(p2,3)) 
    }  
  }
  
  if (n==1){
    v<-append(v,round(p1,3)) 
  }else{
    v<-append(v,round(p2,3)) 
  }  
     
  v<-append(v,round(p_adjust,3))    # adjust p
  v<-append(write_meansd(vname,n,data),v)  #total count
  return(v)
}
###example
write_v_table1_ttest("BMI","地理分区1南方2北方",1,mydata4)   #vname, #groupname  #1=mean sd,2=(25~75)    #dataframe
write_v_table1_ttest("new每周酒精量g","地理分区1南方2北方",2,subset(mydata4,饮酒史二分类==1))

write_v_table1_prop<-function(vname,groupname,x,data){    #n和prop 写table1  x为变量想取的值012所处顺序123
  t<-my_crosstab_plus(data,vname,groupname) 
  t2<-t[x,]  #x=2,取表格第二行，即变量=1时的百分比
  p<-my_crosstab_p(data,vname,groupname)
  return(c(vname,t2[length(t2)],t2[-length(t2)],p))
}
##example
#write_v_table1_prop("B超脂肪肝有无0无1有","地理分区1南方2北方",1,mydata4) 

#no drink, table1
table1n<-write_v_table1_prop("性别1男2女","地理分区1南方2北方",1,mydata4_nodrink) 
table1n<-rbind(table1n,write_v_table1_ttest("年龄","地理分区1南方2北方",1,mydata4_nodrink))
table1n<-rbind(table1n,write_v_table1_prop("吸烟史二分类","地理分区1南方2北方",2,mydata4_nodrink))
table1n<-rbind(table1n,write_v_table1_ttest("BMI","地理分区1南方2北方",1,mydata4_nodrink))
table1n<-rbind(table1n,write_v_table1_ttest("腰围","地理分区1南方2北方",1,mydata4_nodrink))
table1n<-rbind(table1n,write_v_table1_ttest("收缩压","地理分区1南方2北方",1,mydata4_nodrink))
table1n<-rbind(table1n,write_v_table1_ttest("舒张压","地理分区1南方2北方",1,mydata4_nodrink))
table1n<-rbind(table1n,write_v_table1_ttest("ogtt血糖0","地理分区1南方2北方",1,mydata4_nodrink))
table1n<-rbind(table1n,write_v_table1_ttest("ogtt血糖120","地理分区1南方2北方",1,mydata4_nodrink))
table1n<-rbind(table1n,write_v_table1_ttest("胆固醇","地理分区1南方2北方",1,mydata4_nodrink))
table1n<-rbind(table1n,write_v_table1_ttest("甘油三酯","地理分区1南方2北方",2,mydata4_nodrink))
table1n<-rbind(table1n,write_v_table1_ttest("hdlc","地理分区1南方2北方",1,mydata4_nodrink))
table1n<-rbind(table1n,write_v_table1_ttest("ldlc","地理分区1南方2北方",1,mydata4_nodrink))
table1n<-rbind(table1n,write_v_table1_prop("他汀类药物二分类","地理分区1南方2北方",2,mydata4_nodrink))
table1n<-rbind(table1n,write_v_table1_ttest("alt","地理分区1南方2北方",2,mydata4_nodrink))
table1n<-rbind(table1n,write_v_table1_ttest("ast","地理分区1南方2北方",2,mydata4_nodrink))
table1n<-rbind(table1n,write_v_table1_ttest("ggt","地理分区1南方2北方",2,mydata4_nodrink))
table1n<-rbind(table1n,write_v_table1_ttest("尿酸","地理分区1南方2北方",1,mydata4_nodrink))
table1n<-rbind(table1n,write_v_table1_prop("B超脂肪肝有无0无1有","地理分区1南方2北方",2,mydata4_nodrink))
table1n<-rbind(table1n,write_v_table1_ttest("肝脏脂肪含量","地理分区1南方2北方",2,mydata4_nodrink))
write.xlsx(table1n,"table1n.xlsx")

table_temp<-my_crosstab_plus(mydata4_nodrink,"入组受试者类型1正常2IGR3DM","地理分区1南方2北方") 
write.xlsx(as.matrix(table_temp),"temp.xlsx")

my_crosstab_p(mydata4_nodrink,"入组受试者类型1正常2IGR3DM","地理分区1南方2北方") 


write_v_table1_ttest("肝脏脂肪含量","地理分区1南方2北方",2,mydata4)

table1a<-write_v_table1_ttest("new每周酒精量g","地理分区1南方2北方",2,subset(mydata4,饮酒史二分类==1&new每周酒精量g<140))
table1a<-rbind(table1a,write_v_table1_ttest("new至今酒精总量g","地理分区1南方2北方",2,subset(mydata4,饮酒史二分类==1&new每周酒精量g<140)))

library(xlsx)
write.xlsx(table1a,"table1a.xlsx",sheetName = "sheet1")



#校正性别年龄的p计算测试
# test_df <- mydata4
# glm_ploy0 <- glm(alt ~ 地理分区1南方2北方+性别1男2女+年龄, data = test_df,gaussian(link = "identity"))  #连续变量用这个family，二分类变量用binomial(link = "logit"),要验证的分组写第一个。
# p_adjust<-summary(glm_ploy0)$coefficients[2,4]
# glm_ploy0 <- glm(mydata4[,42] ~ mydata4[,6] +mydata4[,19] +mydata4[,21], data = mydata4,gaussian(link = "identity"))  #连续变量用这个family，二分类变量用binomial(link = "logit"),要验证的分组写第一个。
# p_adjust<-summary(glm_ploy0)$coefficients[2,4]









#######################
library(ggplot2)
s<-qplot(lgfbg, lgLFC, data = mydata4_noanti, colour = 地理分区1南方2北方,xlim = c(0.25, 1.5), ylim = c(0, 2.5),mgp=c(3,2,1))

s2<-s+geom_point(size = 1.5)+ geom_smooth(method = "lm",na.rm = TRUE,se = FALSE,size=1.6)

s3<-s2+ theme_bw() + theme(panel.border = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.title=element_blank(),
                           axis.line = element_line(colour = "black",size=1, ),axis.text= element_text(size = 10),axis.title= element_text(size = 15),
                           legend.text = element_text(colour="black", size = 16, face = "bold"))
s4<-s3+xlab("City mpg")+ylab("LFC()")
s5<-s4+theme(legend.position=c(0.3,0.9))+scale_color_manual(values=c("1"="black","2"="red"),labels = c('XXXXXXXXXXXXXXXXXXXX','YXXXXXXXXXXXXXXXXX'))
s5
#s2+theme(legend.position="top")
#s2+ scale_fill_discrete(guide = FALSE)
