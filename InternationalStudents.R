#載入package
library(readr)
library(dplyr)
library(ggplot2)
library(rgdal)
library(rgeos)
library(maptools)
library(RColorBrewer)
library(jsonlite)
library(plotly)

#讀取資料
#大專校院境外學生人數統計
overseasStudentCountry103<-read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_C.csv")
overseasStudentCountry104<-read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_C.csv")
overseasStudentCountry105<-read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_C.csv")
overseasStudentCountry106<-read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_C.csv")
overseasStudentSchool103<-read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_S.csv")
overseasStudentSchool104<-read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_S.csv")
overseasStudentSchool105<-read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_S.csv")
overseasStudentSchool106<-read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_S.csv")


#第一題_前
#欄位名稱處理
ColName<-colnames(overseasStudentCountry103)
ColName<-gsub("-","_",ColName)
colnames(overseasStudentCountry103)<-ColName
colnames(overseasStudentCountry104)<-ColName
#表格合併
overseasStudentCountry<-rbind(overseasStudentCountry103,
                              overseasStudentCountry104,
                              overseasStudentCountry105,
                              overseasStudentCountry106)

ToTWNCountry<-overseasStudentCountry%>%
  mutate(總數=學位生_正式修讀學位外國生+
              `學位生_僑生(含港澳)`+
              學位生_正式修讀學位陸生+
              非學位生_外國交換生+
              非學位生_外國短期研習及個人選讀+
              非學位生_大專附設華語文中心學生+
              非學位生_大陸研修生+
              非學位生_海青班+
              境外專班)%>%
  group_by(國別)%>%
  summarise(總人數=sum(總數))%>%
  arrange(desc(總人數))
head(ToTWNCountry)
#第一題_後
#欄位名稱處理
ColName<-colnames(overseasStudentSchool103)
ColName<-gsub("-","_",ColName)
colnames(overseasStudentSchool103)<-ColName
colnames(overseasStudentSchool104)<-ColName
#表格合併
overseasStudentSchool<-rbind(overseasStudentSchool103,
                             overseasStudentSchool104,
                             overseasStudentSchool105,
                             overseasStudentSchool106)

#資料清洗和字串轉數字
overseasStudentSchool$非學位生_大陸研修生<-gsub("…","NA",overseasStudentSchool$非學位生_大陸研修生)
overseasStudentSchool$非學位生_大陸研修生<-as.numeric(overseasStudentSchool$非學位生_大陸研修生)

ToTWNUniversity<-overseasStudentSchool%>%
  mutate(總數=rowSums(.[4:12],na.rm=T))%>%
  group_by(學校名稱)%>%
  summarise(總人數=sum(總數))%>%
  arrange(desc(總人數))%>%
  filter(總人數<90000)
#第二題
ToTWNCountry_20Row<-rbind(top_n(ToTWNCountry,19),
                          slice(ToTWNCountry,20:n())%>%
                            summarise(國別="其他",總人數=sum(總人數)))

ToTWNCountryBar<-ToTWNCountry_20Row%>%
  ggplot(aes(x=reorder(國別,-總人數),y=總人數))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.4))+
  labs(x="國別")
ToTWNCountryBar

#第三題
#讀取shapefile
worldMap<-readShapeSpatial("ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")
#shapefile轉為data.frame
worldMap.df<-fortify(worldMap)
worldMap.df$id<-as.numeric(worldMap.df$id)
#建立表格
mydata<-data.frame(Name=worldMap$NAME_LONG,ISO3=worldMap$ISO_A3,id=seq(0,length(worldMap$ISO_A3)-1))
#因子轉字串和補遺漏值
mydata$ISO3<-as.character(mydata$ISO3)
mydata$ISO3[56]<-"FRA"
mydata$ISO3[119]<-"NOR"
worldMap.df<-left_join(worldMap.df,mydata,by="id")
#讀取國家中英對照表
countryName<-fromJSON("countries.json")
#國家中英對照表處理
index<-c(2,13,17,40,48,55,73,98,119,120,122,143,153,160,166,177,191,195,199,204,207,209,227,228,229,238)
Name<-c("阿拉伯聯合大公國","澳大利亞","波士尼亞與赫塞哥維納","剛果民主共和國","中國大陸","賽普勒斯","密克羅尼西亞",
        "克羅埃西亞","葛摩聯盟","聖克里斯多福","南韓","馬紹爾群島共和國","馬爾他","納米比亞","納戈爾諾-卡拉巴赫",
        "巴布亞紐幾內亞","塞爾維亞共和國","索羅門群島","新加坡","獅子山共和國","索馬利亞民主共和國","南蘇丹共和國",
        "千里達","吐瓦魯","臺灣","聖文森")
countryName$Taiwan[index]<-Name
#表格合併
worldMap.df<-left_join(worldMap.df,countryName,by="ISO3")
worldMap.df<-worldMap.df%>%
  select(long:ISO3,Taiwan)
colnames(worldMap.df)[10]<-"國別" 
final.data<-left_join(worldMap.df,ToTWNCountry,by="國別")

ToTWNCountryMap<-ggplot()+
  geom_polygon(data=final.data,aes(x=long,y=lat,group=group,fill=總人數),color="black",size=0.25)+
  coord_quickmap()+
  scale_fill_gradientn(colours=brewer.pal(7,"OrRd"))+
  theme_void()
ToTWNCountryMap

#第四題_前
#讀取資料
#大專校院本國學生出國進修交流數
localStudent<-read_csv("Student_RPT.csv",skip=2)
#欄位名稱處理
ColName<-c("學年度","學期","學校設立別","學校類別","學校代碼","學校名稱","系所代碼","系所名稱","學制","對方學校國別",
          "對方學校中文名稱","對方學校英文名稱","本國學生出國進修、交流人數小計","本國學生出國進修、交流人數_男",
          "本國學生出國進修、交流人數_女")
colnames(localStudent)<-ColName
#刪除統計說明
localStudent<-localStudent[1:35020,]
#資料整理
mydata<-localStudent%>%
  filter(學年度>=103)%>%
  group_by(對方學校國別)%>%
  summarise(人數=sum(`本國學生出國進修、交流人數小計`))%>%
  arrange(對方學校國別)
#土耳其、土耳其共和國
mydata<-rbind(mydata[grepl("土耳其",mydata$對方學校國別),]%>%
                summarise(對方學校國別="土耳其",人數=sum(人數)),
              mydata[!grepl("土耳其",mydata$對方學校國別),])
#大陸地區、中國大陸
mydata<-rbind(mydata[grepl("大陸",mydata$對方學校國別),]%>%
                summarise(對方學校國別="中國大陸",人數=sum(人數)),
              mydata[!grepl("大陸",mydata$對方學校國別),])
#丹麥、丹麥王國
mydata<-rbind(mydata[grepl("丹麥",mydata$對方學校國別),]%>%
                summarise(對方學校國別="丹麥",人數=sum(人數)),
              mydata[!grepl("丹麥",mydata$對方學校國別),])
#巴拿馬、巴拿馬共和國
mydata<-rbind(mydata[grepl("巴拿馬",mydata$對方學校國別),]%>%
                summarise(對方學校國別="巴拿馬",人數=sum(人數)),
              mydata[!grepl("巴拿馬",mydata$對方學校國別),])
#比利時，比利時王國
mydata<-rbind(mydata[grepl("比利時",mydata$對方學校國別),]%>%
                summarise(對方學校國別="比利時",人數=sum(人數)),
              mydata[!grepl("比利時",mydata$對方學校國別),])
#立陶宛、立陶宛共和國
mydata<-rbind(mydata[grepl("立陶宛",mydata$對方學校國別),]%>%
                summarise(對方學校國別="立陶宛",人數=sum(人數)),
              mydata[!grepl("立陶宛",mydata$對方學校國別),])
#印尼、印度尼西亞共和國
mydata<-rbind(mydata[grepl("印尼|印度尼西亞",mydata$對方學校國別),]%>%
                summarise(對方學校國別="印尼",人數=sum(人數)),
              mydata[!grepl("印尼|印度尼西亞",mydata$對方學校國別),])
#印度、印度共和國
mydata<-rbind(mydata[grepl("印度",mydata$對方學校國別),]%>%
                summarise(對方學校國別="印度",人數=sum(人數)),
              mydata[!grepl("印度",mydata$對方學校國別),])
#西班牙、西班牙共和國
mydata<-rbind(mydata[grepl("西班牙",mydata$對方學校國別),]%>%
                summarise(對方學校國別="西班牙",人數=sum(人數)),
              mydata[!grepl("西班牙",mydata$對方學校國別),])
#希臘、希臘共和國
mydata<-rbind(mydata[grepl("希臘",mydata$對方學校國別),]%>%
                summarise(對方學校國別="希臘",人數=sum(人數)),
              mydata[!grepl("希臘",mydata$對方學校國別),])
#汶萊、汶萊和平之國
mydata<-rbind(mydata[grepl("汶萊",mydata$對方學校國別),]%>%
                summarise(對方學校國別="汶萊",人數=sum(人數)),
              mydata[!grepl("汶萊",mydata$對方學校國別),])
#拉脫維亞、拉脫維亞共和國
mydata<-rbind(mydata[grepl("拉脫維亞",mydata$對方學校國別),]%>%
                summarise(對方學校國別="拉脫維亞",人數=sum(人數)),
              mydata[!grepl("拉脫維亞",mydata$對方學校國別),])
#波蘭、波瀾共和國
mydata<-rbind(mydata[grepl("波蘭",mydata$對方學校國別),]%>%
                summarise(對方學校國別="波蘭",人數=sum(人數)),
              mydata[!grepl("波蘭",mydata$對方學校國別),])
#芬蘭、芬蘭共和國
mydata<-rbind(mydata[grepl("芬蘭",mydata$對方學校國別),]%>%
                summarise(對方學校國別="芬蘭",人數=sum(人數)),
              mydata[!grepl("芬蘭",mydata$對方學校國別),])
#俄羅斯、俄羅斯聯邦
mydata<-rbind(mydata[grepl("俄羅斯",mydata$對方學校國別),]%>%
                summarise(對方學校國別="俄羅斯",人數=sum(人數)-1),
              mydata[!grepl("俄羅斯",mydata$對方學校國別),],
              c("白俄羅斯共和國",1))
mydata$人數<-as.numeric(mydata$人數)
#南非、南非共和國
mydata<-rbind(mydata[grepl("南非",mydata$對方學校國別),]%>%
                summarise(對方學校國別="南非",人數=sum(人數)),
              mydata[!grepl("南非",mydata$對方學校國別),])
#柬埔寨、柬埔寨王國
mydata<-rbind(mydata[grepl("柬埔寨",mydata$對方學校國別),]%>%
                summarise(對方學校國別="柬埔寨",人數=sum(人數)),
              mydata[!grepl("柬埔寨",mydata$對方學校國別),])
#挪威、挪威王國
mydata<-rbind(mydata[grepl("挪威",mydata$對方學校國別),]%>%
                summarise(對方學校國別="挪威",人數=sum(人數)),
              mydata[!grepl("挪威",mydata$對方學校國別),])
#泰王國、泰國
mydata<-rbind(mydata[grepl("泰國",mydata$對方學校國別),]%>%
                summarise(對方學校國別="泰國",人數=sum(人數)),
              mydata[!grepl("泰國",mydata$對方學校國別),])
#捷克、捷克共和國
mydata<-rbind(mydata[grepl("捷克",mydata$對方學校國別),]%>%
                summarise(對方學校國別="捷克",人數=sum(人數)),
              mydata[!grepl("捷克",mydata$對方學校國別),])
#荷蘭、荷蘭王國
mydata<-rbind(mydata[grepl("荷蘭",mydata$對方學校國別),]%>%
                summarise(對方學校國別="荷蘭",人數=sum(人數)),
              mydata[!grepl("荷蘭",mydata$對方學校國別),])
#斯洛維尼亞、斯洛維尼亞共和國
mydata<-rbind(mydata[grepl("斯洛維尼亞",mydata$對方學校國別),]%>%
                summarise(對方學校國別="斯洛維尼亞",人數=sum(人數)),
              mydata[!grepl("斯洛維尼亞",mydata$對方學校國別),])
#菲律賓、菲律賓共和國
mydata<-rbind(mydata[grepl("菲律賓",mydata$對方學校國別),]%>%
                summarise(對方學校國別="菲律賓",人數=sum(人數)),
              mydata[!grepl("菲律賓",mydata$對方學校國別),])
#越南、越南社會主義共和國
mydata<-rbind(mydata[grepl("越南",mydata$對方學校國別),]%>%
                summarise(對方學校國別="越南",人數=sum(人數)),
              mydata[!grepl("越南",mydata$對方學校國別),])
#奧地利、奧地利共和國
mydata<-rbind(mydata[grepl("奧地利",mydata$對方學校國別),]%>%
                summarise(對方學校國別="奧地利",人數=sum(人數)),
              mydata[!grepl("奧地利",mydata$對方學校國別),])
#愛沙尼亞、愛沙尼亞共和國
mydata<-rbind(mydata[grepl("愛沙尼亞",mydata$對方學校國別),]%>%
                summarise(對方學校國別="愛沙尼亞",人數=sum(人數)),
              mydata[!grepl("愛沙尼亞",mydata$對方學校國別),])
#愛爾蘭、愛爾蘭共和國
mydata<-rbind(mydata[grepl("愛爾蘭",mydata$對方學校國別),]%>%
                summarise(對方學校國別="愛爾蘭",人數=sum(人數)),
              mydata[!grepl("愛爾蘭",mydata$對方學校國別),])
#新加坡、新加坡共和國
mydata<-rbind(mydata[grepl("新加坡",mydata$對方學校國別),]%>%
                summarise(對方學校國別="新加坡",人數=sum(人數)),
              mydata[!grepl("新加坡",mydata$對方學校國別),])
#瑞典、瑞典王國
mydata<-rbind(mydata[grepl("瑞典",mydata$對方學校國別),]%>%
                summarise(對方學校國別="瑞典",人數=sum(人數)),
              mydata[!grepl("瑞典",mydata$對方學校國別),])
#義大利、義大利共和國
mydata<-rbind(mydata[grepl("義大利",mydata$對方學校國別),]%>%
                summarise(對方學校國別="義大利",人數=sum(人數)),
              mydata[!grepl("義大利",mydata$對方學校國別),])
#葡萄牙、葡萄牙共和國
mydata<-rbind(mydata[grepl("葡萄牙",mydata$對方學校國別),]%>%
                summarise(對方學校國別="葡萄牙",人數=sum(人數)),
              mydata[!grepl("葡萄牙",mydata$對方學校國別),])
#蒙古、蒙古國
mydata<-rbind(mydata[grepl("蒙古",mydata$對方學校國別),]%>%
                summarise(對方學校國別="蒙古",人數=sum(人數)),
              mydata[!grepl("蒙古",mydata$對方學校國別),])
#德國、德意志聯邦共和國
mydata<-rbind(mydata[grepl("德國|德意志",mydata$對方學校國別),]%>%
                summarise(對方學校國別="德國",人數=sum(人數)),
              mydata[!grepl("德國|德意志",mydata$對方學校國別),])
#大韓民國(南韓)、南韓
mydata<-rbind(mydata[grepl("南韓",mydata$對方學校國別),]%>%
                summarise(對方學校國別="南韓",人數=sum(人數)),
              mydata[!grepl("南韓",mydata$對方學校國別),])

FromTWNCountry<-mydata%>%
  arrange(desc(人數))
colnames(FromTWNCountry)[1]<-"國別"

head(FromTWNCountry,10)

#第四題_後
FromTWNUniversity<-localStudent%>%
  filter(學年度>=103)%>%
  group_by(學校名稱)%>%
  summarise(人數=sum(`本國學生出國進修、交流人數小計`))%>%
  arrange(desc(人數))
head(FromTWNUniversity,10)

#第五題
FromTWNCountry_20Row<-rbind(top_n(FromTWNCountry,19),
                               slice(FromTWNCountry,20:n())%>%
                                 summarise(國別="其他",人數=sum(人數)))
FromTWNCountryBar<-FromTWNCountry_20Row%>%
  ggplot(aes(x=reorder(國別,-人數),y=人數))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.4))+
  labs(x="國別")

FromTWNCountryBar

#第六題
#讀取shapefile
worldMap<-readShapeSpatial("ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")

#shapefile轉為data.frame
worldMap.df<-fortify(worldMap)

#字串轉數字
worldMap.df$id<-as.numeric(worldMap.df$id)

#建立表格，包含地區名稱、地區ISO3碼、地區id
mydata<-data.frame(Name=worldMap$NAME_LONG,ISO3=worldMap$ISO_A3,id=seq(0,length(worldMap$ISO_A3)-1))

#因子轉字串和補遺漏值
mydata$ISO3<-as.character(mydata$ISO3)
mydata$ISO3[56]<-"FRA"
mydata$ISO3[119]<-"NOR"

#地圖資料合併表格(以id為依據，新增地圖名稱欄位、地區ISO3碼欄位)
worldMap.df<-left_join(worldMap.df,mydata,by="id")

#讀取國家中英對照表
countryName<-fromJSON("countries.json")

#國家中英對照表處理(對照表和開放資料的中文地區名稱不一致，以開放資料的地區名稱為依據，修改對照表的地區名稱)
index<-c(3,13,36,48,98,122,154,191,195,199)
Name<-c("阿富汗伊斯蘭國","澳大利亞","白俄羅斯共和國","中國大陸","克羅埃西亞","南韓","模里西斯共和國",
        "塞爾維亞共和國","所羅門群島","新加坡")
countryName$Taiwan[index]<-Name

#地區資料合併國家中英對照表(以ISO3碼為依據，主要目的為新增中文地區名稱欄位)
worldMap.df<-left_join(worldMap.df,countryName,by="ISO3")

#地區資料選取會用到的欄位
worldMap.df<-worldMap.df%>%
  select(long:ISO3,Taiwan)

#欄位名稱處理
colnames(worldMap.df)[10]<-"對方學校國別" 

#地區資料合併開放資料為最終資料(以國別為依據，新增總人數欄位)
final.data<-left_join(worldMap.df,FromTWNCountry,by="國別")

FromTWNCountryMap<-ggplot()+
  geom_polygon(data=final.data,aes(x=long,y=lat,group=group,fill=人數),color="black",size=0.25)+
  coord_quickmap()+
  scale_fill_gradientn(colours=brewer.pal(7,"Blues"))+
  theme_void()

#顯示結果，灰色區域為無資料
FromTWNCountryMap
ggplotly(FromTWNCountryMap)

#第七題
#世界各主要國家之我國留學生人數統計表
country<-read_csv("https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv")
#資料處理
country<-country[,1:3]

FromTWNAb<-country%>%
  select("國別","總人數")%>%
  arrange(desc(總人數))
head(FromTWNAb,10)

#第八題
#讀取shapefile
worldMap<-readShapeSpatial("ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")

#shapefile轉為data.frame
worldMap.df<-fortify(worldMap)

#字串轉數字
worldMap.df$id<-as.numeric(worldMap.df$id)

#建立表格，包含地區名稱、地區ISO3碼、地區id
mydata<-data.frame(Name=worldMap$NAME_LONG,ISO3=worldMap$ISO_A3,id=seq(0,length(worldMap$ISO_A3)-1))

#因子轉字串和補遺漏值
mydata$ISO3<-as.character(mydata$ISO3)
mydata$ISO3[56]<-"FRA"
mydata$ISO3[119]<-"NOR"

#地圖資料合併表格(以id為依據，新增地圖名稱欄位、地區ISO3碼欄位)
worldMap.df<-left_join(worldMap.df,mydata,by="id")

#讀取國家中英對照表
countryName<-fromJSON("countries.json")

#國家中英對照表處理(對照表和開放資料的中文地區名稱不一致，以開放資料的地區名稱為依據，修改對照表的地區名稱)
index<-c(13,122,199)
Name<-c("澳大利亞","南韓","新加坡")
countryName$Taiwan[index]<-Name

#地區資料合併國家中英對照表(以ISO3碼為依據，主要目的為新增中文地區名稱欄位)
worldMap.df<-left_join(worldMap.df,countryName,by="ISO3")

#地區資料選取會用到的欄位
worldMap.df<-worldMap.df%>%
  select(long:ISO3,Taiwan)

#欄位名稱處理
colnames(worldMap.df)[10]<-"國別" 

#地區資料合併開放資料為最終資料(以國別為依據，新增總人數欄位)
final.data<-left_join(worldMap.df,FromTWNAb,by="國別")

FromTWNAbMap<-ggplot()+
  geom_polygon(data=final.data,aes(x=long,y=lat,group=group,fill=總人數),color="black",size=0.25)+
  coord_quickmap()+
  scale_fill_gradientn(colours=brewer.pal(7,"Blues"))+
  theme_void()

#顯示結果，灰色區域為無資料
FromTWNAbMap

#第九題
#複製表格
ToTW<-ToTWNCountry
FromTW<-FromTWNCountry

#欄位名稱處理
ColName<-c("國別","人數")
colnames(ToTW)<-ColName
colnames(FromTW)<-ColName

#表格合併，取來台的前20名國家和離台的前20名國家
mydata<-rbind(top_n(ToTW,20),top_n(FromTW,20))

#表格新增分類
mydata$類別[1:20]<-"來台"
mydata$類別[21:40]<-"離台"

Analysis<-mydata%>%
  ggplot(aes(x=國別,y=人數,fill=類別))+
  geom_bar(stat="identity",width=0.7)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.4))

#顯示圖表
Analysis

#國別名稱修改
ToTW$國別[88]<-"白俄羅斯共和國"
ToTW$國別[155]<-"馬爾他共和國"
ToTW$國別[117]<-"模里西斯共和國"

#境外學生來台國家數
nrow(ToTW)

#台灣學生離台國家數
nrow(FromTW)

#國家總數
mydata<-full_join(ToTW,FromTW,by="國別")
nrow(mydata)

#有境外學生來台又有台灣學生前往國家數
mydata<-inner_join(ToTW,FromTW,by="國別")
nrow(mydata)

colnames(mydata)<-c("國別","來台人數","離台人數")

Analysis.FromTo<-mydata%>%
  mutate(離台來台人數比=離台人數/來台人數)%>%
  arrange(desc(離台來台人數比))

#顯示表格
head(Analysis.FromTo,10)

Analysis.ToFrom<-mydata%>%
  mutate(來台離台人數比=來台人數/離台人數)%>%
  arrange(desc(來台離台人數比))

#顯示表格
head(Analysis.ToFrom,10)