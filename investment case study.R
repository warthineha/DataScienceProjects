
#Investment Case Study


##Checkpoints - Part 1
## The code in this file uses packages tidyr and dplyr 

library(tidyr)
library(dplyr)

##Checkpoint 1: Data Cleaning 1

##getting comapanies.txt data into dataframe company

companies<-read.table("companies.txt",sep="\t",na.strings = NA,
                      fill = TRUE,header = TRUE,allowEscapes = TRUE,
                      comment.char = "",stringsAsFactors = FALSE,quote="\"")

##getting round2.csv data into dataframe rounds2

rounds2<-read.csv("rounds2.csv",header = TRUE,stringsAsFactors = FALSE,fill = TRUE)

#CODE FOR POPULATING TABLE 1.1

##Changing the case of permalink in both D.F's companies and rounds2 to lower case
##Because it will make it easier to find the unique values in both columns and matching
#these two columns would be easier to get a merged Data Frame

rounds2$company_permalink<-tolower(rounds2$company_permalink)

companies$permalink<-tolower(companies$permalink)

##unique companies  present in rounds2

unique_company_round2<-nrow(dplyr::distinct(rounds2,company_permalink))

unique_company_round2

##unique companies  present in companies

unique_company_companies<-nrow(dplyr::distinct(companies,permalink))

unique_company_companies

#the column in companies dataframe which uniquely reresents each company is permalink

anyDuplicated(companies$permalink)
#the above command gives a 0 value means there are no duplicates it uniquely represents 
#every row in companies dataframe .so,permalink is the column which uniquely represents each company

##Are there any companies in the rounds2 file which are not present in companies
  
sum(!(rounds2$company_permalink %in% companies$permalink)) #the above command gives a value of 0 so the answer is N
  
  
  
#merging companies and round2 to get master_frame dataframe

# changing the case of  by column values in both data frames

rounds2$company_permalink<-tolower(rounds2$company_permalink)

companies$permalink<-tolower(companies$permalink)

master_frame<-merge(rounds2,companies,by.x = "company_permalink",by.y = "permalink",all.x=TRUE)

#no of observations in master_frame dataframe are

no.of.observations<-nrow(master_frame)

no.of.observations


##Checkpoint 2: Funding Type Analysis


##CODE FOR POPULATING TABLE2.1

##filtering masterframe on funding round types seed,angel,venture,private equity

filter_fundround<-dplyr::filter(master_frame,(funding_round_type == "seed" 
                                                   | funding_round_type=="angel" 
                                                   | funding_round_type == "venture" 
                                                   | funding_round_type =="private_equity"))

#grouping filter_fundround_masterframe dataframe by funding_round_type

groupby_fundround<-dplyr::group_by(filter_fundround,funding_round_type)


##finding the avg raised amount for funding type seed,angel,venture,private equity

summarise_fundround<-dplyr::summarise(groupby_fundround,avg_funding=mean(raised_amount_usd,na.rm = TRUE))

summarise_fundround

# Spark Funds wants to invest between 5 to 15 million USD per investment round, 
#so the investment type  most suitable for sparks fund is 

summarise_fundround[summarise_fundround$avg_funding>=5000000 & summarise_fundround$avg_funding<=15000000,] #sparks fund should invest in venture type investment






#CHECKPOINTS:PART2

#CHECKPOINT3 : Country Analysis

#Filtering master_frame D.F on venture type investment
master_venture<-dplyr::filter(master_frame,funding_round_type=="venture")

#Grouping the master_venture D.F by country code
master_venture<-dplyr::group_by(master_venture,country_code)

#Summarising master_venture D.F to get total raised amount for every country
master_venture<-dplyr::summarise(master_venture,raised_amount_usd=sum(raised_amount_usd,na.rm=T))

#removing the rows with no country codes
master_venture<-dplyr::filter(master_venture,country_code != "")

# Arranging master_frame D.F in descending order 
master_venture<-dplyr::arrange(master_venture,desc(raised_amount_usd))

#Storing top9 countries best for venture investment in the dataframe top9
 top9<-master_venture[1:9,] #usa,GBR,ind are the top3 english speaking countries
 top9

##CHECKPOINT4:SECTOR ANALYSIS 1

#splitting the column category_list in master_frame data frame 
#and storing it in new column primary_sector
 
master_frame$primary_sector<-sapply(strsplit(master_frame$category_list,"\\|"), `[`, 1)


#getting mappig.csv data in to the mapping data frame

mapping<-read.csv("mapping.csv",stringsAsFactors = FALSE,header = TRUE)

#converting mapping data form wide to long format

mapping<-tidyr::gather(mapping,key="main_sector",value="values",
                       "Automotive...Sports":"Social..Finance..Analytics..Advertising")

#Filtering mapping D.F to remove rows which have zero values 
mapping<-dplyr::filter(mapping,mapping$values!=0)

#Removing the column values from mapping D.F
mapping$values<-NULL


#merging dataframes companies and mapping to map primary sector to the main sector

master_frame<-merge(master_frame,mapping,by.x="primary_sector" ,by.y = "category_list",all.x = TRUE)


#CHECKPOINT5:SECTOR ANALYSIS 2

#creating dataframes D1,D2 AND D3

#creating D1 for COUntry USA

D1<-dplyr::filter(master_frame,country_code=="USA" & funding_round_type=="venture" & (raised_amount_usd>=5000000 & raised_amount_usd<=15000000))

D1<-dplyr::group_by(D1,main_sector)

COUNT_D1<-dplyr::summarise(D1,total_amount_invested=sum(raised_amount_usd),Total_no_of_investments=n())

D1<-merge(D1,COUNT_D1,by.x = "main_sector",by.y = "main_sector",all.x=TRUE )

#creating D1 for COUntry GBR

D2<-dplyr::filter(master_frame,country_code=="GBR" & funding_round_type=="venture" & (raised_amount_usd>=5000000 & raised_amount_usd<=15000000))

D2<-dplyr::group_by(D2,main_sector)

COUNT_D2<-dplyr::summarise(D2,total_amount_invested=sum(raised_amount_usd),Total_no_of_investments=n())

D2<-merge(D2,COUNT_D2,by.x = "main_sector",by.y = "main_sector",all.x=TRUE )

#creating D1 for COUntry IND

D3<-dplyr::filter(master_frame,country_code=="IND" & funding_round_type=="venture" & (raised_amount_usd>=5000000 & raised_amount_usd<=15000000))

D3<-dplyr::group_by(D3,main_sector)

COUNT_D3<-dplyr::summarise(D3,total_amount_invested=sum(raised_amount_usd),Total_no_of_investments=n())

D3<-merge(D3,COUNT_D3,by.x = "main_sector",by.y = "main_sector",all.x=TRUE )


##CODE FOR POPULATING table5.1

##FOR TOP ENGLISH SPEAKING COUNTRY USA

TOTAL_NO_OF_INVESTMENTS.USA<-dplyr::summarise(D1,TOTAL_NO_OF_INVESTMENTS=n())
TOTAL_NO_OF_INVESTMENTS.USA

TOTAL_INVESTMENTS_USA<-dplyr::summarise(D1,TOTAL_INVESTMENTS_USA=sum(raised_amount_usd,na.rm = TRUE))
TOTAL_INVESTMENTS_USA

#Arranging COUNT_D1 data frame in descending order

COUNT_D1<-dplyr::arrange(COUNT_D1,desc(Total_no_of_investments))

TOP_SECTOR_USA<-COUNT_D1[1,"main_sector"]
TOP_SECTOR_USA

SECOND_TOP_SECTOR_USA<-COUNT_D1[2,"main_sector"]
SECOND_TOP_SECTOR_USA

THIRD_TOP_SECTOR_USA<-COUNT_D1[3,"main_sector"]
THIRD_TOP_SECTOR_USA

INVESTMENTS_IN_TOP.SECTOR.USA<-COUNT_D1[1,"Total_no_of_investments"]
INVESTMENTS_IN_TOP.SECTOR.USA

INVESTMENTS_IN_SECOND_TOP.SECTOR.USA<-COUNT_D1[2,"Total_no_of_investments"]
INVESTMENTS_IN_SECOND_TOP.SECTOR.USA

INVESTMENTS_IN_THIRD_TOP.SECTOR.USA<-COUNT_D1[3,"Total_no_of_investments"]
INVESTMENTS_IN_THIRD_TOP.SECTOR.USA

#the below code is to find the company in  the top sector, Others in USA,
#which received the highest investment

USA_Others<-dplyr::filter(D1,main_sector=="Others")
USA_Others<-dplyr::group_by(USA_Others,company_permalink)
USA_Others<-dplyr::summarise(USA_Others,Total_raised_amount_usd=sum(raised_amount_usd))
permalink<-USA_Others[which.max(USA_Others$Total_raised_amount_usd),]
Top.company_USA_Others<-(D1[D1$company_permalink==permalink$company_permalink,"name"])[1]
Top.company_USA_Others


#the below code is to find the company in  the second top sector, Cleantech / Semiconductors  in USA,
#which received the highest investment

USA_Semiconductors<-dplyr::filter(D1,main_sector=="Cleantech...Semiconductors")
USA_Semiconductors<-dplyr::group_by(USA_Semiconductors,company_permalink)
USA_Semiconductors<-dplyr::summarise(USA_Semiconductors,Total_raised_amount_usd=sum(raised_amount_usd))
permalink<-USA_Semiconductors[which.max(USA_Semiconductors$Total_raised_amount_usd),]
Top.company_USA_Semiconductors<-(D1[D1$company_permalink==permalink$company_permalink,"name"])[1]
Top.company_USA_Semiconductors

  


##FOR SECOND TOP ENGLISH SPEAKING COUNTRY GBR

TOTAL_NO_OF_INVESTMENTS.GBR<-dplyr::summarise(D2,TOTAL_NO_OF_INVESTMENTS=n())
TOTAL_NO_OF_INVESTMENTS.GBR

TOTAL_INVESTMENTS_GBR<-dplyr::summarise(D2,TOTAL_INVESTMENTS_GBR=sum(raised_amount_usd,na.rm = TRUE))
TOTAL_INVESTMENTS_GBR

#Arranging COUNT_D2 data frame in descending order

COUNT_D2<-dplyr::arrange(COUNT_D2,desc(Total_no_of_investments))

TOP_SECTOR_GBR<-COUNT_D2[1,"main_sector"]
TOP_SECTOR_GBR

SECOND_TOP_SECTOR_GBR<-COUNT_D2[2,"main_sector"]
SECOND_TOP_SECTOR_GBR

THIRD_TOP_SECTOR_GBR<-COUNT_D2[3,"main_sector"]
THIRD_TOP_SECTOR_GBR

INVESTMENTS_IN_TOP.SECTOR.GBR<-COUNT_D2[1,"Total_no_of_investments"]
INVESTMENTS_IN_TOP.SECTOR.GBR

INVESTMENTS_IN_SECOND_TOP.SECTOR.GBR<-COUNT_D2[2,"Total_no_of_investments"]
INVESTMENTS_IN_SECOND_TOP.SECTOR.GBR

INVESTMENTS_IN_THIRD_TOP.SECTOR.GBR<-COUNT_D2[3,"Total_no_of_investments"]
INVESTMENTS_IN_THIRD_TOP.SECTOR.GBR

#the below code is to find the company in  the top sector, Others in GBR,
#which received the highest investment

GBR_Others<-dplyr::filter(D2,main_sector=="Others")
GBR_Others<-dplyr::group_by(GBR_Others,company_permalink)
GBR_Others<-dplyr::summarise(GBR_Others,Total_raised_amount_usd=sum(raised_amount_usd))
permalink<-GBR_Others[which.max(GBR_Others$Total_raised_amount_usd),]

Top.company_GBR_Others<-(D2[D2$company_permalink==permalink$company_permalink,"name"])[1]
Top.company_GBR_Others



#the below code is to find the company in  the second top sector, Cleantech / Semiconductors  in GBR,
#which received the highest investment

GBR_Semiconductors<-dplyr::filter(D2,main_sector=="Cleantech...Semiconductors")
GBR_Semiconductors<-dplyr::group_by(GBR_Semiconductors,company_permalink)
GBR_Semiconductors<-dplyr::summarise(GBR_Semiconductors,Total_raised_amount_usd=sum(raised_amount_usd))
permalink<-GBR_Semiconductors[which.max(GBR_Semiconductors$Total_raised_amount_usd),]

Top.company_GBR_Semiconductors<-(D2[D2$company_permalink==permalink$company_permalink,"name"])[1]
Top.company_GBR_Semiconductors



#FOR THIRD TOP ENGLISH SPEAKING COUNTRY IND

TOTAL_NO_OF_INVESTMENTS.IND<-dplyr::summarise(D3,TOTAL_NO_OF_INVESTMENTS=n())
TOTAL_NO_OF_INVESTMENTS.IND

TOTAL_INVESTMENTS_IND<-dplyr::summarise(D3,TOTAL_INVESTMENTS_IND=sum(raised_amount_usd,na.rm = TRUE))
TOTAL_INVESTMENTS_IND

#Arranging COUNT_D3 data frame in descending order

COUNT_D3<-dplyr::arrange(COUNT_D3,desc(Total_no_of_investments))

TOP_SECTOR_IND<-COUNT_D3[1,"main_sector"]
TOP_SECTOR_IND

SECOND_TOP_SECTOR_IND<-COUNT_D3[2,"main_sector"]
SECOND_TOP_SECTOR_IND

THIRD_TOP_SECTOR_IND<-COUNT_D3[3,"main_sector"]
THIRD_TOP_SECTOR_IND

INVESTMENTS_IN_TOP.SECTOR.IND<-COUNT_D3[1,"Total_no_of_investments"]
INVESTMENTS_IN_TOP.SECTOR.IND

INVESTMENTS_IN_SECOND_TOP.SECTOR.IND<-COUNT_D3[2,"Total_no_of_investments"]
INVESTMENTS_IN_SECOND_TOP.SECTOR.IND

INVESTMENTS_IN_THIRD_TOP.SECTOR.IND<-COUNT_D3[3,"Total_no_of_investments"]
INVESTMENTS_IN_THIRD_TOP.SECTOR.IND

#the below code is to find the company in  the top sector, Others in IND,
#which received the highest investment

IND_Others<-dplyr::filter(D3,main_sector=="Others")
IND_Others<-dplyr::group_by(IND_Others,company_permalink)
IND_Others<-dplyr::summarise(IND_Others,Total_raised_amount_usd=sum(raised_amount_usd))
permalink<-IND_Others[which.max(IND_Others$Total_raised_amount_usd),]
Top.company_IND_Others<-(D3[D3$company_permalink==permalink$company_permalink,"name"])[1]
Top.company_IND_Others



#the below code is to find the company in  the second top sector, News,Search and Messaging country  in IND,
#which received the highest investment

IND_News<-dplyr::filter(D3,main_sector=="News..Search.and.Messaging")
IND_News<-dplyr::group_by(IND_News,company_permalink)
IND_News<-dplyr::summarise(IND_News,Total_raised_amount_usd=sum(raised_amount_usd))
permalink<-IND_News[which.max(IND_News$Total_raised_amount_usd),]
Top.company_IND_News<-(D3[D3$company_permalink==permalink$company_permalink,"name"])[1]
Top.company_IND_News






