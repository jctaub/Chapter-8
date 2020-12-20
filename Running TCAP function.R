#running the TCAP function

#the original daaset is being called jen since it had originally been called 
# data which confuses the R code. All the other datasets have been named after 
# their respective teams, with the exception of the Pistner et al dataset which in
# the code was originally called snoke2, since snoke was on both teams

# for NA values they were replaced with B for blank
jen$employ[is.na(jen$employ)]<- 'B'
raab$employ[is.na(raab$employ)]<- 'B'
snoke1$employ[is.na(snoke1$employ)]<- 'B'
snoke2$employ[is.na(snoke2$employ)]<- 'B'
charest$employ[is.na(charest$employ)]<- 'B'
chen1$employ[is.na(chen1$employ)]<- 'B'
chen2$employ[is.na(chen2$employ)]<- 'B'
chen3$employ[is.na(chen3$employ)]<- 'B'
chen4$employ[is.na(chen4$employ)]<- 'B'

jen$occlab1[is.na(jen$occlab1)]<-'B'
raab$occlab1[is.na(raab$occlab1)]<-'B'
snoke1$occlab1[is.na(snoke1$occlab1)]<-'B'
snoke2$occlab1[is.na(snoke2$occlab1)]<-'B'
charest$occlab1[is.na(charest$occlab1)]<-'B'
chen1$occlab1[is.na(chen1$occlab1)]<-'B'
chen2$occlab1[is.na(chen2$occlab1)]<-'B'
chen3$occlab1[is.na(chen3$occlab1)]<-'B'
chen4$occlab1[is.na(chen4$occlab1)]<-'B'

#the original daaset and 2 of the Chen datasets had a divorced record for a
# mairal status, given the rarity of it it was recoded as 'not known'
jen$mar_stat[jen$mar_stat=='Divorced']<-'Not known'
chen2$mar_stat[chen2$mar_stat=='Divorced']<-'Not known'
chen3$mar_stat[chen3$mar_stat=='Divorced']<-'Not known'

#The charest dataset had th occlab1 variable coded numerically with a codebook
# while the others had the categorical written out. Here the Charest had the 
#occlab1 variables changed to match the original and other synthetic datasets
charest$occlab1[charest$occlab1==2]<-'PERSONS ENGAGED IN AGRICULTURE'
charest$occlab1[charest$occlab1==3]<-'PERSONS ENGAGED IN BUILDING, AND WORKS OF CONSTRUCTION'
charest$occlab1[charest$occlab1==4]<-'PERSONS ENGAGED IN COMMERICAL OCCUPATIONS'
charest$occlab1[charest$occlab1==5]<-'PERSONS ENGAGED IN CONVEYANCE OF MEN, GOODS, AND MESSAGES'
charest$occlab1[charest$occlab1==6]<-'PERSONS ENGAGED IN DOMESTIC OFFICES OR SERVICES'
charest$occlab1[charest$occlab1==7]<-'PERSONS ENGAGED IN FISHING'
charest$occlab1[charest$occlab1==8]<-'PERSONS ENGAGED IN GAS WATER AND ELECTRICITY SUPPLY AND SANITARY SERVICE'
charest$occlab1[charest$occlab1==9]<-'PERSONS ENGAGED IN OR IN CONNECTION WITH THE GENERAL LOCAL GOVERNMENT OF THE COUNTRY '
charest$occlab1[charest$occlab1==10]<-'PERSONS ENGAGED IN PROFESSIONAL OCCUPATIONS AND THEIR SUBORDINATE SERVICES'
charest$occlab1[charest$occlab1==11]<-'PERSONS ENGAGED IN THE DEFENCE OF THE COUNTRY'
charest$occlab1[charest$occlab1==12]<-'PERSONS WITHOUT SPECIFIED OCCUPATIONS OR UNOCCUPIED'
charest$occlab1[charest$occlab1==13]<-'PERSONS WORKING AND DEALING IN BRICK, CEMENT, POTTERY, AND GLASS'
charest$occlab1[charest$occlab1==14]<-'PERSONS WORKING AND DEALING IN CHEMICALS, OIL, GREASE, SOAP, RESIN, ETC'
charest$occlab1[charest$occlab1==15]<-'PERSONS WORKING AND DEALING IN DRESS'
charest$occlab1[charest$occlab1==16]<-'PERSONS WORKING AND DEALING IN FOOD, TOBACCO, DRINK, AND LODGING'
charest$occlab1[charest$occlab1==17]<-'PERSONS WORKING AND DEALING IN METALS, MACHINE IMPLEMENTS, AND CONVEYANCES'
charest$occlab1[charest$occlab1==18]<-'PERSONS WORKING AND DEALING IN PAPER, PRINTS, BOOKS, AND STATIONARY'
charest$occlab1[charest$occlab1==19]<-'PERSONS WORKING AND DEALING IN PRECIOUS METALS, JEWELS, WATCHES, INSTRUMENTS, AND GAMES'
charest$occlab1[charest$occlab1==20]<-'PERSONS WORKING AND DEALING IN SKINS, LEATHER, HAIR, AND FEATHERS'
charest$occlab1[charest$occlab1==21]<-'PERSONS WORKING AND DEALING IN TEXTILE FABRICS'
charest$occlab1[charest$occlab1==22]<-'PERSONS WORKING AND DEALING IN WOOD, FURNITURE FITTINGS, AND DECORATIONS'
charest$occlab1[charest$occlab1==23]<-'WORKING IN AND ABOUT, AND WORKING AND DEALING IN THE PRODUCTS OF, MINES AND QUARRIES'

#########################################################################
#running the TCAP function

#Target variable is employ

RCAP6(jen, raab, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'employ')
RCAP6(jen, snoke1, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'employ')
RCAP6(jen, snoke2, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'employ')
RCAP6(jen, charest, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'employ')
RCAP6(jen, chen1, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'employ')
RCAP6(jen, chen2, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'employ')
RCAP6(jen, chen3, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'employ')
RCAP6(jen, chen4, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'employ')


RCAP5(jen, raab, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'employ')
RCAP5(jen, snoke1, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'employ')
RCAP5(jen, snoke2, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'employ')
RCAP5(jen, charest, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'employ')
RCAP5(jen, chen1, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'employ')
RCAP5(jen, chen2, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'employ')
RCAP5(jen, chen3, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'employ')
RCAP5(jen, chen4, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'employ')

RCAP4(jen, raab, 'sex', 'agegroup', 'mar_stat','parish', 'employ')
RCAP4(jen, snoke1, 'sex', 'agegroup', 'mar_stat','parish', 'employ')
RCAP4(jen, snoke2, 'sex', 'agegroup', 'mar_stat','parish', 'employ')
RCAP4(jen, charest, 'sex', 'agegroup', 'mar_stat','parish', 'employ')
RCAP4(jen, chen1, 'sex', 'agegroup', 'mar_stat','parish', 'employ')
RCAP4(jen, chen2, 'sex', 'agegroup', 'mar_stat','parish', 'employ')
RCAP4(jen, chen3, 'sex', 'agegroup', 'mar_stat','parish', 'employ')
RCAP4(jen, chen4, 'sex', 'agegroup', 'mar_stat','parish', 'employ')

RCAP3(jen, raab, 'sex', 'parish', 'mar_stat', 'employ')
RCAP3(jen, snoke1, 'sex',  'mar_stat','parish', 'employ')
RCAP3(jen, snoke2, 'sex', 'mar_stat','parish', 'employ')
RCAP3(jen, charest, 'sex', 'parish', 'mar_stat', 'employ')
RCAP3(jen, chen1, 'sex',  'mar_stat','parish', 'employ')
RCAP3(jen, chen2, 'sex', 'mar_stat','parish', 'employ')
RCAP3(jen, chen3, 'sex', 'parish', 'mar_stat', 'employ')
RCAP3(jen, chen4, 'sex',  'mar_stat','parish', 'employ')

######################################################################
#Target variable is occlab1

RCAP6(jen, raab, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'occlab1')
RCAP6(jen, snoke1, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'occlab1')
RCAP6(jen, snoke2, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'occlab1')
RCAP6(jen, charest, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'occlab1')
RCAP6(jen, chen1, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'occlab1')
RCAP6(jen, chen2, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'occlab1')
RCAP6(jen, chen3, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'occlab1')
RCAP6(jen, chen4, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'occlab1')


RCAP5(jen, raab, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'occlab1')
RCAP5(jen, snoke1, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'occlab1')
RCAP5(jen, snoke2, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'occlab1')
RCAP5(jen, charest, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'occlab1')
RCAP5(jen, chen1, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'occlab1')
RCAP5(jen, chen2, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'occlab1')
RCAP5(jen, chen3, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'occlab1')
RCAP5(jen, chen4, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'occlab1')

RCAP4(jen, raab, 'sex', 'agegroup', 'mar_stat','parish', 'occlab1')
RCAP4(jen, snoke1, 'sex', 'agegroup', 'mar_stat','parish', 'occlab1')
RCAP4(jen, snoke2, 'sex', 'agegroup', 'mar_stat','parish', 'occlab1')
RCAP4(jen, charest, 'sex', 'agegroup', 'mar_stat','parish', 'occlab1')
RCAP4(jen, chen1, 'sex', 'agegroup', 'mar_stat','parish', 'occlab1')
RCAP4(jen, chen2, 'sex', 'agegroup', 'mar_stat','parish', 'occlab1')
RCAP4(jen, chen3, 'sex', 'agegroup', 'mar_stat','parish', 'occlab1')
RCAP4(jen, chen4, 'sex', 'agegroup', 'mar_stat','parish', 'occlab1')

RCAP3(jen, raab, 'sex', 'parish', 'mar_stat', 'occlab1')
RCAP3(jen, snoke1, 'sex',  'mar_stat','parish', 'occlab1')
RCAP3(jen, snoke2, 'sex', 'mar_stat','parish', 'occlab1')
RCAP3(jen, charest, 'sex', 'parish', 'mar_stat', 'occlab1')
RCAP3(jen, chen1, 'sex',  'mar_stat','parish', 'occlab1')
RCAP3(jen, chen2, 'sex', 'mar_stat','parish', 'occlab1')
RCAP3(jen, chen3, 'sex', 'parish', 'mar_stat', 'occlab1')
RCAP3(jen, chen4, 'sex',  'mar_stat','parish', 'occlab1')

############################################################################
#Target variable is hsize

RCAP6(jen, raab, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'hsize')
RCAP6(jen, snoke1, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'hsize')
RCAP6(jen, snoke2, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'hsize')
RCAP6(jen, charest, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'hsize')
RCAP6(jen, chen1, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'hsize')
RCAP6(jen, chen2, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'hsize')
RCAP6(jen, chen3, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'hsize')
RCAP6(jen, chen4, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'hsize')

RCAP5(jen, raab, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'hsize')
RCAP5(jen, snoke1, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'hsize')
RCAP5(jen, snoke2, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'hsize')
RCAP5(jen, charest, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'hsize')
RCAP5(jen, chen1, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'hsize')
RCAP5(jen, chen2, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'hsize')
RCAP5(jen, chen3, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'hsize')
RCAP5(jen, chen4, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'hsize')

RCAP4(jen, raab, 'sex', 'agegroup', 'mar_stat','parish', 'hsize')
RCAP4(jen, snoke1, 'sex', 'agegroup', 'mar_stat','parish', 'hsize')
RCAP4(jen, snoke2, 'sex', 'agegroup', 'mar_stat','parish', 'hsize')
RCAP4(jen, charest, 'sex', 'agegroup', 'mar_stat','parish', 'hsize')
RCAP4(jen, chen1, 'sex', 'agegroup', 'mar_stat','parish', 'hsize')
RCAP4(jen, chen2, 'sex', 'agegroup', 'mar_stat','parish', 'hsize')
RCAP4(jen, chen3, 'sex', 'agegroup', 'mar_stat','parish', 'hsize')
RCAP4(jen, chen4, 'sex', 'agegroup', 'mar_stat','parish', 'hsize')

RCAP3(jen, raab, 'sex', 'parish', 'mar_stat', 'hsize')
RCAP3(jen, snoke1, 'sex',  'mar_stat','parish', 'hsize')
RCAP3(jen, snoke2, 'sex', 'mar_stat','parish', 'hsize')
RCAP3(jen, charest, 'sex', 'parish', 'mar_stat', 'hsize')
RCAP3(jen, chen1, 'sex',  'mar_stat','parish', 'hsize')
RCAP3(jen, chen2, 'sex', 'mar_stat','parish', 'hsize')
RCAP3(jen, chen3, 'sex', 'parish', 'mar_stat', 'hsize')
RCAP3(jen, chen4, 'sex',  'mar_stat','parish', 'hsize')

##########################################################################
#Target variable is servants

RCAP6(jen, raab, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'servants')
RCAP6(jen, snoke1, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'servants')
RCAP6(jen, snoke2, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'servants')
RCAP6(jen, charest, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'servants')
RCAP6(jen, chen1, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'servants')
RCAP6(jen, chen2, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'servants')
RCAP6(jen, chen3, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'servants')
RCAP6(jen, chen4, 'sex', 'agegroup', 'mar_stat','parish', 'ctry_bth', 'under15', 'servants')

RCAP5(jen, raab, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'servants')
RCAP5(jen, snoke1, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'servants')
RCAP5(jen, snoke2, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'servants')
RCAP5(jen, charest, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'servants')
RCAP5(jen, chen1, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'servants')
RCAP5(jen, chen2, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'servants')
RCAP5(jen, chen3, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'servants')
RCAP5(jen, chen4, 'sex', 'agegroup', 'mar_stat','parish',  'under15', 'servants')

RCAP4(jen, raab, 'sex', 'agegroup', 'mar_stat','parish', 'servants')
RCAP4(jen, snoke1, 'sex', 'agegroup', 'mar_stat','parish', 'servants')
RCAP4(jen, snoke2, 'sex', 'agegroup', 'mar_stat','parish', 'servants')
RCAP4(jen, charest, 'sex', 'agegroup', 'mar_stat','parish', 'servants')
RCAP4(jen, chen1, 'sex', 'agegroup', 'mar_stat','parish', 'servants')
RCAP4(jen, chen2, 'sex', 'agegroup', 'mar_stat','parish', 'servants')
RCAP4(jen, chen3, 'sex', 'agegroup', 'mar_stat','parish', 'servants')
RCAP4(jen, chen4, 'sex', 'agegroup', 'mar_stat','parish', 'servants')

RCAP3(jen, raab, 'sex', 'parish', 'mar_stat', 'servants')
RCAP3(jen, snoke1, 'sex',  'mar_stat','parish', 'servants')
RCAP3(jen, snoke2, 'sex', 'mar_stat','parish', 'servants')
RCAP3(jen, charest, 'sex', 'parish', 'mar_stat', 'servants')
RCAP3(jen, chen1, 'sex',  'mar_stat','parish', 'servants')
RCAP3(jen, chen2, 'sex', 'mar_stat','parish', 'servants')
RCAP3(jen, chen3, 'sex', 'parish', 'mar_stat', 'servants')
RCAP3(jen, chen4, 'sex',  'mar_stat','parish', 'servants')