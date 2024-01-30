library(data.table)

#############################
#############################
###
### Settings
###
#############################
#############################

# games
#  context-free -- CF
#  averaged social -- AS
#  detailed social -- DS
gameType = 'AS'
col_you = '#000000'
col_scrounge = '#62a0ea'
col_produce = '#b5835a'
col_others = '#8C92A2'
str_scrounge = 'A'
str_produce = 'B'
bar_str_col = '#fcbb0a'


#############################
#############################
###
### Dataset initialisation
###
#############################
#############################

dat = data.frame()

for(groupSize in c(2,6))
	for(producerNumber in 0:(groupSize-1))
		for(initial_scrounger in c(-100, 0, 100))
			for(production_cost in seq(200,0,-50))
				for(production_gain in 100){
					dat = rbind(dat,
											data.frame(groupSize=groupSize,
																 producerNumber = producerNumber,
																 production_cost = production_cost,
																 production_gain = production_gain,
																 initial_scrounger = initial_scrounger))
				}
				

#############################
#############################
###
### Calculations
###
#############################
#############################

dat = data.table(dat)	
dat[ , initial_producer := initial_scrounger-production_cost]
dat[ , initial_othersAVG := (initial_producer*producerNumber + initial_scrounger*(groupSize-producerNumber-1))/(groupSize-1)]
dat[ , alternative_scrounger := initial_scrounger+production_gain]
dat[ , alternative_producer := initial_producer+production_gain]
dat[ , alternative_othersAVG := (alternative_producer*producerNumber + alternative_scrounger*(groupSize-producerNumber-1))/(groupSize-1)]

#############################
#############################
###
### Transformations
###
#############################
#############################

dat[ , PSG_type := gameType]
dat[ , noSocial_info := PSG_type=='CF']
dat[ , barplots := ifelse(PSG_type=='DS', 'detailed', 'average')]


#############################
#############################
###
### Barplots
###
#############################
#############################

#
# colour
#
dat[ , bar_col_detailedYou := ifelse(barplots=='detailed', col_you, '#ffffff00')]
dat[ , bar_col_detailedScrounge := ifelse(barplots=='detailed', col_scrounge, '#ffffff00')]
dat[ , bar_col_detailedProduce := ifelse(barplots=='detailed', col_produce, '#ffffff00')]
dat[ , bar_col_averageYou := ifelse(barplots=='average', col_you, '#ffffff00')]
dat[ , bar_col_averageOthers := ifelse(barplots=='average', col_others, '#ffffff00')]

#
# texts
#
dat[ , bar_text_detailedYou := ifelse(barplots=='detailed', 'you', '')]
dat[ , bar_text_detailedScrounge := ifelse(barplots=='detailed', str_scrounge, '')]
dat[ , bar_text_detailedProduce := ifelse(barplots=='detailed', str_produce, '')]
dat[ , bar_text_averageYou := ifelse(barplots=='average', 'you', '')]
dat[ , bar_text_averageOthers := ifelse(barplots=='average', 'others', '')]

# text colour
dat[ , bar_text_colour := bar_str_col]


#
# heights
#

# Note: heights are the actual height +2 to avoid disappearing bars when the outcome is 0, and halved to ensure they fit on the screen..
dat[ , bar_height_detailedYou_A := ifelse(barplots=='detailed', (1/2)*initial_scrounger+2, 0)]
dat[ , bar_height_detailedScrounge_A := ifelse(barplots=='detailed', (1/2)*initial_scrounger+2, 0)]
dat[ , bar_height_detailedProduce_A := ifelse(barplots=='detailed', (1/2)*initial_producer+2, 0)]
dat[ , bar_height_averageYou_A := ifelse(barplots=='average', (1/2)*initial_scrounger+2, 0)]
dat[ , bar_height_averageOthers_A := ifelse(barplots=='average', (1/2)*initial_othersAVG+2, 0)]

dat[ , bar_height_detailedYou_B := ifelse(barplots=='detailed', (1/2)*alternative_producer+2, 0)]
dat[ , bar_height_detailedScrounge_B := ifelse(barplots=='detailed', (1/2)*alternative_scrounger+2, 0)]
dat[ , bar_height_detailedProduce_B := ifelse(barplots=='detailed', (1/2)*alternative_producer+2, 0)]
dat[ , bar_height_averageYou_B := ifelse(barplots=='average', (1/2)*alternative_producer+2, 0)]
dat[ , bar_height_averageOthers_B := ifelse(barplots=='average', (1/2)*alternative_othersAVG+2, 0)]



#############################
#############################
###
### Colouring of the persons
###
#############################
#############################

#
# Groupsize 2
#
if(dat[groupSize == 2, .N] >= 1){
	for(i in c(1,2,5,6)){
		dat[groupSize == 2, paste0('col_pers',i,'_A') := '#ffffff00']
		dat[groupSize == 2, paste0('col_pers',i,'_B') := '#ffffff00']
	}

	dat[groupSize == 2, col_pers3_A := col_scrounge] # Person 3 is the player when it's a game of 2
	dat[groupSize == 2, col_pers3_B := col_produce] # Person 3 is the player when it's a game of 2

	dat[groupSize == 2, col_pers4_A := ifelse(producerNumber == 1, col_produce, col_scrounge)]
	dat[groupSize == 2, col_pers4_B := ifelse(producerNumber == 1, col_produce, col_scrounge)]
	
}

#
# Groupsize 6
#
if(dat[groupSize == 6, .N] >= 1){
	dat[groupSize == 6, col_pers1_A := col_scrounge] # Person 1 is the player when it's a game of 6
	dat[groupSize == 6, col_pers1_B := col_produce] # Person 1 is the player when it's a game of 6

	for(i in 2:6){
		dat[groupSize == 6, paste0('col_pers',i,'_A') := ifelse(producerNumber >= (i-1), col_produce, col_scrounge)]
		dat[groupSize == 6, paste0('col_pers',i,'_B') := ifelse(producerNumber >= (i-1), col_produce, col_scrounge)]
	}
}


#############################
#############################
###
### Text of the persons
###
#############################
#############################

#
# Groupsize 2
#
if(dat[groupSize == 2, .N] >= 1){
	# Make sure that the string is empty when the person is not present
	for(i in c(1,2,5,6)){
		dat[groupSize == 2, paste0('str_pers',i,'_A') := '']
		dat[groupSize == 2, paste0('str_pers',i,'_B') := '']
	}

	dat[groupSize == 2, str_pers3_A := str_scrounge] # Person 3 is the player when it's a game of 2
	dat[groupSize == 2, str_pers3_B := str_produce] # Person 3 is the player when it's a game of 2

	dat[groupSize == 2, str_pers4_A := ifelse(producerNumber == 1, str_produce, str_scrounge)]
	dat[groupSize == 2, str_pers4_B := ifelse(producerNumber == 1, str_produce, str_scrounge)]
}

#
# Groupsize 6
#
if(dat[groupSize == 6, .N] >= 1){
	dat[groupSize == 6, str_pers1_A := str_scrounge] # Person 1 is the player when it's a game of 6
	dat[groupSize == 6, str_pers1_B := str_produce] # Person 1 is the player when it's a game of 6

	for(i in 2:6){
		dat[groupSize == 6, paste0('str_pers',i,'_A') := ifelse(producerNumber >= (i-1), str_produce, str_scrounge)]
		dat[groupSize == 6, paste0('str_pers',i,'_B') := ifelse(producerNumber >= (i-1), str_produce, str_scrounge)]
	}
}

#
# subtexts
#

dat[ , sub_pers3_A := '']
dat[ , sub_pers3_B := '']
dat[ , sub_pers1_A := '']
dat[ , sub_pers1_B := '']

if(dat[groupSize == 2, .N] >= 1){
	dat[groupSize == 2, sub_pers3_A := 'you']
	dat[groupSize == 2, sub_pers3_B := 'you']
}
if(dat[groupSize == 6, .N] >= 1){
	dat[groupSize == 6, sub_pers1_A := 'you']
	dat[groupSize == 6, sub_pers1_B := 'you']
}

#############################
#############################
###
### Non-persons (deleting persons in the non-social condition
###
#############################
#############################

for(i in 1:6){
	dat[noSocial_info==T, paste0('col_pers',i,'_A') := '#ffffff00']
	dat[noSocial_info==T, paste0('col_pers',i,'_B') := '#ffffff00']
	dat[noSocial_info==T, paste0('str_pers',i,'_A') := '']
	dat[noSocial_info==T, paste0('str_pers',i,'_B') := '']
}

dat[noSocial_info==T, sub_pers3_A := '']
dat[noSocial_info==T, sub_pers3_B := '']
dat[noSocial_info==T, sub_pers1_A := '']
dat[noSocial_info==T, sub_pers1_B := '']


#############################
#############################
###
### Buttons
###
#############################
#############################

dat[ , buttonA_text := paste0('choose ', str_scrounge)]
dat[ , buttonB_text := paste0('choose ', str_produce)]
dat[ , buttonA_col := col_scrounge]
dat[ , buttonB_col := col_produce]


#############################
#############################
###
### Game type
###
#############################
#############################

dat[ , game := gameType]

#############################
#############################
###
### Writing dataset
###
#############################
#############################

write.table(dat, paste0('experiment_PSG_', gameType, '.csv'),
						sep = ',',
						row.names = F)
