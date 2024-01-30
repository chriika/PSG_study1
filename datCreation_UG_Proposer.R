library(data.table)

#############################
#############################
###
### Settings
###
#############################
#############################

# games
#  Ultimatum Game -- UG
gameType = 'UG_Proposer'
stepS = 5
allowSwitch = 30


#############################
#############################
###
### Dataset initialisation
###
#############################
#############################

dat.up = data.frame()
dat.dn = data.frame()

for(amount in c(400))
	for(shareOffer in (seq(0,100,stepS)/100))
		dat.up = rbind(dat.up,
									 data.frame(amount=amount,
										 				  shareOffer=shareOffer))		
									
for(amount in c(400))
	for(shareOffer in (seq(100,0,-stepS)/100))
		dat.dn = rbind(dat.dn,
								   data.frame(amount=amount,
													    shareOffer=shareOffer))		
													    
dat.adaption = data.frame(adapt = 1:allowSwitch)

#############################
#############################
###
### Calculations
###
#############################
#############################

dat.up = data.table(dat.up)	
dat.up[ , playerShare := (1-shareOffer)*amount]
dat.up[ , otherShare := shareOffer*amount]

dat.dn = data.table(dat.dn)	
dat.dn[ , playerShare := (1-shareOffer)*amount]
dat.dn[ , otherShare := shareOffer*amount]

dat.adaption = data.table(dat.adaption)


#############################
#############################
###
### Barplots
###
#############################
#############################

barColYou. = '#000000'
barColOther. = '#000000'
barTextCol. = '#000000'
barTextYou. = 'you'
barTextOther. = 'other player'

dat.up[ , barColYou := barColYou.]
dat.up[ , barColOther := barColOther.]
dat.dn[ , barColYou := barColYou.]
dat.dn[ , barColOther := barColOther.]

dat.up[ , barTextCol := barTextCol.]
dat.dn[ , barTextCol := barTextCol.]

dat.up[ , barTextYou := barTextYou.]
dat.up[ , barTextOther := barTextOther.]
dat.dn[ , barTextYou := barTextYou.]
dat.dn[ , barTextOther := barTextOther.]


#############################
#############################
###
### Buttons
###
#############################
#############################

btnDecreaseCol. = '#d6341a'
btnIncreaseCol. = '#12864e'
btnMakeCol. = '#dddddd'
btnDecreaseTxt. = 'decrease offer'
btnIncreaseTxt. = 'increase offer'
btnMakeTxt. = 'make offer'
btnDecreaseTxtCol. = '#ffffff'
btnIncreaseTxtCol. = '#ffffff'
btnMakeTxtCol. = '#000000'

dat.up[ , btnDecreaseCol := btnDecreaseCol.]
dat.up[ , btnIncreaseCol := btnIncreaseCol.]
dat.up[ , btnMakeCol := btnMakeCol.]
dat.up[ , btnDecreaseTxt := btnDecreaseTxt.]
dat.up[ , btnIncreaseTxt := btnIncreaseTxt.]
dat.up[ , btnMakeTxt := btnMakeTxt.]
dat.up[ , btnDecreaseTxtCol := btnDecreaseTxtCol.]
dat.up[ , btnIncreaseTxtCol := btnIncreaseTxtCol.]
dat.up[ , btnMakeTxtCol := btnMakeTxtCol.]

dat.dn[ , btnDecreaseCol := btnDecreaseCol.]
dat.dn[ , btnIncreaseCol := btnIncreaseCol.]
dat.dn[ , btnMakeCol := btnMakeCol.]
dat.dn[ , btnDecreaseTxt := btnDecreaseTxt.]
dat.dn[ , btnIncreaseTxt := btnIncreaseTxt.]
dat.dn[ , btnMakeTxt := btnMakeTxt.]
dat.dn[ , btnDecreaseTxtCol := btnDecreaseTxtCol.]
dat.dn[ , btnIncreaseTxtCol := btnIncreaseTxtCol.]
dat.dn[ , btnMakeTxtCol := btnMakeTxtCol.]


#############################
#############################
###
### Confirmation text
###
#############################
#############################

confirmText. = 'Confirm your offer or make adaptions if necessary'

dat.up[ , confirmText := confirmText.]
dat.dn[ , confirmText := confirmText.]


#############################
#############################
###
### Game type
###
#############################
#############################

dat.up[ , game := gameType]
dat.dn[ , game := gameType]
dat.adaption[ , game := gameType]



#############################
#############################
###
### Writing dataset
###
#############################
#############################

write.table(dat.up, paste0('experiment_', gameType, '_up.csv'),
						sep = ',',
						row.names = F)
						
write.table(dat.dn, paste0('experiment_', gameType, '_dn.csv'),
						sep = ',',
						row.names = F)
						
write.table(dat.adaption, paste0('experiment_', gameType, '_adaption.csv'),
						sep = ',',
						row.names = F)
