##########################################################################################
#
# Cutting the images into the right format 
#
##########################################################################################

# standard procedure: cut images of both categories and combine them 
#  add some whitespace (with a grey line) inbetween the images
#  add a black line below them

# we start with unaltered screenshots

# special treatment for coop0 image as this is taken from another part of screen
convert -crop 433x607+475+307 concave1_coop0.png concave1_coop0-C.png
for i in 1 2 3 4 5 6
do
	convert -crop 433x607+1039+307 concave1_coop${i}.png concave1_coop${i}-C.png
done

# create blank image for the choice when there are 6 cooperators (i.e. nobody has to make a choice to cooperate)
convert -size 433x607 xc:white concave1_perspective_coop6-C.png
for i in 0 1 2 3 4 5
do
	convert -crop 433x607+475+307 concave1_perspective_coop${i}.png concave1_perspective_coop${i}-C.png
done



for i in 0 1 2 3 4 5 6
do
	convert concave1_coop${i}-C.png whitespace.png concave1_perspective_coop${i}-C.png +append blackline.png -append concave1_coop${i}_combined.png
done