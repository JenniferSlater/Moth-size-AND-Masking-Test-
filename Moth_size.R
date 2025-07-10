library (imager)

#Start off easy with loading files
moth <- load.image("C:/Users/Jen/OneDrive/Desktop/moths_s25/moths_s25/Geometridae/Protoboarmia_porcelaria/Protoboarmia_porcelaria_527.jpg")
moth_smaller <- resize(moth, -50)
# Plot the image
plot(moth_smaller,main="moths")

#convert 
moth_hsv <- RGBtoHSV(moth_smaller)

#color Masking!!! w/binary channels

#so now we have to extract HSV channels
H <- channel(moth_hsv,1)
S <- channel(moth_hsv,2)
V <- channel(moth_hsv,3)

#THESE ARE THE COLOR RANGES FOR THE HSV (we will need to tweak it eventually to fit with color correction)
white_mask <- (H > 0) & (H < 160) &(S < 0.2)& (V > 0.8)
black_mask <- (V < 0.2)

#Adding other pretty colors :)
green_mask <- (H > 90) & (H < 160) & (S > 0.2) &(S < 0.8)& (V > 0.2)& (V < 0.8)
blue_mask <- (H > 220) & (H < 250) & (S > 0.2) &(S < 0.8)& (V > 0.2)& (V < 0.8)
lightblue_mask <- (H > 160) & (H < 190) & (S > 0.2) &(S < 0.8)& (V > 0.2)& (V < 0.8)

# took me an hour ro learn this but Hue is a circular range 0-360
#SOOO if we look at a color like red it is both at 360 and 0, so I put an OR statment(that's y the line)
#basically from 0-30 OR 306-330
red_mask <- ((H >= 0 & H <= 30) | (H >= 330 & H <= 360))& (S > 0.2) &(S < 0.8)& (V > 0.2)& (V < 0.8)
yellow_mask <- (H > 55) & (H < 70) & (S > 0.2) &(S < 0.8)& (V > 0.2)& (V < 0.8)

grey_mask <- (S < 0.2) & (V > 0.2) & (V < 0.8)

# https://stackoverflow.com/questions/76929194/how-to-apply-a-binary-mask-to-a-rgb-image-in-r-with-imager
apply_hsv_mask <- function(HSVimage, mask){
  hue <- channel(HSVimage, 1) * mask
  saturation <- channel(HSVimage, 2) * mask
  value <- channel(HSVimage, 3) * mask
  return(imappend(list(hue, saturation, value), "c"))
}
#Apply dem masks yo, it will extract specific colors from moth pic
#We don't need these anymore but I am gonna keep them for later

#masked_white <- apply_hsv_mask(moth_hsv, white_mask)
#masked_black <- apply_hsv_mask(moth_hsv, black_mask)
#masked_grey <- apply_hsv_mask(moth_hsv, grey_mask)

#masked_green <- apply_hsv_mask(moth_hsv, green_mask)
#masked_blue <- apply_hsv_mask(moth_hsv, blue_mask)
#masked_lightblue <- apply_hsv_mask(moth_hsv, lightblue_mask)
#masked_red <- apply_hsv_mask(moth_hsv, red_mask)
#masked_yellow <- apply_hsv_mask(moth_hsv, yellow_mask)

#gather them up
masked_colors <- white_mask | black_mask | grey_mask | 
  green_mask | blue_mask | lightblue_mask | 
  red_mask | yellow_mask

#apply mask 
masked_combined <- apply_hsv_mask(moth_hsv, masked_colors)

combined_rgb <- HSVtoRGB(masked_combined)
plot(combined_rgb, main = "All Color Masks Combined")


#I left this so it is easy to tweak it as we go :)
#masked_color <- HSVtoRGB(masked_white)
#plot(masked_color, main = "White Regions")

#masked_color <- HSVtoRGB(masked_black)
#plot(masked_color, main = "Black Regions")

#masked_color <- HSVtoRGB(masked_grey)
#plot(masked_color, main = "Grey Regions")

#masked_color <- HSVtoRGB(masked_green)
#plot(masked_color, main = "Green Regions")

#masked_color <- HSVtoRGB(masked_blue)
#plot(masked_color, main = "Blue Regions")

#masked_color <- HSVtoRGB(masked_lightblue)
#plot(masked_color, main = "Light Blue Regions")

#masked_color <- HSVtoRGB(masked_red)
#plot(masked_color, main = "Red Regions")

#masked_color <- HSVtoRGB(masked_yellow)
#plot(masked_color, main = "Yellow Regions")


summary(H)
summary(S)
summary(V)