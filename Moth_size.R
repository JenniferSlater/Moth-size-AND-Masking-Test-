#DISCLAIMER White AND gray are both background masks until I make a light gray mask
#TO DO- Add all the rectangles and make it so we can average them to find length and width of moth
library (imager)

#Start off easy with loading files
#"C:/Users/Jen/OneDrive/Desktop/moths_set2/moths_set2/Geometridae/Pasiphila_rectangulata/Pasiphila_rectangulata_625.JPG"
#"C:/Users/Jen/OneDrive/Desktop/moths_s25/moths_s25/Geometridae/Protoboarmia_porcelaria/Protoboarmia_porcelaria_527.jpg"
moth_pic <- load.image("C:/Users/slate/Desktop/Protoboarmia_porcelaria_527.JPG")
moth_smaller <- resize(moth_pic,-50, -50)
#Plot the image
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
lightblue_mask <- (H > 165) & (H < 210) & (S > 0.2) &(S < 0.8)& (V > 0.2)& (V < 0.8)

# took me an hour ro learn this but Hue is a circular range 0-360
#SOOO if we look at a color like red it is both at 360 and 0, so I put an OR statment(that's y the line)
#basically from 0-30 OR 306-330
red_mask <- ((H >= 0 & H <= 30) | (H >= 330 & H <= 360))& (S > 0.2) &(S < 0.8)& (V > 0.2)& (V < 0.8)
yellow_mask <- (H > 55) & (H < 75) & (S > 0.2) &(S < 0.8)& (V > 0.2)& (V < 0.8)

gray_mask <- (S < 0.2) & (V > 0.2) & (V < 0.8)

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
#masked_gray <- apply_hsv_mask(moth_hsv, gray_mask)

#masked_green <- apply_hsv_mask(moth_hsv, green_mask)
#masked_blue <- apply_hsv_mask(moth_hsv, blue_mask)
#masked_lightblue <- apply_hsv_mask(moth_hsv, lightblue_mask)
#masked_red <- apply_hsv_mask(moth_hsv, red_mask)
#masked_yellow <- apply_hsv_mask(moth_hsv, yellow_mask)

#gather them up
masked_colors <- white_mask | black_mask | gray_mask | 
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

#masked_color <- HSVtoRGB(masked_gray)
#plot(masked_color, main = "gray Regions")

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

#Inversts the colors so I can see how messy this is
#https://dahtah.github.io/imager/morphology.html
not_mask <- !masked_colors
#plot(not_mask, main = "Future moth Mask")

#clean them up so the moth is more isolated (we will have to tweek evenually)
#I wanted to clean up the masks but it didn't work very well
mask_num <- as.cimg(not_mask)
mask_clean <- threshold(mask_num, "16%")
px <- as.pixset(mask_clean) #convert to pixset

px_blur<-isoblur(px,2)#Helps keep the moth's detail since it was really pixelated
#plot(px_blur,main="Blurred")

px_thresh <- px_blur > 0.2
#plot(px_thresh,main="binary again") #It was true or false earlier and now the blur changed it so we have to reset it back

px_clean<-clean(px_thresh,11) #resizes the pixels small then large again which helps with little dots 
#plot(px_clean,main="Cleaned")

px_fill<-fill(px_clean,9) # fills in the moth, but if it is too strong we lose detail
#plot(px_fill,main="Filled")

#Now we get to see the actual moth, this example looks good but it will definetly vary with moths
#Color correcting will be super important for the moth
masked_moth <- apply_hsv_mask(moth_hsv,px_fill )
masked_color <- HSVtoRGB(masked_moth)
#plot(masked_color, main = "Moth Regions (finally)")

#combine them all together
masks_all<-masked_combined+masked_moth #moth has a few lightspots where it was filled (I think it's a good thing)
combined_masks <- HSVtoRGB(masks_all )
#plot(combined_masks, main = "All Masks Combined")

#I decided to highlight them since I wanted to see all the seprate layers
#highlight(blue_mask)
#highlight(red_mask)
#highlight(yellow_mask)
#highlight(lightblue_mask)
#highlight(black_mask)
#highlight(masked_moth)
#I dont want to worry about white and gray they are too close in color

#MASKING PORTION DONE!!! (now the hard part, figuring out size)
plot(moth_smaller)
mask <- list(black=black_mask,   #I tried them sepratly and it looked good so I am going to loop this
             green=green_mask,
             blue=blue_mask,
             lightblue=lightblue_mask,
             red=red_mask,
             yellow=yellow_mask,
             moth=px_fill
)

#THIS IS FOR SIZE
rectangle_sizes <- list() #empty list, I am gonna have it add as it finds the rectangles
#I have it so it is set up with width,height,color(for identification)
moth_sizes <-list()

for (colors in names(mask)){
  
  mask_rect <- mask[[colors]] #callign the colors
  
  mask_blur <- isoblur(mask_rect, 2) #I ended up having to clean them up I got no choice
  mask_thresh <- mask_blur > 0.2
  mask_clean <- clean(mask_thresh, 11)
  filled_mask <- fill(mask_clean, 9)
  
  labeled_mask <- label(filled_mask)
  num_regions <- max(labeled_mask) #each section
  
  print(mask)
  print(num_regions) #YAY it gave me the right amount 3 regions for black :)
  #plot(mask_clean)
  
  img_height <- dim(moth_smaller)[1]
  img_width <- dim(moth_smaller)[2] 
  
  for (region in 1:num_regions) {  #make a loop to go through all 3 regions
    pix <- which(labeled_mask == region, arr.ind = TRUE)
    #color name              #array thingy for min and max
    xmin <- min(pix[,2]) 
    xmax <- max(pix[,2])
    ymin <- min(pix[,1])
    ymax <- max(pix[,1])
    
    rect( #had to flip them cause it went the wrong way
      xleft = ymin,
      ybottom = xmax,
      xright = ymax,
      ytop = xmin,
      border = "blue",
      lwd =1 #line width so we can see it
    )
    
    #print(sprintf("Region %d: xmin=%d xmax=%d ymin=%d ymax=%d", 
    #              region, xmin, xmax, ymin, ymax)) #tells me position
    width <- xmax - xmin + 1
    height <- ymax - ymin + 1
    #print(sprintf("Region %d: width = %d px, height = %d px", 
    #             region, width, height))#tells me height and width
    
    #Now I wanna just list all the rectangle sizes real quick 
    #but we definetly want to exclude the moth 
    if(colors != "moth"){
      rectangle_sizes<-append (rectangle_sizes,
                               list(list(width=width,height=height,color=colors)))}
    
    else{
      moth_sizes<-append(moth_sizes,
                         list(list(width=width,height=height,color="moth")))}
  }
}

#print(rectangle_sizes)
#print(moth_sizes)
#quick double check

#ok so now I have it set up so there is a list giving us the 
#width,height, color
#so it basically says Width= 150 Height= 92, Color= "yellow")
#if I use $ it allows me to be able to pull an element from the list
rect_longside<-list() #new lists

for (element in rectangle_sizes){
  #I want it to find the longer side 
  longer_side<-max(element$width,element$height) #find the max number from l or w
  rect_longside <- append(rect_longside, longer_side)
}

#I keep getting an error abt "x must be atomic" apparently I have to unlist the varibles from the list?
rect_longside_unlisted <-unlist(rect_longside)

#I am not going to do the average, median is wa better if there are outliers
median_length<-median(rect_longside_unlisted)
print(median_length) #so median is 196 px

px_to_cm= 1/median_length # median length = 1 cm

#FINALLLY THE MOTH
moth_conversion_width <- list() #moth list as a conversion
moth_conversion_height <- list() #list for if there is more than 1 moth

for (centimeters in (moth_sizes)) {
  width_cm <- centimeters$width * px_to_cm
  height_cm <- centimeters$height * px_to_cm
  
  moth_conversion_width <- append(moth_conversion_width, width_cm) #add this to the list
  moth_conversion_height <- append(moth_conversion_height, height_cm)
}
moth_conversion_width_unlisted <-unlist(moth_conversion_width) #unlist it like before
moth_conversion_height_unlisted <-unlist(moth_conversion_height)

print(moth_conversion_width) #print itttt (they look abt 1.5cm,2cm)
print(moth_conversion_height)

##Ok so now we have the height and width in cm YAY!!! but now I want to find the wingshape

# I want to try edge detection on this The website I am refrencing has a great example so I am gonna try to recreate that
#https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html#example-2-edge-detection
Greyscale_moth <- channel(masked_moth, 3)
plot(Greyscale_moth, main="Greyscale of Moth") #Kinda looks weird but trust the process

#gr <- imgradient(Greyscale_moth,"xy")
#plot(gr, main="imgradient")<--Gives error cause it is gradient X and Y direction
#gr #I think it saying Image list of size 2 is good news...

#plot(gr,layout="row")

dx <- imgradient(Greyscale_moth,"x") #this will be the x and y direction
dy <- imgradient(Greyscale_moth,"y")

grad.mag <- sqrt(dx^2+dy^2) #I think this could be adjusted for strength
plot(grad.mag,main="Gradient magnitude")

# so I followed what they have and now I have a nice border 
#now I want to plot ittt :)))
library(ggplot2)

edge_df <- data.frame(x = x_vals, y = -y_vals)

ggplot(edge_df, aes(x = x, y = y)) +
  geom_point(size = 0.3, color = "purple") #make it pretty :)

