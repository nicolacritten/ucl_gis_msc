#!/usr/bin/env python2
# -*- coding: utf-8 -*-

#Import the necessary libraries
import cv2
import numpy as np
import pylab as plt
import glob
import os

#This function applies a bilateral blur to the image
#The input value is an image. In this case the original image from file
#The output value is the image after the blur is applied
def blur(img):
    
    bilat_blur = cv2.bilateralFilter(img,9,75,75)
    
    return bilat_blur


#This function applies various blurs to the image
#The input value is an image. In this case the original image from file
#The output values are the images after the various blurs are applied
def blurexample(img):
    
    #Apply various blurs to the image
    ave_blur = cv2.blur(img,(5,5))
    gaus_blur = cv2.GaussianBlur(img,(5,5),0)
    median_blur = cv2.medianBlur(img,5)
    bilat_blur = cv2.bilateralFilter(img,9,75,75)
    
    return ave_blur, gaus_blur, median_blur, bilat_blur


#This function applies a k-means clustering threshold to the image
#The input values are images: the original image from file, and the image after blurring
#The output value is the image after the k-means clustering
def kmeans(origimg, img):
    
    # Re-shape image into array Mx3 where M in number of pixels
    Z = img.reshape((-1,3))
    # Convert to 32 bit data type
    Z = np.float32(Z)
    
    # define criteria, number of clusters(K) and apply kmeans()
    criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 10, 1.0)
    K = 9 #involved trial and error to get best one
    ret,label,center=cv2.kmeans(Z,K,None,criteria,10,cv2.KMEANS_RANDOM_CENTERS)
    
    # Now convert back into uint8, and make original image
    center = np.uint8(center)
    res = center[label.flatten()]
    res2 = res.reshape((origimg.shape))
        
    return res2


#This function applies a mask using thresholding techniques inRange and Otsu
#The input values are images: the original image from file, and the image after k-means clustering
#The output value is the image after the mask has been applied
def masking(img, origimg):
    
    #Turn the image to greyscale
    grey = cv2.cvtColor(img, cv2.COLOR_RGB2GRAY)
        
    #The threshold value was determined to be 0-30 after trial and error
    mask = cv2.inRange(grey, 0, 30)
    
    #Compute per-element bit-wise conjunction
    res = cv2.bitwise_and(origimg, origimg, mask=mask)
    
    #Convert to grayscale and apply Otsu threshold
    grey = cv2.cvtColor(res, cv2.COLOR_RGB2GRAY)
    ret, otsu = cv2.threshold(grey,0,255,cv2.THRESH_BINARY+cv2.THRESH_OTSU)
    
    return otsu


#This function applies the Canny edge detector and then uses dilation to improve the edges
#The input value is an image. In this case the image after the mask has been applied
#The output value is the image after using Canny edge detector and dilation
def canny(img):
    
    #Perform canny edge detection
    canny = cv2.Canny(img,0,255,3)
    
    #Create kernel
    kernel = np.ones((5,5),np.uint8)
    #Perform dilation
    dilation = cv2.dilate(canny,kernel,iterations = 1)

    return dilation


#This function finds the contours from the edges
#The input value is an image. In this case the image after the edges have been detected
#The output value are the contours
def findcontours(img):
    
    #Find the contours
    image, contours, hierarchy = cv2.findContours(img, cv2.RETR_TREE,
                                                  cv2.CHAIN_APPROX_SIMPLE)
    
    #Create a blank list to hold the contours over a certain area
    contourlist = []
    
    #Compute shape approximation for each contour
    for shape in contours:
            epsilon = 1e-4*cv2.arcLength(shape,True)
            approx = cv2.approxPolyDP(shape,epsilon,True)
            
            #Generate the area of each contour
            area = cv2.contourArea(shape)
               
            #Only add contour to the list if it is bigger than the specified area
            if area > 1000:
                contourlist.append(approx)    
                      
    return contours


#This function draws the contours on the original image
#The input values are the original image from file, and the detected contours
#The output value is the image after the contours have been drawn on
def drawcontours(img, contours):
    
    contimg = cv2.drawContours(img, contours, -1,(0,0,255),3)      
            
    return contimg

    
#This function generates bounding boxes around the contours and draws them on an image
#The input values are the original image from file, and the detected contours
#The output values are the image after the bounding boxes have been drawn on, and the list of bounding box coordinates
def drawbboxes(img, contours):
    
    #Create empty lists to hold the bounding boxes
    bbox = []
    bbox2 = []
    
    imgbbox = img.copy()

    for contour in contours:
        
        #Exclude the contour if its area is not that big
        if cv2.contourArea(contour) > 1000:
            
            #Assign four points of bounding box for each contour
            x, y, w, h = cv2.boundingRect(contour)
            
            #Draw the bounding rectangle from points above
            cv2.rectangle(imgbbox, (x, y), (x + w, y + h), [255,0,0], 3)
            
            #Append the coordinates to the lists
            bbox.append((x,y,w,h))
            bbox2.append([x, y, x+w, y+h])
    
    return imgbbox, bbox2
    

#This function finds the biggest bounding box from the list of bounding boxes detected
#The input value is the list of bounding box coordinates
#The output value is coordinates of the biggest bounding box
def biggestbbox(bbox2):
    
    #Create an empty list for the bounding box areas
    bbox_area = []
    
    
    for i in range(len(bbox2)):
        currentbbox = bbox2[i]
        x = currentbbox[0]
        y = currentbbox[1]
        xmax = currentbbox[2]
        ymax = currentbbox[3]
        
        #Find the area of the bounding box and add it to the list
        area = (xmax-x) * (ymax-y)
        bbox_area.append([x,y,xmax,ymax,area])
    
    #Create an empty array for the largest bounding box
    biggestbbox = [0,0,0,0,0]
    
    #Find the bounding box with the biggest area 
    for n in bbox_area:
        if n[4] > biggestbbox[4]: biggestbbox = n

    return biggestbbox    


#This function generates the accuracy of the method for the given image
#The input values are the original image from file and the coordinates of the largest bounding box
#The output value is the IoU value
def accuracy(origimg, bbox):
    
    #The bounding box generated from this method
    boxA = [bbox[0], bbox[1], bbox[2], bbox[3]]
    
    #The ground-truth bounding box coordinates entered from the program labellmg
    boxB = [x_min, y_min, x_max, y_max]
        
    #Determine the (x, y)-coordinates of the intersection rectangle
    xA = max(boxA[0], boxB[0])
    yA = max(boxA[1], boxB[1])
    xB = min(boxA[2], boxB[2])
    yB = min(boxA[3], boxB[3])
     
    #Compute the area of intersection rectangle
    interArea = (xB - xA + 1) * (yB - yA + 1)
     
    #Compute the area of both the prediction and ground-truth rectangles
    boxAArea = (boxA[2] - boxA[0] + 1) * (boxA[3] - boxA[1] + 1)
    boxBArea = (boxB[2] - boxB[0] + 1) * (boxB[3] - boxB[1] + 1)
     
    #Compute the intersection over union by taking the intersection
    #area and dividing it by the sum of prediction + ground-truth
    #areas - the interesection area
    iou = interArea / float(boxAArea + boxBArea - interArea)
    
    return iou


#This function generates a final image showing the contours, bounding boxes and IoU value
#The input values are the image with the contours drawn on, the IoU value, and the predicted bounding box
#The output value is generated final image
def finalaccimage(img, iou, bbox):
    
    finalimg = img.copy()
    
    #Draw the ground-truth bounding box on the image
    cv2.rectangle(finalimg, (x_min, y_min), (x_max, y_max), [0, 255, 0], 3)
    
    #Draw the prediected bounding box on the image
    cv2.rectangle(finalimg, (bbox[0], bbox[1]), 
                  (bbox[2], bbox[3]), [255, 0, 0], 3)
 
    #Set the font properties
    font = cv2.FONT_HERSHEY_SIMPLEX
    fontScale = 1.2
    fontColor = (255,255,255)
    lineType = 5
    
    #Add text showing the IoU value on the image
    cv2.putText(finalimg,"IoU: {:.4f}".format(iou), 
        (10, 30), 
        font, 
        fontScale,
        fontColor,
        lineType)        
    
    return finalimg



#Code to run the functions starts here
    
#Set the file path for where the generated image files will be saved
savepath = '/Users/Documents/UNI/MASTERS/Image/res_extract/saved'

#Get all the .jpg image files from the given path
files = glob.glob("/Users/Documents/UNI/MASTERS/Image/res_extract/images/*.jpg")

#Create an empty list to add the images to when they are read in
images = []

imnumber = 0

#Iterate through each .jpg file
for myFile in files:
    
    #Prints the place number of each image in the list
    print(imnumber)
    
    #Prints the image name so it is known which image is in which place
    print(myFile)
    
    #Add the image to the list
    images.append(cv2.imread(myFile, 1))
    imnumber = imnumber + 1


#Print the number of images brought in from the file
print(len(images))

#Assign the image that will be analysed from the list
currentimg = images[10]

#Assign the image a name that it will be saved under
savename = 'res1'

#The coordinates for the ground-truth bounding boxes generated from labelmg
#These should be commented out, except for the coordinates referring to the current image

##res1.jpg
#x_min = 249
#y_min = 289
#x_max = 648
#y_max = 536
#
##res2.jpg
#x_min = 167
#y_min = 142
#x_max = 892
#y_max = 452
#
##res3.jpg
#x_min = 439
#y_min = 157
#x_max = 880
#y_max = 482
#
##res4.jpg
#x_min = 414
#y_min = 231
#x_max = 671
#y_max = 443
#
##res5.jpg
#x_min = 407
#y_min = 1
#x_max = 668
#y_max = 681
#
##res6.jpg
#x_min = 461
#y_min = 282
#x_max = 799
#y_max = 440
#
##res7.jpg
#x_min = 400
#y_min = 169
#x_max = 678
#y_max = 403
#
##res8.jpg
#x_min = 163
#y_min = 32
#x_max = 871
#y_max = 673
#
##res9.jpg
#x_min = 313
#y_min = 51
#x_max = 947
#y_max = 661
#
##manyres.jpg
#x_min = 189
#y_min = 45
#x_max = 958
#y_max = 712


#The following code applies each of the functions above to carry out the extraction
#For each stage an image will be printed and then outputed to file
#The name of the file will be the name assigned above, along with the step that has been applied 

#Apply a bilateral blur
blurredimg = blur(currentimg)
plt.imshow(blurredimg)
cv2.imwrite(os.path.join(savepath, savename + 'blur.jpg'), blurredimg)

#Apply kmeans clustering
kmeansimg = kmeans(currentimg, blurredimg)
plt.imshow(kmeansimg)
cv2.imwrite(os.path.join(savepath, savename + 'kmeans.jpg'), kmeansimg)

#Apply a mask to the image
maskedimg = masking(kmeansimg, blurredimg)
plt.imshow(maskedimg)
cv2.imwrite(os.path.join(savepath, savename + 'masked.jpg'), maskedimg)

#Perform canny edge detection
cannyimg = canny(maskedimg)
plt.imshow(cannyimg)
cv2.imwrite(os.path.join(savepath, savename + 'canny.jpg'), cannyimg)

#Get the contours from the canny image
contours = findcontours(cannyimg)

#Print the number of countours generated
print(len(contours))

#Generate an image showing the contours
contoursimg = drawcontours(currentimg, contours)
plt.imshow(contoursimg)
cv2.imwrite(os.path.join(savepath, savename + 'contours.jpg'), contoursimg)

#Generate bounding boxes around the contours
bboxesimg, bboxes = drawbboxes(contoursimg, contours)
plt.imshow(bboxesimg)
cv2.imwrite(os.path.join(savepath, savename + 'bbox.jpg'), bboxesimg)

#Find the biggest bounding box from all the bounding boxes generated
#This bounding box is assumed to be the 'main feature' found from the extraction
#This bounding box will therefore be used for the accuracy assessment
bbox = biggestbbox(bboxes)

#Generate an Intersection over Union score
iou = accuracy(blurredimg, bbox)
    
#Generate a final image showing the countours, the generated bounding box from the contours
#and the groundtruth bounding box generated from labellmg
finalimage = finalaccimage(contoursimg, iou, bbox)
plt.imshow(finalimage)
cv2.imwrite(os.path.join(savepath, savename + 'FINAL.jpg'), finalimage)

#Applies each of the different blurs on the image to assess perform
ave_blur, gaus_blur, median_blur, bilat_blur = blurexample(currentimg)

#Creates two plots showing the different types of blur
plt.figure(figsize=(20,15))
plt.subplot(121),plt.imshow(currentimg),plt.title('Original Image')
plt.xticks([]), plt.yticks([])
plt.subplot(122),plt.imshow(bilat_blur),plt.title('Bilateral Blur')
plt.xticks([]), plt.yticks([])
plt.show()

plt.figure(figsize=(20,15))
plt.subplot(331),plt.imshow(median_blur),plt.title('Median Blur')
plt.xticks([]), plt.yticks([])
plt.subplot(332),plt.imshow(ave_blur),plt.title('Average Blur')
plt.xticks([]), plt.yticks([])
plt.subplot(333),plt.imshow(gaus_blur),plt.title('Gaussian Blur')
plt.xticks([]), plt.yticks([])
plt.show()




#The following section assesses Lake Mead over time from extracted contours
#Three years of images are used from 1984, 2002 and 2016
img1984 = images[9]
img2002 = images[4]
img2016 = images[3]

#Apply a bilateral blur
blurredimg_1984 = blur(img1984)
blurredimg_2002 = blur(img2002)
blurredimg_2016 = blur(img2016)

#Apply kmeans clustering
kmeansimg_1984 = kmeans(img1984, blurredimg_1984)
kmeansimg_2002 = kmeans(img2002, blurredimg_2002)
kmeansimg_2016 = kmeans(img2016, blurredimg_2016)

#Apply a mask
maskedimg_1984 = masking(kmeansimg_1984, blurredimg_1984)
maskedimg_2002 = masking(kmeansimg_2002, blurredimg_2016)
maskedimg_2016 = masking(kmeansimg_2002, blurredimg_2016)

#Perform canny edge detection
cannyimg_1984 = canny(maskedimg_1984)
cannyimg_2002 = canny(maskedimg_1984)
cannyimg_2016 = canny(maskedimg_2016)

#Find the contours
img1984_cont = findcontours(cannyimg_1984, blurredimg_1984)
img2002_cont = findcontours(cannyimg_2002, blurredimg_2002)
img2016_cont = findcontours(cannyimg_2016, blurredimg_2016)

#Layer the contours for each year on top of the 2016 satellite image
cont_img_time = cv2.drawContours(img2016, img1984_cont, -1,(255,0,0),3)
cont_img_time = cv2.drawContours(cont_img_time, img2002_cont, -1,(0,255,0),3)
cont_img_time = cv2.drawContours(cont_img_time, img2016_cont, -1,(0,0,255),3)

#Show the layered contour image
plt.imshow(cont_img_time)

#Write the image to file
cv2.imwrite(os.path.join(savepath, 'lakemead_time.jpg'), cont_img_time)

