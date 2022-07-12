# DLCaveolae
A Deep Learning tool for the automated detection of caveolae in transmission electron microscopy images


Instructions:


0-Install Keras with the Tensorflow backend in RStudio following any of the tutorials available online, for example:
https://tensorflow.rstudio.com/installation/
https://cran.r-project.org/web/packages/keras/vignettes/index.html


1-Download CNN models, TEMCellCNN.hdf5 and TEMCavCNN.h5, and place them in R directory folder.


2-Download and open "DLCaveolae.R" script in RStudio.


3-Edit the section #PARAMETERS if necessary. We recommend that you fully optimized the parameters for one example image before running the script in a large set of images.
 
 The customizable parameters are:
  
  -mode="PREDICT". Leave it untouched for the first time that an image is analized. Change for something different than "PREDICT" if you want to repeat postprocessing reusing previously generated predictions.
  
  -path. The directory of the images. It should be something like "C:/Users/mcmaboy/Desktop/images" for windows or "/home/maria/Escritorio/images" for Ubuntu.
  
  -confidence. Minimum caveolar prediction probability required. Low values allow for a maximum number of caveolar predictions. Higher values provide only the most certain caveolae predictions.
  
  -extensions=c("jpg", "tif", "tiff"). These are the tested image formats. The scripts generates images in svg format, so we don't recommend to use this as input image format.
  
  -pxum. Scale of the images in pixel/micron
  
  -pixelsize=1000*(1/pxum). Automatically calculates pixel size in nanometers. Add scale in pixelsize here if needed.
  
  -mincavnm=50. Usually considered minimum caveolae size in nanometers. Modify if required.
  
  -maxcavnm=100. Usually considered maximum caveolae size in nanometers. Modify if required.
  
  -avcavnm=75. Usually considered average caveolae size in nanometers. Modify if required.
  
  -windowsize=windowsize. The script will calculate a recommended sliding window size using the provided scale. If a different value is required, enter it here.
  
  -pxdisplacement=pxdisplacement. The script will calculate a recommended sliding window displacement using the provided scale. If a different value is required, enter it here.
  
  -FindCellLimits. Use T or TRUE if you want the algorithm to estimate PM and LD. Use F or FALSE if not needed to save processing time.
  
  -LDexpected. Use T or TRUE if you expect a big LD in your cells, like in adipocytes. Use F or FALSE if LD not expected, to avoid unnecessary errors.
  
  -specifyfilelist=F. Use T or TRUE if you want a subset of images from path to be analysez, and enter their filenames as a vector named "listrequired" in the line below.
  
  -deleteENVwhenfinished = T. Use F or FALSE if you want to process only one image and want to keep all the generated datasets in the RStudio environment. Setting it to T or TRUE during processing of several images can lead to errors.
  
  -measurecavdistances. Use T or TRUE if caveolae-PM or caveolae-LD distances are needed.
  
  -measurePMLDdistances. Use T or TRUE if PM-LD distances are needed.

4- Run the script. It may take several minutes per image.


