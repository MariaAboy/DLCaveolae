# DLCaveolae
A Deep Learning tool for the automated detection of caveolae in transmission electron microscopy images

## Instructions:


### 0.1-Install Keras with the Tensorflow backend in RStudio following any of the tutorials available online, for example:
https://tensorflow.rstudio.com/install/
https://cran.r-project.org/web/packages/keras/vignettes/index.html

### 0.2-Install the following R libraries: data.table, dplyr, abind, plyr. 


### 1-Download CNN models, TEMCellCNN.hdf5 and TEMCavCNN.h5, and place them in R directory folder.


### 2-Download and open "DLCaveolae.R" script in RStudio.


### 3-Edit the section #PARAMETERS if necessary. We recommend that you fully optimized the parameters for one example image before running the script in a large set of images.
 
 The customizable parameters are:
  
  -mode="PREDICT". Leave it untouched for the first time that an image is analized. Change for something different than "PREDICT" if you want to repeat postprocessing reusing previously generated predictions.
  
  -path. The directory of the images. It should be something like "C:/Users/myname/Desktop/images" for windows or "/home/myname/Desktop/images" for Ubuntu.
  
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


### 4- Run the script. It may take several minutes per image. You can distribute your images in several folders and run the DLCaveolae.R script in parallel RStudio sessions.  

The script produces the following files:

 4.1 TEXT

_CAVDF.txt  final list of corrected caveolae predictions, with coordinates and assigned to each cell in case there are several cells in the field

_CAVDFDISTANCES.txt  final list of corrected caveolae predictions, with shorter PM and LD distance and closest LD and PM coordinates

_CLUSTERPOINTS_CAVEOLAEPSEUDOSEGMENTATION.txt  clustered caveolae predictions after k-means cluster analysis, providing a caveolae pseudosegmentation. Columns include coordinates, class-consistent group and cluster to which each caveolae belongs, and the k-means implementation option that provided the highest score

_CYTOPREDICTIONS.txt cytoplasmic coordinates with classified contacts

_FINALPREDTHRIMAGE.txt  final cell pseudosegmentation, coordinates with corrected predictions

_intPM.txt     interpolated PM coordinates and cell to which they belong, in case there are several cells per field

_intLD.txt interpolated LD coordinates and cell to which they belong, in case there are several cells per field

_PREDICTIONLIST.txt  initial coordinate predictions. This file is recovered by the script if the image is repeated and the mode is not set to "PREDICT"

_PROBSCAVDF.txt  prediction probabilities for all caveolae predictions


 4.2 SVG IMAGES

_CLUSTERPLOT.svg  class-consistent groups of caveolae with central coordinates derived from k-means cluster analysis

_CLUSTERPOINTS_CAVEOLAEPSEUDOSEGMENTATION.svg  pseudosegmentation of caveolae with central coordinates derived from k-means cluster analysis 

_FINALPLOT.svg  final caveolae central coordinates and predictions

_FINALPLOTDIST.svg  final caveolae central coordinates and predictions + distance calculation to PM and LD

_FINALPLOTINTERPOLATED.svg final caveolae central coordinates and predictions + interpolated surface delimitations

_FINALPLOTlimitsandcav.svg final caveolae central coordinates and predictions + cytoplasmic surfaces

_PREDICTIONSFINAL.svg corrected cell pseudosegmentation

_PREDPLOT.svg   initial cell pseudosegmentation

_PROBSANDCOORDS.svg prediction probability plot


### 5- If your images are tiles of a whole cell stitch and you have a file with the coordinates of each tile, you can run the script MergeSurfaces.R. This may correct potential PM and LD prediction errors

Check the PARAMETERS section so the are the same that you choose for DLCaveolae.R. Make sure to add the directory of your images and the file with your stitch coordinates. The script generates these text files:

CAVDFTOTAL.txt all caveolar coordinates in the stitched image

FINALPM.txt   all predicted PM coordinates in the stitched image

FINALLD.txt  all predicted LD coordinates in the stitched image

PREDICTIONS.txt all coordinate predictions in the stitched image

_LDCORRECTED.txt corrected LD coordinates in the stitched image

_PMCORRECTED.txt corrected PM coordinates in the stitched image


### 6- If you want to view the predictions in ImageJ, open the image and run the script PAINTPOINTS.ijm. Make sure to use the latest Fiji version. You can save the ROIs in the ROI manager window.


## Known possible issues and troubleshooting:
### 1-If prediction results make no sense at all and there are none or few caveolae predictios (black dots in the _PREDPLOT.svg image), try increasing windowsize parameter.
### 2-If too many caveolae central coordinates found:
-If excessive caveolae central coordinates are located in true caveolar areas, try increasing maximum or average caveolae size.
-If excessive caveolae central coordinates are located in erroneous, non caveolar areas, try uncreasing the confidence parameter (defined between 0-1)
### 3-If too few caveolar central coordiantes found:
-If central coordinates are not found, but caveolar predictions are detected (black dots in the _PREDPLOT.svg image) try decreasing the confidence parameter.
-If central coordinates are not found, and no caveolar predictions are detected (black dots in the _PREDPLOT.svg image) try increasing the windowsize or decreasing the displacement.

### Note: If you need to do some troubleshooting of the steps downstream image pseudosegmentation (changes in confidence parameter, change of min, average or max caveolae size to improve central coordinate finding) you can save time by setting the "mode" parameter to something different than "PREDICT".
