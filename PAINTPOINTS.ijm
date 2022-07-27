dir="/home/myname/images/"; // your image directory, notice the / at the end

run("Results... ", "open=[" + dir + "CAVDFTOTAL.txt]");

selectWindow("Results");
numberOfPoints = getValue("results.count");
for (i = 0; i < numberOfPoints; i++) {
	XM = getResult("stitX", i);
	YM = getResult("stitY", i);
	pred = getResult("prediction", i);
	//print(pred);
	if (pred == 0) { 
		color = "magenta";
	}
	if (pred == 1) { 
		color = "cyan";
	}
	if (pred == 2) { 
		color = "yellow";
	}
	
	makePoint(XM, YM, color + " large dot" );
	roiManager("Add");
}

selectWindow("Results");
run("Close");

run("Results... ", "open=[" + dir + "_PMCORRECTED.txt]");
selectWindow("Results");
numberOfPoints = getValue("results.count");
for (i = 0; i < numberOfPoints; i=i+10) {
	XM = getResult("stitX", i);
	YM = getResult("stitY", i);	
	color="black";
	makePoint(XM, YM, color + " large dot" );
	roiManager("Add");
}

selectWindow("Results");
run("Close");

run("Results... ", "open=[" + dir + "_LDCORRECTED.txt]");
selectWindow("Results");
numberOfPoints = getValue("results.count");
for (i = 0; i < numberOfPoints; i=i+10) {
	XM = getResult("stitX", i);
	YM = getResult("stitY", i);	
	color="white";
	makePoint(XM, YM, color + " large dot" );
	roiManager("Add");
}

selectWindow("Results");
run("Close");