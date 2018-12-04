# Work Flow Data Competition
A Weight-Selection Strategy for training Deep Neural Networks for Imbalanced Classification

## Initial Data Parsing
Turn list mode into integrated data

### Method
Take list mode data and integrate it into 4 or 8 Hertz spectral data files.  These files will then be used as input in the "Continued Data Parsing" below.

###Scripts
*	data_parsing.R

### Input & Output
*	main input is the raw list mode data:
**	the "raw" list mode data energy & time delta by photon
**	C:\Users\mooreet_la\projects\SDRD\competition\data\trainingData\listData
**	C:\Users\mooreet_la\projects\SDRD\competition\data\testingData\listData 
*	The file naming is different for the two input sets.
*	Main output is the 4 or 8 Hertz data:
**	Integrated spectra by channels (or other feature) in bins
**	.\competition\data\trainingData\integrated_32Ch25s  (quarter second)
**	.\competition\data\trainingData\integrated_32Ch125s (8th second)
**	Same for 'testingData'

## Continued Data Parsing
Build the training, testing, & validation sets

### Method
### Scripts
### Output

## Model Building

## Notes
1.	Naming convention is that the data set given to us as "training data" is called training (model building) & testing (model building testing).  And the "testing data" is called validation data, used to score the methods.


<sup>1</sup>


