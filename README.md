# The-1000-Soil-ICR-Metagenome
This repository contains the codes used for preparing the figures featured in the 1000 Soil Metagenome manuscript.

There are two sub-folders in this repository, one for the R scripts to generate Figs. 1,2,4,5 and 6 and the Python scripts that were used to pre-process the output of the flux-balanace analysis. The output of the Python script was then inserted into the R script for Fig. 6.

**Execution of the scripts**

The R scripts for Figs 1, 4, 5, and 6 start with lines resembling "set(/path_to_data)." Update these lines to reflect the paths where the input files are located.
For the R script for Fig. 2, ensure that both the code and the folders containing the sample data reside in the same directory.

The names of the Python scripts begin with 'number_,' indicating the sequence of their execution. Each Python script is accompanied by a configuration file (*.config). Modify the lines in the configuration file to specify the full paths for both the input and output files.




