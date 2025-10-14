------------
A STEPWISE BACK-CORRECTION FUNCTION FOR PRECIPITATION REPRESENTATION ON HYDROLOGIC MODELS
------------

A precipitation Back Correction for SWAT - BaCo Tool v1.0

Authors : Dany A. Hernández, Jorge A. Guzmán, Sandra R. Villamizar, Maria L. Chu, Camila Ribeiro, Carlos R. de Mello

Date    : October 2025

------------
DESCRIPTION
------------

BaCo is an optimization tool designed to adjust precipitation representation in hydrological models based on simulated and observed streamflow. The methodology employs a stepwise, nonlinear correction function that systematically improves precipitation inputs while respecting the mass-conservation constraints of rainfall. It provides a means to address spatial and local uncertainty in precipitation data representation within SWAT or similar models.

------------
REQUIREMENTS
------------
Operating System: Windows

Software Required (Both files must be in the same folder):

    - BaCo.exe
    - sk4d.dll
      

Input Data Required:
1. The SWAT output directory (TxtInOut folder).
2. Observed streamflow file in .txt format:
   - The first column must be named "DATE" and contain consecutive numeric dates.
   - The following columns must contain observed discharge data, with each column named after the subbasin number (e.g., 1, 5, etc.).

    Example:

        DATE	1	5
        37622	3.030	1.948
        37623	3.596	1.334
        37624	3.681	1.000
        37625	2.682	0.852
        37626	3.030	0.767
        37627	2.517	0.714


3. A folder named “output” (or similar) where the tool will store generated result files.

------------
INSTRUCTIONS FOR USE
------------

1.  Launch BaCo.exe.

2.  INPUT DATA CONFIGURATION
     - Under the “MODEL” section:
       
       • Click the “SWAT Folder” button and select the main SWAT output folder (TxtInOut).
       • Click “Obs. Streamflow” and select the text file containing observed discharge data.
       • Click “reAnalysis Results” and choose (or create) an output folder where results will be saved.
       

3.  PARAMETER CONFIGURATION
     - In the “Alpha” and “Beta” sections, define:
       
         • The minimum and maximum range values.
         • The step size for each parameter.
       
     - Set the desired value for the PBT (precipitation balance threshold).
       

4.  MODEL EXECUTION
   
     - In the “REACH” panel, select the subbasin (reach) for which observed data are available.
     - Press the green circle to start the optimization process.
       

------------
RESULTS AND VISUALIZATION
------------

During the optimization, BaCo displays real-time monitoring of results through four main visual outputs:

1. Theoretical Plot
   
   Shows a scatterplot of simulated vs. observed discharge (green points) and the theoretical 1:1 line.

3. Metrics Plot
   
   Displays performance indicators per iteration:
   
   - PBIAS (yellow dots)
   - NSE (red line)
   - KGE (black line)
     
   allowing evaluation of model improvement over time.
   

5. Precipitation Plot
   
   Shows the temporal variability of the corrected precipitation.
   

7. Streamflow Plot
   
   Compares observed (black line) and simulated (orange line) streamflow series, highlighting the progressive adjustment achieved by the model.
   

------------
OUTPUT FILES
------------
The output folder will contain:

    - Updated precipitation files.
    - Performance metrics summary.
    - output.rch file
------------
