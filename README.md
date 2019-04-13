This is a prerelease of version rv4_32 of the emep ctm model.
Please do not distribute further. 

You must have MPI and netcdf4 fortran libraries installed. Then edit the Makefile to adapt it to your compiler and netcdf library path, and then just

 > make

If you do not have them already, you can download the (old) input data with:

 > ./catalog.py --input

The metdata for 2015 (EECCA domain) with:

 > ./catalog.py -Y 2015 -m --met-domain EECCA

(You can stop the download after one month, if you just want to test)

See also https://github.com/metno/emep-ctm/tree/tools for more info on the use of catalog.py

Some of the input data must be updated for this version. You can move it to the older directory:

 > cp -r inp_rv4_32_beta EMEP_MSC-W_model.rv4.17.OpenSource/input/


Now you should be ready to run the model
 > mpirun emepctm

User guide at:
https://emep-ctm.readthedocs.io/en/latest//index.html

(Note that with the new version is not compatible with the old FMI ship emissions. A new file will be provided)

