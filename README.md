# Hantek-DSO-2090
Minimal Lazarus/Free Pascal Interface to a Hantek DSO-2090 Digital Oscilloscope.  

This program is written using Lazarus/Free Pascal for Linux, and provides a minimal GUI to control the Hantek DSO-2090 Digital Oscilloscope.  It is intended to help others get started with creating their own software for the DSO-2090. 

Note that this program is _not_ a full-featured oscilloscope GUI for the 2090, and assumes that you have a program like OpenHantek (available here at GitHub) installed on your Linux PC.  This is necessary since the OpenHantek program (or similar) handles the process of UPLOADING some firmware from your PC to the DSO hardware each time you plug it in to a USB port. If you have OpenHantek set up and working, you have this upload requirement covered.

OpenHantek supports multiple Hantek 'scope models and offers many features.  But as a result, it's a complex C++ program.  The code here is an attempt to minimize that complexity and present something easier to understand for the Pascal programmer so that (s)he can write programs that take advantage of the DSO-2090 capabilities. 

Unit "main" contains the GUI for the 'scope hardware.  It calls the "dsoControl" and "display" units.  
Unit "dsoControl" declares all the variables, constants and procedures available to control the 'scope.
Unit "dsoUSB" contains the 'scop-specific procedures to communicate with the DSO-2090.  
Unit "libusb" a Pascal wrapper for the standard USB library.
Unit "Display" handles analysis and display of the data acquired from the 'scope.  This is where you will 
	want to do much of your own special program development.  The unit here simply 	presents the acquired
 	data in a string grid.  This unit has its own form.  In the string grid, column 0 is the data index, column 1
	is the data from channel one and column 2 is the data from 'scope channel two.
	column 

In general, the relationship between the units looks something like this:

		---> Display 
		|
		|
	main ---------> dsoControl -----> dsoUSB -----> libusb

All of the "magic numbers" that are used in the dsoControl and dsoUSB units have been extracted from the OpenHantek
code.

DSOTest is the compiled program, with debug enabled so it is big.
