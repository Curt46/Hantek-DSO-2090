# Hantek-DSO-2090
Minimal Lazarus/Free Pascal Interface to a Hantek DSO-2090 Digital Oscilloscope.  

This program is written using Lazarus/Free Pascal for Linux, and provides a minimal GUI to control the Hantek DSO-2090 Digital Oscilloscope.  It is intended to help others get started with creating their own software for the DSO-2090. 

Note that this program is _not_ a full-featured oscilloscope GUI for the 2090, and assumes that you have a program like OpenHantek (available here at GitHub) installed on your Linux PC.  This is necessary since the OpenHantek program (or similar) handles the process of UPLOADING some firmware from your PC to the DSO hardware each time you plug it in to a USB port. If you have OpenHantek set up and working, you have this upload requirement covered.

OpenHantek supports multiple Hantek 'scope models and offers many features.  But as a result, it's a complex C++ program.  The code here is an attempt to minimize that complexity and present something easier to understand for the Pascal programmer so that (s)he can write programs of their own that take advantage of the DSO-2090 capabilities. 


