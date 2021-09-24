# Binary Morphology

Simple plugin for performing morphological filtering on binary images. 

The classical implementations for morphological filtering are based on computation of minimum and
maximum values with 2D or 3D neighborhood. This may lead to long computation times, especially for
spherical structuring elements.

The implementations in this plugin focus on binary images. It is then possible to replace dilation 
and erosion operation by the computation of a distance map followed by a threshold.

The base configuration has few dependencies:

* ImageJ
* JUnit

## Installation

Simply add the jar file into the "plugins" directory, and restart ImageJ/Fiji. A new "Morphology"
menu will appear in the plugins directory, with the available options.
