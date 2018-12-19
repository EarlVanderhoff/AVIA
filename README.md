# AVIA
Anamorphic Video Information Analysis - Controller

Two classes from AVIA Controller application...

SorbelEdgeDetection: Primarily Histogram Equalization, Median Filtering, and Sorbel Edge Detection - all performed discretely (no Integrated Performance Primatives), to render the vector map values accessible for custom analysis

Edgedetect: Primarily a set of threadpooled subs and functions to enable multi-threaded, real time, intense number crunching

This project was rather massive, consisting of 3 distinct applications, running on independent, geographically remote PCs. Automated, long duration, real-time, HD, video quality analysis from line, stream or file, with operational fidelity.

Agnostic to differences in frame rate, window/letter-boxing, resolution, aspect ratio, compression standard/depth, etc.

PSNR, AVIA MSE, Fuzziness, freezing, tiling, hue, saturation, brightness, audio level, audio sync to a frame), etc.

HDMI, Composite, Component, SDI, most file types, streaming, SPTS/MPTS, adaptive bitrate.The application precedes .dash and H.265

The full application engendered patents for low-motion precise synchronization, and color match weighting for moving video sources.

Functional, though often lacking needed descriptors and elegance. I have much to learn. 
