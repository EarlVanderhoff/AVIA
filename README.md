# AVIA
Anamorphic Video Information Analysis Controller

Two classes excerpted from greater AVIA project Controller application.
SorbelEdgeDetection - Histogram Equalization, Median Filtering, Sorbel Edge Detection - all performed discretely (no IPP), to render the vector map values accessible for custom analysis
Edgedetect - primarily a set of threadpooled subs and functions to enable multi-threaded, real time, intense number crunching

This project was rather massive, consisting of 3 distinct applications, running on independent, geographically remote PCs (compare video from New Jersey with video from Rhode Island - operate from Florida)
Enabling automated, long duration (4+ hour), real-time, HD, multi-input (line, stream, file), video quality analysis with operational fidelity 
Agnostic to differences between sources: frame rate, window/letter-boxing, resolution, aspect ratio, compression standard/depth, etc. (compare diverse video packaging/structures)
Fuzziness, freezing, tiling, hue, saturation, brightness, audio level, audio sync (to a single frame), etc,
HDMI, Composite, Component, SDI, most file types, streaming, SPTS/MPTS, adaptive bitrate.
The application precedes .dash and H.265
The full application engendered patents for low-motion precise synchronization, and color match weighting for moving video sources.
