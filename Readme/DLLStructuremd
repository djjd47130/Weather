# JD Weather DLL Structure

The JD Weather Library is based on a single file `JDWeather.dll` which controls the full weather system. This file has no knowledge of the actual services which use it. Instead, it dynamically loads all DLL's found in the same directory, and tests them each to identify if it's a weather serivce. If so, each of these DLL's is loaded.
