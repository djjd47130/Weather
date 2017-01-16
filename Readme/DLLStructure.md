# JD Weather DLL Structure

The JD Weather Library is based on a single file `JDWeather.dll` which controls the full weather system. This file has no knowledge of the actual services which use it. Instead, it dynamically loads all DLL's found in the same directory, and tests them each to identify if it's a weather serivce. If so, each of these DLL's is loaded.

Each service DLL must be exclusively structured to work with this library. Everything is based on interfaces, so the first step is to create an interface structure matching the requirements. Only one function is exported in the DLL - this function simply creates a new instance of the weather service, and returns such interface.

The service DLL's are NOT to be used directly. Although technically they can, this is not the intention. Instead, in a similar manner as described previously, a single function returns an interface from `JDWeather.dll` which provides detailed listing of each available service.
