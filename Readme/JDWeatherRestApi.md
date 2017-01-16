# JD Weather REST API

### About

1. Allows developers remote access to the same weather library via HTTP
2. Developers register for a single account, and then input their specific service keys
  1. All API calls use the key generated for the JD Weather API
  2. All service-specific keys are individually registered under main user's account
3. Ability to merge weather data from multiple services into one output

### Creating Account

**NOTE** There is currently no ability for a developer to create an account. Please contact the owner of this repository if you wish to gain access.

### Registering Service Keys

**NOTE** There is currently no ability for a developer to register keys. Please contact the owner of this repository if you wish to gain access.



## How to Use

### URL Format

All API calls are made to the following endpoint:

`api.weather.jdsoftwareinc.com:8664/[YOUR_API_KEY]/`

This is followed by one or more specific request(s). The response can include the result of multiple requests at once:

`api.weather.jdsoftwareinc.com:8664/[YOUR_API_KEY]/[REQUEST_ONE]/[REQUEST_TWO]/?s=[SERVICES]`

### Requests

The following are the different values you may pass for a request:

1. `Services` - Returns a list of all available weather services.
2. `Support` - Returns detailed supported properties for given service.
3. `Conditions` - Returns the current observed conditions.
4. `Alerts` - Returns the current weather alerts.
5. `ForecastSummary` - Returns a summarized forecast.
6. `ForecastHourly` - Returns an hourly forecast.
7. `ForecastDaily` - Returns a daily forecast.
8. `Maps` - Returns map layer data

### Query Parameters

The following are the different query parameters you may pass with a request:

1. `s` [REQUIRED] - Name of service or comma-separated list of service names.
  1. Example of single service: `wunderground`
  2. Example of multiple services: `wunderground,accuweather,foreca`
2. `l` [OPTIONAL] - Method of location lookup
  1. `ip` [DEFAULT] - Automatically by IP Address
  2. `zip` - Lookup by Zip / Postal Code
    1. Pass zip code in `l1` parameter
  3. `coords` - Lookup by Coordinates
    1. Pass Latitude in `l1` parameter
    2. Pass Longitude in `l2` parameter
  4. `citystate` - City and State
    1. Pass City in `l1` parameter
    2. Pass State in `l2` parameter
  5. `citycountry` - City and Country
    1. Pass City in `l1` parameter
    2. Pass Country in `l2` parameter
  6. `airport` - Airport Code
    1. Pass Airport Code in `l1` parameter
3. `l1` [CONDITIONAL] - Location Detail 1
4. `l2` [CONDITIONAL] - Location Detail 2
5. `units` [OPTIONAL] - Unit of Measurement
  1. `imperial` [DEFAULT] - Imperial System (Farenheit, Miles, Feet...)
  2. `metric` - Metric System (Celcius, Kilometers, Meters...)
  3. `kelvin` - Kelvin System (Kelvin, Kilometers, Meters...)



## Requests

### Request: Services

Returns an array of all the available weather services.

`api.weather.jdsoftwareinc.com:8664/[YOUR_API_KEY]/Services`

| Property      | Type   | Meaning                       |
| ------------- |--------| ------------------------------|
| caption       | string | User-friendly name of service |
| name          | string | Internal name of service      |
| uid           | string | Unique identifier of service  |
| url_main      | string | Main URL of service website   |
| url_api       | string | URL of Service API Docs       |
| url_register  | string | URL of Service Registration   |
| url_login     | string | URL of Service Login          |
| url_legal     | string | URL of Service Legal Info     |

### Request: Support

Returns multiple arrays of each information type and which properties are supported for each.

`api.weather.jdsoftwareinc.com:8664/[YOUR_API_KEY]/Support/?s=[SERVICE]`

| Property                         | Type         | Meaning                               |
| ---------------------------------|--------------| --------------------------------------|
| caption                          | string       | User-friendly name of service         |
| name                             | string       | Internal name of service              |
| uid                              | string       | Unique identifier of service          |
| supported_units                  | string array | Supported Units of Measure            |
| supported_locations              | string array | Supported Location Lookup Types       |
| supported_info                   | string array | Supported Information Types           |
| supported_conditions             | string array | Supported Condition Properties        |
| supported_alert_types            | string array | Supported Alert Types                 |
| supported_alert_props            | string array | Supported Alert Peroperties           |
| supported_forecasts              | string array | Supported Forecast Types              |
| supported_forecast_summary_props | string array | Supported Forecast Summary Properties |
| supported_forecast_hourly_props  | string array | Supported Forecast Hourly Properties  |
| supported_forecast_daily_props   | string array | Supported Forecast Daily Properties   |


### Request: Conditions

Returns the current weather conditions for a given location.

`api.weather.jdsoftwareinc.com:8664/[YOUR_API_KEY]/Conditions/?s=[SERVICES]&l=[LOC]&l1=[LOC1]&l2=[LOC2]&u=[UNITS]`

| Property      | Type     | Meaning                                 |
| ------------- |----------| --------------------------------------- |
| timestamp     | datetime | The date / time of observation          |
| caption       | string   | Short description of weather conditions |
| station       | string   | Specific station of observation         |

### Request: Alerts



### Request: ForecastSummary



### Request: ForecastHourly



### Request: ForecastDaily



### Request: Maps

Returns maps containing the desired map layers.

**NOTE**: This request **CANNOT** be combined with other types of requests, as it does not return JSON or XML data.

