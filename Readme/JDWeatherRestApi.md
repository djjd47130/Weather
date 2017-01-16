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

### Using API

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

### Query Parameters

The following are the different query parameters you may pass with a request:

1. `s` [REQUIRED] - Name of service or comma-separated list of service names.
2. `l` [OPTIONAL] - Method of location lookup
  1. `ip` - Automatically by IP Address
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

