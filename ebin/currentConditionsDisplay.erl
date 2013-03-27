-module(currentConditionsDisplay).

-export([display/1, new/1, update/4, new/1]).

-implements([observer, displayElement]).

display(ObjectID) ->
    io:format("Current conditions: ~p F degrees and "
	      "~p % humidity ~n",
	      [ooe:lookup_attr(ObjectID, 'Temperature'),
	       ooe:lookup_attr(ObjectID, 'Humidity')]).

update(ObjectID, Temperature, Humidity, Pressure) ->
    ooe:update_attr(ObjectID, 'Temperature', Temperature),
    ooe:update_attr(ObjectID, 'Humidity', Humidity),
    display(ObjectID).

new(WeatherData) ->
    ObjectID = ooe:new([{'Humidity', []},
			{'Temperature', []}, {'WeatherData', []}]),
    ooe:update_attr(ObjectID, 'WeatherData', WeatherData),
    Temp = ooe:lookup_attr(ObjectID, 'WeatherData'),
    (erlang:element(1,
		    Temp)):register_observer(erlang:element(2, Temp),
					     ooe:lookup_attr(ObjectID,
							     'WeatherData')),
    {currentConditionsDisplay, ObjectID}.

