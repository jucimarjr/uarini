-module(medicoes).
-export([generate_data/1, time_microseg/0, bandwidth_calc/2]).

%%-----------------------------------------------------------------------------
%% bandwidth_calc(Data, Time)
%% calcula o tráfego na rede baseado no tamanho dos dados e quanto tempo levou 
%%
%% Data (binary) = os dados usados no teste
%% Time (µs)  = o tempo total do teste

bandwidth_calc(Data, Time) ->
    Megabytes = (size(Data) / math:pow(2, 20)),
    Seconds = (Time * 1.0e-6),
    Megabytes / Seconds.

%%-----------------------------------------------------------------------------
%% generate_data(Size)
%%   gera um dado de tamanho Size bytes
%%
%%   Size = integer

generate_data(Size) -> generate_data(Size, []).

generate_data(0, Bytes) ->
    list_to_binary(Bytes);
generate_data(Size, Bytes) ->
    generate_data(Size - 1, [1 | Bytes]).

%%-----------------------------------------------------------------------------
%% time_microseg()
%% captura o tempo atual em microsegundos

time_microseg() ->
    {MS, S, US} = now(),
    (MS * 1.0e+12) + (S * 1.0e+6) + US.
