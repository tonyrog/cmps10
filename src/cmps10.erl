%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Interface for reading the registers 
%%% @end
%%% Created :  5 Nov 2014 by Tony Rogvall <tony@rogvall.se>

-module(cmps10).

-export([read/3, write/3]).
-export([version/1]).
-compile(export_all).

-include_lib("i2c/include/i2c.hrl").

-ifndef(CMPS10_CHIP_ADDR).
-define(CMPS10_CHIP_ADDR, 16#60).
-endif.

read0(Bus) ->
    [{version, version(Bus)},
     {bearing0, compass_bearing0(Bus)},
     {bearing1, compass_bearing1(Bus)},
     {pitch, pitch_angle(Bus)},
     {roll, roll_angle(Bus)},
     {magnometer, 
      {magnometer_x(Bus),
       magnometer_y(Bus),
       magnometer_z(Bus)}},
     {accelerometer,
      {accelerometer_x(Bus),
       accelerometer_y(Bus),
       accelerometer_z(Bus)}}].

read(Bus) ->
    case read(Bus, 0, 22) of
	{ok, <<>>,
	 <<Version,Bearing0,
	   Bearing1:16,
	   Pitch:8/signed,
	   Roll:8/signed,
	   _:4/binary,
	   MagX:16/signed,MagY:16/signed,MagZ:16/signed,
	   AccX:16/signed,AccY:16/signed,AccZ:16/signed
	   >>} ->
	    [{version, Version},
	     {bearing0, (Bearing0/256)*360},
	     {bearing1, (Bearing1/10)},
	     {pitch, Pitch},
	     {roll, Roll},
	     {magnometer, {MagX,MagY,MagZ}},
	     {accelerometer, {AccX,AccY,AccZ}}]
    end.

version(Bus) ->    
    case read(Bus, 0, 1) of
	{ok, [<<>>, <<Version>>]} -> Version
    end.

compass_bearing0(Bus) ->
    case read(Bus, 1, 1) of
	{ok, [<<>>, <<Bearing0>>]} -> (Bearing0/256)*360
    end.

compass_bearing1(Bus) ->
    case read(Bus, 2, 2) of
	{ok, [<<>>, <<Bearing:16>>]} -> Bearing/10
    end.

pitch_angle(Bus) ->
    case read(Bus, 4, 1) of
	{ok, [<<>>, <<Pitch:8/signed>>]} -> Pitch
    end.

roll_angle(Bus) ->
    case read(Bus, 5, 1) of
	{ok, [<<>>, <<Roll:8/signed>>]} -> Roll
    end.

magnometer_x(Bus) ->
    case read(Bus, 10, 2) of
	{ok, [<<>>, <<Raw:16/signed>>]} -> Raw
    end.

magnometer_y(Bus) ->
    case read(Bus, 12, 2) of
	{ok, [<<>>, <<Raw:16/signed>>]} -> Raw
    end.

magnometer_z(Bus) ->
    case read(Bus, 14, 2) of
	{ok, [<<>>, <<Raw:16/signed>>]} -> Raw
    end.

accelerometer_x(Bus) ->
    case read(Bus, 16, 2) of
	{ok, [<<>>, <<Raw:16/signed>>]} -> Raw
    end.

accelerometer_y(Bus) ->
    case read(Bus, 18, 2) of
	{ok, [<<>>, <<Raw:16/signed>>]} -> Raw
    end.

accelerometer_z(Bus) ->
    case read(Bus, 20, 2) of
	{ok, [<<>>, <<Raw:16/signed>>]} -> Raw
    end.


read(Bus, Addr, Size) when is_integer(Addr), Addr >= 0,
			   is_integer(Size), Size >= 0, Size =< 16#ffff ->
    A = ?CMPS10_CHIP_ADDR,
    i2c:rdwr(Bus, [#i2c_msg {addr=A,flags=[],len=1,data=(<<Addr:8>>)},
		   #i2c_msg {addr=A,flags=[rd],len=Size,data=(<<>>)}]).

write(Bus, Addr, Data) when is_binary(Data), is_integer(Addr), Addr >= 0 ->
    A = ?CMPS10_CHIP_ADDR,
    Size = byte_size(Data),
    i2c:rdwr(Bus, [#i2c_msg{addr=A,flags=[],len=1+Size,
			    data=(<<Addr:16,Data/binary>>)}]).

