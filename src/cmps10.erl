%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Interface for reading the registers 
%%% @end
%%% Created :  5 Nov 2014 by Tony Rogvall <tony@rogvall.se>

-module(cmps10).

-export([read/3, write/3]).

-include_lib("i2c/include/i2c.hrl").

-ifndef(CMPS10_CHIP_ADDR).
-define(CMPS10_CHIP_ADDR, 16#60).
-endif.

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

