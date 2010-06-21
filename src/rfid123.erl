%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2010, Tony Rogvall
%%% @doc
%%%  MOD-RFID125 interface (HID Keyboard)
%%% @end
%%% Created : 24 May 2010 by Tony Rogvall <tony@rogvall.se>

-module(rfid123).

-export([i/0, iv/0, ic/0, icv/0]).

-compile(export_all).

devices() ->
    usb:device_list(filter()).

filter() ->
    [{vid,16#15BA},{pid,16#0010}].

test() ->
    libusb:start(),
    libusb:set_debug(3),
    [D] = devices(),
    {ok,H} = open(D),
    libusb:clear_halt(H, 16#81),
    read_loop(H, 10).


i() -> usb:i(filter()).
iv() -> usb:iv(filter()).
ic() -> usb:ic(filter()).
icv() -> usb:icv(filter()).

open(Device) ->
    Iface = 0,
    case libusb:open(Device) of
	{ok, H} ->
	    Res = libusb:set_configuration(H, 1),
	    io:format("set_configuration: result=~p\n", [Res]),
	    case claim(H, Iface) of
		ok ->
		    io:format("claimed interface\n"),
		    {ok,H};
		Error ->
		    libusb:close(H),
		    Error
	    end;
	Error ->
	    Error
    end.

claim(H, Iface) ->
    case libusb:claim_interface(H, Iface) of
	ok -> ok;
	_Error ->
	    io:format("Claim: error = ~p\n", [_Error]),
	    libusb:release_interface(H, Iface),
	    case libusb:claim_interface(H, Iface) of
	    	ok -> ok;
		Error1 ->
		    io:format("Claim1: error = ~p\n", [Error1]),
		    Error1
	    end
    end.

read_loop(_H, 0) ->
    ok;
read_loop(H, I) ->
    case libusb:interrupt_transfer_read(H, 16#81, 8, 1000) of
	{ok, AsyncRef} ->
	    receive 
		{libusb_transfer_completed, AsyncRef, H, Result} ->
		    io:format("libusb_transfer_completed\n", []),
		    {ok, Result};
		{libusb_transfer_error, AsyncRef, H, Reason} ->
		    io:format("libusb_transfer_error: ~p\n", [Reason]),
		    read_loop(H, I-1);
		Other ->
		    io:format("read_loop: got ~p\n", [Other]),
		    read_loop(H, I-1)
	    end;
	Error ->
	    io:format("no async ref: error=~p\n", [Error]),
	    Error
    end.

