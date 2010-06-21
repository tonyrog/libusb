%%
%% PL2303 usb driver
%%
-module(pl2303).

-include("libusb_drv.hrl").
-include("pl2303.hrl").

-export([set_configuration/5, get_configuration/1]).
-export([set_control/3]).
-export([set_break/2]).
-export([devices/0]).
-export([open/1]).
-export([i/0, iv/0, ic/0, icv/0]).

-export([test_write/0, test_read/0]).

test_write() ->
    [Device|_] = devices(),
    {ok,Handle} = open(Device),
    Res = usb:bulk_write(Handle, 16#02, <<"hello world\r\n">>, 1000),
    io:format("Write: ~p\n", [Res]),
    {ok,Handle}.

test_read() ->
    [_,Device|_] = devices(),
    {ok,Handle} = open(Device),
    Res = usb:bulk_read(Handle, 16#83, 10, 10000),
    io:format("Read: ~p\n", [Res]),
    {ok,Handle}.



filter() ->
    {any,
     [{all,[{vid, ?VID_PROLIFIC},{pid, ?PID_PROLIFIC_2303}]},
      {all,[{vid, ?VID_UC_232A},{pid,?PID_UC_232A}]}]}.

devices() ->
    usb:device_list(filter()).

i() -> usb:i(filter()).
iv() -> usb:iv(filter()).
ic() -> usb:ic(filter()).
icv() -> usb:icv(filter()).


open(Device) ->
    case libusb:get_device_descriptor(Device) of
	{ok, D} ->
	    ChipType = 
		case D#libusb_device_descriptor.bcdDevice of
		    ?PROLIFIC_REV_H -> ?PL2303_TYPE_1;
		    ?PROLIFIC_REV_X -> ?PL2303_TYPE_REV_HX;
		    ?PROLIFIC_REV_HX_CHIP_D -> ?PL2303_TYPE_REV_HX;
		    ?PROLIFIC_REV_1 -> ?PL2303_TYPE_1;
		    _ -> ?PL2303_TYPE_UNKNOWN
		end,
	    io:format("pl2303: ChipType=~w\n", [ChipType]),
	    case libusb:open(Device) of
		{ok, Handle} ->
		    ok = libusb:set_configuration(Handle, 1),
		    ok = libusb:reset_device(Handle),
		    init_chip(Handle, ChipType),
		    set_configuration(Handle, 9600, 1, 0, 8),
		    {ok, Handle};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

%% Handle is a open control handle
%% Baud is baud rate in bauds:
%%   (75,150,300,600,1200,1800,2400,3600,4800,9600,19200,38400,
%%    57600,115200,230400,460800)
%%  StopBit = 0  (1 stop bit)
%%            1  (1.5 stop bit)
%%            2  (2 stop bits)
%%  Parity = 0 (none)
%%         = 1 (odd)
%%         = 2 (even)
%%         = 3 (mark)
%%         = 4 (space)
%% CharLength = 5,6,7,8
%%
set_configuration(Handle, Baud, StopBits, Parity, CharLength) ->
    write_request(Handle, ?SET_LINE_REQUEST, 0, 0,
		  <<Baud:32/little, StopBits, Parity, CharLength>>).

get_configuration(Handle) ->
    case read_request(Handle, ?GET_LINE_REQUEST, 0, 0, 7) of
	{ok, <<Baud:32/little, StopBits, Parity, CharLength>>} ->
	    {ok, [Baud, StopBits, Parity, CharLength]};
	Error ->
	    Error
    end.

%% Dtr = 0, 1
%% Rts = 0, 1
set_control(Handle, Dtr, Rts) ->			      
    Value = Dtr bor (Rts bsl 1),
    write_request(Handle, ?SET_CONTROL_REQUEST, Value, 0).

%% On=true, Off=false
set_break(Handle, On) ->
    Value = if On -> ?BREAK_ON;
	       true -> ?BREAK_OFF
	    end,
    write_request(Handle, ?BREAK_REQUEST, Value, 0).

%% Magic? init
init_chip(Handle,Type) ->    
    read_vendor(Handle,  16#8484, 0),
    write_vendor(Handle, 16#0404, 0),
    read_vendor(Handle,  16#8484, 0),
    read_vendor(Handle,  16#8383, 0),
    read_vendor(Handle,  16#8484, 0),
    write_vendor(Handle, 16#0404, 1),
    read_vendor(Handle,  16#8484, 0),
    read_vendor(Handle,  16#8383, 0),
    %%	write_vendor (Handle, 16#81, 1);
    write_vendor(Handle, 0, 1),
    write_vendor(Handle, 1, 0),
    if Type == ?PL2303_TYPE_REV_HX ->
	    write_vendor(Handle, 2, 16#44),
	    %% reset upstream data pipes
	    write_vendor(Handle, 8, 0),
	    write_vendor(Handle, 9, 0);	    
       true ->
	    write_vendor(Handle, 2, 16#24)
    end.

%%
%% Write interface/class
%%
write_request(Handle, Request, Value, Index) ->
    write_request(Handle, Request, Value, Index, <<>>).

write_request(Handle, Request, Value, Index, Data) ->
    io:format("pl2303: write request ~w, ~w, ~w, ~w\n", 
	      [Request, Value, Index, Data]),
    usb:control_write(Handle, ?PL2303_IFO_REQUEST_TYPE,
		      Request, Value, Index, Data, 
		      ?PL2303_WRITE_TIMEOUT).

read_request(Handle, Request, Value, Index, Length) ->
    io:format("pl2303: read request ~w, ~w, ~w, ~w\n", 
	      [Request, Value, Index, Length]),
    usb:control_read(Handle, ?PL2303_IFI_REQUEST_TYPE,
		     Request,
		     Value, Index, Length, ?PL2303_READ_TIMEOUT).
    


%%
%% Write device/vendor 
%%
write_vendor(Handle, Value, Index) ->
    io:format("write_vendor: ~w, ~w\n", [Value, Index]),
    Res = usb:control_write(Handle, ?PL2303_VNDO_REQUEST_TYPE,
			    ?VENDOR_WRITE_REQUEST,
			    Value, Index, <<>>,
			    ?PL2303_WRITE_TIMEOUT),
    io:format("write_vendor: result = ~p\n", [Res]),
    Res.

%%
%% Read device/vendor 
%%
read_vendor(Handle, Value, Index) ->
    io:format("read_vendor: ~w, ~w\n", [Value, Index]),
    Res = usb:control_read(Handle, ?PL2303_VNDI_REQUEST_TYPE,
			   ?VENDOR_READ_REQUEST,
			   Value, Index, 1,
			   ?PL2303_READ_TIMEOUT),
    io:format("read_vendor: result = ~p\n", [Res]),
    Res.    
		      
		      


