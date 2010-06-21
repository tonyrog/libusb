%%% File    : ftdi.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : FTDI interface
%%%
%%% On Mac OS X run:
%%%        sudo kextunload -b com.FTDI.driver.FTDIUSBSerialDriver
%%%        This will be fixed when detach_kernel_driver is fixed in libusb
%%%
%%% Created : 26 Mar 2010 by Tony Rogvall <tony@rogvall.se>

-module(ftdi).
-compile(export_all).

-include("libusb_drv.hrl").


-define(VID_FTDI,   16#0403).
-define(PID_CANUSB, 16#FFA8).

-define(FTDI_DEFAULT_EEPROM_SIZE, 128).

-define(FTDI_CHIP_TYPE_AM,    0).
-define(FTDI_CHIP_TYPE_BM,    1).
-define(FTDI_CHIP_TYPE_2232C, 2).
-define(FTDI_CHIP_TYPE_R,     3).
-define(FTDI_CHIP_TYPE_2232H, 4).
-define(FTDI_CHIP_TYPE_4232H, 5).

%% ftdi_set_line_property() 
-define(FTDI_PARITY_NONE,  0).
-define(FTDI_PARITY_ODD,   1).
-define(FTDI_PARITY_EVEN,  2).
-define(FTDI_PARITY_MARK,  3).
-define(FTDI_PARITY_SPACE, 4).

%% ftdi_set_line_property()
-define(FTDI_STOP_BIT_1,  0).
-define(FTDI_STOP_BIT_15, 1).
-define(FTDI_STOP_BIT_2,  2).

%% Number of bits for ftdi_set_line_property()
-define(FTDI_BITS_7, 7).
-define(FTDI_BITS_8, 8).

%% Break type for ftdi_set_line_property2() 
-define(FTDI_BREAK_OFF, 0).
-define(FTDI_BREAK_ON, 1).

%% MPSSE bitbang modes
%% < switch off bitbang mode, back to regular serial/FIFO
-define(FTDI_MPSSE_BITMODE_RESET, 16#00).
%% < classical asynchronous bitbang mode, introduced with B-type chips
-define(FTDI_MPSSE_BITMODE_BITBANG, 16#01).
%%  MPSSE mode, available on 2232x chips
-define(FTDI_MPSSE_BITMODE_MPSSE, 16#02).
%% < synchronous bitbang mode, available on 2232x and R-type chips
-define(FTDI_MPSSE_BITMODE_SYNCBB, 16#04).
%% < MCU Host Bus Emulation mode, available on 2232x chips 
-define(FTDI_MPSSE_BITMODE_MCU,    16#08).
%% CPU-style fifo mode gets set via EEPROM
%% < Fast Opto-Isolated Serial Interface Mode, available on 2232x chips
-define(FTDI_MPSSE_BITMODE_OPTO,   16#10).
%% < Bitbang on CBUS pins of R-type chips, configure in EEPROM before 
-define(FTDI_MPSSE_BITMODE_CBUS,   16#20).
%% < Single Channel Synchronous FIFO mode, available on 2232H chips
-define(FTDI_MPSSE_BITMODE_SYNCFF, 16#40).

%% Port interface for chips with multiple interfaces 

-define(FTDI_INTERFACE_ANY, 0).
-define(FTDI_INTERFACE_A,   1).
-define(FTDI_INTERFACE_B,   2).
-define(FTDI_INTERFACE_C,   3).
-define(FTDI_INTERFACE_D,   4).

%% Shifting commands IN MPSSE Mode
-define(MPSSE_WRITE_NEG, 16#01).   %% Write TDI/DO on negative TCK/SK edge
-define(MPSSE_BITMODE,   16#02).   %% Write bits, not bytes 
-define(MPSSE_READ_NEG,  16#04).   %% Sample TDO/DI on negative TCK/SK edge 
-define(MPSSE_LSB,       16#08).   %% LSB first 
-define(MPSSE_DO_WRITE,  16#10).   %% Write TDI/DO 
-define(MPSSE_DO_READ,   16#20).   %% Read TDO/DI 
-define(MPSSE_WRITE_TMS, 16#40).   %% Write TMS/CS 

%% FTDI MPSSE commands 
-define(SET_BITS_LOW,   16#80).
%%BYTE DATA
%%BYTE Direction
-define(SET_BITS_HIGH,  16#82).
%%BYTE DATA
%%BYTE Direction
-define(GET_BITS_LOW,   16#81
-define(GET_BITS_HIGH,  16#83
-define(LOOPBACK_START, 16#84
-define(LOOPBACK_END,   16#85
-define(TCK_DIVISOR,    16#86
%% Value Low 
%% Value HIGH  %%rate is 12000000/((1+value)*2) 
-define(DIV_VALUE(Rate),
	if Rate > 6000000 -> 0;
	   (6000000 div Rate - 1) > 16#ffff ->  16#ffff;
	   true -> (6000000 div /Rate - 1)
	end).

%% Commands in MPSSE and Host Emulation Mode 
-define(SEND_IMMEDIATE, 16#87).
-define(WAIT_ON_HIGH,   16#88).
-define(WAIT_ON_LOW,    16#89).

%% Commands in Host Emulation Mode 
-define(READ_SHORT,     16#90).
%% Address_Low 
-define(READ_EXTENDED,  16#91).
%% Address High 
%% Address Low  
-define(WRITE_SHORT,    16#92).
%% Address_Low 
-define(WRITE_EXTENDED, 16#93).
%% Address High 
%% Address Low  

%% Definitions for flow control 
-define(SIO_RESET,          16#00). %% Reset the port 
-define(SIO_MODEM_CTRL,     16#01). %% Set the modem control register 
-define(SIO_SET_FLOW_CTRL,  16#02). %% Set flow control register 
-define(SIO_SET_BAUD_RATE,  16#03). %% Set baud rate 
-define(SIO_SET_DATA,       16#04). %% Set the data characteristics of the port 

-define(FTDI_DEVICE_OUT_REQTYPE, (?LIBUSB_REQUEST_TYPE_VENDOR bor 
				  ?LIBUSB_RECIPIENT_DEVICE bor
				  ?LIBUSB_ENDPOINT_OUT)).
-define(FTDI_DEVICE_IN_REQTYPE, (?LIBUSB_REQUEST_TYPE_VENDOR bor 
				 ?LIBUSB_RECIP_DEVICE bor
				 ?LIBUSB_ENDPOINT_IN)).

%% Requests 
-define(SIO_RESET_REQUEST,             ?SIO_RESET).
-define(SIO_SET_BAUDRATE_REQUEST,      ?SIO_SET_BAUD_RATE).
-define(SIO_SET_DATA_REQUEST,          ?SIO_SET_DATA).
-define(SIO_SET_FLOW_CTRL_REQUEST,     ?SIO_SET_FLOW_CTRL).
-define(SIO_SET_MODEM_CTRL_REQUEST,    ?SIO_MODEM_CTRL).
-define(SIO_POLL_MODEM_STATUS_REQUEST, 16#05).
-define(SIO_SET_EVENT_CHAR_REQUEST,    16#06).
-define(SIO_SET_ERROR_CHAR_REQUEST,    16#07).
-define(SIO_SET_LATENCY_TIMER_REQUEST, 16#09).
-define(SIO_GET_LATENCY_TIMER_REQUEST, 16#0A).
-define(SIO_SET_BITMODE_REQUEST,       16#0B).
-define(SIO_READ_PINS_REQUEST,         16#0C).
-define(SIO_READ_EEPROM_REQUEST,       16#90).
-define(SIO_WRITE_EEPROM_REQUEST,      16#91).
-define(SIO_ERASE_EEPROM_REQUEST,      16#92).

-define(SIO_RESET_SIO, 0).
-define(SIO_RESET_PURGE_RX, 1).
-define(SIO_RESET_PURGE_TX, 2).

-define(SIO_DISABLE_FLOW_CTRL, 16#0).
-define(SIO_RTS_CTS_HS,  (16#1 bsl 8)).
-define(SIO_DTR_DSR_HS,  (16#2 bsl 8)).
-define(SIO_XON_XOFF_HS, (16#4 bsl 8)).

-define(SIO_SET_DTR_MASK, 16#1).
-define(SIO_SET_DTR_HIGH, ( 1 bor ( SIO_SET_DTR_MASK  bsl 8))).
-define(SIO_SET_DTR_LOW,  ( 0 bor ( SIO_SET_DTR_MASK  bsl 8))).
-define(SIO_SET_RTS_MASK, 16#2).
-define(SIO_SET_RTS_HIGH, ( 2 bor ( SIO_SET_RTS_MASK bsl 8 ))).
-define(SIO_SET_RTS_LOW,  ( 0 bor ( SIO_SET_RTS_MASK bsl 8 ))).

%%
%%    Poll modem status information
%%
%%    This function allows the retrieve the two status bytes of the device.
%%    The device sends these bytes also as a header for each read access
%%    where they are discarded by ftdi_read_data(). The chip generates
%%    the two stripped status bytes in the absence of data every 40 ms.
%%
%%    Layout of the first byte:
%%    - B0..B3 - must be 0
%%    - B4       Clear to send (CTS)
%%                 0 = inactive
%%                 1 = active
%%    - B5       Data set ready (DTS)
%%                 0 = inactive
%%                 1 = active
%%    - B6       Ring indicator (RI)
%%                 0 = inactive
%%                 1 = active
%%    - B7       Receive line signal detect (RLSD)
%%                 0 = inactive
%%                 1 = active
%%
%%    Layout of the second byte:
%%    - B0       Data ready (DR)
%%    - B1       Overrun error (OE)
%%    - B2       Parity error (PE)
%%    - B3       Framing error (FE)
%%    - B4       Break interrupt (BI)
%%    - B5       Transmitter holding register (THRE)
%%    - B6       Transmitter empty (TEMT)
%%    - B7       Error in RCVR FIFO
%%
%%    \param ftdi pointer to ftdi_context
%%    \param status Pointer to store status information in. Must be two bytes.
%%
%%    \retval  0: all fine
%%    \retval -1: unable to retrieve status information

%%
%%    Set flowcontrol for ftdi chip
%%
%%    \param ftdi pointer to ftdi_context
%%    \param flowctrl flow control to use. should be 
%%           SIO_DISABLE_FLOW_CTRL, SIO_RTS_CTS_HS, SIO_DTR_DSR_HS or SIO_XON_XOFF_HS   
%%
%%    \retval  0: all fine
%%    \retval -1: set flow control failed
%%
-record(ftdi,
	{
	  dev,
	  read_timeout = 5000,
	  write_timeout = 5000,
	  type = ?FTDI_CHIP_TYPE_BM,  %% chip type
	  baudrate = -1,
	  bitbang_enabled = 0,
	  bitbang_mode    = 1,
	  max_packet_size = 0,
	  interface = 0,
	  index = 0,
	  in_ep = 16#02,
	  out_ep = 16#81
	}).

set_interface(FTDI, ?FTDI_INTERFACE_ANY) ->
    FTDI;
set_interface(FTDI, ?FTDI_INTERFACE_A) ->
    FTDI;
set_interface(FTDI, ?FTDI_INTERFACE_B) ->
    FTDI#ftdi { interface = 1, index = ?FTDI_INTERFACE_B,
		in_ep = 16#04, out_ep = 16#83 };
set_interface(FTDI, ?FTDI_INTERFACE_C) ->
    FTDI#ftdi { interface = 2, index = ?FTDI_INTERFACE_C,
		in_ep = 16#06, out_ep = 16#85 };
set_interface(FTDI, ?FTDI_INTERFACE_D) ->
    FTDI#ftdi { interface = 3, index = ?FTDI_INTERFACE_D,
		in_ep = 16#08, out_ep = 16#87 }.

set_type(FTDI, D) when is_record(D, libusb_device_descriptor) ->
    case D#libusb_device_descriptor.bcdDevice of
	16#400 ->
	    FTDI#ftdi { type = ?FTDI_CHIP_TYPE_BM };
	16#200 when D#libusb_device_descriptor.iSerialNumber =:= 0 ->
	    FTDI#ftdi { type = ?FTDI_CHIP_TYPE_BM };
	16#200 ->
	    FTDI#ftdi { type = ?FTDI_CHIP_TYPE_AM };
	16#500 ->
	    FTDI#ftdi { type  = ?FTDI_CHIP_TYPE_2232C };
	16#600 ->
	    FTDI#ftdi { type = ?FTDI_CHIP_TYPE_R };
	16#700 ->
	    FTDI#ftdi { type = ?FTDI_CHIP_TYPE_2232H };
	16#800 ->
	    FTDI#ftdi { type = ?FTDI_CHIP_TYPE_4232H };
       true ->
	    FTDI
    end.

filter_ftdi() ->    
    [{vid,?VID_FTDI}].

filter_canusb() ->    
    [{vid,?VID_FTDI},{pid,?PID_CANUSB}].

i() -> usb:i(filter_ftdi()).
iv() -> usb:iv(filter_ftdi()).
ic() -> usb:ic(filter_ftdi()).
icv() -> usb:icv(filter_ftdi()).

devices() ->
    usb:device_list(filter_ftdi()).

canusb_devices() ->
    usb:device_list(filter_canusb()).

canusb_iv() ->
    usb:iv(filter_canusb()).

reset(FTDI) ->
    usb:contol_write(FTDI#ftdi.dev, ?FTDI_DEVICE_OUT_REQTYPE, 
		     ?SIO_RESET_REQUEST,
		     ?SIO_RESET_SIO, 
		     FTDI#ftdi.index, <<>>, FTDI#ftdi.write_timeout).

try_divisor(Divisor, TryDivisor, ChipType) ->
    if  TryDivisor =< 8 ->
	    8;
	ChipType =/= ?FTDI_CHIP_TYPE_AM, TryDivisor < 12 ->
	    12;
	Divisor < 16 ->
	    16;
	ChipType =:= ?FTDI_CHIP_TYPE_AM ->
	    Am_adjust_up = {0, 0, 0, 1, 0, 3, 2, 1},
	    TryDivisor1 = TryDivisor + 
		element((TryDivisor band 7)+1, Am_adjust_up),
	    if TryDivisor1 > 16#1FFF8 ->
		    16#1FFF8;
	        true ->
		    TryDivisor1
	    end;
	TryDivisor > 16#1FFFF ->
	    16#1FFFF;
	true ->
	    TryDivisor
    end.
	    


ftdi_convert_baudrate(Baudrate,_ChipType,_Index) when Baudrate =< 0 -> 
    {-1, 0, 0};
ftdi_convert_baudrate(Baudrate,ChipType,Index) ->
    Divisor0 = 24000000 div Baudrate,
    Divisor =
	if ChipType == ?FTDI_CHIP_TYPE_AM ->
		Am_adjust_dn = {0, 0, 0, 1, 0, 1, 2, 3},
		%% Round down to supported fraction (AM only)
		Divisor0 - element((Divisor0 band 7)+1, Am_adjust_dn);
	   true ->
		Divisor0
	end,

    TryDivisor1 = try_divisor(Divisor, Divisor, ChipType),
    Baud1 = (24000000 + (TryDivisor1 div 2)) div TryDivisor1,
    BaudDiff1 = abs(Baud1 - Baudrate),

    TryDivisor2 = try_divisor(Divisor, Divisor+1, ChipType),
    Baud2 = (24000000 + (TryDivisor2 div 2)) div TryDivisor2,
    BaudDiff2 = abs(Baud2 - Baudrate),

    {BestBaud,BestDivisor} =
	if BaudDiff1 < BaudDiff2 ->
		{Baud1,TryDivisor1};
	   true ->
		{Baud2,TryDivisor2}
	end,

    %% Encode the best divisor value
    FracCode = {0, 3, 2, 4, 1, 5, 6, 7},
    Encoded = (BestDivisor bsr 3) bor
	(element((BestDivisor band 7)+1, FracCode) bsl 14),
    EncodedDivisor =
	case Encoded of
	    0 -> 0;  %% 3000000 baud
	    16#4001 -> 1; %% 2000000 baud (BM only)
	    _ -> Encoded
	end,
    if ChipType =:= ?FTDI_CHIP_TYPE_2232C;
       ChipType =:= ?FTDI_CHIP_TYPE_2232H;
       ChipType =:= ?FTDI_CHIP_TYPE_4232H ->
	    {BestBaud,
	     EncodedDivisor band 16#FFFF,
	     ((EncodedDivisor bsr 8) band 16#FF00) bor Index};
       true ->
	    {BestBaud,
	     EncodedDivisor band 16#FFFF,
	     ((EncodedDivisor bsr 8) band 16#FFFF)}
    end.


open(Device) ->
    Iface = 0,
    case libusb:open(Device) of
	{ok,H} ->
	    case libusb:kernel_driver_active(H, Iface) of
		{ok, false} ->
		    case libusb:claim_interface(H, Iface) of
			ok -> {ok,H};
			Error ->
			    libusb:close(H),
			    Error
		    end;
		{ok, true} ->
		    {error, kernel_driver};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

			
		
    



