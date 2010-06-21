%%
%% USB utilities
%%
-module(usb).

-compile(export_all).
-import(lists, [foreach/2, foldl/3]).

-include("libusb.hrl").

-export([i/0]).

-ifdef(debug).
-define(dbg(Fmt,As), io:format("usb: "++Fmt++"\n", As)).
-else.
-define(dbg(Fmt,As), ok).
-endif.

start() ->
    libusb_drv:start().

%% Sparse info
i()       -> info([], []).
i(Filter) -> info(Filter, []).

%% Verbose info
iv()       -> info([], [verbose]).
iv(Filter) -> info(Filter, [verbose]).

%% Sparse info & config
ic() ->  info([], [config]).
ic(Filter) -> info(Filter, [config]).

%% Verbose info & config
icv() -> info([], [verbose,config]).
icv(Filter) -> info(Filter, [verbose,config]).

%%
%% Pick up all info and present it
%%
info(Filter, Opts) ->
    usbids:open(),
    each_device_descriptor(
      fun(Device,D) -> 
	      i_device_descriptor(Device,D,Opts),
	      case proplists:get_bool(config, Opts) of
		  true ->
		      N = D#libusb_device_descriptor.bNumConfigurations,
		      each_config_descriptor(
			fun(I,C) ->
				i_config_descriptor(Device,C,I,Opts)
			end, Device, N);
		  false ->
		      ok
	      end
      end,
      Filter).


%% Info for single device
i_device(Device) ->
    i_device(Device, []).

i_device(Device, Opts) ->
    case libusb_drv:get_device_descriptor(Device) of
	{ok,D} ->
	    usbids:open(),
	    i_device_descriptor(Device, D, Opts);
	_Error ->
	    %% warning?
	    ignore
    end.

%% Display config use {index,I} for specific index
i_config(Device) ->
    i_config(Device,[]).
i_config(Device,Opts) ->
    usbids:open(),
    case libusb_drv:get_device_descriptor(Device) of
	{ok,D} ->
	    case proplists:get_value(index, Opts) of
		undefined ->
		    N = D#libusb_device_descriptor.bNumConfigurations,
		    each_config_descriptor(
		      fun(I,C) ->
			      i_config_descriptor(Device,C,I,Opts)
		      end, Device, N);
		active ->
		    case libusb_drv:get_active_config_descriptor(Device) of
			{ok,C} ->
			    i_config_descriptor(Device, C, active, Opts);
			Error ->
			    Error
		    end;
		Index ->
		    case libusb_drv:get_config_descriptor(Device, Index) of
			{ok,C} ->
			    i_config_descriptor(Device, C, Index, Opts);
			Error ->
			    Error
		    end
	    end;
	Error ->
	    Error
    end.

%% Apply Fun on filtered device list
each_device(Fun, Filter) ->
    foreach(Fun, device_list(Filter)).

each_device_descriptor(Fun, Filter) ->
    foreach(fun(Device) ->
		    case libusb_drv:get_device_descriptor(Device) of
			{ok,D} ->
			    Fun(Device, D);
			_ ->
			    ignore
		    end
	    end, device_list(Filter)).

each_config_descriptor(Fun, Device, N) ->
    foreach(
      fun(I) ->
	      case libusb_drv:get_config_descriptor(Device, I) of
		  {ok,C} ->
		      Fun(I,C);
		  _ ->
		      ignore
	      end
      end, lists:seq(0, N-1)).


device_list() ->
    device_list([]).

device_list(Filter) ->
    case libusb_drv:get_device_list() of
	{ok, DeviceList} ->
	    lists:filter(fun(Device) ->
				 filter_device(Device, Filter)
			 end, DeviceList);
	_Error ->
	    []
    end.

%%
%% Filter a device  (FIXME allow OR AND NOT )
%%  {vid,Vid} 
%%  {pid,Pid}
%%  {bus,BusID}
%%  {address,Addr}
%%  {manufacturer, SubString}
%%

filter_device(_Device, []) -> true;
filter_device(Device, Filter) ->
    case libusb_drv:get_device_descriptor(Device) of
	{ok,D} ->
	    filter_device_expr(Device, D, Filter);
	_Error ->
	    false
    end.

filter_device_expr(Device, D, List) when is_list(List) ->
    filter_device_expr(Device, D, {all,List});
filter_device_expr(Device, D, {all,List}) ->
    lists:all(fun(Attr) ->
		      filter_device_expr(Device,D,Attr)
	      end, List);
filter_device_expr(Device, D, {any,List}) ->
    lists:any(fun(Attr) ->
		      filter_device_expr(Device,D,Attr)
	      end, List);
filter_device_expr(Device, D, {none,List}) ->
    lists:all(fun(Attr) ->
		      not filter_device_expr(Device,D,Attr)
	      end, List);
filter_device_expr(Device, D, Av={_,_}) ->
    filter_device_attr(Device, D, Av).


filter_device_attr(Device, D, Attr) ->
    case Attr of
	{vid,Vid} ->
	    D#libusb_device_descriptor.idVendor == Vid;
	{pid,Pid} ->
	    D#libusb_device_descriptor.idProduct == Pid;
	{bus,ID} ->
	    case libusb_drv:get_bus_number(Device) of
		{ok,ID} -> true;
		_ -> false
	    end;
	{address,Addr} ->
	    case libusb_drv:get_device_address(Device) of
		{ok,Addr} -> true;
		_ -> false
	    end;
	{manufacturer,ID} ->
	    case get_string(Device,D#libusb_device_descriptor.iManufacturer) of
		{ok,Manufacturer} when is_list(ID) ->
		    case re:run(Manufacturer, ID) of
			{match, _} -> true;
			_ -> false
		    end;
		_ -> false
	    end;
	_ -> %% unkown
	    true
    end.



i_device_descriptor(Device, D, Opts) ->    
    case proplists:get_bool(verbose, Opts) of
	true ->
	    i_device_descriptor_verbose(Device, D);
	false ->
	    i_device_descriptor_sparse(Device, D)
    end.
	
%% main descriptor info
i_device_descriptor_sparse(Device, D) ->    
    Vid = D#libusb_device_descriptor.idVendor,
    Pid = D#libusb_device_descriptor.idProduct,
    IProd = D#libusb_device_descriptor.iProduct,
    case usbids:lookup_pid(Vid,Pid) of
	{ok,ProdName} ->
	    io:format("u-~s: vid=~s, pid=~s\n",
		      [ProdName,format_uint16(Vid),format_uint16(Pid)]);
	_ when IProd == 0 ->
	    io:format("Unknown: vid=~s, pid=~s\n",
		      [format_uint16(Vid),format_uint16(Pid)]);
	_ ->
	    with_device(
	      Device,
	      fun(Handle) ->	    
		      case get_string_descriptor_unicode(Handle,IProd) of
			  {ok,Product} ->
			      io:format("p~s: vid=~s, pid=~s\n",
					[Product,format_uint16(Vid),
					 format_uint16(Pid)]);
			  _ ->
			      io:format("Unknown: vid=~s, pid=~s\n",
					[format_uint16(Vid),
					 format_uint16(Pid)])
		      end
	      end)
    end.

i_device_descriptor_verbose(Device, D) ->
    io:format("USB device: ~s\n", [format_handle(Device)]),
    {ok,Bus} = libusb_drv:get_bus_number(Device),
    io:format("  Bus Number: ~s\n", [format_uint8(Bus)]),
    {ok,Addr} = libusb_drv:get_device_address(Device),
    io:format("  Address: ~s\n", [format_uint8(Addr)]),
    io:format("  USB Version: ~s\n", 
	      [format_version(D#libusb_device_descriptor.bcdUSB)]),
    Class = D#libusb_device_descriptor.bDeviceClass,
    case usbids:lookup_class(Class) of
	{ok,CName} when Class > 0 ->
	    io:format("  Class[~s]: ~s\n", 
		      [format_uint8(Class),CName]);
	_ ->
	    io:format("  Class: ~s\n", 
		      [format_uint8(Class)])
    end,
    SubClass = D#libusb_device_descriptor.bDeviceSubClass,
    case usbids:lookup_subclass(Class,SubClass) of
	{ok,SName} ->
	    io:format("  Subclass[~s]: ~s\n", 
		      [format_uint8(SubClass),SName]);
	_ ->
	    io:format("  Subclass: ~s\n", 
		      [format_uint8(SubClass)])
    end,
    Proto = D#libusb_device_descriptor.bDeviceProtocol,
    case usbids:lookup_protocol(Class,SubClass,Proto) of
	{ok,PName} ->
	    io:format("  Protocol[~s]: ~s\n",  
		      [format_uint8(Proto),PName]);
	_ ->
	    io:format("  Protocol: ~s\n",  
		      [format_uint8(Proto)])
    end,
    
    io:format("  MaxPacketSize0: ~w\n",
	      [D#libusb_device_descriptor.bMaxPacketSize0]),
    Vid = D#libusb_device_descriptor.idVendor,
    Pid = D#libusb_device_descriptor.idProduct,
    case usbids:lookup_pid(Vid,Pid) of
	{ok,ProdName} ->
	    io:format("  Product ID: ~s (~s)\n", 
		      [format_uint16(Pid),ProdName]);
	_ ->
	    io:format("  Product ID: ~s\n",  [format_uint16(Pid)])
    end,
    case usbids:lookup_vid(Vid) of
	{ok,VendName} ->
	    io:format("  Vendor ID: ~s (~s)\n", 
		      [format_uint16(Vid), VendName]);
	_ ->
	    io:format("  Vendor ID: ~s\n", 
		      [format_uint16(Vid)])
    end,
    io:format("  Version: ~s\n", 
	      [format_version(D#libusb_device_descriptor.bcdDevice)]),
    
    #libusb_device_descriptor { iManufacturer=IManuf,
				iProduct=IProd,
				iSerialNumber=ISerial } = D,
    with_device(Device,
		fun(Handle) ->
			case get_string_descriptor_unicode(Handle,IManuf) of
			    {ok,Manufacturer} ->
				io:format("  Manufacturer[~w]: ~p\n",
					  [IManuf,Manufacturer]);
			    _ when IManuf == 0 ->
				ok;
			    _Err1 ->
				io:format("  iManufacturer: ~w\n", [IManuf])
			end,
			case get_string_descriptor_unicode(Handle,IProd) of
			    {ok,Product} ->
				io:format("  Product[~w]: ~p\n", 
					  [IProd,Product]);
			    _ when IProd == 0 ->
				ok;
			    _Err2 ->
				io:format("  iProduct: ~w\n", [IProd])
			end,
			case get_string_descriptor_unicode(Handle,ISerial) of
			    {ok,Serial} ->
				io:format("  SerialNumber[~w]: ~p\n", 
					  [ISerial,Serial]);
			    _ when ISerial == 0 ->
				ok;
			    _Err3 ->
				io:format("  iSerialNumber: ~w\n", [ISerial])
			end
		end),
    
    io:format("  NumConfigurations: ~w\n", 
	      [D#libusb_device_descriptor.bNumConfigurations]),
    io:format("\n").


%% Display info about a config descriptor
i_config_descriptor(Device, C, Index, _Opts) ->
    io:format("Config: ~w\n", [Index]),
    print_config_descriptor(C, Device).

print_config_descriptor(C, Device) ->
    io:format("ConfigurationValue: ~w\n",
	      [C#libusb_config_descriptor.bConfigurationValue]),
    IConfig = C#libusb_config_descriptor.iConfiguration,
    case get_string(Device,IConfig) of
	{ok,Config} ->
	    io:format("Configuration[~s]: ~s\n", 
		      [format_uint8(IConfig), Config]);
	_Error ->
	    io:format("iConfiguration: ~s\n",
		      [format_uint8(IConfig)])
    end,
    BMAttr = C#libusb_config_descriptor.bmAttributes,
    Attrs = if BMAttr band 16#40 =:= 16#40 -> [self_powered];
	       true -> [] 
	    end ++
	    if BMAttr band 16#20 =:= 16#20 -> [remote_wakeup];
	       true -> [] 
	    end,
    io:format(" Attributes: ~s: ~p\n", [format_uint8(BMAttr),Attrs]),
    io:format("   MaxPower: ~wmA\n", [2*C#libusb_config_descriptor.'MaxPower']),
    foldl(fun(Alt, I) ->
		  io:format(" Interface: ~w\n", [I]),
		  print_alt(Alt, Device),
		  I+1
	  end, 0, C#libusb_config_descriptor.interface).

print_alt(Alt, Device) ->
    foldl(fun(IF, IFi) ->
		 io:format("    Alternativ: ~w\n", [IFi]),
		 print_interface(IF, Device)
	 end, 0, Alt#libusb_interface.altsetting).

print_interface(IF, Device) ->
    INum = IF#libusb_interface_descriptor.bInterfaceNumber,
    IAlt = IF#libusb_interface_descriptor.bAlternateSetting,
    io:format("    Interface Number: ~w\n", [INum]),
    io:format("    Alternate Setting: ~w\n", [IAlt]),
    Class = IF#libusb_interface_descriptor.bInterfaceClass,
    case usbids:lookup_class(Class) of
	{ok,CName} when Class > 0 ->
	    io:format("    Class[0x~2.16.0B]: ~s\n", [Class,CName]);
	_ ->
	    io:format("    Class: 0x~2.16.0B\n", [Class])
    end,
    SubClass = IF#libusb_interface_descriptor.bInterfaceSubClass,
    case usbids:lookup_subclass(Class,SubClass) of
	{ok,SName} ->
	    io:format("    Subclass[0x~2.16.0B]: ~s\n", [SubClass,SName]);
	_ ->
	    io:format("    Subclass: 0x~2.16.0B\n", [SubClass])
    end,
    Proto = IF#libusb_interface_descriptor.bInterfaceProtocol,
    case usbids:lookup_protocol(Class,SubClass,Proto) of
	{ok,PName} ->
	    io:format("    Protocol[0x~2.16.0B]: ~s\n",  [Proto,PName]);
	_ ->
	    io:format("    Protocol: 0x~2.16.0B\n",  [Proto])
    end,
    IInterface = IF#libusb_interface_descriptor.iInterface,
    case get_string(Device,IInterface) of
	{ok,Interface} ->
	    io:format("    Interface[0x~2.16.0B]: ~s\n", [IInterface, Interface]);
	_Error ->
	    io:format("    iInterface: 0x~2.16.0B\n", [IInterface])
    end,	
    foldl(fun(E,Ei) ->
		  io:format("    EndPoint: ~w\n", [Ei]),
		  print_endpoint(E),
		  Ei+1
	  end, 0, IF#libusb_interface_descriptor.endpoint).


print_endpoint(E) ->
    Addr = E#libusb_endpoint_descriptor.bEndpointAddress,
    Dir = 
	if (Addr band ?LIBUSB_ENDPOINT_IN) =:= ?LIBUSB_ENDPOINT_IN ->
		"IN";
	   true -> 
		"OUT"
	end,
    io:format("      Address: ~s ~s\n", 
	      [Dir, format_uint8(Addr)]),
    Type = case E#libusb_endpoint_descriptor.bmAttributes band 16#3 of
	       2#00 -> "Control";
	       2#01 -> "Isochrounous";
	       2#10 -> "Bulk";
	       2#11 -> "Interrupt"
	   end,
    io:format("      Attributes: ~s 0x~2.16.0B\n", [Type, E#libusb_endpoint_descriptor.bmAttributes]),
    io:format("      MaxPacketSize: ~w\n", [E#libusb_endpoint_descriptor.wMaxPacketSize]),
    io:format("      Interval: ~w\n", [E#libusb_endpoint_descriptor.bInterval]),
    io:format("      Refresh: ~w\n", [E#libusb_endpoint_descriptor.bRefresh]),
    io:format("      SynchAddress: ~w\n", [E#libusb_endpoint_descriptor.bSynchAddress]).
    

    
format_uint8(I) ->
    io_lib:format("0x~2.16.0B", [I]).

format_uint16(I) ->
    io_lib:format("0x~4.16.0B", [I]).

format_handle(H) ->
    format_uint32(H).

format_uint32(I) ->
    io_lib:format("0x~8.16.0B", [I]).


format_version(BCD) ->
    erlang:integer_to_list(BCD bsr 8, 16) ++ "." ++
	erlang:integer_to_list((BCD bsr 4) band 16#f, 16) ++
	if (BCD band 16#f) == 0 -> "";
	   true -> "." ++ erlang:integer_to_list(BCD band 16#f, 16)
	end.
		 
get_langid_list(Device) ->
    with_device(Device,
		fun(Handle) ->
			get_lang_descriptor(Handle)
		end).

get_string(Device, Index) ->
    with_device(Device,
		fun(Handle) ->
			get_string_descriptor_unicode(Handle, Index)
		end).

get_string(Device, Index, LangId) ->
    with_device(Device,
		fun(Handle) ->
			get_string_descriptor_unicode(Handle, Index, LangId)
		end).

with_device(Device, Fun) ->
    case libusb_drv:open(Device) of
	{ok,Handle} ->
	    Res = (catch Fun(Handle)),
	    libusb_drv:close(Handle),
	    Res;
	Error -> Error
    end.

get_string_descriptor_unicode(_Handle, 0) ->
    {error, invalid_param};
get_string_descriptor_unicode(Handle, Index) ->
    case get_lang_descriptor(Handle) of
	{ok, [LangId|_]} -> %% only first language used
	    get_string_descriptor_unicode(Handle, Index, LangId);
	Error ->
	    Error
    end.

get_string_descriptor_unicode(_Handle, 0, _LangId) ->
    {error, invalid_param};
get_string_descriptor_unicode(Handle, Index, LangId) ->
    case get_string_descriptor(Handle, Index, LangId) of
	{ok, <<_, ?LIBUSB_DT_STRING, Str/binary>>} ->
	    {ok, [X || <<X/utf16-little>> <= Str]};
	{ok, _} ->
	    {error, io};
	Error -> Error
    end.

get_lang_descriptor(Handle) ->
    case get_string_descriptor(Handle, 0, 0) of
	{ok, <<_, ?LIBUSB_DT_STRING, Langs/binary>>} ->
	    {ok, [X || <<X:16/little>> <= Langs]};
	{ok, _} ->
	    {error, io};
	Error -> Error
    end.

get_string_descriptor(Handle, Index, LangId) ->
    control_read(Handle,
		 ?LIBUSB_ENDPOINT_IN,
		 ?LIBUSB_REQUEST_GET_DESCRIPTOR,
		 (?LIBUSB_DT_STRING bsl 8) bor Index,
		 LangId, 255, 1000).


control_read(Handle, RequestType, Request, Value, Index, Length, Timeout) ->
    case libusb_drv:control_transfer_read(Handle, RequestType, Request, 
				      Value, Index, Length, Timeout) of
	{ok, AsyncRef} ->
	    transfer_wait(Handle, AsyncRef);
	Error ->
	    Error
    end.

control_write(Handle, RequestType, Request, Value, Index, Data, Timeout) ->
    case libusb_drv:control_transfer_write(Handle, RequestType, Request, 
				       Value, Index, Data, Timeout) of
	{ok, AsyncRef} ->
	    transfer_wait(Handle, AsyncRef);
	Error ->
	    Error
    end.

bulk_read(Handle, EndPoint, Length, Timeout) ->
    case libusb_drv:bulk_transfer_read(Handle, EndPoint, Length, Timeout) of
	{ok, AsyncRef} ->
	    transfer_wait(Handle, AsyncRef);
	Error ->
	    Error
    end.


bulk_write(Handle, EndPoint, Data, Timeout) ->
    case libusb_drv:bulk_transfer_write(Handle, EndPoint, Data, Timeout) of
	{ok, AsyncRef} ->
	    transfer_wait(Handle, AsyncRef);
	Error ->
	    Error
    end.

interrupt_read(Handle, EndPoint, Length, Timeout) ->
    case libusb_drv:interrupt_transfer_read(Handle, EndPoint, Length, Timeout) of
	{ok, AsyncRef} ->
	    transfer_wait(Handle, AsyncRef);
	Error ->
	    Error
    end.


interrupt_write(Handle, EndPoint, Data, Timeout) ->
    case libusb_drv:interrupt_transfer_write(Handle, EndPoint, Data, Timeout) of
	{ok, AsyncRef} ->
	    transfer_wait(Handle, AsyncRef);
	Error ->
	    Error
    end.

transfer_wait(Handle, AsyncRef) ->
    ?dbg("transfer_wait: ~p ref=~p", [Handle, AsyncRef]),
    receive
	{libusb_transfer_completed, AsyncRef, Handle, Result} ->
	    ?dbg("libusb_transfer_completed", []),
	    {ok, Result};
	{libusb_transfer_error, AsyncRef, Handle, Reason} ->
	    ?dbg("libusb_transfer_error: ~p", [Reason]),
	    {error, Reason}
%%	Other ->
%%	    ?dbg("transfer_wait: got ~p", [Other]),
%%	    {error, bad_reply}
    end.

