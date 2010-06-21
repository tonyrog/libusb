%%% File    : usbids.erl
%%% Author  : Tony Rogvall <tony@rogvall.se>
%%% Description : USB pid/vid utils
%%% Created : 26 Mar 2010 by Tony Rogvall <tony@rogvall.se>

-module(usbids).


-compile(export_all).
-import(lists, [reverse/1]).


build() ->
    %% input file is located in priv/usb.ids
    %% output file will be located in $(HOME)/.usbids.dets
    Lib = code:priv_dir(libusb),
    SrcFile = filename:join(Lib, "usb.ids"),
    {ok,Fd} = file:open(SrcFile, [read]),
    DetsFile = filename:join(os:getenv("HOME"), ".usbids.dets"),
    {ok,Dets} = dets:open_file(usbids,[{file,DetsFile}]),
    Res = build(Fd, Dets),
    dets:close(Dets),
    Res.


open() ->
    DetsFile = filename:join(os:getenv("HOME"), ".usbids.dets"),
    dets:open_file(usbids,[{file,DetsFile},{access,read}]).

close() ->
    dets:close(usbids).

lookup_vid(Vid)     -> lookup_item('VID', Vid).
lookup_pid(Vid,Pid) -> lookup_item('VID',Vid,Pid).
lookup_class(Class) -> lookup_item('C', Class).
lookup_subclass(Class,SubClass) -> lookup_item('C',Class,SubClass).
lookup_protocol(Class,SubClass,Proto) -> lookup_item('C',Class,SubClass,Proto).
    
lookup_item(Tag, ID) -> lookup_name({Tag,ID}).
lookup_item(Tag, ID1,ID2) -> lookup_name({Tag,ID1,ID2}).
lookup_item(Tag, ID1,ID2,ID3) -> lookup_name({Tag,ID1,ID2,ID3}).

lookup_name(Key) ->
    try dets:lookup(usbids, Key) of
	[{_,Name}] ->
	    {ok,Name};
	[] ->
	    {error, not_found}
    catch
	error:_ ->
	    {error, no_service}
    end.    
    

%% Tag = 'VID'  vendor/product/interface  <name>
%%     = 'C'    class/subclass/protocol   <name>
%%     = 'AT'   terminal_type  <name>
%%     = 'HID'  descriptor_type <name>
%%     = 'R'    item_type <name>
%%     = 'BIAS' item_type <name>
%%     = 'PHY'  item_type <name>
%%     = 'HUT'  hi_page/hid_usage <name>
%%     = 'L'    language_id/dialect_id <name>
%%
%% Stored in dets like
%%     {{Tag,ID1},Name}
%%     {{Tag,ID1,ID2},Name}
%%     {{Tag,ID1,ID2,ID3},Name}
%% 
build(Fd, Dets) ->
    build(Fd, 1, undefined, [], Dets).

build(Fd, Ln, ParentTag, ParentIDs, Dets) ->
    case file:read_line(Fd) of
	{ok, Line} ->
	    io:format("scan: [~p]\n", [Line]),
	    case scan_line(Line) of
		comment ->
		    build(Fd, Ln+1, ParentTag, ParentIDs, Dets);
		empty ->
		    build(Fd, Ln+1, ParentTag, ParentIDs, Dets);
		{Tag,ID,Name} when is_atom(Tag) ->
		    dets:insert(Dets, {{Tag,ID}, Name}),
		    build(Fd, Ln+1, Tag, [ID], Dets);
		{ID2,1,Name} when is_integer(ID2) ->
		    case ParentIDs of
			[ID1|_] ->
			    dets:insert(Dets, {{ParentTag,ID1,ID2}, Name}),
			    build(Fd, Ln+1, ParentTag, [ID1,ID2], Dets)
		    end;
		{ID3,2,Name} when is_integer(ID3) ->
		    case ParentIDs of
			[ID1,ID2|_] ->
			    dets:insert(Dets, {{ParentTag,ID1,ID2,ID3}, Name}),
			    build(Fd, Ln+1, ParentTag, [ID1,ID2,ID3], Dets)
		    end;
		_Other ->
		    io:format("Error:~w: [~s]\n", [Ln,Line]),
		    build(Fd, Ln+1, ParentTag, ParentIDs, Dets)
	    end;
	eof ->
	    {ok,Ln}
    end.


%% # comment
%% <number> <data>
%% <tag> <number> <data>
%% <tab> <number> <data>
%% <tab> <tab> <number> <data>
%%
scan_line([$#|_Cs]) ->
    comment;
scan_line(Cs) ->
    case scan_item(Cs) of
	empty -> 
	    empty;
	{ID,0,Cs1} when is_integer(ID) ->
	    {'VID', ID, trim(Cs1)};
	{Tag,0,Cs1} when is_atom(Tag) ->
	    case scan_item(Cs1) of
		{ID,_,Cs2} when is_integer(ID) ->
		    {Tag, ID, trim(Cs2)}
	    end;
	{ID,Level,Cs1} when is_integer(ID) ->
	    {ID,Level,trim(Cs1)}
    end.

scan_item([$\n]) -> empty;
scan_item([$\r,$\n]) -> empty;
scan_item([$\r]) -> empty;
scan_item([$\t,$\t|Cs]) -> scan_item(Cs,2,[]);
scan_item([$\t|Cs])     -> scan_item(Cs,1,[]);
scan_item([C|Cs])       -> scan_item(Cs,0,[C]);
scan_item([]) -> empty.

scan_item([],_Level,[]) ->   empty;
scan_item([$\s|Cs],Level,Acc) ->
    {make_item(reverse(Acc),Level),Level,Cs};
scan_item([$\t|Cs],Level,Acc) ->
    {make_item(reverse(Acc),Level),Level,Cs};
scan_item([C|Cs],Level,Acc) ->
    scan_item(Cs,Level,[C|Acc]);
scan_item([],Level,Acc) ->
    {make_item(reverse(Acc),Level),Level,[]}.

make_item([$C], 0) -> %% special case
    'C';
make_item(Cs,Level) ->
    try erlang:list_to_integer(Cs,16) of
	Value -> Value
    catch 
	error:_ ->
	    if Level == 0 ->
		    list_to_atom(Cs);
	       true ->
		    Cs
	    end
    end.
	    

skip_ws([$\s|Cs]) -> skip_ws(Cs);
skip_ws([$\t|Cs]) -> skip_ws(Cs);
skip_ws([$\r|Cs]) -> skip_ws(Cs);
skip_ws([$\n|Cs]) -> skip_ws(Cs);
skip_ws(Cs) -> Cs.

trim(Cs) ->
    reverse(skip_ws(reverse(skip_ws(Cs)))).

    
    

