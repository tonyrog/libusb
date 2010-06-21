-ifndef(__PL2303_HRL__).
-define(__PL2303_HRL__, true).

-define(PROLIFIC_REV_H,         16#0202).
-define(PROLIFIC_REV_X,         16#0300).
-define(PROLIFIC_REV_HX_CHIP_D,	16#0400).
-define(PROLIFIC_REV_1,         16#0001).

-define(SET_LINE_REQUEST,      16#20).
-define(GET_LINE_REQUEST,      16#21).
-define(SET_CONTROL_REQUEST,   16#22).
-define(BREAK_REQUEST,         16#23).

-define(CONTROL_DTR, 16#01).
-define(CONTROL_RTS, 16#02).
-define(BREAK_ON,   16#ffff).
-define(BREAK_OFF,  16#0000).

-define(VENDOR_WRITE_REQUEST_TYPE,  16#40).  %% out, vendor, device
-define(VENDOR_WRITE_REQUEST,       16#01).

-define(VENDOR_READ_REQUEST_TYPE,   16#c0).  %% in, vendor, device
-define(VENDOR_READ_REQUEST,        16#01).

-define(VID_SIEMENS,              16#11f5).
-define(PID_SIEMENS_X65,          16#0003).

-define(VID_PROLIFIC,             16#067B).
-define(PID_PROLIFIC_2303,        16#2303).

-define(VID_UC_232A,              16#0557).
-define(PID_UC_232A,              16#2008).


-define(SET_DCR0,                       16#00).
-define(GET_DCR0,                       16#80).
-define(DCR0_INIT,                      16#01).
-define(DCR0_INIT_H,                    16#41).
-define(DCR0_INIT_X,                    16#61).

-define(SET_DCR1,                       16#01).
-define(GET_DCR1,                       16#81).
-define(DCR1_INIT_H,                    16#80).
-define(DCR1_INIT_X,                    16#00).

-define(SET_DCR2,                       16#02).
-define(GET_DCR2,                       16#82).
-define(DCR2_INIT_H,                    16#24).
-define(DCR2_INIT_X,                    16#44).

-define(RESET_DOWNSTREAM_DATA_PIPE,     16#08).
-define(RESET_UPSTREAM_DATA_PIPE,       16#09).

-define(PL2303_TYPE_UNKNOWN, 0).
-define(PL2303_TYPE_1,       1).
-define(PL2303_TYPE_REV_X,   2).
-define(PL2303_TYPE_REV_HX,  3).
-define(PL2303_TYPE_REV_H,   4).

-define(PL2303_IFO_REQUEST_TYPE, (?LIBUSB_REQUEST_TYPE_CLASS bor
				  ?LIBUSB_RECIPIENT_INTERFACE bor
				  ?LIBUSB_ENDPOINT_OUT)).

-define(PL2303_IFI_REQUEST_TYPE, (?LIBUSB_REQUEST_TYPE_CLASS bor
				  ?LIBUSB_RECIPIENT_INTERFACE bor
				  ?LIBUSB_ENDPOINT_IN)).

-define(PL2303_VNDO_REQUEST_TYPE, (?LIBUSB_REQUEST_TYPE_VENDOR bor
				   ?LIBUSB_RECIPIENT_DEVICE bor
				   ?LIBUSB_ENDPOINT_OUT)).
-define(PL2303_VNDI_REQUEST_TYPE, (?LIBUSB_REQUEST_TYPE_VENDOR bor
				   ?LIBUSB_RECIPIENT_DEVICE bor
				   ?LIBUSB_ENDPOINT_IN)).

-define(PL2303_WRITE_TIMEOUT, 1000).
-define(PL2303_READ_TIMEOUT,  1000).

-endif.

