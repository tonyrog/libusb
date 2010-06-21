MODULES = \
	libusb_drv \
	usb \
	usbids \
	pl2303 \
	ftdi \
	rfid123


EBIN = ../ebin
ERLC = erlc

include ../vsn.mk
VSN=$(LIBUSB_VSN)

APP_NAME= libusb
APP_FILE= $(APP_NAME).app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

override ERLC_FLAGS = 

OBJS = $(MODULES:%=$(EBIN)/%.beam)

TARGET_FILES = $(OBJS) $(APP_TARGET)

debug: ERLC_FLAGS += -Ddebug 

all: $(TARGET_FILES)

all32: $(TARGET_FILES)

debug: all

release: all

depend:
	edep -MM -o ../ebin $(ERLC_FLAGS) $(MODULES:%=%.erl) > depend.mk

dialyze:
	dialyzer --src -o dia.out $(ERLC_FLAGS) -c $(MODULES:%=%.erl) 


edoc:
	erl -noshell -pa doc -run edoc_run application "libusb" '".."' '[{private,true},no_packages,{sort_functions,false}]'

clean:
	rm -f $(OBJS)

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	sed -e 's;%VSN%;$(VSN);'  $< > $@

-include depend.mk

$(EBIN)/%.beam:	%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN) $<
