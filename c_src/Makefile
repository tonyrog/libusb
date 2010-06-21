
EAPI_DIR = ../../eapi
include $(EAPI_DIR)/make/eapi_drv.mk
# FIXME config - require >= libusb-1.0 
ifeq ($(OSNAME), Linux)
LIBUSB_DIR=/usr
else
LIBUSB_DIR=/usr/local
endif

CFLAGS += -I $(EAPI_DIR)/include -I $(LIBUSB_DIR)/include/libusb-1.0
LDFLAGS += -L$(LIBUSB_DIR)/lib -lusb-1.0

LIBUSB_DRV = $(PRIVDIR)/libusb_drv.so
LIBUSB_DRV_OBJS = \
	$(OBJDIR)/$(EAPI_PFX)libusb_drv.o \
	$(OBJDIR)/$(EAPI_PFX)libusb_drv_impl.o

EAPI_LIB = $(EAPI_DIR)/lib/$(TYPE)/$(EAPI_PFX)eapi_drv.a

all:	debug release

debug:
	$(MAKE) driver TYPE=debug

release:
	$(MAKE) driver TYPE=release

clean: 
	$(MAKE) clean_internal TYPE=release
	$(MAKE) clean_internal TYPE=debug

driver: $(LIBUSB_DRV)

depend:

clean_internal:
	-rm -f $(OBJDIR)/$(EAPI_PFX)*.o
	-rm -f $(LIBUSB_DRV)

$(LIBUSB_DRV): $(LIBUSB_DRV_OBJS)
	$(LD_SHARED) -o $@ $(LIBUSB_DRV_OBJS) $(EAPI_LIB) $(LDFLAGS)

$(OBJDIR)/$(EAPI_PFX)libusb_drv.o: $(EAPI_DIR)/include/eapi_drv.h
$(OBJDIR)/$(EAPI_PFX)libusb_drv_impl.o: $(EAPI_DIR)/include/eapi_drv.h

$(OBJDIR)/$(EAPI_PFX)%.o: %.c
	$(CC) -c -o $@ $(CFLAGS) -I$(ERL_C_INCLUDE_DIR) $<
