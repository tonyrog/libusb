//
// Implementation of USB driver API
//
#include <poll.h>
#include "libusb.h"
#include "eapi_drv.h"
#include "libusb_api.h"

ErlDrvTermData am_libusb_transfer_completed;
ErlDrvTermData am_libusb_transfer_error;
ErlDrvTermData am_error;
ErlDrvTermData am_timeout;
ErlDrvTermData am_cancelled;
ErlDrvTermData am_stall;
ErlDrvTermData am_no_device;
ErlDrvTermData am_overflow;
ErlDrvTermData am_other;

struct _libusb_drv_ctx_t;

typedef struct _transfer_data_t
{
    struct _transfer_data_t* next;
    struct _transfer_data_t* prev;
    struct _libusb_drv_ctx_t* dctx;
    struct libusb_transfer *transfer;

    ErlDrvTermData caller;  // process to receive notifications
    uint32_t       ref;     // async reference value
    ErlDrvBinary*  bin;     // binary used for read operations
} transfer_data_t;

typedef struct _libusb_drv_ctx_t
{
    libusb_context*  usb_ctx;
    transfer_data_t* t_first;
    transfer_data_t* t_last;
    eapi_ctx_t*      ctx;     // back link to eapi context
} libusb_drv_ctx_t;

#define LIBUSB_DRV_CONTEXT(ctx) ((libusb_drv_ctx_t*)(ctx)->user_data)
#define LIBUSB_CONTEXT(ctx) (LIBUSB_DRV_CONTEXT(ctx)->usb_ctx)

ErlDrvEntry libusb_drv_entry;

static void libusb_drv_fd_added_cb(int fd, short events, void *user_data)
{
    eapi_ctx_t* ctx = (eapi_ctx_t*) user_data;

    EAPI_DRV_DBG("fd_added_cb fd=%d, mask=%04x", fd, events);
    if ((events & (POLLIN|POLLOUT)) == (POLLIN|POLLOUT))
	driver_select(ctx->port,(ErlDrvEvent)fd, ERL_DRV_READ|ERL_DRV_WRITE,1);
    else if (events & POLLIN)
	driver_select(ctx->port,(ErlDrvEvent)fd, ERL_DRV_USE|ERL_DRV_READ, 1);
    else if (events & POLLOUT)
	driver_select(ctx->port,(ErlDrvEvent)fd, ERL_DRV_WRITE, 1);
}

static void libusb_drv_fd_removed_cb(int fd, void* user_data)
{
    eapi_ctx_t* ctx = (eapi_ctx_t*) user_data;

    EAPI_DRV_DBG("fd_removed fd=%d", fd);
    driver_select(ctx->port,(ErlDrvEvent)fd, ERL_DRV_READ|ERL_DRV_WRITE, 0);
}

static void libusb_drv_ready_input(ErlDrvData d, ErlDrvEvent e)
{
    eapi_ctx_t* ctx = (eapi_ctx_t*) d;
    struct timeval timeout;
    (void) e;
    EAPI_DRV_DBG("ready_input fd=%d", (int) e);
    // lock ? should not be needed with only one driver instance
    timeout.tv_sec = 0;
    timeout.tv_usec = 0;
    libusb_handle_events_timeout(LIBUSB_CONTEXT(ctx), &timeout);
    EAPI_DRV_DBG("ready_input done fd=%d", (int) e);
}

static void libusb_drv_ready_output(ErlDrvData d, ErlDrvEvent e)
{
    eapi_ctx_t* ctx = (eapi_ctx_t*) d;
    struct timeval timeout;
    (void) e;
    EAPI_DRV_DBG("ready_output fd=%d", (int) e);
    // lock ? should not be needed with only one driver instance
    timeout.tv_sec = 0;
    timeout.tv_usec = 0;
    libusb_handle_events_timeout(LIBUSB_CONTEXT(ctx), &timeout);
    EAPI_DRV_DBG("ready_output done fd=%d", (int) e);
}


// Called from edrv_start
static void libusb_drv_impl_init(ErlDrvData d)
{
    libusb_drv_ctx_t* dctx;
    eapi_ctx_t* ctx = (eapi_ctx_t*) d;

    EAPI_DRV_DBG("libusb_drv_impl_init, ctx=%p", ctx);
    dctx = (libusb_drv_ctx_t*) malloc(sizeof(libusb_drv_ctx_t));

    libusb_init(&dctx->usb_ctx);
    dctx->t_first   = 0;
    dctx->t_last    = 0;
    dctx->ctx       = ctx;

    ctx->user_data = dctx; // back link

    am_libusb_transfer_completed = driver_mk_atom("libusb_transfer_completed");
    am_libusb_transfer_error = driver_mk_atom("libusb_transfer_error");
    am_error = driver_mk_atom("error");
    am_timeout = driver_mk_atom("timeout");
    am_cancelled = driver_mk_atom("cancelled");
    am_stall = driver_mk_atom("stall");
    am_no_device = driver_mk_atom("no_device");
    am_overflow = driver_mk_atom("overflow");
    am_other = driver_mk_atom("other");

    libusb_set_pollfd_notifiers(dctx->usb_ctx,
				libusb_drv_fd_added_cb,
				libusb_drv_fd_removed_cb,
				ctx);
}

// Called from edrv_stop
static void libusb_drv_impl_final(ErlDrvData d)
{
    eapi_ctx_t* ctx = (eapi_ctx_t*) d;
    libusb_drv_ctx_t* dctx = (libusb_drv_ctx_t*) ctx->user_data;
    transfer_data_t* ptr;

    EAPI_DRV_DBG("final, ectx=%p\r\n", ctx);
    
    // loop and cancel all transfers
    ptr = dctx->t_first;
    while(ptr) {
	// FIXME: transfer_cancel (no side effect)
	ptr = ptr->next;
    }
    libusb_exit(dctx->usb_ctx);
    free(dctx);
}

DRIVER_INIT(libusb_drv)
{
    eapi_driver_init(&libusb_drv_entry, 
		     libusb_drv_impl_init,
		     libusb_drv_impl_final);
    libusb_drv_entry.driver_name = "libusb_drv";
    libusb_drv_entry.ready_input = libusb_drv_ready_input;
    libusb_drv_entry.ready_output = libusb_drv_ready_output;
    return (ErlDrvEntry*) &libusb_drv_entry;
}

static void put_libusb_error(eapi_ctx_t* ctx, cbuf_t* c_out, int err)
{
    (void) ctx;
    switch(err) {
    case LIBUSB_SUCCESS: 
	eapi_put_error(c_out, "success"); break;
    case LIBUSB_ERROR_IO:
	eapi_put_error(c_out, "io"); break;
    case LIBUSB_ERROR_INVALID_PARAM: 
	eapi_put_error(c_out, "invalid_param"); break;
    case LIBUSB_ERROR_ACCESS: 
	eapi_put_error(c_out, "access"); break;
    case LIBUSB_ERROR_NO_DEVICE: 
	eapi_put_error(c_out, "no_device"); break;
    case LIBUSB_ERROR_NOT_FOUND:
	eapi_put_error(c_out, "not_found"); break;
    case LIBUSB_ERROR_BUSY:
	eapi_put_error(c_out, "busy"); break;
    case LIBUSB_ERROR_TIMEOUT:
	eapi_put_error(c_out, "timeout"); break;
    case LIBUSB_ERROR_OVERFLOW:
	eapi_put_error(c_out, "overflow"); break;
    case LIBUSB_ERROR_PIPE:
	eapi_put_error(c_out, "pipe"); break;
    case LIBUSB_ERROR_INTERRUPTED:
	eapi_put_error(c_out, "interrupted"); break;
    case LIBUSB_ERROR_NO_MEM:
	eapi_put_error(c_out, "no_mem"); break;
    case LIBUSB_ERROR_NOT_SUPPORTED:
	eapi_put_error(c_out, "not_supported"); break;
    case LIBUSB_ERROR_OTHER:
	eapi_put_error(c_out, "other"); break;
    default: 
	eapi_put_error(c_out, "unknown"); break;
    }
}

// #define CONFIG_DEBUG
#ifdef CONFIG_DEBUG

#define print_field(f, t, fmt, ptr, name)			\
    fprintf((f), "%*s %s: " fmt "\r\n", (t), "", #name, (ptr)->name)

#define print_message(f, t, fmt, arg) \
    fprintf((f), "%*s " fmt "\r\n", (t), "", (arg))

static void print_endpoint(FILE* f,int t,const struct libusb_endpoint_descriptor *p)
{
    print_field(f, t, "%u", p, bLength);
    print_field(f, t, "%u", p, bDescriptorType);
    print_field(f, t, "%u", p, bEndpointAddress);
    print_field(f, t, "%u", p, bmAttributes);
    print_field(f, t, "%u", p, wMaxPacketSize);
    print_field(f, t, "%u", p, bInterval);
    print_field(f, t, "%u", p, bRefresh);
    print_field(f, t, "%u", p, bSynchAddress);
    print_field(f, t, "%p", p, extra);
    print_field(f, t, "%u", p, extra_length);
}

static void print_interface_descriptor(FILE* f,int t,const struct libusb_interface_descriptor* p)
{
    int i;

    print_field(f, t, "%u", p, bLength);
    print_field(f, t, "%u", p, bDescriptorType);
    print_field(f, t, "%u", p, bInterfaceNumber);
    print_field(f, t, "%u", p, bAlternateSetting);
    print_field(f, t, "%u", p, bNumEndpoints);
    print_field(f, t, "%u", p, bInterfaceClass);
    print_field(f, t, "%u", p, bInterfaceSubClass);
    print_field(f, t, "%u", p, bInterfaceProtocol);
    print_field(f, t, "%u", p, iInterface);
    for (i = 0; i < p->bNumEndpoints; i++) {
	print_message(f, t, "BEGIN Endpoint: %d {", i);
	print_endpoint(f, t+2, &p->endpoint[i]);
	print_message(f, t, "%s", "}");
    }
    print_field(f, t, "%p", p, extra);
    print_field(f, t, "%u", p, extra_length);
}

static void print_interface(FILE* f, int t, const struct libusb_interface* p)
{
    int i;

    print_message(f, t, "BEGIN Settings %s", "{");
    t += 2;
    print_field(f, t, "%u", p, num_altsetting);
    for (i = 0; i < p->num_altsetting; i++) {
	print_message(f, t, "BEGIN Interface: %d {", i);
	print_interface_descriptor(f, t+2, &p->altsetting[i]);
	print_message(f, t, "%s", "}");
    }
}

static void print_config_descriptor(FILE* f, struct libusb_config_descriptor* p)
{
    int t = 2;
    int i;

    print_message(f, 0, "BEGIN Configuration %s", "{");
    print_field(f, t, "%u", p, bLength);
    print_field(f, t, "%u", p, bDescriptorType);
    print_field(f, t, "%u", p, wTotalLength);
    print_field(f, t, "%u", p, bNumInterfaces);
    print_field(f, t, "%u", p, bConfigurationValue);
    print_field(f, t, "%u", p, iConfiguration);
    print_field(f, t, "%u", p, bmAttributes);
    print_field(f, t, "%u", p, MaxPower);
    for (i = 0; i < p->bNumInterfaces; i++) {
	print_message(f, t+2, "BEGIN Interface: %d {", i);
	print_interface(f, t+2, &p->interface[i]);
	print_message(f, t+2, "%s", "}");
    }
    print_field(f, t, "%p", p, extra);
    print_field(f, t, "%u", p, extra_length);
    print_message(f, 0, "%s", "}");
}
#endif

void libusb_drv_impl_set_debug(eapi_ctx_t* ctx, cbuf_t* c_out, int level)
{
    libusb_set_debug(LIBUSB_CONTEXT(ctx), level);
    cbuf_put_tag_ok(c_out);
}

void libusb_drv_impl_get_device_list(eapi_ctx_t* ctx, cbuf_t* c_out)
{
    libusb_device** dev_list;
    ssize_t n;
    
    if ((n = libusb_get_device_list(LIBUSB_CONTEXT(ctx), &dev_list)) < 0)
	put_libusb_error(ctx, c_out, n);
    else {
	int i;
	cbuf_put_tuple_begin(c_out, 2);
	cbuf_put_tag_ok(c_out);
	cbuf_put_list_begin(c_out, n);
	for (i = 0; i < n; i++) {
	    // FIXME: erlang side must reference this
	    eapi_put_pointer(ctx, c_out, dev_list[i]);
	}
	cbuf_put_list_end(c_out, n);
	cbuf_put_tuple_end(c_out, 2);
	// FIXME: ref/unref
	libusb_free_device_list(dev_list, 0);
    }
}

void libusb_drv_impl_ref_device(eapi_ctx_t* ctx, cbuf_t* c_out, 
			     libusb_device* dev)
{
    (void) ctx;
    if (!dev)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	libusb_ref_device(dev);
	cbuf_put_tag_ok(c_out);
    }
}

void libusb_drv_impl_unref_device(eapi_ctx_t* ctx, cbuf_t* c_out, 
			       libusb_device* dev)
{
    (void) ctx;
    if (!dev)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	libusb_unref_device(dev);
	cbuf_put_tag_ok(c_out);
    }
}

void libusb_drv_impl_get_configuration(eapi_ctx_t* ctx, cbuf_t* c_out, 
				       libusb_device_handle *dev_handle)
{
    int config;
    int r;

    if (!dev_handle)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	if ((r = libusb_get_configuration(dev_handle, &config)) < 0)
	    put_libusb_error(ctx, c_out, r);
	else {
	    cbuf_put_tuple_begin(c_out, 2);
	    cbuf_put_tag_ok(c_out);	
	    cbuf_put_int(c_out, config);
	    cbuf_put_tuple_end(c_out, 2);
	}
    }
}
	
void libusb_drv_impl_get_device_descriptor(eapi_ctx_t* ctx, cbuf_t* c_out, 
					 libusb_device *dev)
{
    struct libusb_device_descriptor desc;
    int r;

    if (!dev)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	if ((r = libusb_get_device_descriptor(dev, &desc)) < 0) 
	    put_libusb_error(ctx, c_out, r);
	else {
	    cbuf_put_tuple_begin(c_out, 2);
	    cbuf_put_tag_ok(c_out);	
	    e_struct_libusb_device_descriptor(ctx, c_out, &desc);
	    cbuf_put_tuple_end(c_out, 2);
	}
    }
}

void libusb_drv_impl_get_active_config_descriptor(eapi_ctx_t* ctx, cbuf_t* c_out, 
					       libusb_device *dev)
{
    int r;
    struct libusb_config_descriptor *config;

    if (!dev)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	if ((r = libusb_get_active_config_descriptor(dev, &config)) < 0)
	    put_libusb_error(ctx, c_out, r);
	else {
	    cbuf_put_tuple_begin(c_out, 2);
	    cbuf_put_tag_ok(c_out);	
	    e_struct_libusb_config_descriptor(ctx, c_out, config);
	    cbuf_put_tuple_end(c_out, 2);
	    libusb_free_config_descriptor(config);
	}
    }
}

void libusb_drv_impl_get_config_descriptor(eapi_ctx_t* ctx, cbuf_t* c_out, 
					libusb_device *dev,
					uint8_t config_index)
{
    int r;
    struct libusb_config_descriptor *config;

    EAPI_DRV_DBG("get_config_descriptor: dev=%p, config_index=%d", 
	dev, config_index);

    if (!dev)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	if ((r = libusb_get_config_descriptor(dev, config_index, &config)) < 0)
	    put_libusb_error(ctx, c_out, r);
	else {
#ifdef CONFIG_DEBUG
	    print_config_descriptor(stderr, config);
#endif
	    cbuf_put_tuple_begin(c_out, 2);
	    cbuf_put_tag_ok(c_out);
	    e_struct_libusb_config_descriptor(ctx, c_out, config);
	    cbuf_put_tuple_end(c_out, 2);
	    libusb_free_config_descriptor(config);
	}
    }
}

void libusb_drv_impl_get_config_descriptor_by_value(eapi_ctx_t* ctx, 
						 cbuf_t* c_out, 
						 libusb_device *dev,
						 uint8_t bConfigurationValue)
{
    int r;
    struct libusb_config_descriptor *config;

    if (!dev)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	if ((r = libusb_get_config_descriptor_by_value(dev, bConfigurationValue,
						       &config)) < 0)
	    put_libusb_error(ctx, c_out, r);
	else {
	    cbuf_put_tuple_begin(c_out, 2);
	    cbuf_put_tag_ok(c_out);	
	    e_struct_libusb_config_descriptor(ctx, c_out, config);
	    cbuf_put_tuple_end(c_out, 2);
	    libusb_free_config_descriptor(config);
	}
    }
}

void libusb_drv_impl_get_bus_number(eapi_ctx_t* ctx, cbuf_t* c_out, 
				 libusb_device *dev)
{
    uint8_t r;
    (void) ctx;

    if (!dev)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	r = libusb_get_bus_number(dev);
	cbuf_put_tuple_begin(c_out, 2);
	cbuf_put_tag_ok(c_out);	
	cbuf_put_uint8(c_out, r);
	cbuf_put_tuple_end(c_out, 2);
    }
}

void libusb_drv_impl_get_device_address(eapi_ctx_t* ctx, cbuf_t* c_out, 
				     libusb_device *dev)
{
    uint8_t r;
    (void) ctx;    

    if (!dev)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	r = libusb_get_device_address(dev);
	cbuf_put_tuple_begin(c_out, 2);
	cbuf_put_tag_ok(c_out);	
	cbuf_put_uint8(c_out, r);
	cbuf_put_tuple_end(c_out, 2);
    }
}


void libusb_drv_impl_get_max_packet_size(eapi_ctx_t* ctx, cbuf_t* c_out, 
				      libusb_device *dev,
				      uint8_t endpoint)
{
    int r;
    (void) ctx;    

    if (!dev)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	r = libusb_get_max_packet_size(dev, endpoint);
	cbuf_put_tuple_begin(c_out, 2);
	cbuf_put_tag_ok(c_out);	
	cbuf_put_int(c_out, r);
	cbuf_put_tuple_end(c_out, 2);
    }
}

void libusb_drv_impl_get_max_iso_packet_size(eapi_ctx_t* ctx, cbuf_t* c_out, 
					  libusb_device *dev,
					  uint8_t endpoint)
{
    int r;
    (void)ctx;

    if (!dev)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	r = libusb_get_max_iso_packet_size(dev, endpoint);
	cbuf_put_tuple_begin(c_out, 2);
	cbuf_put_tag_ok(c_out);	
	cbuf_put_int(c_out, r);
	cbuf_put_tuple_end(c_out, 2);
    }
}

void libusb_drv_impl_open(eapi_ctx_t* ctx, cbuf_t* c_out, libusb_device* dev)
{
    libusb_device_handle* handle;
    int r;

    if (!dev)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	if ((r = libusb_open(dev, &handle)) < 0)
	    put_libusb_error(ctx, c_out, r);
	else {
	    cbuf_put_tuple_begin(c_out, 2);
	    cbuf_put_tag_ok(c_out);
	    eapi_put_pointer(ctx, c_out, handle);
	    cbuf_put_tuple_end(c_out, 2);
	}
    }
}


void libusb_drv_impl_close(eapi_ctx_t* ctx, cbuf_t* c_out,
			libusb_device_handle* dev_handle)
{
    (void) ctx;
    if (dev_handle)
	libusb_close(dev_handle);
    cbuf_put_tag_ok(c_out);
}

void libusb_drv_impl_get_device(eapi_ctx_t* ctx, cbuf_t* c_out,
			     libusb_device_handle* dev_handle)
{
    libusb_device* dev = 0;
    if (dev_handle)
	dev = libusb_get_device(dev_handle);
    eapi_put_pointer(ctx, c_out, dev);
}


void libusb_drv_impl_set_configuration(eapi_ctx_t* ctx,cbuf_t* c_out,
				       libusb_device_handle* dev_handle,
				       int configuration)
{
    int r;

    if (!dev_handle)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	if ((r = libusb_set_configuration(dev_handle, configuration)) < 0)
	    put_libusb_error(ctx, c_out, r);
	else
	    cbuf_put_tag_ok(c_out);
    }
}

void libusb_drv_impl_claim_interface(eapi_ctx_t* ctx,cbuf_t* c_out,
				     libusb_device_handle* dev_handle,
				     int iface)
{
    int r;

    if (!dev_handle)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	if ((r = libusb_claim_interface(dev_handle, iface)) < 0)
	    put_libusb_error(ctx, c_out, r);
	else
	    cbuf_put_tag_ok(c_out);
    }
}

void libusb_drv_impl_release_interface(eapi_ctx_t* ctx,cbuf_t* c_out,
				       libusb_device_handle* dev_handle,
				       int iface)
{
    int r;

    if (!dev_handle)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	if ((r = libusb_release_interface(dev_handle, iface)) < 0)
	    put_libusb_error(ctx, c_out, r);
	else
	    cbuf_put_tag_ok(c_out);
    }
}


void libusb_drv_impl_open_device_with_vid_pid(eapi_ctx_t* ctx,cbuf_t* c_out,
					      uint16_t vendor_id,
					      uint16_t product_id)
{
    libusb_device_handle* handle;

    if (!(handle = libusb_open_device_with_vid_pid(LIBUSB_CONTEXT(ctx),
						   vendor_id, product_id)))
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	cbuf_put_tuple_begin(c_out, 2);
	cbuf_put_tag_ok(c_out);
	eapi_put_pointer(ctx, c_out, handle);
	cbuf_put_tuple_end(c_out, 2);
    }
}


void libusb_drv_impl_set_interface_alt_setting(eapi_ctx_t* ctx,cbuf_t* c_out,
					       libusb_device_handle* dev_handle,
					       int interface_number,
					       int alternate_setting)
{
    int r;

    if (!dev_handle)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	if ((r = libusb_set_interface_alt_setting(dev_handle, interface_number,
						  alternate_setting)) < 0)
	    put_libusb_error(ctx, c_out, r);
	else
	    cbuf_put_tag_ok(c_out);
    }
}

void libusb_drv_impl_clear_halt(eapi_ctx_t* ctx,cbuf_t* c_out,
				libusb_device_handle* dev_handle,
				int endpoint)
{
    int r;

    if (!dev_handle)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	if ((r = libusb_clear_halt(dev_handle, endpoint)) < 0)
	    put_libusb_error(ctx, c_out, r);
	else
	    cbuf_put_tag_ok(c_out);
    }
}

void libusb_drv_impl_reset_device(eapi_ctx_t* ctx,cbuf_t* c_out,
				  libusb_device_handle* dev_handle)
{
    int r;

    if (!dev_handle)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
    else {
	if ((r = libusb_reset_device(dev_handle)) < 0)
	    put_libusb_error(ctx, c_out, r);
	else
	    cbuf_put_tag_ok(c_out);
    }
}

// allocate and link transfer data
static transfer_data_t* transfer_item_new(eapi_ctx_t* ctx,
					  ErlDrvBinary* bin,
					  struct libusb_transfer *transfer)
{
    transfer_data_t* td;
    libusb_drv_ctx_t* dctx = (libusb_drv_ctx_t*) ctx->user_data;

    if (!(td = (transfer_data_t*) malloc(sizeof(transfer_data_t))))
	return 0;
    td->transfer = transfer;
    td->next   = 0;
    td->prev   = 0;
    td->dctx   = dctx;
    td->caller = driver_caller(ctx->port);
    td->ref    = ctx->eref++;
    if (bin) driver_binary_inc_refc(bin);
    td->bin    = bin;

    // link td last in transfer list
    if ((td->prev = dctx->t_last))
	dctx->t_last->next = td;
    dctx->t_last = td;

    return td;
}

static void transfer_item_unlink(transfer_data_t* td)
{
    transfer_data_t* n = td->next;
    transfer_data_t* p = td->prev;
    libusb_drv_ctx_t* dctx = td->dctx;

    if (p)
	p->next = n;
    else
	dctx->t_first = n;

    if (n) 
	n->prev = p;
    else
	dctx->t_last = p;
    td->next = 0;
    td->prev = 0;
}

static void transfer_item_free(transfer_data_t* td)
{
    transfer_item_unlink(td);
    // cancel ? remove transfer
    driver_free_binary(td->bin);
    libusb_free_transfer(td->transfer);
    free(td);
}

//
// Send asyncrounus messages about transfer state
//
// {libusb_transfer_completed, AsyncRef, DevHandle, WriteLength}
// {libusb_transfer_completed, AsyncRef, DevHandle, ReadBinary}
// {libsub_transfer_error,     AsyncRef, DevHandle, Reason}
//
// Reason =   timeout | pipe | no_device | other
//
// 
static void transfer_read_cb(struct libusb_transfer *transfer)
{
    ErlDrvTermData term_data[12];
    ErlDrvTermData tag;
    ErlDrvTermData value = am_other;
    ErlDrvTermData value_type;
    transfer_data_t* td = (transfer_data_t*) transfer->user_data;
    libusb_drv_ctx_t* dctx = td->dctx;
    int i;

    EAPI_DRV_DBG("read_cb: ref=%u status=%d", td->ref, transfer->status);

    // FIXME: handle LIBUSB_TRANSFER_TYPE_ISOCHRONOUS
    //  must check for errors in all packets and 
    //   maybe compact the binary a bit?
    switch (transfer->status) {
    case LIBUSB_TRANSFER_COMPLETED:
	tag = am_libusb_transfer_completed;
	value_type = ERL_DRV_BINARY;
	goto reply;
    case LIBUSB_TRANSFER_ERROR:  value = am_error; goto error;
    case LIBUSB_TRANSFER_TIMED_OUT: value = am_timeout; goto error;
    case LIBUSB_TRANSFER_CANCELLED: value = am_cancelled; goto error;
    case LIBUSB_TRANSFER_STALL: value = am_stall; goto error;
    case LIBUSB_TRANSFER_NO_DEVICE: value = am_no_device; goto error;
    case LIBUSB_TRANSFER_OVERFLOW: value  = am_overflow; goto error;
    default: goto error;
    }

error:
    tag = am_libusb_transfer_error;
    value_type = ERL_DRV_ATOM;
    
reply:
    term_data[0] = ERL_DRV_ATOM;
    term_data[1] = tag;
    term_data[2] = ERL_DRV_UINT;
    term_data[3] = td->ref;
    term_data[4] = ERL_DRV_UINT;
    term_data[5] = (ErlDrvTermData) eapi_drv_ptr(dctx->ctx, transfer->dev_handle);
    term_data[6] = value_type;
    i = 7;
    if (value_type == ERL_DRV_BINARY) {
	term_data[i++] = (ErlDrvTermData) td->bin;
	term_data[i++] = (ErlDrvTermData) transfer->actual_length;
	if (transfer->type == LIBUSB_TRANSFER_TYPE_CONTROL)
	    term_data[i++] = LIBUSB_CONTROL_SETUP_SIZE;
	else
	    term_data[i++] = 0;
    }
    else {
	term_data[i++] = value;
    }
    term_data[i++] = ERL_DRV_TUPLE;
    term_data[i++] = 4;
	
    driver_send_term(dctx->ctx->port, td->caller, term_data, i);

    transfer_item_free(td);
}

static void transfer_write_cb(struct libusb_transfer *transfer)
{
    ErlDrvTermData term_data[10];
    ErlDrvTermData tag;
    ErlDrvTermData value;
    ErlDrvTermData value_type;
    transfer_data_t* td = (transfer_data_t*) transfer->user_data;
    libusb_drv_ctx_t* dctx = td->dctx;

    EAPI_DRV_DBG("write_cb: ref=%u status=%d", td->ref, transfer->status);

    // FIXME: handle LIBUSB_TRANSFER_TYPE_ISOCHRONOUS
    //  must check for errors in all packets and 
    //  sum length of all packets (or send a list)
    switch (transfer->status) {
    case LIBUSB_TRANSFER_COMPLETED:
	tag   = am_libusb_transfer_completed;
	value = (ErlDrvTermData) transfer->actual_length;
	value_type = ERL_DRV_UINT;
	goto reply;
    case LIBUSB_TRANSFER_ERROR:	    value = am_error;  goto error;
    case LIBUSB_TRANSFER_TIMED_OUT: value = am_timeout; goto error;
    case LIBUSB_TRANSFER_CANCELLED: value = am_cancelled; goto error;
    case LIBUSB_TRANSFER_STALL:     value = am_stall; goto error;
    case LIBUSB_TRANSFER_NO_DEVICE: value = am_no_device; goto error;
    case LIBUSB_TRANSFER_OVERFLOW:  value = am_overflow; goto error;
    default: value = am_other; goto error;
    }

error:
    tag = am_libusb_transfer_error;
    value_type = ERL_DRV_ATOM;
    
reply:
    term_data[0] = ERL_DRV_ATOM;
    term_data[1] = tag;
    term_data[2] = ERL_DRV_UINT;
    term_data[3] = td->ref;
    term_data[4] = ERL_DRV_UINT;
    term_data[5] = (ErlDrvTermData) eapi_drv_ptr(dctx->ctx, transfer->dev_handle);
    term_data[6] = value_type;
    term_data[7] = value;
    term_data[8] = ERL_DRV_TUPLE;
    term_data[9] = 4;

    driver_send_term(dctx->ctx->port, td->caller, term_data, 10);

    transfer_item_free(td);
}


void libusb_drv_impl_control_transfer_write(
    eapi_ctx_t* ctx,cbuf_t* c_out,
    libusb_device_handle* dev_handle,
    uint8_t request_type,
    uint8_t request,
    uint16_t value,
    uint16_t index,
    eapi_binary_t data,
    unsigned int timeout)
{
    int r = LIBUSB_ERROR_NO_MEM;
    struct libusb_transfer *transfer = 0;
    transfer_data_t* td = 0;
    ErlDrvBinary* bin = data.bin;
    uint16_t length;

    if (!dev_handle ||
	(request_type & LIBUSB_ENDPOINT_DIR_MASK) != LIBUSB_ENDPOINT_OUT) {
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
	return;
    }

    if (!(transfer = libusb_alloc_transfer(0)))
	goto error;
    length = bin->orig_size - data.offset;
    if (!(bin = driver_alloc_binary(LIBUSB_CONTROL_SETUP_SIZE + length)))
	goto error;
    if (!(td = transfer_item_new(ctx, bin, transfer)))
	goto error;
    
    libusb_fill_control_setup((unsigned char*)bin->orig_bytes,
			      request_type, request,
			      value, index, length);

    memcpy(bin->orig_bytes + LIBUSB_CONTROL_SETUP_SIZE,
	   (unsigned char*)(bin->orig_bytes + data.offset),
	   length);

    libusb_fill_control_transfer(transfer, dev_handle,
				 (unsigned char*) bin->orig_bytes,
				 transfer_write_cb, 
				 td, timeout);
    if ((r = libusb_submit_transfer(transfer)) < 0)
	goto error;
    cbuf_put_tuple_begin(c_out, 2);
    cbuf_put_tag_ok(c_out);
    cbuf_put_uint32(c_out, td->ref);
    cbuf_put_tuple_end(c_out, 2);
    return;

error:
    if (td) 
	transfer_item_free(td);
    else {
	if (transfer) libusb_free_transfer(transfer);
    }
    put_libusb_error(ctx, c_out, LIBUSB_ERROR_NO_MEM);
}


void libusb_drv_impl_control_transfer_read(
    eapi_ctx_t* ctx,cbuf_t* c_out,
    libusb_device_handle* dev_handle,
    uint8_t request_type,
    uint8_t request,
    uint16_t value,
    uint16_t index,
    uint16_t length,
    unsigned int timeout)
{
    int r = LIBUSB_ERROR_NO_MEM;
    struct libusb_transfer *transfer = 0;
    transfer_data_t* td = 0;
    ErlDrvBinary* bin = 0;

    if (!dev_handle ||
	(request_type & LIBUSB_ENDPOINT_DIR_MASK) != LIBUSB_ENDPOINT_IN) {
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
	return;
    }
    if (!(transfer = libusb_alloc_transfer(0)))
	goto error;
    if (!(bin = driver_alloc_binary(LIBUSB_CONTROL_SETUP_SIZE + length)))
	goto error;
    if (!(td = transfer_item_new(ctx, bin, transfer)))
	goto error;

    libusb_fill_control_setup((unsigned char*)bin->orig_bytes,
			      request_type, request,
			      value, index, length);

    libusb_fill_control_transfer(transfer, dev_handle, 
				 (unsigned char*)bin->orig_bytes,
				 transfer_read_cb, 
				 td, timeout);
    EAPI_DRV_DBG("submit: control_transfer ref=%u", td->ref);
    if ((r = libusb_submit_transfer(transfer)) < 0)
	goto error;
    cbuf_put_tuple_begin(c_out, 2);
    cbuf_put_tag_ok(c_out);
    cbuf_put_uint32(c_out, td->ref);    
    cbuf_put_tuple_end(c_out, 2);
    EAPI_DRV_DBG("submit: done ref=%u", td->ref);
    return;

error:
    if (td) 
	transfer_item_free(td);
    else {
	if (bin) driver_free_binary(bin);
	if (transfer) libusb_free_transfer(transfer);
    }
    put_libusb_error(ctx, c_out, r);

}


void libusb_drv_impl_bulk_transfer_write(
    eapi_ctx_t* ctx,cbuf_t* c_out,
    libusb_device_handle* dev_handle,
    uint8_t endpoint,
    eapi_binary_t data,
    unsigned int timeout)
{
    int r = LIBUSB_ERROR_NO_MEM;
    struct libusb_transfer *transfer = 0;
    transfer_data_t* td = 0;
    ErlDrvBinary* bin = data.bin;

    if (!dev_handle ||
	(endpoint & LIBUSB_ENDPOINT_DIR_MASK) != LIBUSB_ENDPOINT_OUT) {
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
	return;
    }
    if (!(transfer = libusb_alloc_transfer(0)))
	goto error;
    if (!(td = transfer_item_new(ctx, bin, transfer)))
	goto error;

    libusb_fill_bulk_transfer(
	transfer, dev_handle, endpoint,
	(unsigned char*)(bin->orig_bytes + data.offset),
	bin->orig_size - data.offset,
	transfer_write_cb, td, timeout);

    if ((r = libusb_submit_transfer(transfer)) < 0)
	goto error;
    cbuf_put_tuple_begin(c_out, 2);
    cbuf_put_tag_ok(c_out);
    cbuf_put_uint32(c_out, td->ref);    
    cbuf_put_tuple_end(c_out, 2);
    return;

error:
    if (td)
	transfer_item_free(td);
    else {
	if (transfer) libusb_free_transfer(transfer);
    }
    put_libusb_error(ctx, c_out, r);
}


void libusb_drv_impl_bulk_transfer_read(
    eapi_ctx_t* ctx,cbuf_t* c_out,
    libusb_device_handle* dev_handle,
    uint8_t endpoint,
    int length,
    unsigned int timeout)
{
    int r = LIBUSB_ERROR_NO_MEM;
    struct libusb_transfer *transfer = 0;
    transfer_data_t* td = 0;
    ErlDrvBinary* bin = 0;

    if (!dev_handle ||
	(endpoint & LIBUSB_ENDPOINT_DIR_MASK) != LIBUSB_ENDPOINT_IN) {
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
	return;
    }

    if (!(transfer = libusb_alloc_transfer(0)))
	goto error;
    if (!(bin = driver_alloc_binary(length)))
	goto error;
    if (!(td = transfer_item_new(ctx, bin, transfer)))
	goto error;

    libusb_fill_bulk_transfer(
	transfer, dev_handle, endpoint,
	(unsigned char*) bin->orig_bytes, length,
	transfer_read_cb, td, timeout);

    if ((r = libusb_submit_transfer(transfer)) < 0)
	goto error;
    cbuf_put_tuple_begin(c_out, 2);
    cbuf_put_tag_ok(c_out);
    cbuf_put_uint32(c_out, td->ref);    
    cbuf_put_tuple_end(c_out, 2);
    return;

error:
    if (td)
	transfer_item_free(td);
    else {
	if (bin) driver_free_binary(bin);
	if (transfer) libusb_free_transfer(transfer);
    }
    put_libusb_error(ctx, c_out, r);
}


void libusb_drv_impl_interrupt_transfer_write(
    eapi_ctx_t* ctx,cbuf_t* c_out,
    libusb_device_handle* dev_handle,
    uint8_t endpoint,
    eapi_binary_t data,
    unsigned int timeout)
{
    int r = LIBUSB_ERROR_NO_MEM;
    struct libusb_transfer *transfer = 0;
    transfer_data_t* td = 0;
    ErlDrvBinary* bin = data.bin;

    if (!dev_handle ||
	(endpoint & LIBUSB_ENDPOINT_DIR_MASK) != LIBUSB_ENDPOINT_OUT) {
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
	return;
    }

    if (!(transfer = libusb_alloc_transfer(0)))
	goto error;
    if (!(td = transfer_item_new(ctx, bin, transfer)))
	goto error;

    libusb_fill_interrupt_transfer(
	transfer, dev_handle, endpoint,
	(unsigned char*)(bin->orig_bytes + data.offset),
	bin->orig_size - data.offset,
	transfer_write_cb, td, timeout);

    if ((r = libusb_submit_transfer(transfer)) < 0)
	goto error;
    cbuf_put_tuple_begin(c_out, 2);
    cbuf_put_tag_ok(c_out);
    cbuf_put_uint32(c_out, td->ref);    
    cbuf_put_tuple_end(c_out, 2);
    return;

error:
    if (td) 
	transfer_item_free(td);
    else {
	if (transfer) libusb_free_transfer(transfer);
    }
    put_libusb_error(ctx, c_out, r);
}


void libusb_drv_impl_interrupt_transfer_read(
    eapi_ctx_t* ctx,cbuf_t* c_out,
    libusb_device_handle* dev_handle,
    uint8_t endpoint,
    int length,
    unsigned int timeout)
{
    int r = LIBUSB_ERROR_NO_MEM;
    struct libusb_transfer *transfer = 0;
    transfer_data_t* td = 0;
    ErlDrvBinary* bin = 0;

    if (!dev_handle ||
	(endpoint & LIBUSB_ENDPOINT_DIR_MASK) != LIBUSB_ENDPOINT_IN) {
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
	return;
    }

    if (!(transfer = libusb_alloc_transfer(0)))
	goto error;
    if (!(bin = driver_alloc_binary(length)))
	goto error;
    if (!(td = transfer_item_new(ctx, bin, transfer)))
	goto error;

    libusb_fill_interrupt_transfer(
	transfer, dev_handle, endpoint,
	(unsigned char*) bin->orig_bytes, length,
	transfer_read_cb, td, timeout);

    if ((r = libusb_submit_transfer(transfer)) < 0)
	goto error;
    cbuf_put_tuple_begin(c_out, 2);
    cbuf_put_tag_ok(c_out);
    cbuf_put_uint32(c_out, td->ref);    
    cbuf_put_tuple_end(c_out, 2);
    return;

error:
    if (td)
	transfer_item_free(td);
    else {
	if (bin) driver_free_binary(bin);
	if (transfer) libusb_free_transfer(transfer);
    }
    put_libusb_error(ctx, c_out, r);
}

// FIXME: accept an array with packet_size's
void libusb_drv_impl_iso_transfer_write(
    eapi_ctx_t* ctx,cbuf_t* c_out,
    libusb_device_handle* dev_handle,
    uint8_t endpoint,
    eapi_binary_t data,
    int num_iso_packets,
    unsigned int packet_size,
    unsigned int timeout)
{
    int r = LIBUSB_ERROR_NO_MEM;
    struct libusb_transfer *transfer = 0;
    transfer_data_t* td = 0;
    ErlDrvBinary* bin = bin;

    if (!dev_handle ||
	(endpoint & LIBUSB_ENDPOINT_DIR_MASK) != LIBUSB_ENDPOINT_OUT) {
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
	return;
    }
    if (!(transfer = libusb_alloc_transfer(num_iso_packets)))
	goto error;
    if (!(td = transfer_item_new(ctx, bin, transfer)))
	goto error;

    libusb_fill_iso_transfer(
	transfer, dev_handle, endpoint,
	(unsigned char*)(bin->orig_bytes + data.offset),
	bin->orig_size - data.offset,
	num_iso_packets,
	transfer_write_cb, td, timeout);
    libusb_set_iso_packet_lengths(transfer, packet_size);

    if ((r = libusb_submit_transfer(transfer)) < 0)
	goto error;
    cbuf_put_tuple_begin(c_out, 2);
    cbuf_put_tag_ok(c_out);
    cbuf_put_uint32(c_out, td->ref);    
    cbuf_put_tuple_end(c_out, 2);
    return;
error:
    if (td)
	transfer_item_free(td);
    else {
	if (transfer) libusb_free_transfer(transfer);
    }
    put_libusb_error(ctx, c_out, r);
}

// FIXME: accept an array with packet_size's
void libusb_drv_impl_iso_transfer_read(
    eapi_ctx_t* ctx,cbuf_t* c_out,
    libusb_device_handle* dev_handle,
    uint8_t endpoint,
    int length,
    int num_iso_packets,
    unsigned int packet_size,
    unsigned int timeout)
{
    int r = LIBUSB_ERROR_NO_MEM;
    struct libusb_transfer *transfer = 0;
    transfer_data_t* td = 0;
    ErlDrvBinary* bin = 0;

    if (!dev_handle ||
	(endpoint & LIBUSB_ENDPOINT_DIR_MASK) != LIBUSB_ENDPOINT_IN) {
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_INVALID_PARAM);
	return;
    }

    if (!(transfer = libusb_alloc_transfer(num_iso_packets)))
	goto error;
    if (!(bin = driver_alloc_binary(length)))
	goto error;
    if (!(td = transfer_item_new(ctx, bin, transfer)))
	goto error;

    libusb_fill_iso_transfer(
	transfer, dev_handle, endpoint,
	(unsigned char*) bin->orig_bytes, length,
	num_iso_packets,
	transfer_read_cb, td, timeout);
    libusb_set_iso_packet_lengths(transfer, packet_size);

    if ((r = libusb_submit_transfer(transfer)) < 0)
	goto error;
    cbuf_put_tuple_begin(c_out, 2);
    cbuf_put_tag_ok(c_out);
    cbuf_put_uint32(c_out, td->ref);    
    cbuf_put_tuple_end(c_out, 2);
    return;

error:
    if (td) 
	transfer_item_free(td);
    else {
	if (bin) driver_free_binary(bin);
	if (transfer) libusb_free_transfer(transfer);
    }
    put_libusb_error(ctx, c_out, r);
}

void libusb_drv_impl_cancel_transfer(eapi_ctx_t* ctx, cbuf_t* c_out,
				    uint32_t ref)
{
    libusb_drv_ctx_t* dctx = LIBUSB_DRV_CONTEXT(ctx);
    transfer_data_t* td = dctx->t_first;
    int r = 0;

    while(td) {
	transfer_data_t* n = td->next;
	if (td->ref == ref) {
	    r = libusb_cancel_transfer(td->transfer);
	    break;
	}
	td = n;
    }
    if (!td)
	put_libusb_error(ctx, c_out, LIBUSB_ERROR_NOT_FOUND);
    else if (r)
	put_libusb_error(ctx, c_out, r);
    else
	cbuf_put_tag_ok(c_out);
}

void libusb_drv_impl_kernel_driver_active(eapi_ctx_t* ctx,cbuf_t* c_out,
					 libusb_device_handle *dev, int interface)
{
    int r = libusb_kernel_driver_active(dev, interface);
    if (r < 0)
	put_libusb_error(ctx, c_out, r);
    else {
	cbuf_put_tuple_begin(c_out, 2);	
	cbuf_put_tag_ok(c_out);
	cbuf_put_boolean(c_out, r);
	cbuf_put_tuple_end(c_out, 2);	
    }
}

void libusb_drv_impl_detach_kernel_driver(eapi_ctx_t* ctx,cbuf_t* c_out,
					 libusb_device_handle *dev, int interface)
{
    int r = libusb_detach_kernel_driver(dev, interface);
    if (r < 0)
	put_libusb_error(ctx, c_out, r);
    else
	cbuf_put_tag_ok(c_out);
}

void libusb_drv_impl_attach_kernel_driver(eapi_ctx_t* ctx,cbuf_t* c_out,
					 libusb_device_handle *dev, int interface)
{
    int r = libusb_attach_kernel_driver(dev, interface);
    if (r < 0)
	put_libusb_error(ctx, c_out, r);
    else
	cbuf_put_tag_ok(c_out);
}
