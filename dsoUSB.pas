unit dsoUSB;

{ This unit constitutes the DOS-2090 interface via the LIBUSB unit.
         DSOControl unit uses --> dsoUSB unit uses --> libusb unit

  All of this is drawn from the HantekDSO program.

  Note that this unit attempts to connect to the DSO-2090 at initialization,
  and disconnects on finalization.}

{$mode objfpc}{$H+}

interface

uses
  SysUtils,Dialogs {In DSOInit only}, libusb;

var   lastUSB: integer;
  { For debugging use.  Contain the ID number of the last USB call made:
     0: WriteBulk  1: ReadBulk  2: WriteControl   3: ReadControl}

function WriteBulk(buffer: pointer; length: integer): integer;
function ReadBulk(buffer: pointer; length: integer): integer;
function WriteControl( request: byte; buffer: pointer; length,value: integer): integer;
function ReadControl(request: byte; buffer: pointer; length,value: integer):integer;

implementation

CONST
 DSO_PRODUCT_ID = $2090;     //My model number
 DSO_VENDOR_ID = $04b5;      //Magic number for Hantek
 USBATTEMPTS = 3;
 EP_BULK_OUT = $02;   // Endpoint for sending commands to DSO bulk write translates to $02
 EP_BULK_IN  = $86;   // Endpoint for reading data from DSO bulk read traslates to $86
 TIMEOUT = 500;



VAR
  DSODevicePtr: Pusbdevice;  //pointer to usb device node for 2090 scope
  DSOHandle: Pusb_dev_handle;

procedure dsoInit;
 {Called one time in this unit's Initialization section to establish connection
  to the DSO via USB. }
 var BusPtr: Pusb_bus;
     DevicePtr: Pusbdevice;
 begin
  {first, we must locate and open the scope for access}
   usb_init;
   usb_find_busses;
   usb_find_devices;
   BusPtr := usb_get_busses; //get pointer to USB first bus in bus list
              {  usb_bus = record
                          next : Pusb_bus;
                          prev : Pusb_bus;
                          dirname : array[0..(PATH_MAX+1)-1] of char;  //bus ID number as a pchar string
                          devices : Pusbdevice;                        //pointer to device list for this bus
                          location : u_int32_t;                        //the bus ID number 8 - 1
                          root_dev : Pusbdevice;                       //pointer to the first device or maybe the "hub"?
                        end; }
   DSODevicePtr := nil;   		//gobal variable -- assume no board is found
   while (busPtr <> nil) do
    begin
     DevicePtr := busPtr^.devices; //set up working pointer to first device on this bus
       while (DevicePtr <> nil)do
         begin
          { usbdevice  = record
                          next : Pusbdevice;
                          prev : Pusbdevice;
                          filename : array[0..(PATH_MAX+1)-1] of char;
                          bus : Pusb_bus;    //pointer back to host bus
                          descriptor : usb_device_descriptor;
                          config : Pusb_config_descriptor;
                          dev : pointer;
                          devnum : u_int8_t;
                          num_children : byte;
                          children : ^Pusbdevice;
                         end; }
          if (DevicePtr^.descriptor.idVendor = DSO_VENDOR_ID) {Hantek vendor ID} and
             (DevicePtr^.descriptor.idProduct = DSO_PRODUCT_ID) {scope type 2090} then
            begin DSODevicePtr := DevicePtr; DevicePtr := nil end
            else DevicePtr := DevicePtr^.next;
         end;
        if (DSODevicePtr = nil) then busPtr := busPtr^.next
        else busPtr := nil; //we've found our scope
    end;
   {Now, if we've found the board, we will take control of it and initialize its hardware.
    We know from experiment that the scope has two endpoints $02 in and $86 out, and that
    both have maxPacketsize $200 or 512 decimal}
   if DSODevicePtr <> nil then //We've located the 2090 scope
    begin
      DSOHandle := usb_open(DSODevicePtr);
      usb_claim_interface(DSOHandle, 0);
    end
   else begin ShowMessage('Unable to locate Hantek 2090:  halting. '); halt end;
 end;

 procedure DSOClose;
 {Called in this unit's Finalization section}
 begin
   usb_release_interface(DSOHandle, 0);
   usb_close(DSOHandle)
 end;

{======================================================================================
  Bulk Read/Write
 ======================================================================================}
function WriteBulk(buffer: pointer; length: integer): integer;
 {We don't handle a possible timeout here.  USB call returns # bytes written on success
  or -1 on failure.  Not called directly outside this unit.}
var i,rv: integer;
begin
  i := 0;
  rv := -1;
  lastUSB := 0;
  while (rv = -1) and (i < USBATTEMPTS) do
   begin
    rv := usb_bulk_write(DSOHandle, EP_BULK_OUT, buffer, length, timeout);
    inc(i);
   end;
  result := rv;
end;

function ReadBulk(buffer: pointer; length: integer): integer;
 {We don't handle a possible timeout here.  As far as I can tell, USB call
  DOES NOT return # bytes read on success -- only 0 on success and negative on fail}
var i, rv: integer;
begin
  i := 0;
  rv := -1;
  lastUSB := 1;
  while (rv = -1) and (i < USBATTEMPTS) do
   begin
    rv := usb_bulk_read(DSOHandle, EP_BULK_IN, buffer,length,timeout);
           {EP_BULK_IN = $06, USB_ENDPOINT_IN = $80}
    inc(i);
   end;
  result := rv;
end;

{======================================================================================
  Control Read/Write
 ======================================================================================}
function WriteControl( request: byte; buffer: pointer; length,value: integer): integer;
 {We don't handle a possible timeout here.  USB call returns # bytes read on success
  or -1 on failure. }
var i, rv: integer;
begin
  i := 0;
  rv := -1;
  lastUSB := 2;
  while (rv = -1) and (i < USBATTEMPTS) do
   begin
    rv := usb_control_msg(DSOHandle, ( USB_TYPE_VENDOR {40} OR USB_ENDPOINT_OUT {0}),
                    request, value, 0, buffer, length, timeout);
    inc(i);
   end;
  result := rv;
end;

function ReadControl(request: byte; buffer: pointer; length,value: integer):integer;
 {We don't handle a possible timeout here.  USB call returns # bytes read on success
  or -1 on failure.  Not called directly outside this unit.}
var i, rv: integer;
begin
  i := 0;
  rv := -1;
  lastUSB := 3;
  while (rv = -1) and (i < USBATTEMPTS) do
   begin
    rv := usb_control_msg(DSOHandle, (USB_TYPE_VENDOR {40} OR USB_ENDPOINT_IN {80}),
                    request, value, 0, buffer, length, timeout);
    inc(i);
   end;
  result := rv;
end;

Initialization
 DSOInit;

Finalization
 DSOClose;

end.

