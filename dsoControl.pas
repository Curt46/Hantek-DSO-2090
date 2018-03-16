unit dsoControl;
{ This unit constitutes the DOS-2090 hardware control interface.  The scope
  is controlled through these procedures and functions.

             "dsoControl" unit uses --> "dsoUSB" unit uses --> "libusb" unit

  All of this is drawn from the HantekDSO program.  There are a lot of "magic
  numbers" used that I have copied directly from OpenHantek and HantekDSO
  programs.

  Note that this unit calls GetChannelLevels during initialization since it must
  always be executed at least once.}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, dsoUSB;

const
{DBuffer index constants.  The data from channel 1 appears in DBuffer[i,1]
 and data from Channel 2 values are in DBuffer[i,0].  These constants just
 help remind us of this.}
CH1 = 1;
CH2 = 0;

{data buffer sizes}
BUFFER_SMALL = 10240;
BUFFER_LARGE = 32768;
BUFFER_MIN   = 256;

{Coupling}
AC = 0;				DC = 1;				NOCOUPLING = 2;
{VRange}
V5V = 0;       V2V = 1;       V1V = 2;       V500mV = 3;
V200mV = 4;    V100mV = 5;    V50mV = 6;      V20mV = 7;     V10mV = 8;

{Trigger Constants}
TRIGPOSITIVE = 0;   TRIGNEGATIVE = 1;
TRIGCh2 = 0;        TRIGCH1 = 1;    TRIGALT = 2;   TRIGEXT = 3;
{SelectedChannels}
USECH1 = 0;   USECH2 = 1;   USECH1CH2 = 2;   USECHNONE = 3;

type
 TRawData = Record
 case boolean of
  { dual channels in use }
   True:  (DB:  array[0..32767,0..1] of byte);
   {one channel in use}
   False: (SB:  array[0..65535] of byte);
 end;

var
 {=== Variables related to hardware control===}
 RawTriggerPoint: integer; //returned by call to GetCaptureState
 triggerpoint: integer; //derived from RawTriggerPoint
 CaptureState: integer; //returned by call to GetCaptureState

 SampleRate: integer;   // Sample rate in samples per millisecond
 BufferSize: integer;   // BUFFEER_SMALL = 10240 or BUFFER_LARGE = 32768
 BufferDuration: double;// BufferSize/SampleRate -- duration in MS
 SampleDuration: double;// Sample time in us as 1e3/Samplerate

 Ch1V,Ch1Offset,Ch1Coupling,Ch1Filter: integer;
 Ch2V,Ch2Offset,Ch2Coupling,Ch2Filter: integer;
 TrigOffset,TrigSlope,TrigSource,TrigPosition,TrigFilter: integer;

 SRFast,SRSlow: integer;
 UseChannels: integer;
 DisplayRate: integer;
 ChannelsInUse: integer;

 channelLevels: array[0..1,0..8,0..1] of word; //filled on initialization, used

 Data: TRawData;

 //DBuffer: array[0..32767,0..1] of byte; //filled by call to GetChannelData

 {=== Procedures for Running the Scope State Machine === }
 procedure GetCaptureState;
 procedure CaptureStart;
 procedure TriggerEnabled;
 procedure ForceTrigger;
 procedure GetChannelData(buffersize: integer);

 {=== Procedures for Setting Scope Controls === }
 procedure SetHardwareTriggerAndSampleRate(SRFast: byte; SRSlow: word; SelectedChannels,
            triggerSource,triggerSlope, triggerPosition, bufferSize: integer);
 procedure SetHardwareVoltageAndCoupling(ch1Voltage,ch2Voltage,
                  ch1Coupling,ch2Coupling, triggerSource: integer);
 procedure SetHardwareFilter(channel1, channel2, trigger: integer);
 procedure SetHardwareOffsets(ch1Offset,ch2Offset, extOffset: integer);

 {=== A procedures for getting scope calibrartion data -- "GetChannelLevels" --
 is called during this unit's intialization below and is not exported. }
implementation

const
 CONTROL_BEGINCOMMAND = $B3;
 B_BUFFER: array[0..9] of byte = ($0F, $03, $03, $03, 0, 0, 0, 0, 0, 0);
 {These used with CONTROL_BEGINCOMMAND calls in many procedures here }


{==========================================================================================
  This is just a debug helper.  On most failures here, we just execute a "halt."
 ==========================================================================================}
procedure ErrorAbort(S: string; index: integer);
begin
  ShowMessage(S + ' - ' + IntToStr(index) + '  LastUSB = '+IntToStr(LastUSB)+'.  Aborting.');
  halt
end;

{==========================================================================================
                     PROCEDURES FOR RUNNING THE SCOPE STATE MACHINE

The DSO-2090 has three machine states:
 0 -- the scope is idle.
 1 -- the scope is filling its data buffers with data
 2 -- the scope has filled its data buffers.

 Once the scope is in state 2, it will stay in that state until forced back to state 0
 (idle) by executing a StartCapture command (see below).

==========================================================================================}
CONST cmdGetCaptureState = 6;  //magic number
procedure GetCaptureState;
{Sets the global variable CaptureState and TriggerPoint.
 CaptureState values are
   CAPTURE_VALUE0 = 0 --> no data caputure in progress -- idle,
   CAPTURE_VALUE1 = 1 --> data capture in progress,
   CAPTURE_VALUE2 = 2 --> capture completed, data available in buffer.
 If CaptureState = 2, the variable TriggerPoint contains the index of the trigger
 point in the completed data buffer.  Otherwise, TriggerPoint := 0. }
var command: array[0..1] of byte = (cmdGetCaptureState, 0);
    CapState: array[0..511] of byte; {readbulk returns 512 bytes here, but only 4 bytes used.}
    TestBit: word;
Const S = 'GetCaptureState';
begin
 if ( writeControl(CONTROL_BEGINCOMMAND, @B_BUFFER, 10,0) < 0) then  ErrorAbort(S,1);
 if ( writeBulk(@command, sizeof(command)) < 0) then  ErrorAbort(S,2);
 if ( readBulk(@CapState, 512) < 0) then ErrorAbort(S,3);
 CaptureState := CapState[0];
 if ( CaptureState = 2 ) then {convert the triggerpoint value returned into a buffer index}
  begin
   RawTriggerPoint := (CapState[1] shl 16) or (CapState[3] shl 8) or CapState[2];
   TriggerPoint := RawTriggerPoint;
   TestBit := 1;
   While TestBit <> 0 do
    begin
     if (( TriggerPoint and TestBit ) > 0) then TriggerPoint := TriggerPoint xor (TestBit - 1);
     TestBit := TestBit shl 1;
    end;
  end
 else triggerPoint := 0;  {if CaptureState <> 2}
end;

CONST cmdTriggerEnabled = 4; //magic number
procedure TriggerEnabled;
{If the CaptureState is 0 (see GetCaptureState above), This procedure will cause the scope
 to begin looking for a trigger event.  It is possible that it will never find one, so
 the CaptureState will never change from the value 0.  If a trigger event IS found, the
 scope will enter CaptureState 1 (data capture in progress) and then CaptureState 2 when
 the data buffers have been filled.}
var command: array[0..1] of byte = (cmdTriggerEnabled, 0);
const S = 'TriggerEnabled';
begin
 if ( writeControl(CONTROL_BEGINCOMMAND, @B_BUFFER, 10,0 ) < 0) then ErrorAbort(S,1);
 if ( writeBulk(@command, sizeof(command)) < 0 ) then ErrorAbort(S,2);
end;

CONST cmdForceTrigger = 2; //magic number
procedure ForceTrigger;
{If the CaptureState is 0 (see GetCaptureState above) and TriggerEnabled (see above) has
 been called, this procedure will FORCE the scope to begin data capture immediately, causing
 it to enter CaptureState 1, then CaptureState 2 when the data buffers have been filled}
var command: array[0..1] of byte = (cmdForceTrigger, 0);
const S = 'Force Trigger';
begin
 if ( writeControl(CONTROL_BEGINCOMMAND, @B_BUFFER, 10,0) < 0 ) then ErrorAbort(S,1);
 if ( writeBulk(@command, sizeof(command))<0 ) then ErrorAbort(S,3);
end;

CONST cmdCaptureStart = 3; //magic number
procedure CaptureStart;
{This procedure seems to FORCE the scope into CaptureState 0 (idle).  In general, to
 get a buffer of data from the scope, we will do CaptureStart to force the scope to
 be idle, then a TriggerEnable to start looking for a trigger event, then a ForceTrigger
 if we want to start an immediate data capture.}
var command: array[0..1] of byte = (cmdCaptureStart, 0);
const S = 'CaptureStart';
begin
 if ( writeControl(CONTROL_BEGINCOMMAND, @B_BUFFER, 10,0)< 0 ) then ErrorAbort(S,1);
 if ( writeBulk(@command, sizeof(command)) < 0 ) then ErrorAbort(S,2);
end;

CONST cmdGetChannelData = 5; //magic number
procedure GetChannelData(buffersize: integer);
{Once the scope has entered CaptureState 2, it has filled its data buffer.  This
 procedure moves the scope data into the global variable DBuffer here.  The data remains
 in the scope's data buffers after it has been moved to the computer, and the scope
 sits idle until it receives a new "StartCapture" command.

 DBuffer provides space for 64K bytes:  DBuffer = array [0..3276,0..1] of byte.
 If two channels are in use, DBuffer[i,0] contains channel 2 data and DBuffer[i,1]
 contains channel 1 data.  If only one channel is in use, DBuffer[i,0] and DBuffer[i,1]
 contain sequential samples from the one channel in use.}
var command: array[0..1] of byte = (cmdGetChannelData, 0);
    packets: integer;
    i: integer;
Const S = 'GetChannelData';
begin
 if ( writeControl(CONTROL_BEGINCOMMAND, @B_BUFFER, 10,0) < 0 ) then ErrorAbort(S,1);
 if ( writeBulk(@command, sizeof(command)) < 0 ) then ErrorAbort(S,2);
 packets := ((bufferSize*sizeof(word)) div 512); //512 = readBulk packet length
 i := 0;
 while (i < packets) do
  begin
   if(readBulk(@Data.SB[0] + (i shl 9){*512}, 512) < 512) then ErrorAbort(S,5);
   inc(i);
  end;
end;


{=========================================================================================
                 PROCEDURES FOR SETTING THE SCOPE CONTROLS
=========================================================================================}
CONST cmdSetTriggerAndSampleRate = 1; //magic number
procedure SetHardwareTriggerAndSampleRate(SRFast: byte; SRSlow: word; selectedChannels,
            triggerSource,triggerSlope, triggerPosition, bufferSize: integer);
var
    command: array [0..11] of byte = (cmdSetTriggerAndSampleRate, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    TSR1SampleRate3,TSR1BufferSize3,TSR1TrigSource2,
    TSR2TrigSlope1, TSR2UsedCnl2: byte;
const S = 'SetTrigger - ';
begin
 TSR1TrigSource2 := ord(Triggersource) and $03;  //Set triggersource = (TRIGCH2,TRIGCH1,TRIGALT,TRIGEXT)
 {Not sure about the following, but this arrangement seems to ensure that a useful trigger point
  is returned for the BUFFER_MIN case}
 if (buffersize = BUFFER_LARGE) then TSR1BufferSize3 := $08
 else
 if (BufferSize = BUFFER_SMALL) then TSR1BufferSize3 := $04
 else TSR1BufferSize3 := 0;

 TSR2UsedCnl2 := selectedChannels;  //0 = Ch1, 1 = Ch2, 2 = Both, 3 = none   TWO LSBs
 if TriggerSlope = TRIGNEGATIVE then TSR2TrigSlope1 := $08 else  TSR2TrigSlope1 := 0;

 TSR1SampleRate3 := ( SRFast and $07 ) shl 5;

 if (writeControl(CONTROL_BEGINCOMMAND, @B_BUFFER, 10,0) < 0) then ErrorAbort(S,1);
 command[2] := TSR1SampleRate3 or TSR1TrigSource2 or TSR1BufferSize3;
 command[3] :=  TSR2UsedCnl2 or TSR2TrigSlope1 or TSR2TrigSlope1;
 command[4] := lo(SRSlow);
 command[5] := hi(SRSlow);
 command[6] := triggerPosition and $ff;
 command[7] := (triggerPosition shr 8) and $ff;
 command[10] := (triggerPosition shr 16) and $ff;

 if (writeBulk(@command, sizeof(command)) < 0) then ErrorAbort(S,3);
end;

CONST CONTROL_SETRELAYS = $B5;       //magic number
      cmdSetVoltageAndCoupling = 7;  //magic number
procedure SetHardwareVoltageAndCoupling(ch1Voltage,ch2Voltage,ch1Coupling,ch2Coupling,
                                triggerSource: integer);
var command: array[0..7] of byte = (cmdSetVoltageAndCoupling, $0F, 0, 0, 0, 0, 0, 0);
    relays: array[0..16] of byte = ( $00, $04, $08, $02, $20, $40, $10, $01,
                                       0, 0, 0, 0, 0, 0, 0, 0, 0 );
    temp: integer;
    voltagebyte: byte;
Const S = 'SetVoltage';
begin
 voltagebyte := 2 - (ch1Voltage mod 3) ;
 temp := (2 - (ch2Voltage mod 3)) shl 2;
 voltagebyte := voltagebyte or temp;
 voltagebyte := voltagebyte or $30;
 if (writeControl(CONTROL_BEGINCOMMAND, @B_BUFFER, 10,0) < 0) then ErrorAbort(S,1);
 command[2] := voltagebyte;
 if (writeBulk(@command, sizeof(command)) < 0) then ErrorAbort(S,3);
 ch1Voltage := ch1Voltage div 3;
 ch2Voltage := ch2Voltage div 3;
 if (ch1Voltage > 0) then relays[1]  := not(relays[1]);
 if (ch1Voltage > 1) then relays[2]  := not(relays[2]);
 if (ch1Coupling <> 0) then relays[3]:= not(relays[3]);
 if (ch2Voltage > 0) then relays[4]  := not(relays[4]);
 if (ch2Voltage > 1) then relays[5]  := not(relays[5]);
 if (ch2Coupling <> 0) then relays[6]:= not(relays[6]);
 if (triggerSource = TrigExt) then relays[7] := not(relays[7]);
 if (writeControl(CONTROL_SETRELAYS, @relays, sizeof(relays),0) < 0) then ErrorAbort(S,4);
end;


CONST cmdSetFilter = 0;  //magic number
procedure SetHardwareFilter(channel1, channel2, trigger: integer);
var   command: array [0..7] of byte = (cmdSetFilter,$0F, 0, 0, 0, 0, 0, 0);
      filterbyte: byte;
Const S = 'SetFilter';
begin
  filterbyte := $00;
  if channel1 > 0 then filterbyte := $01;
  if channel2 > 0 then filterbyte := filterbyte + $02;
  if trigger  > 0 then filterbyte := filterbyte + $04;
  if (writeControl(CONTROL_BEGINCOMMAND, @B_BUFFER, 10,0) < 0) then ErrorAbort(S,1);
  command[2] := filterbyte;
  if (writeBulk(@command, sizeof(command)) < 0) then ErrorAbort(S,3);
end;

CONST CONTROL_SETOFFSET = $B4; //magic number
procedure SetHardwareOffsets(ch1Offset,ch2Offset,extOffset: integer);
var offset: array[0..16] of byte = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
    temp: word;
begin
 temp := (ch1Offset shr 8) or $20;  //msb of offset aa1a aaaa
 offset[0] := temp;
 offset[1] := ch1Offset and $ff;    //lsb of offset
 temp := (ch2Offset shr 8) or $20;
 offset[2] := temp;
 offset[3] := ch2Offset and $ff;
 temp := (extOffset shr 8) or $20;
 offset[4] := temp;
 offset[5] := extOffset and $ff;
 if (writeControl(CONTROL_SETOFFSET, @offset, sizeof(offset),0) < 0) then ErrorAbort('SetOffset - ',1);
end;


{=========================================================================================
                 PROCEDURE FOR GETTING THE SCOPE CALIBRATION DATA
=========================================================================================}
CONST VALUE_CHANNELLEVEL = $08; //magic number
      CONTROL_COMMAND = $A2;    //magic number
procedure GetChannelLevels;
{ This returns 36 words with the MSB in the _lower_ byte and LSB in the _upper_ byte.
 We refeverse the byte order here.  This procedure is executed during the initionalization
 of this unit, and must be executed once.}
var i: integer;
begin
 if (ReadControl(CONTROL_COMMAND, @channelLevels, SizeOf(channelLevels),VALUE_CHANNELLEVEL) < 0) then
			  ErrorAbort('GetchannelLevel',1);
 for i := 0 to 8 do
   begin
    channelLevels[0,i,0] := swap(channelLevels[0,i,0]);
    channelLevels[0,i,1] := swap(channelLevels[0,i,1]);
    channelLevels[1,i,0] := swap(channelLevels[1,i,0]);
    channelLevels[1,i,1] := swap(channelLevels[1,i,1]);
   end;
end;

Initialization
GetChannelLevels;

end.


