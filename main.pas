unit main;

{$mode objfpc}{$H+}


{======================================================================================
	This unit provides the GUI to the Hantek DSO-2090 oscilloscope.  There are five
  units involved:
                    -----> Display
                    |
                    |
                	MAIN --> dsoControl --> dsoUSB --> libusb.

  All of the variables and constants used in 'scope control are contained in the
  dsoControl unit. The dsoUSB unit handles all of the processes and "magic numbers"
  associated with communicating with the DSO via USB.  Libusb is of course the
  Pascal wrapper for the standard (old) usb library.

  The MAIN unit causes data to be acquired from the 'scope each time the "SAMPLE" button
  on the GUI is clcked.  Execution is then passed to unit "DISPLAY" to format the
  data into the (usually graphical) form desired by the programmer.
 ======================================================================================}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  dsoControl, {the unit that interfaces to the DSO firmware}
  LCLType,    {for message box constants}
  Display;    {to display the acquired 'scope data}


{ TForm1 }

type
  TForm1 = class(TForm)
    BSample: TButton;

    GBCh1: TGroupBox;
      CBCh1BW: TCheckBox;
      CBCh1Range: TComboBox;
      CBCh1Coupling: TComboBox;
      LCh1Offset: TLabel;
      SBCh1Offset: TScrollBar;
      PCh1Trig: TPanel;

    GBCh2: TGroupBox;
      CBCh2BW: TCheckBox;
      CBCh2Range: TComboBox;
      CBCh2Coupling: TComboBox;
      LCh2Offset: TLabel;
      SBCh2Offset: TScrollBar;
      PCh2Trig: TPanel;

    GBTrigger: TGroupBox;
      CBTrigBW: TCheckBox;
      CBTrigSlope: TComboBox;
      LTrigOffset: TLabel;
      SBTrigOffset: TScrollBar;
      LTrigPosition: TLabel;
      SBTrigPosition: TScrollBar;
      CBTrigMode: TComboBox;
      CBTrigSource: TComboBox;
      PExtTrig: TPanel;

    GBSampling: TGroupBox;
      CBBufferSize: TComboBox;
      CBSampleRate: TComboBox;
      LSampleRate: TLabel;
      LDurationCap: TLabel;
      LuSSampleValue: TLabel;
      LDuration: TLabel;
      LOtherRateValue: TLabel;
      LuSSample: TLabel;
      LOther: TLabel;
      LOtherSampleRate: TLabel;


    Label1: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label17: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;

    procedure CBSampleRateChange(Sender: TObject);
    procedure CBSampleRateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BSampleClick(Sender: TObject);
    procedure CBTrigSourceChange(Sender: TObject);
    procedure DoBufferChange(Sender: TObject);
    procedure DoBWUpdate(Sender: TObject);
    procedure DoTrigSlopeChange(Sender: TObject);
    procedure DoUpdateCoupling(Sender: TObject);
    procedure DoUpdateRange(Sender: TObject);
    procedure SBOffsetChange(Sender: TObject);
    procedure SBTrigPositionChange(Sender: TObject);
  private
    procedure CalculateDelays;
  public
  end;

var
  Form1: TForm1;
  {Following are TRUE if DSO hardware controls need to be updated}
  UpdateOffsets: boolean;
  UpdateBWFilters: boolean;
  UpdateRangeControls: boolean;
  UpdateSamplingControls: boolean;

implementation

{$R *.lfm}

{ TForm1 }
procedure TForm1.FormCreate(Sender: TObject);
begin
 {These calls initialize all of the scope control variables to the values indicated
  by the GUI controls when the program starts. This makes it easy to keep the
  variables coordinated with the visual controls as we change our form design.}
 DoUpdateCoupling(Self); //Initialize Ch1Coupling, Ch2Coupling and number of channels in use.
 DoBufferChange(Self);  //Initialize buffer size
 CBSampleRateChange(self);
 SBOffsetChange(Self); //Initialize Ch1Offset,Ch2Offset,TrigOffset from visual contols
 DoBWUpdate(Self); //Initialize Ch1Filter,Ch2Filter,TrigFilter from visual controls
 CBTrigSourceChange(self); //Initialize Trigsource
 DoUpdateRange(Self); //Initialize Ch1V,Ch2V
 DoTrigSlopeChange(Self); //Initialize TrigSlope
 SBTrigPositionChange(Self);
end;

{======================================================================================
   SET UP DSO HARDWARE CONDITIONS AFTER VISUAL CONTROL CHANGES AS REQUIRED
 ======================================================================================}
Procedure SetUpScopeHardware;
var Dosleep: boolean;
begin
  DoSleep := UpdateOffsets or UpdateBWFilters or UpdateRangeControls or UpdateSamplingControls;

  if ( UpdateOffsets ) then
  begin
   SetHardwareOffsets(Ch1Offset,Ch2Offset,TrigOffset);
   UpdateOffsets := false;
  end;

  If ( UpdateBWFilters ) then
  begin
   SetHardwareFilter(Ch1Filter,Ch2Filter,TrigFilter);
   UpdateBWFilters := false;
  end;

  If ( UpdateRangeControls ) then
   begin
     SetHardwareVoltageAndCoupling(Ch1V,Ch2V,Ch1Coupling,Ch2Coupling,TrigSource);
     UpdateRangeControls := false;
   end;

  If ( UpdateSamplingControls) then
   begin
    SetHardwareTriggerAndSampleRate(SRFast,SRSlow,UseChannels,TrigSource,TrigSlope,TrigPosition,BufferSize);
    UpdateSamplingControls := false;
   end;

  If DoSleep then sleep(25); //give the hardware some time to settle into its new state
end;

{======================================================================================
   ACQUIRE SCOPE DATA ON "SAMPLE" BUTTON CLICK

   Note that once data has been acquired, control is passed to the "Display" unit
   which has its own form.  The display unit is where all of the work associated
   with creating a visual presentation of the acquired scope data if performed.

   (To emulate a conventional 'scope, you will want to "automate" a periodic
   SAMPLE button click that takes into account things like the time it takes
   to fill the data buffers, down- and up-sampling as required, adjusting trigger
   position, and so on.  That work is left to you.)

 ======================================================================================}
procedure TForm1.BSampleClick(Sender: TObject);
var
 Attempts: integer;
 SampleDelay: integer;
begin
 {Define how long we will wait for our data buffers to be filled.  This time
  will depenc on the sample rate and buffer size.}
 SampleDelay := round(BufferDuration*5);
 {Set the 'scope hardware controls if necessary.}
 InitializeScopeHardware;
 {Force the capture state to state zero.}
 CaptureStart;
 TriggerEnabled;
 {We will force 'scope triggering if in "auto" trigger mode.}
 if ( CBTrigMode.ItemIndex = 1 ) then forcetrigger;
 {Now we begin to try to fill our data buffers.}
 Attempts := 0;
 GetCaptureState;

 while ( CaptureState <> 2 ) do
  begin
   inc(attempts);
   if (attempts > SampleDelay ) then
    begin
     {We probably never got a trigger, so try to recover...}
     if ( Application.MessageBox('No trigger -- Try Auto mode?', '  No Trigger',MB_YESNO) = IDYES ) then
       begin Attempts := 0; CBTrigMode.ItemIndex := 1; ForceTrigger end
     else
       {Something isn't working!  We abort the program.}
       Begin ShowMessage('Halting program.'); halt end;
    end;
    sleep(1); GetCaptureState;
 end; {while}

 {If we arrive here, CaptureState = 2 and the 'scope hardware data buffers have been filled
  with sample data. The procedure below moves the samples from the hardware into our software
  data buffers.}
 GetChannelData(BufferSize);
 {We reset the 'scope state to zero.  May not be needed, but can't hurt!}
 CaptureStart;
 {Now, we pass control to our "Display" unit (which has its own GUI as "Form2") to create a
 visual presentation of our sample data.}

 {First, we may want to note some operational or debug info on a Form2 memo...}
 Form2.Memo1.clear;
 Form2.BBaseClearClick(Self);
 Form2.Memo1.lines.Add('It took about '+IntToStr(Attempts)+' Ms to fill the scope hardware buffers.');
 Form2.Memo1.lines.Add('');
 {And here, we pass control to our primary display routine, whatever it may be.}
 Form2.ProcessDataBuffers;
end;

{======================================================================================
   HANDLE VISUAL CONTROL CHANGES

   Note that the controls are "coupled," so changing one can have an effect on another.
   Switching from two channels in use to just one, for example, changes the possible
   sample rates and buffer sizes that can be used.
 ======================================================================================}
procedure TForm1.CalculateDelays;
{ Calculate and display some data related to sample rate and buffer size}
begin
 BufferDuration := BufferSize/SampleRate;
 LDuration.Caption := FloatToStr(BufferDuration)+ ' Ms.';
 SampleDuration := 1e3/SampleRate;
 LuSSampleValue.Caption := FloatToStr(SampleDuration)+ ' us.';
end;


procedure TForm1.DoUpdateCoupling(Sender: TObject);
{This procedure determins which channels are in use.  Note that the number of
 channels in use can effect triggering, buffer size, and sample rate.  }
var temp: integer;
begin
  ChannelsInUse := 1;
  Ch1Coupling := CBCh1Coupling.ItemIndex;
  Ch2Coupling := CBCh2Coupling.ItemIndex;
  Temp := 0;
   if CBCh1Coupling.ItemIndex = 2 {OFF} then  Temp := 1;
   if CBCh2Coupling.ItemIndex = 2 {OFF} then  Temp := Temp + 2;
   Case Temp of
    0:  begin UseChannels := USECH1CH2; ChannelsInUse := 2 end;
    1:  UseChannels := USECH2;    //1
    2:  UseChannels := USECH1;    //0
   else
    begin
     ChannelsInUse := 0;
     ShowMessage('No Channels selected');
     UseChannels := USECHNONE;
    end;
   end; {case}
  {The number of samples available after data acquisition depends on the number
   of channels in use.}
  CBBufferSize.Items.clear;
  if ( ChannelsInUse = 1 ) then
   begin
    CBBufferSize.Items.add('20K');
    CBBufferSize.Items.add('64K');
    CBBufferSize.Items.add('1024');
   end
  else
   begin
    CBBufferSize.Items.add('10K');
    CBBufferSize.Items.add('32K');
    CBBufferSize.Items.add('512');
   end;
  CBBufferSize.ItemIndex := 0;
  {The number of samples aqcuired pre millisecond depends on the number of channels
   in use.  For one channel, the maximum sample rate is 100,000 S/MS, while it is
   half that if two channels are in use.}
  CBSampleRate.Items.Clear;
  if ( ChannelsInUse = 1 ) then
   begin
    CBSampleRate.Items.add('100K'); //hardware sample rate is 50000
    CBSampleRate.Items.add('50K');  //hardware sample rate is 25000
    CBSampleRate.Items.add('25K');  //hardware sample rate is 12500
    CBSampleRate.Items.add('20K');//hardware sample rate is 6250
    CBSampleRate.Items.add('10K');  //hardware sample rate is 5000
    CBSampleRate.Items.add('5K');   //hardware sample rate is 2500
    CBSampleRate.Items.add('2.5K'); // etc.  hardware sample rate is 1/2 the displayed rate
    CBSampleRate.Items.add('1K');
    CBSampleRate.Items.add('500');
    CBSampleRate.Items.add('250');
    CBSampleRate.Items.add('100');
    CBSampleRate.Items.add('Other');
   end
  else
   begin
    CBSampleRate.Items.add('50K');  //With two channels in use, hardware sample rate is
    CBSampleRate.Items.add('25K');  //  equal to the displayed rate.
    CBSampleRate.Items.add('12.5K');
    CBSampleRate.Items.add('10K');
    CBSampleRate.Items.add('5K');
    CBSampleRate.Items.add('2.5K');
    CBSampleRate.Items.add('1K');
    CBSampleRate.Items.add('500');
    CBSampleRate.Items.add('250');
    CBSampleRate.Items.add('100');
    CBSampleRate.Items.add('Other');
   end;
  CBsampleRate.ItemIndex := 0;
  CBTrigSourceChange(Self);
  DoBufferChange(self);
  CBSampleRateChange(self);
  UpdateRangeControls := True;
  UpdateSamplingControls := True; {uses UseChannels derived from Coupling settings}
end;

procedure TForm1.CBTrigSourceChange(Sender: TObject);
    procedure ShowTrigWarning;
    begin
     ShowMessage('A selected trigger channel is'+#13#10+'OFF.  No trigger selected.')
    end;
begin
 PCh1Trig.Color := clSilver;
 PCh2Trig.Color := clSilver;
 PExtTrig.Color := clSilver;
 Case CBTrigSource.ItemIndex of
  0: begin  //Ch1 is trigger
      if (( UseChannels = UseCh2 ) or ( UseChannels = UseChNone ))  then ShowTrigWarning
      else
       begin
        PCh1Trig.Color := clLime;
        TrigSource := 1;
       end;
     end;
  1: begin  //Ch2 is trigger
      if (( UseChannels = UseCh1) or ( UseChannels = UseChNone))  then ShowTrigWarning
      else
       begin
        PCh2Trig.Color := clLime;
        TrigSource := 0;
       end;
     end;
  2: begin //alt trigger
      if ( UseChannels <> UseCh1Ch2 ) then ShowTrigWarning
      else
       begin
        PCh1Trig.Color := clLime;
        PCh2Trig.Color := clLime;
        TrigSource := 2;
       end;
     end;
  3: begin //ext trigger
      PExtTrig.Color := clLime;
      TrigSource := 3;
     end;
 end; {case}
 Case CBTrigSource.ItemIndex of
   0:  TrigSource := 1; //TRIGCH1;
   1:  TrigSource := 0; //TRIGCH2;
   2:  TrigSource := 2; //TRIGALT;
   3:  TrigSource := 3; //TRIGEXT;
 end; {case}
 UpdateRangeControls := True; //uses TrigSource
 UpdateSamplingControls := True;
end;

procedure TForm1.DoBufferChange(Sender: TObject);
begin
 case CBBufferSize.ItemIndex of
  0: BufferSize := 10240;
  1: BufferSize := 32768;
  2: BufferSize := 512;
 end; {Case}
 SBTrigPositionChange(Self);
 CalculateDelays;
 UpdateSamplingControls := True;
end;

procedure TForm1.CBSampleRateChange(Sender: TObject);

    function GetInput: integer;
    var V: integer;
        Fail:  boolean;
    begin
     Fail := false;
     try
      V := StrToInt(Inputbox('Sample Rate','Enter sample rate < 10000 S/Ms', LOtherRateValue.Caption));
     except
      Fail := True;
     end;
     if ( V > 10000 ) or ( Fail ) then
      begin
       ShowMessage('Invalid rate -- using last value '+ LOtherRateValue.Caption);
       V := StrToInt(LOtherRateValue.Caption);
      end
     else LOtherRateValue.Caption := IntToStr(V);
    result := V;
   end;

begin
 LOtherSampleRate.Caption := '';
 SRSlow := $FFFF;
 SampleRate := 50000;
 if ( ChannelsInUse = 1 ) then
  begin
   case CBSampleRate.ItemIndex of
    0: Begin SRFast := 1;  SampleRate := 50000 end; //100k
    1: Begin SRFast := 2;  SampleRate := 25000 end; //50k
    2: Begin SRFast := 4;  SampleRate := 12500 end; //25k
    3: Begin SRFast := 3;  SampleRate := 10000 end; //20k
    4: SampleRate := 5000; //10k
    5: SampleRate := 2500; //5k
    6: SampleRate := 1250; //2.5k
    7: SampleRate := 500;  //1k
    8: SampleRate := 250;  //500
    9: SampleRate := 125; //250
    10: SampleRate := 50;  //100
    11: SampleRate := GetInput div 2;
   end {case}
  end
 else
  begin
   case CBSampleRate.ItemIndex of
    0: Begin SRFast := 1;  SampleRate := 50000 end;   //50K
    1: Begin SRFast := 2;  SampleRate := 25000 end;   //25K
    2: Begin SRFast := 4;  SampleRate := 12500 end;   //12.5k
    3: Begin SRFast := 3;  SampleRate := 10000 end;   //10k
    4: SampleRate := 5000; //5K
    5: SampleRate := 2500; //2.5k
    6: SampleRate := 1000; //1K
    7: SampleRate := 500;  //500
    8: SampleRate := 250;  //250
    9: SampleRate := 100;  //100
    10: SampleRate := GetInput;
   end {case};
  end;
 if SampleRate < 10000 then
  begin
   SRSlow := $FFFF xor (((50000 Div SampleRate) - 4) div 2);
   SRFast := 4;
  end;
 CalculateDelays;
 UpdateSamplingControls := True;
end;

procedure TForm1.CBSampleRateClick(Sender: TObject);
begin
  CBSampleRateChange(Self);
end;

procedure TForm1.SBOffsetChange(Sender: TObject);
var OffsetMin, OffsetRange, PosRange: word;
    {some correction factors specific to my scope follow...}
    Ch1Correct: array[0..8] of integer = (1,1,2,1,1,2,1,1,2);
    Ch2Correct: array[0..8] of integer = (0,0,0,0,1,0,0,0,0);
    InvertedPosition: integer;
begin
  LCh1Offset.caption := IntToStr(SBCh1Offset.position);
  LCh2Offset.caption := IntToStr(SBCh2Offset.position);
  LTrigOffset.caption := IntToHex(SBTrigOffset.position,2);
  OffsetMin := channelLevels[0,(8 - Ch1V),0];
  {OffsetRange := channelLevels[0,(8 - Ch1VRange),1] - OffsetMin;
   -- = 104 for me in all but one case where it has value 103, so just use 104 here }
  OffsetRange := 104;
  PosRange := SBCh1Offset.Max - SBCh1Offset.Min;
  {We invert slider position so that increasing position correspond to increasing offset}
  InvertedPosition := SBCh1Offset.Max - SBCh1Offset.Position;
  Ch1Offset := ((SBCh1Offset.Max - InvertedPosition)*OffsetRange) div PosRange + OffsetMin - Ch1Correct[Ch1V];

  OffsetMin := channelLevels[1,(8 - Ch2V),0];
  {OffsetRange := channelLevels[1,(8 - Ch2VRange),1] - OffsetMin;
   -- Almost always = 109 for me, so just use 109 for all range settings.}
  OffsetRange := 109;
  PosRange := SBCh2Offset.Max - SBCh2Offset.Min;
  InvertedPosition := SBCh2Offset.Max - SBCh2Offset.Position;
  Ch2Offset := ((SBCH2Offset.Max - InvertedPosition)*OffsetRange) div PosRange + OffsetMin - Ch2Correct[Ch2V];

  OffsetMin := 0;
  OffsetRange := $FF;
  PosRange := SBTrigOffset.Max - SBTrigOffset.Min;
  InvertedPosition := SBTrigOffset.Max - SBTrigOffset.Position;
  TrigOffset := ((SBTrigOffSet.Max - InvertedPosition)*OffsetRange) div PosRange + OffsetMin;
  UpdateOffsets := True;
end;

procedure TForm1.SBTrigPositionChange(Sender: TObject);
begin
 {===========================================================================================
     The TriggerPosition sets the position of the pretrigger in samples. The left side (0 %)
     is 0x77660 when using the small buffer and 0x78000 when using the large buffer according
     to OpenHantek.   I've not worked with this to test it.
  ===========================================================================================}
  If ( BufferSize = 32768 ) or ( BufferSize = 65536 ) then TrigPosition := $78000 else TrigPosition := $77660;
  inc(TrigPosition,(BufferSize-1)*SBTrigPosition.Position div 100);
  LTrigPosition.Caption := IntToStr(SBTrigPosition.Position);
  UpdateSamplingControls := True;
end;

procedure TForm1.DoBWUpdate(Sender: TObject);
begin
 if CBCh1BW.Checked then CBCh1BW.Color := clLime else CBCh1BW.Color := clWhite;
 if CBCh2BW.Checked then CBCh2BW.Color := clLime else CBCh2BW.Color := clWhite;
 if CBTrigBW.Checked then CBTrigBW.Color := clLime else CBTrigBW.Color := clWhite;
 Ch1Filter := ord( CBCh1BW.Checked );
 Ch2Filter := ord( CBCh2BW.Checked );
 TrigFilter := ord( CBTrigBW.Checked );
 UpdateBWFilters := True;
end;


procedure TForm1.DoUpdateRange(Sender: TObject);
begin
  Ch1V := CBCh1Range.ItemIndex;
  Ch2V := CBCh2Range.ItemIndex;
  UpdateRangeControls := True;
  SBOffsetChange(Self);  //changing range changes offset values
end;

procedure TForm1.DoTrigSlopeChange(Sender: TObject);
begin
  TrigSlope := CBTrigSlope.ItemIndex;
  UpdateSamplingControls := True;
end;

end.
