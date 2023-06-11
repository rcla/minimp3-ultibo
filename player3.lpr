program player3;

{$mode delphi}{$H+}
{$hints off}
{$notes off}

{  Player mp3 based Pauls openmax                     }
{  You can select the audio output by HDMI or Jack    }

uses
  RaspberryPi3,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Console,
  VC4,
  uIL_Client,
  uOMX,
  uminimp3,
  Ultibo;

var
  Handle:TWindowHandle;
  FMP3File: TMP3File;
  AudioStream : TMemoryStream;
  client : PILCLIENT_T;
  Channels : Word;
  SampleRate : LongWord;
  BitsPerSample : Word;
  comps : array of PCOMPONENT_T;
  eos : boolean;

procedure Log (s : string);
begin
  ConsoleWindowWriteLn (Handle, s);
end;

  // Audio specific routines
  function ComponentName (comp : PCOMPONENT_T) : string;
  var
    n : array [0..127] of char;
    cv, sv : OMX_VERSIONTYPE;
    cu : OMX_UUIDTYPE;
  begin
    Result := '';
    n[0] := #0;
    FillChar (n, 128, 0);
    if OMX_GetComponentVersion (ilclient_get_handle (comp), n, @cv, @sv, @cu) = OMX_ErrorNone then
      begin
        Result := string (n);
        Result := Copy (Result, 14, length (Result) - 13); // remove OMX.Broadcom.
      end;
  end;

  procedure PrintState (handle : OMX_HANDLETYPE);
  var
    state : OMX_STATETYPE;
    err : OMX_ERRORTYPE;
  begin
    err := OMX_GetState (handle, @state);
    if (err <> OMX_ErrorNone) then
      Log ('Error getting state.')
    else
      case state of
        OMX_StateLoaded           : Log ('State Loaded.');
        OMX_StateIdle             : Log ('State Idle.');
        OMX_StateExecuting        : Log ('State Executing.');
        OMX_StatePause            : Log ('State Pause.');
        OMX_StateWaitForResources : Log ('State Wait.');
        OMX_StateInvalid          : Log ('State Invalid.');
        else                        Log ('State Unknown');
        end;
  end;

procedure port_settings_callback (userdata : pointer; comp : PCOMPONENT_T; data : LongWord); cdecl;
begin
//
end;

procedure empty_buffer_callback (userdata : pointer; comp : PCOMPONENT_T; data : LongWord); cdecl;
begin
//
end;

procedure fill_buffer_callback (userdata : pointer; comp : PCOMPONENT_T; data : LongWord); cdecl;
begin
//
end;

procedure eos_callback (userdata : pointer; comp : PCOMPONENT_T; data : LongWord); cdecl;
begin
  Log (ComponentName (comp) + ' End Of Stream Detected.');
  eos := true;
end;

procedure error_callback (userdata : pointer; comp : PCOMPONENT_T; data : LongWord); cdecl;
begin
  Log (ComponentName (comp) + ' Error : ' + OMX_ErrToStr (data));
end;

function read_into_buffer_and_empty (comp : PCOMPONENT_T;
                                     buff : POMX_BUFFERHEADERTYPE) : OMX_ERRORTYPE;
var
  buff_size : integer;
  read : integer;
begin
  buff_size := buff.nAllocLen;
  read := AudioStream.Size - AudioStream.Position;
  if read > buff_size then read := buff_size;
  AudioStream.Read (buff.pBuffer^, read);
  buff.nFilledLen := read;
  if AudioStream.Position = AudioStream.Size then
    buff.nFlags := buff.nFlags or OMX_BUFFERFLAG_EOS;
  Result := OMX_EmptyThisBuffer (ilclient_get_handle (comps[0]), buff);
end;

procedure OMXCheck (n : string; r : OMX_ERRORTYPE);
begin
  if r <> OMX_ErrorNone then raise Exception.Create (n + ' ' + OMX_ErrToStr (r));
end;

procedure ILCheck (n : string; e : integer);
begin
  if e <> 0 then raise Exception.Create (n + ' Failed');
end;

procedure PlayFile (dn : string);
var
  res : integer;
  param : OMX_PARAM_PORTDEFINITIONTYPE;
  pcm : OMX_AUDIO_PARAM_PCMMODETYPE;
  dest : OMX_CONFIG_BRCMAUDIODESTINATIONTYPE;
  hdr : POMX_BUFFERHEADERTYPE;
begin
  if not (Channels in [1 .. 2]) then exit;
  if AudioStream = nil then exit;
  if AudioStream.Size = 0 then exit;
  AudioStream.Seek (0, soFromBeginning);
  SetLength (comps, 2);
  client := nil;
  eos := false;
  for res := low (comps) to high (comps) do comps[res] := nil;
  // initialise OMX and IL client
  try
    OMXCheck ('OMX Init', OMX_Init);
    client := ilclient_init;
    // set callbacks
    ilclient_set_port_settings_callback (client, @port_settings_callback, nil);
    ilclient_set_empty_buffer_done_callback (client, @empty_buffer_callback, nil);
    ilclient_set_fill_buffer_done_callback (client, @fill_buffer_callback, nil);
    ilclient_set_eos_callback (client, @eos_callback, nil);
    ilclient_set_error_callback (client, @error_callback, nil);
    // create render
    ILCheck ('Create Render', ilclient_create_component (client, @comps[0], 'audio_render', ILCLIENT_ENABLE_INPUT_BUFFERS or ILCLIENT_DISABLE_ALL_PORTS));
    // confirm port is set to pcm
    FillChar (param, sizeof (OMX_PARAM_PORTDEFINITIONTYPE), 0);
    param.nSize := sizeof (OMX_PARAM_PORTDEFINITIONTYPE);
    param.nVersion.nVersion := OMX_VERSION;
    param.nPortIndex := 100; // audio input
    OMXCheck ('Get Port', OMX_GetParameter (ilclient_get_handle (comps[0]), OMX_IndexParamPortDefinition, @param));
    param.format.audio.eEncoding := OMX_AUDIO_CodingPCM;
    OMXCheck ('Set Port ', OMX_SetParameter (ilclient_get_handle (comps[0]), OMX_IndexParamPortDefinition, @param));
    // set sampling rate, channels and bits per sample
    FillChar (pcm, sizeof (OMX_AUDIO_PARAM_PCMMODETYPE), 0);
    pcm.nSize := sizeof (OMX_AUDIO_PARAM_PCMMODETYPE);
    pcm.nVersion.nVersion := OMX_VERSION;
    pcm.nPortIndex := 100;
    OMXCheck ('Get PCM',OMX_GetParameter (ilclient_get_handle (comps[0]), OMX_IndexParamAudioPcm, @pcm));
    pcm.nChannels := Channels;
    pcm.nBitPerSample := BitsPerSample;
    pcm.nSamplingRate := SampleRate;
    FillChar (pcm.eChannelMapping, sizeof (pcm.eChannelMapping), 0);
    case Channels of
      1 :
        begin
          pcm.eChannelMapping[0] := OMX_AUDIO_ChannelCF;
        end;
      2 :
        begin
          pcm.eChannelMapping[1] := OMX_AUDIO_ChannelRF;
          pcm.eChannelMapping[0] := OMX_AUDIO_ChannelLF;
        end;
     end;
    OMXCheck ('Set PCM', OMX_SetParameter (ilclient_get_handle (comps[0]), OMX_IndexParamAudioPcm, @pcm));
    // set to idle and enable buffers
    ilclient_change_component_state (comps[0], OMX_StateIdle);
    ILCheck ('Enable Buffers', ilclient_enable_port_buffers (comps[0], 100, nil, nil, nil));
    ilclient_enable_port (comps[0], 100);
    // set to executing
    ilclient_change_component_state (comps[0], OMX_StateExecuting);
    // set destination
    dest.nSize:= sizeof (OMX_CONFIG_BRCMAUDIODESTINATIONTYPE);
    dest.nVersion.nVersion := OMX_VERSION;
    FillChar (dest.sName, sizeof (dest.sName), 0);
    dest.sName := dn;
    OMXCheck ('Set Dest', OMX_SetConfig (ilclient_get_handle (comps[0]), OMX_IndexConfigBrcmAudioDestination, @dest));
    while AudioStream.Position < AudioStream.Size do
      begin
        hdr := ilclient_get_input_buffer (comps[0], 100, 1);
        if hdr <> nil then read_into_buffer_and_empty (comps[0], hdr);
      end;
    while not eos do sleep (100);
  finally
    ilclient_change_component_state (comps[0], OMX_StateLoaded);
    ilclient_cleanup_components (@comps[0]);
    ilclient_destroy (client);
    client := nil;
    OMX_DeInit;
    end;
  Log ('End of Playback ...');
end;


procedure WaitForSDDrive;
begin
  while not DirectoryExists ('C:\') do sleep (500);
end;

begin
  Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
  ConsoleWindowWriteLn(Handle,'Welcome to Player mp3 based Pauls openmax');
  ConsoleWindowWriteLn(Handle,'You can select the audio output by HDMI or Jack');
  ConsoleWindowWriteLn(Handle,'');

  WaitForSDDrive;

  ConsoleWindowWriteLn(Handle,'Load file test.mp3');
  FMP3File := TMP3File.Create();
  FMP3File.LoadFromFile('C:\test.mp3');

  Channels := FMP3File.FMP3Channels;
  SampleRate := FMP3File.FMP3Hz;
  BitsPerSample := cBitsPerSample;

  ConsoleWindowWriteLn(Handle,'====== Info File mp3 ======================');
  ConsoleWindowWriteLn(Handle,'Channels = '       + Inttostr(Channels) );
  ConsoleWindowWriteLn(Handle,'Frequency (hz) = ' + Inttostr(SampleRate) );
  ConsoleWindowWriteLn(Handle,'Layers = '         + Inttostr(FMP3File.FMP3Layer) );
  ConsoleWindowWriteLn(Handle,'Bitrate (kps) = '  + Inttostr(FMP3File.FMP3Bitrate_kbps) );
  ConsoleWindowWriteLn(Handle,'===========================================');

  BCMHostInit;

  AudioStream := TMemoryStream.Create;

  //Decode file Mp3 to AudioStream
  FMP3File.DecodeTo(AudioStream);
  AudioStream.Position := 0;

  ConsoleWindowWriteLn(Handle,'Start Play');

  //use hdmi audio output
  PlayFile ('hdmi');

  //use audio output jack
  //PlayFile ('local');

  AudioStream.Free;
  FMP3File.Free;

  {Halt the thread}
  ThreadHalt(0);

end.

