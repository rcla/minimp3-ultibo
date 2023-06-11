program mp3towav;

{$mode objfpc}{$H+}

{  Example of using uminimp3 to convert an mp3 file to wav  }

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
  uminimp3,
  Ultibo;

var
  Handle:TWindowHandle;
  FMP3File: TMP3File;

procedure WaitForSDDrive;
begin
  while not DirectoryExists ('C:\') do sleep (500);
end;

begin
  Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
  ConsoleWindowWriteLn(Handle,'Convert MP3');
  ConsoleWindowWriteLn(Handle,'');

  WaitForSDDrive;

  ConsoleWindowWriteLn(Handle,'Load file test.mp3');
  FMP3File := TMP3File.Create();
  FMP3File.LoadFromFile('C:\test.mp3');

  ConsoleWindowWriteLn(Handle,'====== Info File mp3 ======================');
  ConsoleWindowWriteLn(Handle,'Channels = '       + Inttostr(FMP3File.FMP3Channels) );
  ConsoleWindowWriteLn(Handle,'Frequency (hz) = ' + Inttostr(FMP3File.FMP3Hz) );
  ConsoleWindowWriteLn(Handle,'Layers = '         + Inttostr(FMP3File.FMP3Layer) );
  ConsoleWindowWriteLn(Handle,'Bitrate (kps) = '  + Inttostr(FMP3File.FMP3Bitrate_kbps) );
  ConsoleWindowWriteLn(Handle,'===========================================');

  ConsoleWindowWriteLn(Handle,'');
  ConsoleWindowWriteLn(Handle,'Start Decode mp3 to wav');
  FMP3File.DecodeTo('C:\test.wav') ;

  ConsoleWindowWriteLn(Handle,'Finished the decoding');

  FMP3File.Free;

  {Halt the thread}
  ThreadHalt(0);

end.

