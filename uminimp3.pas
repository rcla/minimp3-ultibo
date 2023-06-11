unit uminimp3;
{$mode objfpc}{$H+}
{$hints off}

{  Using the minimp3 library for ultibo                                        }
{                                                                              }
{  Original Code @DasZiesel https://github.com/DasZiesel/minimp3-delphi        }
{                                                                              }
{  Code adapted for Ultibo @rcla https://github.com/rcla/minimp3-ultibo                                              }
{                                                                              }
{  minimp3 library @lieff https://github.com/lieff/minimp3                     }
{                                                                              }

interface

uses
  Classes, SysUtils, Math, minimp3lib;

const
  cBitsPerSample = 16;
  WAVE_FORMAT_PCM = 1;

type
  
  tWAVEFORMATEX = packed record
    wFormatTag: Word;       { format type }
    nChannels: Word;        { number of channels (i.e. mono, stereo, etc.) }
    nSamplesPerSec: DWORD;  { sample rate }
    nAvgBytesPerSec: DWORD; { for buffer estimation }
    nBlockAlign: Word;      { block size of data }
    wBitsPerSample: Word;   { number of bits per sample of mono data }
    cbSize: Word;           { the count in bytes of the size of }
  end;
  PWaveFormatEx = ^TWaveFormatEx;
  
  TWaveFormatHeader = record // Cut WaveformatEx (leave cbSize);
      wFormatTag: Word;       { format type }
      nChannels: Word;        { number of channels (i.e. mono, stereo, etc.) }
      nSamplesPerSec: DWORD;  { sample rate }
      nAvgBytesPerSec: DWORD; { for buffer estimation }
      nBlockAlign: Word;      { block size of data }
      wBitsPerSample: Word;   { number of bits per sample of mono data }
  end;

  TRIFFWaveFileHeader = record
    riffIdent: DWORD;
    riffSize : DWORD;
    waveIdent: DWORD;
    fmtIdent : DWORD;
    fmtLength: DWORD;
    wavehdr  : TWaveFormatHeader;
    dataIdent: DWORD;
    dataSize : DWORD;
  end;

  TMemoryStreamExtend = class(TMemoryStream)
    private
      function GetMemoryAtPosition: Pointer;
      function GetBytesLeft: Longint;
    public
      function IncPtr(const Bytes: Longint): Boolean;
      property DataPtr: Pointer read GetMemoryAtPosition;
      property DataLen: Longint read GetBytesLeft;
    end;
    
  TMP3File = class
  private
    FMP3Decoder     : mp3dec_t;
    FMP3Data        : TMemoryStreamExtend;

    FPCMBuffer      : Pointer;
    FPCMBufferOffset: NativeInt;
    FPCMBufferLen   : NativeInt;
    FPCMFrameSize   : NativeInt;
    FPCMTotalSize   : NativeInt;

    procedure ClearStream();
    function GetFormat(fmt: PWaveFormatEx): Boolean;

  public

    // Info
    FMP3Channels    : Integer;
    FMP3Hz          : Cardinal;
    FMP3Layer       : Integer;
    FMP3Bitrate_kbps: Cardinal;
    FAvgBytesPerSec : Cardinal;

    constructor Create;
    destructor Destroy; override;

    procedure DecodeTo(const Buffer: Pointer; BufLen: Integer); overload;
    procedure DecodeTo(const Filename: String); overload;
    procedure DecodeTo(const Stream: TStream); overload;

    procedure LoadFromFile(const Filename: String);
  end;
  
var
  auxerro:Integer;

implementation

procedure ZeroMemory(Destinationz: Pointer; sizem: DWORD);
begin
  FillChar(Destinationz^, sizem, #0);
end;

procedure CopyMemory(Destination, Source: Pointer; Lengthm: DWORD);
begin
  Move(Source^, Destination^, Lengthm);
end;

 { TMemoryStreamExtend }

 function TMemoryStreamExtend.GetBytesLeft: Longint;
 begin
   Result := Size - Position;
 end;

 function TMemoryStreamExtend.GetMemoryAtPosition: Pointer;
 begin
   Result := {%H-}Pointer(NativeUInt(Memory) + Position);
 end;

 function TMemoryStreamExtend.IncPtr(const Bytes: Longint): Boolean;
 begin
   Result := ((Position + Bytes) < Size);
   Position := Position + Min(Bytes, Size - Position);
 end;
 
{ TMP3File }

procedure TMP3File.ClearStream;
begin
  if Assigned(FMP3Data) then
    FreeAndNil(FMP3Data);

  if (FPCMBuffer <> nil) then
    FreeMem(FPCMBuffer, FPCMBufferLen);

  FPCMBuffer := nil;
  FPCMBufferLen := 0;
  FPCMTotalSize := 0;
end;

constructor TMP3File.Create;
begin
  FMP3Data := nil;
  FPCMBuffer := nil;
  FPCMBufferLen := 0;
  FPCMBufferOffset := 0;
  FPCMTotalSize := 0;
end;

procedure TMP3File.DecodeTo(const Stream: TStream);
var
  pcmDat: mp3d_sample_p;
  pmcLen: Cardinal;
  samples: Integer;
  frame_info: mp3dec_frame_info_t;
begin
  pmcLen := FPCMFrameSize;

  GetMem(pcmDat, pmcLen);
  try
    repeat
      samples := mp3dec_decode_frame(
        @FMP3Decoder,
        FMP3Data.DataPtr,
        FMP3Data.DataLen,
        pcmDat,
        @frame_info
      );

      if (samples = 1152) then
        Stream.Write(pcmDat^, pmcLen)
      else
        auxerro:=1;//OutputDebugString('+++++ INVALID BLOCK ++++++');

      FPCMTotalSize := FPCMTotalSize + FPCMFrameSize;
    until not FMP3Data.IncPtr(frame_info.frame_offset + frame_info.frame_bytes);
  finally
    FreeMem(pcmDat, pmcLen);
  end;
end;

procedure TMP3File.DecodeTo(const Filename: String);
var
  fileStream: TFileStream;
  wavFileHeader: TRIFFWaveFileHeader;
  pcmData: mp3d_sample_p;
  pcmLength: Cardinal;
  wex: tWAVEFORMATEX;
  samples, wavHeaderLen: Integer;
  frame_info: mp3dec_frame_info_t;
begin

  if FileExists(Filename) then DeleteFile(Filename);

  pcmLength := MINIMP3_MAX_SAMPLES_PER_FRAME * FMP3Channels;
  GetMem(pcmData, pcmLength);
  fileStream := TFileStream.Create(Filename, fmCreate);

  wavHeaderLen := SizeOf(TRIFFWaveFileHeader);
  ZeroMemory(@wavFileHeader, wavHeaderLen);

  wavFileHeader.riffIdent := 1179011410;//FOURCC_RIFF;

  GetFormat(@wex);

  wavFileHeader.waveIdent := 1163280727; //mmioStringToFOURCC('WAVE', 0);
  wavFileHeader.fmtIdent  := 544501094;//mmioStringToFOURCC('fmt', 0);
  wavFileHeader.fmtLength := 16;
  wavFileHeader.dataIdent := 1635017060;//mmioStringToFOURCC('data', 0);

  wavFileHeader.wavehdr.wFormatTag := wex.wFormatTag;
  wavFileHeader.wavehdr.nChannels := wex.nChannels;
  wavFileHeader.wavehdr.nSamplesPerSec := wex.nSamplesPerSec;
  wavFileHeader.wavehdr.nAvgBytesPerSec := wex.nAvgBytesPerSec;
  wavFileHeader.wavehdr.nBlockAlign := wex.nBlockAlign;
  wavFileHeader.wavehdr.wBitsPerSample := wex.wBitsPerSample;
  fileStream.Write(wavFileHeader, wavHeaderLen);
  try
    FMP3Data.Position := 0;

    repeat
      samples := mp3dec_decode_frame(
        @FMP3Decoder,
        FMP3Data.DataPtr,
        FMP3Data.DataLen,
        pcmData,
        @frame_info
      );

      Assert(samples = 1152, 'Unknow');

      fileStream.Write(pcmData^, pcmLength);
      until not FMP3Data.IncPtr(frame_info.frame_offset + frame_info.frame_bytes);

      wavFileHeader.riffSize := fileStream.Size - 8;
      wavFileHeader.dataSize := fileStream.Size - 44;
      fileStream.Position := 0;
      fileStream.Write(wavFileHeader, wavHeaderLen);
  finally
    FreeMem(pcmData, pcmLength);
    fileStream.free;
  end;
end;

procedure TMP3File.DecodeTo(const Buffer: Pointer; BufLen: Integer);
var
  pcmData: mp3d_sample_p;
  pcmSize: NativeInt;
  size: Cardinal;
  samples: Integer;
  frame_info: mp3dec_frame_info_t;
  iRest: Integer;
begin
  pcmData := {%H-}Pointer(NativeUint(FPCMBuffer) + FPCMBufferOffset);
  pcmSize := FPCMBufferLen - FPCMBufferOffset;
  size := FPCMBufferOffset;
  FPCMBufferOffset := 0;

  while (pcmSize >= FPCMFrameSize) do
  begin
    samples := mp3dec_decode_frame(
      @FMP3Decoder,
      FMP3Data.DataPtr,
      FMP3Data.DataLen,
      pcmData,
      @frame_info
    );

    pcmData := {%H-}mp3d_sample_p(NativeUInt(pcmData) + FPCMFrameSize);
    pcmSize := pcmSize - FPCMFrameSize;
    size := size + FPCMFrameSize;
    FPCMTotalSize := FPCMTotalSize + FPCMFrameSize;
    if not FMP3Data.IncPtr(frame_info.frame_offset + frame_info.frame_bytes) then
    begin
      auxerro:=1;//OutputDebugString(PChar('PANIC'));
      break;
    end;
  end;

  CopyMemory(Buffer, FPCMBuffer, BufLen);
  iRest := size - BufLen; // \DCberhang verschieben auf den Anfang

  if (iRest > 0) then
  begin
    FPCMBufferOffset := iRest;
    CopyMemory(
      FPCMBuffer,
      {%H-}Pointer(NativeUInt(FPCMBuffer) + BufLen),
      iRest
    );
  end;
end;

destructor TMP3File.Destroy;
begin
  ClearStream();
  inherited;
end;

function TMP3File.GetFormat(fmt: PWaveFormatEx): Boolean;
var
  nAvgBytesPerSec: Integer;
  nBlockAlign: Integer;
begin
  Result := Assigned(fmt) and (FMP3Data <> nil) and (FMP3Data.Size > 0);

  if Result then
  begin
    nAvgBytesPerSec := (cBitsPerSample * FMP3Channels * FMP3Hz) shr 3;
    nBlockAlign     := (cBitsPerSample * FMP3Channels) shr 3;

    // Wave Header
    ZeroMemory(fmt, SizeOf(tWAVEFORMATEX));
    fmt^.wFormatTag := WAVE_FORMAT_PCM;
    fmt^.nChannels := FMP3Channels;
    fmt^.nSamplesPerSec := FMP3Hz;
    fmt^.wBitsPerSample := cBitsPerSample;
    fmt^.nBlockAlign := nBlockAlign;
    fmt^.nAvgBytesPerSec := nAvgBytesPerSec;
  end;
end;

procedure TMP3File.LoadFromFile(const Filename: String);
var
  fs: TFileStream;
  samples: Integer;
  frame_info: mp3dec_frame_info_t;
//  dwDataBytes: NativeInt;
begin
  ClearStream();

  try
    fs := TFileStream.Create(Filename, fmOpenRead);
    try
      FMP3Data := TMemoryStreamExtend.Create();
      FMP3Data.CopyFrom(fs, fs.Size);
    finally
      fs.Free();
    end;
  except
    raise;
  end;

  // Reset Position
  FMP3Data.Position := 0;

  // Init Decoder
  mp3dec_init(@FMP3Decoder);

  // Decoder the first block; without write pcm data; need info
  samples := mp3dec_decode_frame(
    @FMP3Decoder,
    FMP3Data.DataPtr,
    FMP3Data.DataLen,
    nil,
    @frame_info
  );

  Assert(samples = 1152, 'Invalid mp3');

  // Skip junk
  Assert(
    FMP3Data.IncPtr(frame_info.frame_offset),
    'no data in file'
  );

  //
//  dwDataBytes := FMP3Data.Size - frame_info.frame_offset;
  FMP3Channels := frame_info.channels;
  FMP3Hz := frame_info.hz;
  FMP3Layer := frame_info.layer;
  FMP3Bitrate_kbps := frame_info.bitrate_kbps;
  FAvgBytesPerSec := (cBitsPerSample * FMP3Channels * FMP3Hz) shr 3;

  // Single Frame 16-bit samples (2 Bytes) * Channels (1 = Mono; 2 = Stereo)
  FPCMFrameSize := (MINIMP3_MAX_SAMPLES_PER_FRAME * frame_info.channels);

  // Calculate the Buffer to hold all decrypted Frames avoid avgBytesPerSecond
  FPCMBufferLen := FPCMFrameSize * ((FAvgBytesPerSec div FPCMFrameSize) + 1);
  FPCMBufferOffset := 0;
  GetMem(FPCMBuffer, FPCMBufferLen);
end; 

end.
