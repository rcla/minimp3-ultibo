unit minimp3lib;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SysCalls;

{$linklib minimp3}

const
  MINIMP3_MAX_SAMPLES_PER_FRAME = 1152*2;

type
  mp3d_sample_p = PWord;

  mp3dec_frame_info_t = record
    frame_bytes, frame_offset, channels, hz, layer, bitrate_kbps: integer;
  end;
  mp3dec_frame_info_p = ^mp3dec_frame_info_t;

  mp3dec_t = record
    mdct_overlap: array[0..1, 0..287] of single;
    qmf_state: array[0..959] of single;
    reserv: integer;
    free_format_bytes: integer;
    header: array[0..3] of Byte;
    reserv_buf: array[0..510] of Byte;
  end;
  mp3dec_p = ^mp3dec_t;

function mp3dec_version: Integer; cdecl; external;
procedure mp3dec_init(dec: mp3dec_p); cdecl; external;
function mp3dec_decode_frame(dec: mp3dec_p; mp3_data: PByte; mp3_bytes: Integer; pcm_data: mp3d_sample_p; info: mp3dec_frame_info_p): Integer; cdecl; external;

implementation

end.
