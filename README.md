minimp3 for ultibo
==========

Using the [minimp3](https://github.com/lieff/minimp3) library for Ultibo.

Based on [minimp3-delphi](https://github.com/DasZiesel/minimp3-delphi)


#### Info
Easily get information from mp3 file

```pascal
  FMP3File := TMP3File.Create();
  FMP3File.LoadFromFile('C:\test.mp3');

  ConsoleWindowWriteLn(Handle,'====== Info File mp3 ======================');
  ConsoleWindowWriteLn(Handle,'Channels = '       + Inttostr(FMP3File.FMP3Channels) );
  ConsoleWindowWriteLn(Handle,'Frequency (hz) = ' + Inttostr(FMP3File.FMP3Hz) );
  ConsoleWindowWriteLn(Handle,'Layers = '         + Inttostr(FMP3File.FMP3Layer) );
  ConsoleWindowWriteLn(Handle,'Bitrate (kps) = '  + Inttostr(FMP3File.FMP3Bitrate_kbps) );
  ConsoleWindowWriteLn(Handle,'===========================================');
```

## Examples

You need to copy the **test.mp3** file to the SD card.

* Mp3towav
	* decode the file 'C:\test.mp3' to 'C:\test.wav'
* Player1
	* play file 'C:\test.mp3', (Only audio output jack) Based Example 20 PWM Sound  
* Player2
	* play file 'C:\test.mp3', (Only audio output jack) Based Advanced example - PWM PCM
* Player3
	* play file 'C:\test.mp3', (Audio output HDMI or jack) Based Paul's OMX Audio Test  	

## Helpful links

 * minimp3-delphi
	* https://github.com/DasZiesel/minimp3-delphi
 * minimp3 library
	* https://github.com/lieff/minimp3
 * Ultibo examples
	* https://github.com/ultibohub/Examples
 * Paul's openmax
	* https://github.com/pjde-ultibo/openmax
