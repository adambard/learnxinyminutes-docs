---
category: tool
name: FFmpeg
contributors:
    - ["Mark Santos", "https://www.markstudios.com"]
filename: learnffmpeg.sh
---

[FFmpeg](https://ffmpeg.org/) is a command-line toolkit for converting,
cutting, filtering and inspecting audio and video. It ships three programs:
`ffmpeg` converts and processes, `ffprobe` inspects, and `ffplay` plays.

```bash
# The basic shape: inputs with -i, then the output file.
# The output format is guessed from the file extension.
ffmpeg -i input.mov output.mp4

# An option applies to the next file named after it. Options before -i
# affect that input; options before the output name affect the output.
#   ffmpeg [input options] -i input [output options] output

ffmpeg -y -i in.mp4 out.mkv              # Overwrite the output, don't ask
ffmpeg -n -i in.mp4 out.mkv              # Never overwrite
ffmpeg -hide_banner -i in.mp4 out.mkv    # Skip the build/config banner
ffmpeg -loglevel error -i in.mp4 out.mkv # Print errors only

# No files at hand? The lavfi input format generates test media.
# Five seconds of a 1280x720, 30 fps test pattern plus a 440 Hz tone:
ffmpeg -f lavfi -i testsrc2=duration=5:size=1280x720:rate=30 \
       -f lavfi -i sine=frequency=440:duration=5 \
       -c:v libx264 -c:a aac -shortest test.mp4
```

## Inspecting files with ffprobe

```bash
ffprobe test.mp4                       # Human-readable summary

# Duration in seconds, nothing else
ffprobe -v error -show_entries format=duration -of csv=p=0 test.mp4
# 5.000000

# Codec, size and frame rate of the first video stream
ffprobe -v error -select_streams v:0 \
        -show_entries stream=codec_name,width,height,r_frame_rate \
        -of csv=p=0 test.mp4
# h264,1280,720,30/1

# Everything, as JSON (pairs well with jq)
ffprobe -v error -show_format -show_streams -of json test.mp4
```

## Containers, streams and codecs

A file is a container (MP4, MKV, MOV, WebM) that holds streams: video,
audio, subtitles. Each stream is encoded with a codec. Stream specifiers
pick streams: `v` is video, `a` audio, `s` subtitles, and `0:a:1` is the
second audio stream of the first input (counting starts at 0).

```bash
# Remux: change the container without re-encoding. Fast and lossless,
# but the new container has to support the codecs inside.
ffmpeg -i in.mkv -c copy out.mp4

# Re-encode to H.264 + AAC, the most widely playable combination.
# -crf sets quality (0-51, lower is better; 18-28 is the useful range).
# -preset trades encoding speed for file size (ultrafast ... veryslow).
ffmpeg -i in.mov -c:v libx264 -crf 23 -preset medium \
       -c:a aac -b:a 128k out.mp4

# Keep the video as is, re-encode only the audio
ffmpeg -i in.mp4 -c:v copy -c:a aac -b:a 192k out.mp4

# Many players and browsers only play 8-bit 4:2:0 H.264
ffmpeg -i in.mov -c:v libx264 -pix_fmt yuv420p out.mp4

# Move the index to the front, so web playback starts before the
# download finishes
ffmpeg -i in.mp4 -c copy -movflags +faststart out.mp4

# H.265/HEVC: smaller files at the same quality, slower to encode.
# The hvc1 tag makes Apple players accept it.
ffmpeg -i in.mp4 -c:v libx265 -crf 28 -tag:v hvc1 -c:a copy out.mp4

# VP9 + Opus in WebM (-b:v 0 lets -crf alone set the quality)
ffmpeg -i in.mp4 -c:v libvpx-vp9 -crf 32 -b:v 0 -c:a libopus out.webm

# Drop streams: -an (audio), -vn (video), -sn (subtitles)
ffmpeg -i in.mp4 -an -c:v copy silent.mp4

# Choose streams yourself with -map: the video of the first input,
# the audio of the second. -shortest stops at the end of the shorter one.
ffmpeg -i video.mp4 -i music.wav -map 0:v -map 1:a \
       -c:v copy -c:a aac -shortest out.mp4
```

## Cutting

```bash
# -ss is the start, -t the duration, -to the end time.
# Times are seconds (12.5) or [HH:]MM:SS[.ms] (00:01:02.5).
ffmpeg -ss 00:00:01.5 -i in.mp4 -t 2 -c:v libx264 -c:a aac clip.mp4

# -ss before -i seeks in the input, which is fast. Output timestamps then
# restart at zero, so an output -to acts like -t:
ffmpeg -ss 1 -i in.mp4 -to 3 -c:v libx264 -c:a aac three-seconds.mp4
# Put both before -i to cut from 1 s to 3 s of the original:
ffmpeg -ss 1 -to 3 -i in.mp4 -c:v libx264 -c:a aac two-seconds.mp4

# With -c copy nothing is re-encoded, so a cut can only start on a
# keyframe and may begin a little early. Re-encode when it must be exact.
ffmpeg -ss 1 -to 3 -i in.mp4 -c copy rough-cut.mp4
```

## Filters

```bash
# -vf (video) and -af (audio) take a filter chain: filters separated by
# commas, options as key=value pairs separated by colons.

# Scale to 1280 wide; -2 keeps the aspect ratio with an even height
ffmpeg -i in.mp4 -vf scale=1280:-2 out.mp4

# Change the frame rate
ffmpeg -i in.mp4 -vf fps=24 out.mp4

# Crop a centered 9:16 vertical frame out of a 16:9 one
ffmpeg -i in.mp4 -vf "crop=ih*9/16:ih" vertical.mp4

# Fit a vertical clip into a 1920x1080 frame with black bars
ffmpeg -i vertical.mp4 \
       -vf "scale=-2:1080,pad=1920:1080:(ow-iw)/2:(oh-ih)/2" boxed.mp4

# Rotate 90 degrees clockwise, then mirror
ffmpeg -i in.mp4 -vf "transpose=1,hflip" out.mp4

# Play twice as fast: setpts for the video, atempo for the audio
ffmpeg -i in.mp4 -vf "setpts=PTS/2" -af "atempo=2" fast.mp4

# Fade in from black and from silence over the first second
ffmpeg -i in.mp4 -vf "fade=t=in:st=0:d=1" -af "afade=t=in:st=0:d=1" out.mp4

# -filter_complex builds a graph with several inputs or outputs.
# [0:v] is the video of the first input; names like [m] are your own labels.

# Put logo.png in the top-right corner with a 20 px margin
ffmpeg -i in.mp4 -i logo.png \
       -filter_complex "[0:v][1:v]overlay=W-w-20:20" -c:a copy out.mp4

# Two clips side by side (they need the same height)
ffmpeg -i left.mp4 -i right.mp4 \
       -filter_complex "[0:v][1:v]hstack" -an side-by-side.mp4

# Burn text into the picture (needs a build with libfreetype)
ffmpeg -i in.mp4 \
       -vf "drawtext=text='Take 2':x=20:y=20:fontsize=48:fontcolor=white" \
       -c:a copy titled.mp4
```

## Audio

```bash
# Extract the audio track without re-encoding (AAC goes in .m4a)
ffmpeg -i in.mp4 -vn -c:a copy audio.m4a

# Convert to MP3 (-q:a 2 is variable bitrate, around 190 kbps)
ffmpeg -i in.wav -c:a libmp3lame -q:a 2 out.mp3

# Resample to 48 kHz stereo
ffmpeg -i in.wav -ar 48000 -ac 2 out.wav

# Halve the volume, or raise it by 6 dB
ffmpeg -i in.wav -af "volume=0.5" quieter.wav
ffmpeg -i in.wav -af "volume=6dB" louder.wav

# Measure loudness (EBU R128). The summary at the end gives the
# integrated loudness in LUFS. "-f null -" decodes without writing a file.
ffmpeg -i in.mp4 -af ebur128 -f null -

# Normalize to -14 LUFS integrated with peaks under -1 dBTP, a common
# target for streaming. loudnorm works at a high sample rate inside, so
# set the output rate with -ar. A single pass is close, not exact: for an
# exact result, run it once with print_format=json added and "-f null -"
# as the output, then pass the input_i, input_tp, input_lra and
# input_thresh it prints back as measured_I, measured_TP, measured_LRA
# and measured_thresh. If that gain would push peaks past TP, it falls
# back to the one-pass mode.
ffmpeg -i in.mp4 -c:v copy -af "loudnorm=I=-14:TP=-1:LRA=11" \
       -ar 48000 -c:a aac out.mp4

# Find gaps quieter than -35 dB that last at least half a second
ffmpeg -i in.mp4 -af "silencedetect=noise=-35dB:d=0.5" -f null -

# Mix a music bed under a voice at 20% volume. normalize=0 stops amix
# from turning every input down to make room.
ffmpeg -i voice.wav -i music.mp3 -filter_complex \
       "[1:a]volume=0.2[m];[0:a][m]amix=inputs=2:duration=first:normalize=0" \
       mix.wav
```

## Images and GIFs

```bash
# One frame at 3 seconds, as a thumbnail
ffmpeg -ss 3 -i in.mp4 -frames:v 1 thumb.jpg

# One frame per second as numbered PNGs: frame_0001.png, frame_0002.png...
ffmpeg -i in.mp4 -vf fps=1 frame_%04d.png

# And back: an image sequence to a video
ffmpeg -framerate 30 -i frame_%04d.png -c:v libx264 -pix_fmt yuv420p out.mp4

# A still image for as long as an audio track lasts
ffmpeg -loop 1 -i cover.jpg -i song.mp3 -c:v libx264 -tune stillimage \
       -pix_fmt yuv420p -c:a aac -shortest out.mp4

# A good-looking GIF: build a palette from the clip, then draw with it
ffmpeg -i in.mp4 -filter_complex \
       "fps=12,scale=480:-1,split[a][b];[a]palettegen[p];[b][p]paletteuse" \
       out.gif
```

## Joining files

```bash
# Files with the same codecs and settings: the concat demuxer joins them
# without re-encoding. list.txt has one line per file:
#   file 'part1.mp4'
#   file 'part2.mp4'
ffmpeg -f concat -safe 0 -i list.txt -c copy joined.mp4

# Files with different codecs (but the same frame size): the concat
# filter decodes and re-encodes them
ffmpeg -i a.mp4 -i b.mp4 -filter_complex \
       "[0:v][0:a][1:v][1:a]concat=n=2:v=1:a=1[v][a]" \
       -map "[v]" -map "[a]" joined.mp4
```

## Subtitles

```bash
# Add soft subtitles, a track the viewer can switch on and off.
# MP4 stores text subtitles as mov_text.
ffmpeg -i in.mp4 -i subs.srt -map 0 -map 1 -c copy -c:s mov_text out.mp4

# Burn them into the picture (needs a build with libass)
ffmpeg -i in.mp4 -vf subtitles=subs.srt -c:a copy burned.mp4

# Pull the first subtitle track out of an MKV as SRT
ffmpeg -i in.mkv -map 0:s:0 subs.srt
```

## Further Reading

* [ffmpeg documentation](https://ffmpeg.org/ffmpeg.html)
* [ffprobe documentation](https://ffmpeg.org/ffprobe.html)
* [Filters documentation](https://ffmpeg.org/ffmpeg-filters.html)
* [FFmpeg wiki](https://trac.ffmpeg.org/wiki)
