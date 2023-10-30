#!/usr/bin/python

__author__ = 'ban'
__date__ = '2023-09-13'
__version__ = '0.1'

import collections
import re
import os
import sys
import soundfile

import cueparser

def offset_to_frame(offset, frame_rate):
    """Convert offset to frame number.

    Args:
        offset: Offset to convert
        frame_rate: Base framerate
    """
    offset = offset.split(':')
    minutes=int(offset[0])
    seconds=int(offset[1])
    frames=int(offset[2])
    frame = (minutes * 60 + seconds) * frame_rate + ( frames * frame_rate ) // 75
    return frame

def frame_to_offset(frame, frame_rate):
    """Convert frame number to offset.

    Args:
        frame: Frame number to convert
        frame_rate: Base framerate
    """
    frames = ((frame % frame_rate) * 75) // frame_rate
    seconds = ((frame - frames) // frame_rate)
    minutes = seconds // 60
    seconds = seconds if minutes == 0 else seconds % (minutes * 60)
    offset = f'{minutes:02d}:{seconds:02d}:{frames:02d}'
    return offset

def cue_split_by_performer(cue_file_name):
    """Split CUE/WAV by performer

    Args:
        cue_file_name: CUE file name with path.
    """
    cuesheet = cueparser.CueSheet()
    cuesheet.setOutputFormat("%performer% - %title%\n%file% (%format%, %rem%, %songwriter%)\n%tracks%", "%performer% - %title% (%offset%, %index%, %songwriter%)")

    with open(cue_file_name, "r", encoding="utf-8-sig") as incue:
        cuesheet.setData(incue.read())

    cuesheet.parse()

    by_performers = collections.defaultdict(list)

    with soundfile.SoundFile(os.path.join(os.path.dirname(cue_file_name),
        os.path.basename(cuesheet.file.replace("\\","/"))), "rb") as infile:
        wav_channels = infile.channels
        wav_format = infile.format
        wav_samplerate = infile.samplerate
        wav_subtype = infile.subtype

        for i, t in enumerate(cuesheet.tracks):
            if i == len(cuesheet.tracks) - 1:
                n = None
            else:
                n = cuesheet.tracks[i + 1]
            match = re.match('^(.*?) - (.*)$', t.title)
            if match:
                performer = match.group(1).strip()
                title = match.group(2).strip()
                start = t.offset
                if n:
                    end = n.offset
                else:
                    end = frame_to_offset(infile.frames, wav_samplerate)
                by_performers[performer].append([title, start, end])

            print(f'{performer} - {title} ({start}[{offset_to_frame(start, wav_samplerate):d}], {end}[{offset_to_frame(end, wav_samplerate):d}])')

        for p in by_performers:
            file_name = re.sub(r'[^\w_. -]', '_', cuesheet.title + "_" + p)
            file_name = os.path.join(os.path.dirname(cue_file_name), file_name)

            if os.path.exists(file_name + ".cue"):
                os.remove(file_name + ".cue")

            if os.path.exists(file_name + ".mrk"):
                os.remove(file_name + ".mrk")

            with open(file_name + ".cue", "a", encoding="utf-8") as outcue, \
                open(file_name + ".mrk", "a", encoding="utf-8") as outmrk:
                print("TITLE \"" + cuesheet.title + "_" + p+ "\"", file=outcue)
                print("FILE \"" + file_name + ".wav\"", file=outcue)

                print("Markers\n{", file=outmrk)

                data = []
                frames_total = 0

                for i, t in enumerate(by_performers[p]):
                    print("  TRACK " + "{:02d}".format(i + 1) + " AUDIO", file=outcue)
                    print("    TITLE \"" + t[0] + "\"", file=outcue)
                    print("    INDEX " + "01 " + frame_to_offset(frames_total, wav_samplerate), file=outcue)

                    if i == 0:
                        print("\tMarker\n\t{", file=outmrk)
                    else:
                        print(f"\tMarker{i}\n\t{{", file=outmrk)
                    print(f"\t\tName8={p} - {t[0]}", file=outmrk)
                    print(f"\t\tPos={frames_total}", file=outmrk)
                    if i == 0:
                        # First element
                        print("\t\tType=2", file=outmrk)
                        print(f"\t\tRightId={i + 1}", file=outmrk)
                    elif i == len(by_performers[p]) - 1:
                        # Last element
                        print("\t\tType=4", file=outmrk)
                        print(f"\t\tLeftId={i - 1}", file=outmrk)
                    else:
                        # Any other element
                        print("\t\tType=4", file=outmrk)
                        print(f"\t\tRightId={i + 1}", file=outmrk)
                        print(f"\t\tLeftId={i - 1}", file=outmrk)
                    print(f"\t\tId={i}", file=outmrk)
                    print("\t}", file=outmrk)

                    start_frame = offset_to_frame(t[1], wav_samplerate)
                    end_frame = offset_to_frame(t[2], wav_samplerate)

                    infile.seek(start_frame)

                    data.append(infile.read(frames=end_frame - start_frame))
                    frames_total += (end_frame - start_frame)

                if os.path.exists(file_name + ".wav"):
                    os.remove(file_name + ".wav")

                with soundfile.SoundFile(file_name + ".wav", "wb",
                    samplerate=wav_samplerate,
                    channels=wav_channels,
                    format=wav_format,
                    subtype=wav_subtype) as outfile:

                    for chunk in data:
                        outfile.write(chunk)

                    outfile.close()

                print("}", file=outmrk)

                outcue.close()
                outmrk.close()

if __name__ == '__main__':

    if len(sys.argv) <= 1:
        print('Usage:', os.path.basename(sys.argv[0]), 'cue-sheet-file')
        sys.exit()

    cue_file_name = sys.argv[1].strip()

    print('Splitting CUE file: ', cue_file_name)

    cue_split_by_performer(cue_file_name)
