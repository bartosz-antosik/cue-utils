#!/usr/bin/python

__author__ = 'ban'
__date__ = '2023-10-26'
__version__ = '0.1'

import os
import sys
import numpy as np
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

def detect_islands(a, trigger_val, stopind_inclusive=True):
    # https://stackoverflow.com/questions/50151417/numpy-find-indices-of-groups-with-same-value

    # Setup "sentients" on either sides to make sure we have setup
    # "ramps" to catch the start and stop for the edge islands
    # (left-most and right-most islands) respectively
    a_ext = np.r_[False, a==trigger_val, False]

    # Get indices of shifts, which represent the start and stop indices
    idx = np.flatnonzero(a_ext[:-1] != a_ext[1:])

    # Lengths of islands if needed
    lens = idx[1::2] - idx[:-1:2]

    # Using a step size of 2 would get us start and stop indices for each island
    return list(zip(idx[:-1:2], idx[1::2]-int(stopind_inclusive))), lens

def cue_detect_zeros(cue_file_name):
    """Starting from CUE detect suspicious zeroes in WAV

    Args:
        cue_file_name: CUE file name with path.
    """

    cuesheet = cueparser.CueSheet()
    cuesheet.setOutputFormat("%performer% - %title%\n%file% (%format%, %rem%, %songwriter%)\n%tracks%", "%performer% - %title% (%offset%, %index%, %songwriter%)")

    with open(cue_file_name, "r", encoding="utf-8-sig") as incue:
        cuesheet.setData(incue.read())

    print(f'Parsing CUE file: {cue_file_name}')

    cuesheet.parse()

    wav_file_name = os.path.join(os.path.dirname(cue_file_name),
        os.path.basename(cuesheet.file.replace("\\","/")));

    print(f'Detecting ZEROS in WAV file: {wav_file_name}')

    start_frame = 0

    with soundfile.SoundFile(wav_file_name, "rb") as infile:
        wav_channels = infile.channels
        wav_format = infile.format
        wav_samplerate = infile.samplerate
        wav_subtype = infile.subtype

        # Collect offsets to all tags
        for i, t in enumerate(cuesheet.tracks):
            if i == len(cuesheet.tracks) - 1:
                end_frame = infile.frames
            else:
                end_frame = offset_to_frame(cuesheet.tracks[i + 1].offset, wav_samplerate)

            data_chunk = infile.read(frames=end_frame - start_frame)

            for channel in range(data_chunk.shape[1]):
                zeros, zeros_len = detect_islands(data_chunk[:, channel], trigger_val=0.0)

                for i, l in enumerate(zeros_len):
                    # Skip shorter than 1/4 of a second
                    if l >= wav_samplerate / 4:
                        # Skip zeros adjacent to start/end of a track
                        if end_frame - (start_frame + zeros[i][1]) > 10:
                        # if zeros[i][0] > 10 and end_frame - (start_frame + zeros[i][1]) > 10:
                            print(f'Suspicious ZEROS in channel: {channel} at: {frame_to_offset(start_frame + zeros[i][0], wav_samplerate)}, duration: {frame_to_offset(l, wav_samplerate)}')

            start_frame = end_frame

if __name__ == '__main__':

    if len(sys.argv) <= 1:
        print('Usage:', os.path.basename(sys.argv[0]), 'cue-sheet-file')
    else:
        cue_file_name = sys.argv[1].strip()

    cue_detect_zeros(cue_file_name)
