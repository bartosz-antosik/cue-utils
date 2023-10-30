# CUE/WAV utilities

Utilities for automation of particular operations on CUE sheet/WAV data files.

## cuesplit.py

This is a simple CUE/WAV splitter which splits according to the rules required by RCKL (Radiowe Centrum Kultury Radiowej) of PR2 (Polskie Radio 2). It can split RF64 WAVE files, larger than 4TB.

## cuezeros.py

Detect digital silence spruts in WAV files. Detects areas of consistent zeros and thanks to some simple heuristics eliminates those which are most probably a silence between tracks and reports the others. It helps to detect without listening certain audio tracks defects.