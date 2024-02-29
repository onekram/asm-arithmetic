#!/usr/bin/env python3

import glob

def check(filename: str):
    tabs = set()

    with open(filename) as file:
        for line_no, line in enumerate(file, 1):
            right_stripped = line.rstrip()

            trailing_spaces = len(line) - len(right_stripped)
            if line[-trailing_spaces:] != '\n':
                print(f'Extra trailing whitespaces in {filename} on line {line_no}: {repr(line[-trailing_spaces])}')
                exit(1)

            first_non_ws_pos = len(right_stripped) - len(right_stripped.lstrip())
            if first_non_ws_pos == 0:
                continue

            tabs.add(line[:first_non_ws_pos])

    if len(tabs) > 1:
        print(f'Too many unique tabulation sequences in {filename}:')
        for tab in tabs:
            print(f'\t- {repr(tab)}')
        exit(1)

for filename in glob.iglob('src/*.asm'):
    check(filename)
