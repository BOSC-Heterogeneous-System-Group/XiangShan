#!/usr/bin/env python3

import sys
import os

def remove_diffftest(v_path, tmp_file = '.tmp.v'):
  S_COMMON = 1
  S_FILTER = 2
  with open(v_path, 'r') as src, open(tmp_file, 'w') as dest:
    state = S_COMMON
    next_state = S_COMMON
    for line in src:
      if state == S_COMMON:
        if line.startswith('  Difftest'):
          next_state = S_FILTER
      elif state == S_FILTER:
        if line.startswith('  );'):
          next_state = S_COMMON

      if state == S_COMMON and next_state == S_COMMON:
        dest.write(line)

      state = next_state

  os.system(f"mv {v_path} {v_path}.backup")
  os.system(f"mv {tmp_file} {v_path}")

if __name__ == '__main__':
  remove_diffftest(sys.argv[1]);
