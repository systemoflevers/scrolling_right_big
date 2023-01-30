#!/usr/bin/python3

def main():
  map_columns = []
  for i in range((0x4000 * 0x1ff) // 18):
    digits = [int(d) for d in list(str(i))]
    need_padding = 18 - len(digits)
    digits = ([0] * need_padding) + digits
    map_columns.append(digits)
  
  map_values = [i for c in map_columns for i in c]
  map_bytes = bytes(map_values)
  with open('long_map.tilemap', 'wb') as f:
    f.write(map_bytes)


if __name__ == '__main__':
  main()