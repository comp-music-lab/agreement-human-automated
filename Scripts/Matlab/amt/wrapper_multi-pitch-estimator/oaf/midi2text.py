from mido import MidiFile
import sys

def main():
 args = sys.argv
 input_file_name = args[1]
 output_file_name = args[2]

 mid = MidiFile(input_file_name, clip=True)
 print(mid)
 
 for track in mid.tracks:
  print(track)

 with open(output_file_name, 'wt') as f:
  for track in mid.tracks:
   for msg in track:
    f.writelines(str(msg)  + '\n' )

if __name__ == '__main__':
 main()