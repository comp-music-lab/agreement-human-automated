import tensorflow as tf
import tensorflow_hub as hub

import numpy as np
import matplotlib.pyplot as plt
import librosa
from librosa import display as librosadisplay

import logging
import math
import statistics
import sys

from IPython.display import Audio, Javascript
from scipy.io import wavfile

from base64 import b64decode

import music21
from pydub import AudioSegment

EXPECTED_SAMPLE_RATE = 16000
MAX_ABS_INT16 = 32768.0

def main():
 
 args = sys.argv
 input_file_name = args[1]
 output_file_name = args[2]

 logger = logging.getLogger()
 logger.setLevel(logging.ERROR)

 print("tensorflow: %s" % tf.__version__)
 print("librosa: %s" % librosa.__version__)

 converted_audio_file = convert_audio_for_model(input_file_name)

 sample_rate, audio_samples = wavfile.read(converted_audio_file, 'rb')

 # Show some basic information about the audio.
 duration = len(audio_samples)/sample_rate
 print(f'Sample rate: {sample_rate} Hz')
 print(f'Total duration: {duration:.2f}s')
 print(f'Size of the input: {len(audio_samples)}')

 audio_samples = audio_samples / float(MAX_ABS_INT16)
 
 # Loading the SPICE model is easy:
 model = hub.load("https://tfhub.dev/google/spice/2")

 # We now feed the audio to the SPICE tf.hub model to obtain pitch and uncertainty outputs as tensors.
 model_output = model.signatures["serving_default"](tf.constant(audio_samples, tf.float32))
 pitch_outputs = model_output["pitch"]
 uncertainty_outputs = model_output["uncertainty"]

 # 'Uncertainty' basically means the inverse of confidence.
 confidence_outputs = 1.0 - uncertainty_outputs

 confidence_outputs = list(confidence_outputs)
 pitch_outputs = [float(x) for x in pitch_outputs]

 indices = range(len(pitch_outputs))
 confident_pitch_outputs = [ (i,p)  
  for i, p, c in zip(indices, pitch_outputs, confidence_outputs) if  c >= 0.9  ]
 confident_pitch_outputs_x, confident_pitch_outputs_y = zip(*confident_pitch_outputs)

 confident_pitch_values_hz = [ output2hz(p) for p in confident_pitch_outputs_y ]
 
 pitch_outputs_and_rests = [
    output2hz(p) if c >= 0.9 else 0
    for i, p, c in zip(indices, pitch_outputs, confidence_outputs)
 ]

 # results
 print(len(pitch_outputs_and_rests))

 with open(output_file_name, 'wt') as f:
 	f.writelines(str(i) + '\n' for i in pitch_outputs_and_rests)
 
def convert_audio_for_model(user_file, output_file='converted_audio_file.wav'):
  audio = AudioSegment.from_file(user_file)
  audio = audio.set_frame_rate(EXPECTED_SAMPLE_RATE).set_channels(1)
  audio.export(output_file, format="wav")
  return output_file

def output2hz(pitch_output):
  # Constants taken from https://tfhub.dev/google/spice/2
  PT_OFFSET = 25.58
  PT_SLOPE = 63.07
  FMIN = 10.0;
  BINS_PER_OCTAVE = 12.0;
  cqt_bin = pitch_output * PT_SLOPE + PT_OFFSET;
  return FMIN * 2.0 ** (1.0 * cqt_bin / BINS_PER_OCTAVE)

if __name__ == '__main__':
 main()