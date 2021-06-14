In order to replicate the analysis, please follow the following operations:

1. Place the audio files under./audio/paper folder.

2. Obtain outputs of pYIN, TONY and Melodia (csv file) by using GUI software apps of them.
 - Please note that pYIN output by TONY software occasionally duplicates or skips a frame, and in such case, the script throws an assertion error. If you meet the assertion error, please inspect the output csv file by TONY software and fix the frame duplication or missing.

3. Obtain output of OAF (midi file) by using Google Colab Notebook.
https://colab.research.google.com/notebooks/magenta/onsets_frames_transcription/onsets_frames_transcription.ipynb#scrollTo=j79eR9mp_oaH

4. Place the outputs of the above 4 models in the folders under the "./(frame|note)-level/[model name]/".

5. Install AD-NNMF, STF, Madmom, SS-PNN, SPICE and CREPE. Please also read the text files under the "./wrapper_(single|multi)-pitch-estimator/[model name]" for the additional information.

6. Run "./wrapper_single-pitch-estimator/runwrapper.m" to obtain standardized F0 contour of the single-pitch estimation AMT models.

7. Run "./wrapper_multi-pitch-estimator/[model name]/mscreening.m" to obtain standardized F0 contour of the multi-pitch estimation AMT models.

8. Run "./post-processed/postproc.m"

9. Run "./post-processed/pseq2nseq.m" and you can see the note sequences of each model under the "./notesequences/paper/raw/" folder.