function wrapper_sspnn(audiofilepath)
    outputdir = '../frame-level/ss-pnn/';
    filename_prefix = 'ss-pnn_';
    
    dt = 0.02;
    C0 = 8.17579892;
    
    for n=1:length(audiofilepath)
        %%
        s = strsplit(audiofilepath{n}, '/');
        s = strsplit(s{end}, '.');
        songname = s{1};
        
        %%
        outputfilepath = strcat(outputdir, filename_prefix, songname, '.txt');
        
        audiofilepath_f = strcat(char(java.io.File(fullfile(pwd, audiofilepath{n})).getCanonicalPath));
        outputpath_f = strcat(char(java.io.File(fullfile(pwd, outputfilepath)).getCanonicalPath));
        
        currentdir = pwd;
        cd('./scripts/Vocal-Melody-Extraction/');
        
        command = [...
            'python ./VocalMelodyExtraction.py -p testing -d audio -m note_PNN -i "',...
            audiofilepath_f,...
            '" -oo "', outputpath_f, '"'...
            ];
        
        %%
        [status, cmdout] = system(command);
        fprintf('ss-pnn: %s (%d) %s\n', songname, status, cmdout);

        cd(currentdir);

        %% read model output
        fileID = fopen(outputpath_f, 'r');
        A = fscanf(fileID, '%f %f %f', [2 Inf]);
        fclose(fileID);
        
        %%
        f0sequence = A(2, :);
        
        %% mapping to frequency
        I = f0sequence ~= 0;

        f0sequence = f0sequence./100;
        f0sequence(I) = f0sequence(I) + 3.55;
        f0sequence(I) = C0.*2.^(f0sequence(I)./12);

       %% save
        t0 = 0;
       
        save(strcat(outputdir, filename_prefix, songname, '_f0'), 'f0sequence', 'dt', 't0');
    end
end