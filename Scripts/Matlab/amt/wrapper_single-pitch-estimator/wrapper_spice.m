function wrapper_spice(audiofilepath)
    outputdir = '../frame-level/spice/';
    filename_prefix = 'spice_';
    
    dt = 0.032; % 512 / 16000
    
    for n=1:length(audiofilepath)
        %%
        s = strsplit(audiofilepath{n}, '/');
        s = strsplit(s{end}, '.');
        songname = s{1};
        
        %%
        outputfilepath = strcat(outputdir, filename_prefix, songname, '.txt');
        
        command = [...
            'python ./scripts/spice_script.py "',...
            audiofilepath{n},...
            '" "', outputfilepath, '"'...
            ];
        
        %%
        [status, cmdout] = system(command);
        fprintf('spice: %s (%d) %s\n', songname, status, cmdout);
        
        %% read model output
        fileID = fopen(outputfilepath, 'r');
        A = fscanf(fileID, '%f %f %f', [1 Inf]);
        fclose(fileID);
        
        %%
        f0sequence = A;
        
       %% save
       t0 = 0;
       
        save(strcat(outputdir, filename_prefix, songname, '_f0'), 'f0sequence', 'dt', 't0');
    end
end