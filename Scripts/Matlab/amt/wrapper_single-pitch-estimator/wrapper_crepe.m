function wrapper_crepe(audiofilepath)
    outputdir = '../frame-level/crepe/';
    filename_prefix = 'crepe_';
    
    dt = 0.01;
    confidence_threhold = 0.8;
    
    for n=1:length(audiofilepath)
        %%
        s = strsplit(audiofilepath{n}, '/');
        s = strsplit(s{end}, '.');
        songname = s{1};
        
        %%
        command = [...
            'python -m crepe -c full --viterbi "',...
            audiofilepath{n},...
            '" -o "', outputdir, '"'...
            ];
        
        %%
        [status, cmdout] = system(command);
        fprintf('crepe: %s (%d) %s\n', songname, status, cmdout);
        
        %% read model output
        outputfilepath = strcat(outputdir, songname, '.f0.csv');
        A = csvread(outputfilepath, 1, 0);
        
        %%
        f0sequence = A(:, 2);
        
        %% remove low confidence estimation
        confidence = A(:, 3);
        I = confidence <= confidence_threhold;
        f0sequence(I) = 0;
        
       %% save
       t0 = 0;
       
        save(strcat(outputdir, filename_prefix, songname, '_f0'), 'f0sequence', 'dt', 't0');
    end
end