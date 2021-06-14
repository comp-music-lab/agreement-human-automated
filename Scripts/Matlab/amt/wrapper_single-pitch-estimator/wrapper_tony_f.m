function wrapper_tony_f
    %%
    datadir = '../frame-level/tony (frame)/';
    outputdir = '../frame-level/tony (frame)/';
    
    %%
    folderinfo = dir(datadir);
    tonyfilepath = {};
    for i=1:length(folderinfo)
        if ~isempty(regexp(folderinfo(i).name, 'tony_.*_frame[.]csv', 'ONCE'))
            tonyfilepath{end + 1} = strcat(datadir, folderinfo(i).name);
        end
    end
    
    %%
    convert(tonyfilepath, outputdir);
end

function convert(tonyfilepath, outputdir)
    %% config
    filename_prefix = 'tony (frame)_';
    dt = 0.00580498866;
    
    %% conversion
    for n=1:length(tonyfilepath)
        %% read data
        A = csvread(tonyfilepath{n});
        
        %% frame validation
        abserr = abs(diff(A(:, 1)) - dt);
        
        if ~all(abserr < 1e-9)
            disp(find(abserr >= 1e-9));
            fprintf('tony: time-frame is incosistent: %d frames of %s\n', sum(abserr >= 1e-9), tonyfilepath{n});
            assert(false, 'error!');
        end        
        
        %% extract target segment
        f0sequence = A(:, 2);
       
       %% save
        s = strsplit(tonyfilepath{n}, '_');
        songname = s{end - 1};
       
        t0 = A(1, 1);
        
        save(strcat(outputdir, filename_prefix, songname, '_f0'), 'f0sequence', 'dt', 't0');
    end
end