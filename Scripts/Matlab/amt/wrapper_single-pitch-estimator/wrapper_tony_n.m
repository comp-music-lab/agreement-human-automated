function wrapper_tony_n
    %%
    datadir = '../note-level/tony (note)/';
    outputdir = '../note-level/tony (note)/';
    
    %%
    folderinfo = dir(datadir);
    tonyfilepath = {};
    for i=1:length(folderinfo)
        if ~isempty(regexp(folderinfo(i).name, 'tony_.*_note[.]csv', 'ONCE'))
            tonyfilepath{end + 1} = strcat(datadir, folderinfo(i).name);
        end
    end
    
    %%
    convert(tonyfilepath, outputdir);
end

function convert(tonyfilepath, outputdir)
    %% config
    filename_prefix = 'tony (note)_';
    dt = 0.00580498866;
    
    %% conversion
    for n=1:length(tonyfilepath)
        %% read data
        A = csvread(tonyfilepath{n});
        
        %% extract target segment
        frac = A(:, 3)./dt - round(A(:, 3)./dt);
        assert(all(abs(frac) < 1e-5), 'tony_note: inconsistent time resolution');
        
        t = 0:dt:(A(end, 1) + A(end, 3));
        notelen = round(A(:, 3)./dt);
        
        f0sequence = zeros(length(t), 1);
        
        for i=1:size(A, 1)
            f0 = A(i, 2);
            
            [~, n_start] = min(abs(t - A(i, 1)));
            [~, n_end] = min(abs(t - (A(i, 1) + A(i, 3))));
            
            assert(notelen(i) == (n_end - n_start), 'tony_note: inconsistent time resolution')
            
            f0sequence(n_start:n_end) = f0;
        end
        
       %% save
        s = strsplit(tonyfilepath{n}, '_');
        songname = s{end - 1};
       
        t0 = 0;
        
        save(strcat(outputdir, filename_prefix, songname, '_f0'), 'f0sequence', 'dt', 't0');
    end
end