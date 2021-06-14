function wrapper_melodia
    %%
    datadir = '../frame-level/melodia/';
    outputdir = '../frame-level/melodia/';
    
    %%
    folderinfo = dir(datadir);
    melodiafilepath = {};
    for i=1:length(folderinfo)
        if ~isempty(regexp(folderinfo(i).name, 'melodia_.*_frame[.]csv', 'ONCE'))
            melodiafilepath{end + 1} = strcat(datadir, folderinfo(i).name);
        end
    end
    
    %%
    convert(melodiafilepath, outputdir);
end

function convert(melodiafilepath, outputdir)
    %% config
    filename_prefix = 'melodia_';
    dt = 0.0029;
    
    %% conversion
    for n=1:length(melodiafilepath)
        %% read data
        A = csvread(melodiafilepath{n});
        
        %% frame validation
        abserr = abs(diff(A(:, 1)) - dt);
        assert(all(abserr < 1e-5), 'melodia: time-frame is incosistent');
        
        %% extract target segment
        f0sequence = A(:, 2);
       
        %% remove minus value which indicates no existence of melody
        I = f0sequence <= 0;
        f0sequence(I) = 0;
        
       %% save
        s = strsplit(melodiafilepath{n}, '_');
        songname = s{end - 1};
       
        t0 = A(1, 1);
        
        save(strcat(outputdir, filename_prefix, songname, '_f0'), 'f0sequence', 'dt', 't0');
    end
end