function postproc
    %%
    f0filedir = {'../frame-level/ad-nnmf/', '../frame-level/crepe/', '../frame-level/melodia/',...
        '../frame-level/oaf/', '../frame-level/spice/', '../frame-level/ss-pnn/',...
        '../frame-level/stf/', '../frame-level/tony (frame)/',...
        '../note-level/madmom/', '../note-level/tony (note)/'};
    
    A4 = 440;
    
    conf.numvoices = 12;
    conf.offset = 10;
    conf.dec_t = 0.01;
    conf.lenfil = 0.25;
    conf.minlen = 0.15;
    
    %% define frequency bins
    freqset = deffreqbins(A4, conf.numvoices, conf.offset);
    
    %%
    for i=1:length(f0filedir)
        folderinfo = dir(f0filedir{i});
        
        %%
        for k=1:length(folderinfo)
            if ~isempty(regexp(folderinfo(k).name, '.*_f0[.]mat', 'ONCE'))
                f0file = strcat(f0filedir{i}, folderinfo(k).name);
                load(f0file);

                %%
                s = strsplit(folderinfo(k).name, '_');
                modelname = s{1};
                songname = s{2};
                
                %%
                pitchsequence = pitchquantization(f0sequence, freqset);
                
                %%
                if ~isempty(regexp(f0filedir{i}, '.*/frame-level/.*', 'ONCE'))
                    [filteredsequence, noise, dec_t] = pitchsmoothing(pitchsequence, dt, conf);
                else
                    filteredsequence = pitchsequence;
                    noise = [];
                    dec_t = dt;
                end
                
                %%
                save(strcat('./', modelname, '/raw/', modelname, '_', songname, '_pseq.mat'), 'filteredsequence', 'noise', 'dec_t', 't0');
            end
        end
    end
    
end