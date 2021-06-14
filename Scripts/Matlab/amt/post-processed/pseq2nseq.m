function pseq2nseq
    %%
    pseqdir = './';
    outputdir = '../notesequence/paper/';
    
    type = 'raw';
    
    conf.offset = 10;
    conf.notenameset = {'C', 'd', 'D', 'e', 'E', 'F', 'g', 'G', 'a', 'A', 'b', 'B'};
    A4 = 440;
    
    %%
    conf.numvoices = length(conf.notenameset);
    freqset = deffreqbins(A4, conf.numvoices, conf.offset);
    
    %%
    folderinfo = dir(pseqdir);
    
    for i=1:length(folderinfo)
        if folderinfo(i).isdir && isempty(regexp(folderinfo(i).name, '[.]*', 'ONCE'))
            modeldir = strcat(pseqdir, folderinfo(i).name, '/');
            folderinfo_i = dir(strcat(modeldir, '/', type, '/'));
            
            for j=1:length(folderinfo_i)
                if ~isempty(regexp(folderinfo_i(j).name, '.*_pseq[.]mat', 'ONCE'))
                    load(strcat(modeldir, '/', type, '/', folderinfo_i(j).name));
                    
                    %%
                    unison = true;
                    notes = pitchsegmentation(filteredsequence, unison);
                    [notesequence_inclrp, ~] = pitchsymbolization(notes, freqset, conf.notenameset, conf.numvoices);
                    
                    %%
                    unison = false;
                    notes = pitchsegmentation(filteredsequence, unison);
                    [notesequence_exclrp, ~] = pitchsymbolization(notes, freqset, conf.notenameset, conf.numvoices);
                    
                    %%
                    s = split(folderinfo_i(j).name, '_');
                    modelname = s{1};
                    songname = s{2};
                    
                    %% save
                    output_filename = strcat(outputdir, type, '/', modelname, '_', songname, '_noteseq_', 'incl-rp.csv');
                    writetable(cell2table(notesequence_inclrp), output_filename, 'WriteVariableNames', false);
                    
                     %% save
                    output_filename = strcat(outputdir, type, '/', modelname, '_', songname, '_noteseq_', 'excl-rp.csv');
                    writetable(cell2table(notesequence_exclrp), output_filename, 'WriteVariableNames', false);
                end
            end
        end
    end
end