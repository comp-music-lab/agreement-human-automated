function runwrapper
    %%
    audiodatadir = '../audio/paper/';
    
    %%
    folderinfo = dir(audiodatadir);
    
    audiofilepath = {};
    
    for i=1:length(folderinfo)
        if ~folderinfo(i).isdir
            audiofilepath{end + 1} = strcat(audiodatadir, folderinfo(i).name);
        end
    end
    
    %%
    wrapper_melodia();
    wrapper_tony_n();
    wrapper_tony_f();
    wrapper_crepe(audiofilepath);
    wrapper_spice(audiofilepath);
    wrapper_sspnn(audiofilepath);
end