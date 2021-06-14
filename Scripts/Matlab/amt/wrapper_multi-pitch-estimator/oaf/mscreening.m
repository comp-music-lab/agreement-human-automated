function mscreening
    %%
    addpath('../ewt/');
    addpath('../viterbi/');
    
    %%
    audiodatadir = '../../audio/paper/';
    oafdatadir = '../../frame-level/oaf/';
    
    filename_prefix = 'oaf_';
    outputdir = '../../frame-level/oaf/';
    dt = 0.002285;
    
    C0 = 8.17579892;
    F = C0 .* 2.^((0:127)./12);
    
    N = 8196;
    M = 8000;
    
    %%
    folderinfo = dir(audiodatadir);
    audiofilepath = {};
    songlist = {};
    
    for i=1:length(folderinfo)
        if ~folderinfo(i).isdir
            audiofilepath{end + 1} = strcat(audiodatadir, folderinfo(i).name);
            s = split(folderinfo(i).name, '.');
            songlist{end + 1} = s{1};
        end
    end
   
     %%
    freqband = readtable('../freqband.csv');
    
    %%    
    for n=1:length(audiofilepath)
        %%
        idx = cellfun(@(x) strcmp(x, songlist{n}), freqband.song);
        
        ylim_lb = freqband.freq_lb(idx);
        ylim_ub = freqband.freq_ub(idx);
        adjust = false;
        
        %%
        %{
        ylim_lb = 50;
        ylim_ub = 2000;
        adjust = true;
        %}
        
        %%
        [x, fs] = audioread(audiofilepath{n});
        
        if size(x, 2) == 2
            x = mean(x, 2);
        end
        
        %%
        figure(1);
        spectrogram(x, hann(N), M, N, fs, 'yaxis');
        ylim([ylim_lb ylim_ub]./1000);
        colorbar off
        
        while adjust
            ylim_lb = input('input the frequency band (lower limit): ');
            figure(1);
            ylim([ylim_lb ylim_ub]./1000);
            
            ylim_ub = input('input the frequency band (higher limit): ');
            figure(1);
            ylim([ylim_lb ylim_ub]./1000);
            
            fprintf('playback the result\n');
            y = ewtbandpass(x, ylim_lb, ylim_ub, fs);
            sound(y, fs);
            
            s = input('bandpass sounds good? (Y/N) ', 's');
            if strcmp('Y', s)
                adjust = false;
            end
        end
        
        %%
        s = strsplit(audiofilepath{n}, '/');
        s = strsplit(s{end}, '.');
        songname = s{1};
        
        oaffilepath = strcat(oafdatadir, filename_prefix, songname, '.mid');
        outputfilename = strcat(filename_prefix, songname, '.txt');
        
        command = [...
            'python ./midi2text.py "',...
            oaffilepath,...
            '" "', outputdir, outputfilename, '"'...
            ];
        
        %%
        [status, cmdout] = system(command);
        fprintf('madmom: %s (%d) %s\n', songname, status, cmdout);
        
        %% read model output
        midmat = h_matshape(strcat(outputdir, outputfilename));
        t = ((1:size(midmat, 2)) - 1) .* dt;
        
        %%
        figure(3);
        subplot(2, 1, 1);
        spectrogram(x, hann(N), M, N, fs, 'yaxis');
        colorbar off
        hold on;
        
        for k=1:size(midmat, 1)
            if ~all(midmat(k, :) == 0)
                I = midmat(k, :) ~= 0;
                
                scatter(t, F(k).*I./1000, 1, [0.89 0.259 0.204]);
                hold on;
            end
        end
        
        plot([t(1) t(end)], [ylim_lb ylim_lb]./1000, '-.m');
        hold on;
        
        plot([t(1) t(end)], [ylim_ub ylim_ub]./1000, '-.m');
        hold off;
        
        ylim([F(1) 8000]./1000);
        
        %%
        I = F < ylim_lb;
        midmat(I, :) = 0;
        
        I = F > ylim_ub;
        midmat(I, :) = 0;
        
        %%
        figure(2);
        subplot(3, 1, 1);
        surf(t, F, midmat, 'edgecolor', 'none');
        view(0, 90);
        axis tight;
        ylim([ylim_lb ylim_ub]);
        
        %%
        f0mat = viterbidec(midmat);
        
        %%
        figure(2);
        subplot(3, 1, 2);
        surf(t, F, f0mat, 'edgecolor', 'none');
        view(0, 90);
        axis tight;
        ylim([ylim_lb ylim_ub]);
        
        %%
        f0sequence = zeros(size(f0mat, 2), 1);
        
        for i=1:length(f0sequence)
            k = find(f0mat(:, i));
            
            if ~isempty(k)
                f0sequence(i) = F(k);
            end
        end
        
        %%
        figure(2);
        subplot(3, 1, 3);
        scatter(t, f0sequence, 2);
        xlim([t(1) t(end)]);
        ylim([ylim_lb ylim_ub]);
        
        %%
        figure(3);
        subplot(2, 1, 2);
        spectrogram(x, hann(N), M, N, fs, 'yaxis');
        colorbar off
        hold on;
        
        for k=1:size(f0mat, 1)
            if ~all(f0mat(k, :) == 0)
                I = f0mat(k, :) ~= 0;
                
                scatter(t, F(k).*I./1000, 1, [0.89 0.259 0.204]);
                hold on;
            end
        end
        
        plot([t(1) t(end)], [ylim_lb ylim_lb]./1000, '-.m');
        hold on;
        
        plot([t(1) t(end)], [ylim_ub ylim_ub]./1000, '-.m');
        hold off;
        
        ylim([F(1) 8000]./1000);
        
        drawnow
        
        %% save
        t0 = 0;
        save(strcat(outputdir, filename_prefix, songname, '_f0'), 'f0sequence', 'dt', 't0');
    end
end

function T = h_getlength(oaffilepath)
    T = 0;

    fileID = fopen(oaffilepath, 'r');

    tline = fgetl(fileID);
    while ischar(tline)
        if strcmp('note_on', tline(1:7))
            str = split(tline);

            str_t = split(str{5}, '=');
            t = str2double(str_t{2});

            T = T + t;
        end

        tline = fgetl(fileID);
    end

    fclose(fileID);
end

function midmat = h_matshape(oaffilepath)
    %%
    T = h_getlength(oaffilepath);
    midmat = zeros(128, T);
    
    %%
    offset = 1;
    vstat = zeros(128, 1);

    fileID = fopen(oaffilepath, 'r');

    tline = fgetl(fileID);
    while ischar(tline)
        if strcmp('note_on', tline(1:7))
            str = split(tline);

            str_t = split(str{5}, '=');
            t = str2double(str_t{2});

            offset = offset + t;

            str_n = split(str{3}, '=');
            n = str2double(str_n{2});
            n = n + 1;

            str_v = split(str{4}, '=');
            v = str2double(str_v{2});

            if v == 0
                v_n = midmat(n, vstat(n));
                midmat(n, vstat(n):(offset - 1)) = v_n;
            else
                midmat(n, offset) = v;
            end

            vstat(n) = offset;
        end

        tline = fgetl(fileID);
    end

    fclose(fileID);
end