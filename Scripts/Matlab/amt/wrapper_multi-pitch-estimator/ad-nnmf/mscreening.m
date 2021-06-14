function mscreening
    %%
    addpath('../ewt/');
    addpath('../viterbi/');
    
    %%
    audiodatadir = '../../audio/paper/';
    
    filename_prefix = 'ad-nnmf_';
    outputdir = '../../frame-level/ad-nnmf/';
    dt = 0.02;
    
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
        
        %%
        currentdir = pwd;
        
        inputFile = char(java.io.File(fullfile(pwd, audiofilepath{n})).getCanonicalPath);
        outputFile = strcat(outputdir, filename_prefix, songname, '.txt');
        outputFile = char(java.io.File(fullfile(pwd, outputFile)).getCanonicalPath);
        
        cd('./attack-decay-piano-nmf/')
        test_doMultiF0;
        
        cd(currentdir);
        
        %% read model output
        midmat = h_matshape(outputFile, F, dt);
        t = ((1:size(midmat, 2)) - 1) .* dt;
        
        %%
        figure(3);
        subplot(2, 1, 1);
        spectrogram(x, hann(N), M, N, fs, 'yaxis');
        colorbar off
        hold on;
        
        for k=1:size(midmat, 1)
            if ~all(midmat(k, :) == 0)
                scatter(t, F(k).*midmat(k, :)./1000, 1, [0.89 0.259 0.204]);
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
        xlim([t(1) t(end)]);
        ylim([ylim_lb ylim_ub]);
        
        %%
        f0mat = viterbidec(midmat);
        
        %%
        figure(2);
        subplot(3, 1, 2);
        surf(t, F, f0mat, 'edgecolor', 'none');
        view(0, 90);
        xlim([t(1) t(end)]);
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
                scatter(t, F(k).*f0mat(k, :)./1000, 1, [0.89 0.259 0.204]);
                hold on;
            end
        end
        
        plot([t(1) t(end)], [ylim_lb ylim_lb]./1000, '-.m');
        hold on;
        
        plot([t(1) t(end)], [ylim_ub ylim_ub]./1000, '-.m');
        hold off;
        
        ylim([F(1) 8000]./1000);
        
        drawnow;
        
        %% save
        t0 = 0;
        save(strcat(outputdir, filename_prefix, songname, '_f0'), 'f0sequence', 'dt', 't0');
    end
end

function midmat = h_matshape(nnmffilepath, F, dt)
    %%
    fileID = fopen(nnmffilepath, 'r');
    A = fscanf(fileID, '%f %f %f', [3 Inf]);
    fclose(fileID);
    
    %%
    t = 0:dt:max(A(2, :));
    
    %%
    T = length(t);
    midmat = zeros(128, T);
    
    %%
    for i=1:size(A, 2)
        [~, n_start] = min(abs(t - A(1, i)));
        [~, n_end] = min(abs(t - A(2, i)));
        [~, k] = min(abs(F - A(3, i)));
        
        midmat(k, n_start:n_end) = 1;
    end
end