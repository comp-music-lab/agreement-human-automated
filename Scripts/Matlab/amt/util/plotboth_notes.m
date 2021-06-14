function plotboth_notes
    %%
    addpath('../ewt/');
    
    %%
    type = 'raw';
    
    %%
    audiodatadir = '../../audio/paper/';
    f0filedir = {'../../frame-level/ad-nnmf/', '../../frame-level/crepe/', '../../frame-level/melodia/',...
        '../../frame-level/oaf/', '../../frame-level/spice/', '../../frame-level/ss-pnn/',...
        '../../frame-level/stf/', '../../frame-level/tony (frame)/',...
        '../../note-level/tony (note)/', '../../note-level/madmom/'};
    pseqfiledir = {'../../post-processed/ad-nnmf/', '../../post-processed/crepe/', '../../post-processed/melodia/',...
        '../../post-processed/oaf/', '../../post-processed/spice/', '../../post-processed/ss-pnn/',...
        '../../post-processed/stf/', '../../post-processed/tony (frame)/',...
        '../../post-processed/tony (note)/', '../../post-processed/madmom/'};
    noteseqdir = '../../notesequence/paper/';
    figuredir = '../../figures/paper/TFR/';
    
    N = 8196;
    M = 8000;
    
    %%
    audiofilepath = {};
    folderinfo = dir(audiodatadir);
    songlist = {};
    
    for i=1:length(folderinfo)
        if ~folderinfo(i).isdir
            audiofilepath{end + 1} = strcat(audiodatadir, folderinfo(i).name);
            s = split(folderinfo(i).name, '.');
            songlist{end + 1} = s{1};
        end
    end
    
    %%
    noteseqdir = strcat(noteseqdir, type, '/');
    folderinfo_noteseq = dir(noteseqdir);
    
    %%
    freqband = readtable('../freqband.csv');
    
    %%
    for i=1:length(audiofilepath)
        %%
        idx = cellfun(@(x) strcmp(x, songlist{i}), freqband.song);
        
        ylim_lb = freqband.freq_lb(idx);
        ylim_ub = freqband.freq_ub(idx);
        adjust = false;
        
        %{
        ylim_lb = 50;
        ylim_ub = 2000;
        adjust = true;
        %}
        
        %%
        s = strsplit(audiofilepath{i}, '/');
        s = strsplit(s{end}, '.');
        songname = s{1};
        f0songfile = strcat('.*_', songname, '_f0[.]mat');
        pseqsongfile = strcat('.*_', songname, '_pseq[.]mat');
        
        %%
        [x, fs] = audioread(audiofilepath{i});
        
        if size(x, 2) == 2
            x = mean(x, 2);
        end
        
        %%
        figure(1);
        spectrogram(x, hann(N), M, N, fs, 'yaxis');
        ylim([ylim_lb ylim_ub]./1000);
        xlabel('Time (s)');
        ylabel('Frequency (kHz)');
        colorbar off
        title(sprintf('%s - STFT', songname));
            
        set(gcf, 'Position', [10 10 800 500]);
        
       %%
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
        saveas(figure(1), strcat(figuredir, songname, '_STFT', '.png'));
        
        %%
        for j=1:length(f0filedir)
            folderinfo = dir(f0filedir{j});
            f0file = '';
            
            for k=1:length(folderinfo)
                if ~isempty(regexp(folderinfo(k).name, f0songfile, 'ONCE'))
                    f0file = strcat(f0filedir{j}, folderinfo(k).name);
                    
                    break;
                end
            end
            
            %%
            load(f0file);
            t = dt .* ((1:length(f0sequence)) - 1) + t0;
            
            %%
            s = strsplit(f0filedir{j}, '/');
            modelname = s{end - 1};
            
            clf(figure(j + 1));
            figure(j + 1);
            
            subplot(3, 1, 1);
            spectrogram(x, hann(N), M, N, fs, 'yaxis');
            ylim([ylim_lb ylim_ub]./1000);
            xlabel('Time (s)');
            ylabel('Frequency (kHz)');
            colorbar off
            hold on;
            scatter(t, f0sequence./1000, 2, [0.89 0.259 0.204]);
            hold off;
            
            xtickelem = 0:floor(max(t));
            xticks(xtickelem);
            xticklabels(arrayfun(@(x) num2str(x), xtickelem, 'UniformOutput', false));
            
            axis tight;
            ylim([ylim_lb ylim_ub]./1000);
            title(sprintf('%s - %s', songname, modelname));
            
            %%
            folderinfo = dir(strcat(pseqfiledir{j}, type, '/'));
            pseqfile = '';
            
            for k=1:length(folderinfo)
                if ~isempty(regexp(folderinfo(k).name, pseqsongfile, 'ONCE'))
                    pseqfile = strcat(pseqfiledir{j}, type, '/', folderinfo(k).name);
                    
                    break;
                end
            end
            
            %%
            load(pseqfile);
            t = dec_t .* ((1:length(filteredsequence)) - 1) + t0;
            
            %%
            subplot(3, 1, 2);
            spectrogram(x, hann(N), M, N, fs, 'yaxis');
            ylim([ylim_lb ylim_ub]./1000);
            xlabel('Time (s)');
            ylabel('Frequency (kHz)');
            colorbar off
            hold on;
            scatter(t, filteredsequence./1000, 2, [0.89 0.259 0.204]);
            if ~isempty(noise)
                hold on;
                scatter(t, noise./1000, 2, [229 151 105]./255, 'marker', 'x');
            end
            
            %% with unison interval
            pattern = strcat(strrep(strrep(modelname, '(', '[(]'), ')', '[)]'), '_', songname, '_noteseq_incl[-]rp[.]csv');
            
            for k=1:length(folderinfo_noteseq)
                if ~isempty(regexp(folderinfo_noteseq(k).name, pattern, 'ONCE'))
                    notesequence = table2cell(...
                            readtable(strcat(noteseqdir, folderinfo_noteseq(k).name), 'ReadVariableNames', false)...
                        );
                    
                    break;
                end
            end
            
            %%
            pos = zeros(2, length(notesequence));
            buf = -1;
            l = 0;
            
            for k=1:length(filteredsequence)
                if filteredsequence(k) ~= buf && filteredsequence(k) ~= 0
                    l = l + 1;
                    pos(1, l) = k;
                    pos(2, l) = filteredsequence(k)./1000;
                    
                    buf = filteredsequence(k);
                elseif filteredsequence(k) ~= buf && filteredsequence(k) == 0
                    buf = filteredsequence(k);
                end
            end
            
            assert(l == length(notesequence), 'note sequence is inconsistent');
            
            %%
            for k=1:length(notesequence)
                text(t(pos(1, k)), pos(2, k) - 0.015, notesequence{k}, 'FontSize', 7.4, 'Color', [0.2 0.2 0.2]);
            end
            
            scatter(t(pos(1, :)), pos(2, :), 18, [0.89 0.259 0.204]);
            
            hold off;
            
            xtickelem = 0:floor(max(t));
            xticks(xtickelem);
            xticklabels(arrayfun(@(x) num2str(x), xtickelem, 'UniformOutput', false));
            
            axis tight;
            ylim([ylim_lb ylim_ub]./1000);
            title(sprintf('%s - %s (post-processed with unison interval)', songname, modelname));
            
            %%
            subplot(3, 1, 3);
            spectrogram(x, hann(N), M, N, fs, 'yaxis');
            ylim([ylim_lb ylim_ub]./1000);
            xlabel('Time (s)');
            ylabel('Frequency (kHz)');
            colorbar off
            hold on;
            scatter(t, filteredsequence./1000, 2, [0.89 0.259 0.204]);
            if ~isempty(noise)
                hold on;
                scatter(t, noise./1000, 2, [229 151 105]./255, 'marker', 'x');
            end
            
            %% without unison interval
            pattern = strcat(strrep(strrep(modelname, '(', '[(]'), ')', '[)]'), '_', songname, '_noteseq_excl[-]rp[.]csv');
            
            for k=1:length(folderinfo_noteseq)
                if ~isempty(regexp(folderinfo_noteseq(k).name, pattern, 'ONCE'))
                    notesequence = table2cell(...
                            readtable(strcat(noteseqdir, folderinfo_noteseq(k).name), 'ReadVariableNames', false)...
                        );
                    
                    break;
                end
            end
            
            %%
            pos = zeros(2, length(notesequence));
            buf = -1;
            l = 0;
            
            for k=1:length(filteredsequence)
                if filteredsequence(k) ~= buf && filteredsequence(k) ~= 0
                    l = l + 1;
                    pos(1, l) = k;
                    pos(2, l) = filteredsequence(k)./1000;
                    
                    buf = filteredsequence(k);
                end
            end
            
            assert(l == length(notesequence), 'note sequence is inconsistent');
            
            %%
            for k=1:length(notesequence)
                text(t(pos(1, k)), pos(2, k) - 0.015, notesequence{k}, 'FontSize', 7.4, 'Color', [0.2 0.2 0.2]);
            end
            
            scatter(t(pos(1, :)), pos(2, :), 18, [0.89 0.259 0.204]);
            
            hold off;
            
            xtickelem = 0:floor(max(t));
            xticks(xtickelem);
            xticklabels(arrayfun(@(x) num2str(x), xtickelem, 'UniformOutput', false));
            
            axis tight;
            ylim([ylim_lb ylim_ub]./1000);
            title(sprintf('%s - %s (post-processed without unison interval)', songname, modelname));
            
            set(gcf, 'Position', [10 10 800 750]);
            
            %%
            saveas(figure(j + 1), strcat(figuredir, songname, '_', modelname, '_', type, '_both_notes.png'));
        end
    end
end