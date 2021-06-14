function plotboth_notes_paper
    %%
    addpath('../ewt/');
    
    %%
    type = 'raw';
    
    %%
    audiofilepath = {'../../audio/paper/NAIV-075.wav'};
    f0filedir = {'../../note-level/tony (note)/'};
    pseqfiledir = {'../../post-processed/tony (note)/'};
    noteseqdir = '../../notesequence/paper/';
    figuredir = '../../figures/paper/End-to-End/';
    
    N = 8196;
    M = 8000;
    
    t_s = 6.3;
    t_e = 14.0;
    
    SCATTER_SIZE = 4;
    XTICK_FONTSIZE = 24;
    
    %%
    noteseqdir = strcat(noteseqdir, type, '/');
    folderinfo_noteseq = dir(noteseqdir);
    
    %%
    for i=1:length(audiofilepath)
        %%
        ylim_lb = 150.00;
        ylim_ub = 400.00;
        adjust = false;
        
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
        xlim([t_s t_e]);
           
        set(gca, 'XTickLabel' , get(gca,'XTickLabel'), 'fontsize', XTICK_FONTSIZE)
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
            
            spectrogram(x, hann(N), M, N, fs, 'yaxis');
            ylim([ylim_lb ylim_ub]./1000);
            xlabel('Time (s)');
            ylabel('Frequency (kHz)');
            colorbar off
            hold on;
            scatter(t, f0sequence./1000, SCATTER_SIZE, [0.89 0.259 0.204]);
            hold off;
            
            xlim([t_s t_e]);
            ylim([ylim_lb ylim_ub]./1000);
            
             %%
            set(gca, 'XTickLabel' , get(gca,'XTickLabel'), 'fontsize', XTICK_FONTSIZE);
            set(gcf, 'Position', [10 10 809 500]);
            saveas(figure(j + 1), strcat(figuredir, songname, '_', modelname, '_', type, '_both_notes_paper_1.png'));
            
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
            clf(figure(j + 1));
            figure(j + 1);
            
            spectrogram(x, hann(N), M, N, fs, 'yaxis');
            ylim([ylim_lb ylim_ub]./1000);
            xlabel('Time (s)');
            ylabel('Frequency (kHz)');
            colorbar off
            hold on;
            scatter(t, filteredsequence./1000, SCATTER_SIZE, [0.89 0.259 0.204]);
            if ~isempty(noise)
                hold on;
                scatter(t, noise./1000, SCATTER_SIZE*2, [229 151 105]./255, 'marker', 'x');
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
            scatter(t(pos(1, :)), pos(2, :), 18, [0.89 0.259 0.204]);
            
            hold off;
           
            xlim([t_s t_e]);
            ylim([ylim_lb ylim_ub]./1000);
            
            %%
            set(gca, 'XTickLabel' , get(gca,'XTickLabel'), 'fontsize', XTICK_FONTSIZE);
            set(gcf, 'Position', [10 10 809 500]);
            saveas(figure(j + 1), strcat(figuredir, songname, '_', modelname, '_', type, '_both_notes_paper_2.png'));
        end
    end
end