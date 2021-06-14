function plotaudiowave
    %%
    audiofilepath = '../../audio/paper/NAIV-075.wav';
    figuredir = '../../figures/paper/End-to-End/';
    figurename = 'NAIV-075_audiowave.png';
    
    t_s = 6.3;
    t_e = 14.0;
    
    %%
    [x, fs] = audioread(audiofilepath);
    
    if size(x, 2) == 2
        x = mean(x, 2);
    end
    
    t = (0:(length(x) - 1))./fs;
    
    figure(1);
    plot(t, x);
    xlim([t_s t_e]);
    
    set(gcf, 'Position', [10 10 809 500]);
    saveas(figure(1), strcat(figuredir, figurename));
end