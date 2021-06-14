function y = ewtbandpass(x, f_lb, f_ub, fs, gpumode)
    if nargin < 5
        gpumode = false;
    end
    
    %%
    al = 0.97;
    myewt = ewt(al);
    
    %%
    w_n = [0 f_lb f_ub fs/2] .* (2*pi/fs);
    [W, ~] = myewt.i_ewt(x, w_n, gpumode);
    
    %%
    r = myewt.i_iewt(W, w_n);
    
    %%
    y = r(:, 2);
end