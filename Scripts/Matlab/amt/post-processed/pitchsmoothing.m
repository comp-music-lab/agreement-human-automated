function [Y_p, Y_n, dec_t] = pitchsmoothing(X, dt, conf)
    %% parameters
    dec_t = conf.dec_t;
    lenfil = conf.lenfil;
    minlen = conf.minlen;
    
    %% decimation
    if dt < dec_t
        dec_n = round(dec_t/dt);
        X = X(1:dec_n:end);
        dec_t = dec_n * dt;
    else
        dec_t = dt;
    end
    
    %% apply median filter
    if floor(mod(lenfil/dec_t, 2)) == 0
        lenfil_n = floor(lenfil/dec_t) + 1;
    else
        lenfil_n = floor(lenfil/dec_t);
    end
    
    X = medfilt1(X, lenfil_n);
    
    %% pick up f0 longer than 0.1 second
    minlen_n = round(minlen/dec_t);
    T = length(X);
    f0_j = 0;
    count = 0;
    noteflag = zeros(T, 1);
    
    for t=1:T
        f0_i = X(t);
        
        if f0_i ~= 0
            if f0_i == f0_j
                count = count + 1;
                
                if count >= minlen_n
                    noteflag(t - count + 1) = 1;
                end
            else
                if count >= minlen_n
                    noteflag((t - count + 1):(t - 1)) = 1;
                end
                
                count = 1;
            end
        else
            if count >= minlen_n
                noteflag((t - count + 1):(t - 1)) = 1;
            end
            
            count = 0;
        end
        
        f0_j = f0_i;
    end
    
    t = T + 1;
    if count >= minlen_n
        noteflag((t - count + 1):(t - 1)) = 1;
    end
    
    %% divide the original sequence into positive and negative
    Y_p = X;
    I = noteflag == false;
    Y_p(I) = 0;
    
    Y_n = X;
    I = noteflag == true;
    Y_n(I) = 0;
end