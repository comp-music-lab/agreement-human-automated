function Y = pitchquantization(X, freqset)
    T = length(X);
    Y = zeros(1, T);
    
    for t=1:T
        f0 = X(t);
        
        if f0 ~= 0
            [~, locs] = min(abs(f0 - freqset));
            Y(t) = freqset(locs);
        end
    end
end