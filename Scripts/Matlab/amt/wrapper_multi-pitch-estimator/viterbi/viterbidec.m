function y = viterbidec(x)
    y = x;

    L = size(x, 2);
    X = sum(x ~= 0);
    
    if X(L) > 1
        idx = find(y(:, L) ~= 0);
        x_i = y(idx, L);
        r = tiedrank(-x_i);
        
        p = r;
        
        [~, j] = min(p);
        
        y(1:(idx(j) - 1), L) = 0;
        y((idx(j) + 1):end, L) = 0;

        X(L) = 1;
    end
    
    for i=(L - 1):-1:1
        if X(i) > 1
            if X(i + 1) == 1
                idx = find(y(:, i) ~= 0);
                x_i = y(idx, i);
                r = tiedrank(-x_i);
                
                w = find(y(:, i + 1) ~= 0);
                
                p = r + abs(w - idx);
            elseif X(i + 1) == 0
                idx = find(y(:, i) ~= 0);
                x_i = y(idx, i);
                r = tiedrank(-x_i);
                
                p = r;
            end
            
            [~, j] = min(p);
            
            y(1:(idx(j) - 1), i) = 0;
            y((idx(j) + 1):end, i) = 0;
            
            X(i) = 1;
        end
    end
end