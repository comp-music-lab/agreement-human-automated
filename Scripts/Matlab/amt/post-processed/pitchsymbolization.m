function [Y, notes] = pitchsymbolization(notes, freqset, notenameset, numvoices)
    %% convert pitch to note names
    L = length(notes);
    Y = cell(L, 1);
    C1 = freqset(1);
    
    for l=1:L
        rangeid = floor(log2(notes(l) / C1));
        idx = find(notes(l) == freqset);
        
        noteidx = mod(idx, numvoices);
        if noteidx == 0
            noteidx = noteidx + numvoices;
        end
        
        letter = notenameset{noteidx};
        
        Y{l} = strcat(letter, num2str(rangeid));
    end
end