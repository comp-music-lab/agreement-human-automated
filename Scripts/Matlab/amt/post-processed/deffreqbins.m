function freqset = deffreqbins(A4, numvoices, offset)
    idx_max = numvoices * 4;
    idx_min = -numvoices * 4 - offset + 1;
    idx_n = idx_min:idx_max;
    
    freqset = A4 .* 2.^(idx_n./numvoices);
end