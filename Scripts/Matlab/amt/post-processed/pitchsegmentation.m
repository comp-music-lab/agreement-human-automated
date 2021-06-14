function notes = pitchsegmentation(X, unison)
    if unison
        buf = [0 X];
        
        segdif = diff(buf);
        
        notes = X(segdif ~= 0);
        
        notes = notes(notes ~= 0);
    else
        buf = [0 X(X ~= 0)];
        
        segdif = diff(buf);
        
        I = find(segdif ~= 0) + 1;
        
        notes = buf(I);
    
        notes = notes(notes ~= 0);
    end
end