function test_human_machine_twosample
    %%
    c = 1;
    L = 500;
    priorodds = 1;
    
    datadir = './input/';
    outputdir = './output/';
    outputfileID = 'human-machine_two-sample_test';

    agreementvar = {'coef', 'averagePID', 'editsim', 'averageLEVD'};
    testcase = {'without_unison', 'with_unison'};
    
    X_s = 'tony (note)';
    
    %%
    nbpobj_r = lib.nbpfittest(c, L, 'robust');
    nbpobj_n = lib.nbpfittest(c, L, 'normal');
    
    %%
    for i=1:length(testcase)
        %%
        fileinfo = dir(strcat(datadir, testcase{i}, '/*.csv'));
        irrstat = readtable(strcat(datadir, testcase{i}, '/', fileinfo.name));
        
        %%
        idx_x = cell2mat(cellfun(@(x) contains(x, X_s), irrstat.machine, 'UniformOutput', false));

        idx_y = cell2mat(cellfun(@(x) ~contains(x, X_s), irrstat.machine, 'UniformOutput', false));
        Y_s = unique(irrstat.machine(idx_y));
        
        result = zeros(length(Y_s), 8);
        result_table = [];

        %%
        transcriber_h = unique(irrstat.human);
        transcriber_h(end + 1) = {''};
        
        %%
        fprintf('%s:\n', testcase{i});
        
        for l=1:length(transcriber_h)
            for j=1:length(agreementvar)
                for k=1:length(Y_s)
                    %%
                    idx_y = cell2mat(cellfun(@(x) contains(x, Y_s{k}), irrstat.machine, 'UniformOutput', false));
                    idx_l = cell2mat(cellfun(@(x) contains(x, transcriber_h{l}), irrstat.human, 'UniformOutput', false));

                    x = irrstat.(agreementvar{j})(idx_x & idx_l);
                    y = irrstat.(agreementvar{j})(idx_y & idx_l);

                    %%
                    [p, teststat] = lib.adtest().test(x, y);

                    %%
                    A = lib.pb_effectsize(x, y);

                    %%
                    if strcmp(agreementvar{j}, 'NOTUSED')
                        lnbf = nbpobj_r.test(x, y);
                        [posterior_H0, posterior_H1] = nbpobj_r.posterior(priorodds, lnbf);
                    else
                        lnbf = nbpobj_n.test(x, y);
                        [posterior_H0, posterior_H1] = nbpobj_n.posterior(priorodds, lnbf);
                    end

                    %%
                    N_x = length(x);
                    N_y = length(y);
                    
                    result(k, :) = [p, teststat, lnbf, posterior_H0, posterior_H1 A N_x N_y];
                    fprintf('[%s, %s] %s (N = %d) vs. %s (N = %d) - %e, %3.3f, %3.3f, %3.3f, %3.3f\n',...
                        agreementvar{j}, transcriber_h{l}, X_s, N_x, Y_s{k}, N_y, p, teststat, posterior_H0, posterior_H1, A);
                end

                %%
                O = array2table(result, 'VariableNames', {'pval', 'adstat', 'lnbf', 'posterior_H0', 'posterior_H1', 'A', 'N_x', 'N_y'});
                O.machine = Y_s;
                O.var = repmat({agreementvar(j)}, [length(Y_s) 1]);
                O.human = repmat({transcriber_h(l)}, [length(Y_s) 1]);

                %%
                result_table = [result_table; O];
            end
        end
        
        %%
        outputfilename = strcat(outputdir, testcase{i}, '/', outputfileID, '_', testcase{i}, '.csv');
        writetable(result_table, outputfilename, 'WriteVariableNames', true, 'WriteRowNames', true);
        
        %%
        fprintf('\n');
    end
end