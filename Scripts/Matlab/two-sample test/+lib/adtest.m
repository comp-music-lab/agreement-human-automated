classdef adtest < handle
    properties
    end
    
    methods
        function obj = adtest()
        end
        
        function [p_h0, teststat] = test(obj, x, y)
            %%
            statfun = @obj.adstat;
            h0cdf = @obj.adcdf;
            
            %%
            teststat = statfun(x, y);
            p_cdf = h0cdf(teststat);
            
            %%
            p_h0 = 1 - p_cdf;
        end
        
        function p = adpdf(obj, v)
            h0cdf = @obj.adcdf;
            h0pdf = @(x) obj.numericalpdf(x, h0cdf);
            
            p = h0pdf(v);
        end
        
        function x = adinv(obj, p)
            h0cdf = @obj.adcdf;
            
            x = obj.numericalpinv(p, h0cdf);
        end
        
        function p = adcdf(obj, z)
            z = z(:);

            idx_1 = z >= 100;
            idx_p = z < 100;
            z_p = z(idx_p);

            p = zeros(length(z), 1);

            if ~isempty(z_p)
                j = 0:1000;
                t = (pi^2/8) .* (z_p.^(-1) * (4.*j + 1).^2);

                c_0 = exp(log(pi) - t - 0.5.*log(2.*t));
                c_1 = sqrt(pi^3/2) .* erfc(sqrt(t));

                I = 20;
                coef_z = cell2mat(arrayfun(@(x) exp(x.*log(z_p./8) - sum(log(1:x))), 1:I, 'UniformOutput', false));

                F = c_0 .* 0;
                for u=1:length(z_p)
                    for v=1:length(j)
                        F(u, v) = c_0(u, v) + c_1(u, v)*coef_z(u, 1);

                        c_m = c_1(u, v);
                        c_n = c_0(u, v);
                        for l=2:I
                            c_l = ((l - 1.5 - t(u, v))*c_m + t(u, v)*c_n) / (l - 1);

                            F(u, v) = F(u, v) + c_l*coef_z(u, l);

                            c_n = c_m;
                            c_m = c_l;
                        end
                    end
                end

                coef = @(j) real(exp(j*log(-0.5) + (-1)*sum(log(1:j)) + sum(log((1:j).*2 - 1)) + log(4*j + 1)));
                C = arrayfun(coef, j);
                C = C(:);

                p(idx_p) = (F * C)./z_p;
            end

            p(idx_1) = 1;
        end
        
        function A = adstat(obj, x, y)
            N_x = length(x);
            N_y = length(y);

            N = N_x + N_y;
            xy = sort([x; y], 'asc');

            A = sum(arrayfun(@(i) (numel(find(x <= xy(i)))*N - N_x*i)^2/(i*(N - i)), 1:(N - 1)))/(N_x*N_y);
        end
        
        function D = numericalpinv(obj, al, cdffun)
            D = 0.5;
            jump = 0.3;
            criticalp = 1 - al;
            p = cdffun(D);

            while p < criticalp || abs(p - criticalp) > 1e-12
                if p < criticalp
                    D = D + jump;
                else
                    D = D - jump;
                end

                p = cdffun(D);
                jump = jump * 0.9;
            end
        end
        
        function pdf = numericalpdf(obj, x, cdffun)
            eps = 1e-8;

            pdf = (cdffun(x + eps) - cdffun(x - eps))./(2 * eps);
        end
    end
end