classdef ewt < handle
    properties
        al
    end
    
    methods
        function obj = ewt(al)
            obj.al = al;
        end
        
        function [W, E] = i_ewt(obj, x, w_n, gpumode)
            %%
            if gpumode
                x = gpuArray(cast(x, 'single'));
                w_n = gpuArray(cast(w_n, 'single'));
                obj.al = gpuArray(cast(obj.al, 'single'));
            end
            
            %%
            gm = obj.h_tightgm(w_n, obj.al);
            
            %%
            PI = obj.h_angfreq(length(x));
            
            %%
            x = x(:);
            L = length(x);

            W = zeros(L, length(w_n) - 1);
            E = zeros(L, 1);
            
            if gpumode
                PI = gpuArray(cast(PI, 'single'));
                W = gpuArray(cast(W, 'single'));
                E = gpuArray(cast(E, 'single'));
            end
            
            %%
            X = fft(x);

            %% Compute
            phi = obj.scalingfun(PI, gm, w_n(2));
            phi = fftshift(phi);
            W(:, 1) = ifft(X .* conj(phi));
            E = E + sum(abs(phi).^2, 2);

            for n=2:(length(w_n) - 1)
                psi = obj.waveletfun(PI, gm, w_n(n), w_n(n + 1));
                psi = fftshift(psi);
                W(:, n) = ifft(X .* conj(psi));
                E = E + sum(abs(psi).^2, 2);
            end
        end
        
        function gm = h_tightgm(obj, w_n, alpha)
            gm = min(...
                (w_n(2:end) - w_n(1:end - 1)) ./ (w_n(2:end) + w_n(1:end - 1))...
                );

            gm = gm * alpha;
        end
        
        function PI = h_angfreq(obj, L)
            PI = (0:(L - 1)) .* (2*pi/L);
            PI = fftshift(PI);
            I = PI >= pi;
            PI(I) = PI(I) - 2*pi;
        end
        
        function y = be(obj, x)
            y = x.^4 .* (35 - 84.*x + 70.*(x.^2) - 20.*(x.^3));
        end

        function phi = scalingfun(obj, PI, gm, w)
            phi = zeros(length(PI), 1);

            if isa(PI, 'gpuArray')
                phi = gpuArray(cast(phi, 'single'));
            end
            
            %%
            I = abs(PI) <= (1 - gm)*w;
            phi(I) = 1;

            %%
            I = (1 - gm) * w < abs(PI) & abs(PI) <= (1 + gm) * w;
            C = obj.h_coef(PI(I), gm, w);
            phi(I) = cos(0.5*pi .* obj.be(C));
        end

        function psi = waveletfun(obj, PI, gm, w_1, w_2)
            psi = zeros(length(PI), 1);

            if isa(PI, 'gpuArray')
                psi = gpuArray(cast(psi, 'single'));
            end
            
            %%
            I = (1 + gm) * w_1 < abs(PI) & abs(PI) < (1 - gm) * w_2;
            psi(I) = 1;

            %%
            I = (1 - gm) * w_2 <= abs(PI) & abs(PI) <= (1 + gm) * w_2;
            C = obj.h_coef(PI(I), gm, w_2);
            psi(I) = cos(0.5*pi .* obj.be(C));

            %%
            I = (1 - gm) * w_1 <= abs(PI) & abs(PI) <= (1 + gm) * w_1;
            C = obj.h_coef(PI(I), gm, w_1);
            psi(I) = sin(0.5*pi .* obj.be(C));
        end

        function C = h_coef(obj, PI, gm, w)
            C = 1/(2*gm*w) .* (abs(PI) - (1 - gm)*w);
        end

        function r = i_iewt(obj, W, w_n)
            %%
            gm = obj.h_tightgm(w_n, obj.al);
            
            %%
            PI = obj.h_angfreq(size(W, 1));
            
            %%
            r = zeros(size(W, 1), size(W, 2));

            if isa(W, 'gpuArray')
                PI = gpuArray(cast(PI, 'single'));
                r = gpuArray(cast(r, 'single'));
            end
            
             %% Compute
            phi = obj.scalingfun(PI, gm, w_n(2));
            phi = fftshift(phi);
            w = ifft(fft(W(:, 1)) .* phi);

            %i_mean = sum(abs(imag(w)))/numel(w);
            %assert(i_mean < 1e-8, sprintf('average imaginary value: %e', i_mean));
            r(:, 1) = real(w);

            for n=2:(length(w_n) - 1)
                psi = obj.waveletfun(PI, gm, w_n(n), w_n(n + 1));
                psi = fftshift(psi);
                w = ifft(fft(W(:, n)) .* psi);

                %i_mean = sum(abs(imag(w)))/numel(w);
                %assert(i_mean < 1e-8, sprintf('average imaginary value: %e', i_mean));
                r(:, n) = real(w);
            end
        end
    end
end