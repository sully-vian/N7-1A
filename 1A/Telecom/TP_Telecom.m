%%%%%%%%%%%%%%%%%%%
%%% TPs Telecom %%%
%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Étude d'une chaîne de transmission en bade de base %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% constantes
Fe = 24000; % feq ech en Hz
Te = 1 / Fe; % période ech en s
Rb = 3000; % debit binaire en bit/s
Tb = 1 / Rb;
N = 500; % nb ech

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Étude de modulateurs en bade de base - Efficacité spectrale %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close all

mod1 = struct('Ns', Tb / Te, 'bits', randi([0, 1], 1, N), 'mapping', [-1, 1], 'filtre', ones(1, Tb / Te), 'color', 'blue');

mod2 = struct('Ns', 2 * Tb / Te, 'bits', randi([0, 3], 1, N), 'mapping', [-3, -1, 1, 3], 'filtre', ones(1, 2 * Tb / Te), 'color', 'red');

mod3 = struct('Ns', Tb / Te, 'bits', randi([0, 1], 1, N), 'mapping', [-1, 1], 'filtre', rcosdesign(0.5, N, Tb / Te), 'color', 'green');

modulateurs = [mod1, mod2, mod3];

for i = 1:3
    mod = modulateurs(i);
    [s, Sx, tps, freq] = moduler(mod.Ns, mod.bits, mod.mapping, mod.filtre, N, Rb, Fe);

    subplot(2, 2, i)
    plot(tps, s, mod.color)
    title("Modulateur " + i + " - bits envoyés")
    xlabel("temps")

    subplot(2,2,4)
    semilogy(freq, fftshift(Sx), mod.color)
    hold on
end

title("Superposition des DSP")
xlabel("fréquence")
legend("modulateur 1", "modulateur 2", "modulateur 3")

function [s, Sx, tps, freq] = moduler(Ns, bits, mapping, h, N, Rb, Fe)
    tps = linspace(0, N / Rb, N * Ns); % échelle de temps
    a = mapping(bits + 1); % +1 car MATLAB indexe à 1
    diracs = kron(a, [1 zeros(1, Ns - 1)]); % somme diracs
    s = filter(h, 1, diracs);
    s = reshape(s', 1, numel(s));
    Sx = pwelch(s, [], [], [], Fe, "twosided"); % DSP
    freq = linspace(-Fe / 2, Fe / 2, length(Sx)); % échelle freq
end

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %% Étude des interférances entre symbole - Critère de Nyquist %%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % TODO

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %% Étude de l'impact d'un canal à bruit additif, blanc et Gaussien %%
% %% - Efficacité en puissance                                       %%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % TODO
