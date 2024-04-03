%%%%%%%%%%%%%%%%%%%
%%% TPs Telecom %%%
%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Étude d'une chaine de transmission en bade de base %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all

Fe = 24000; % feq ech en Hz
Te = 1 / Fe; % période ech en s
Rb = 3000; % debit binaire en bit/s
Tb = 1 / Rb;
N = 500; % nb ech

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Étude de modulateurs en bade de base - Efficacité spectrale %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close all

%%%% Modulateur 1 %%%%

% params
Ns1 = Tb / Te; % Nb ech
tps1 = linspace(0, N / Rb, N * Ns1); % échelle de temps

% création signal + mapping
bits1 = randi([0, 1], 1, N); % générer bits
a1 = bits1 * 2 - 1; % mapping 1->1 et 0->-1

%filtre
diracs1 = kron(a1, [1 zeros(1, Ns1 - 1)]); % somme diracs
h1 = ones(1, Ns1); % filtre de réponse impulsionnelle rectangulaire

% sortie
s1 = filter(h1, 1, diracs1);
s1 = reshape(s1', 1, numel(s1));
Sx1 = pwelch(s1, [], [], [], Fe, "twosided"); % DSP

freq1 = linspace(-Fe / 2, Fe / 2, length(Sx1)); % échelle freq

figure("name", "Modulation 1")
subplot(2, 1, 1)
plot(tps1, s1);
title("bits émis")
xlabel("temps")

subplot(2, 1, 2)
semilogy(freq1, fftshift(Sx1))
title("DSP")
xlabel("fréquence")

%%%% Modulateur 2 %%%%

% params
Ns2 = 2 * Ns1; % Nb ech
tps2 = linspace(0, N / Rb, N * Ns2); % échelle de temps

% création signal + mapping
bits2 = randi([0, 3], 1, N); % générer bits
mapping = [-3, -1, 1, 3];
a2 = mapping(bits2 + 1); % +1 car MATLAB indexe à 1

% filtre
diracs2 = kron(a2, [1 zeros(1, Ns2 - 1)]); % somme diracs
h2 = ones(1, Ns2); % filtre de réponse impulsionnelle rectangulaire

% sortie
s2 = filter(h2, 1, diracs2);
s2 = reshape(s2', 1, numel(s2));

Sx2 = pwelch(s2, [], [], [], Fe, "twosided"); % DSP

freq2 = linspace(-Fe / 2, Fe / 2, length(Sx2)); % échelle freq

figure("name", "Modulation 2")
subplot(2, 1, 1)
plot(tps2, s2);
title("bits émis")
xlabel("temps")

subplot(2, 1, 2)
semilogy(freq2, fftshift(Sx2))
title("DSP")
xlabel("fréquence")

%%%% Modulateur 3 %%%%

% TODO

%%%% Superposition %%%%

figure("name", "superposition des DSP")
semilogy(freq1, fftshift(Sx1))
hold on
semilogy(freq2, fftshift(Sx2), 'r')
% plot(freq3, Sx3)

% en rouge c'est mieux parce que l'énergie est concentrée sur une petite
% bande de fréquence

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Étude des interférances entre symbole - Critère de Nyquist %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Étude de l'impact d'un canal à bruit additif, blanc et Gaussien %%
%% - Efficacité en puissance                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO
