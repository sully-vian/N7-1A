close all
clear all
clc

% données constantes:
Fe = 24000; % fréquence d'échantillonnage
Te = 1 / Fe; % période d'échantillonnage
Rb = 3000; % débit binaire
Tb = 1/3000; % période binaire

%Répartition des bits
N = 1000; % nombre de bits
bits = randi([0 1], 1, N); % message binaire

% Données Chaine 1:
Ns = Tb / Te;
M1 = 2; % ordre de modulation
L1 = log2(M1); % nombre de bits par symbole
Ts1 = L1 * Tb; % période du symbole
Rs1 = 1 / Ts1; % débit du symbole
Ns1 = Ts1 / Te; % nb bits par symbole
long1 = N * Ns1 / L1; % longuer du signal
T1 = long1 * Te; % duréé du signal

%mapping binaire
Mapping1 = 2 * bits - 1;
diracs1 = kron(Mapping1, [1 zeros(1, Ns1 -1)]);

%filtrage de mise en forme:
h1 = ones(1, Ns1); % réponse inpulsionelle
x1 = filter(h1, 1, diracs1);

%bruit
TEB = zeros(1, 9);
TEB1 = zeros(1, 9);

for EbN0dB = 0:8
    Px1 = mean(abs(x1) .^ 2);
    sigma12 = (Px1 * Ns1) / (2 * log2(M1) * (10 ^ (EbN0dB / 10)));
    sigma1 = sqrt(sigma12);
    bruit1 = sigma1 * randn(1, length(x1));

    x1_boucle = x1 + bruit1;
    hr1 = h1;
    y1 = filter(hr1, 1, x1_boucle);

    n0 = 8;
    y_echant = y1(n0:Ns1:end);
    BitsRecuperes = (sign(y_echant) + 1) / 2; % demapping
    erreur = abs(bits - BitsRecuperes);
    TEB1(EbN0dB + 1) = qfunc(sqrt(2 * 10 ^ (EbN0dB / 10)));
    TEB(EbN0dB + 1) = mean(erreur);
end

figure("Name", "TEB1")
semilogy(TEB);
hold on
semilogy(TEB1);
legend('TEB observé', 'TEB théorique')
xlabel('Eb/N0(dB)')
ylabel('TEB')
g1 = conv(h1, hr1);

figure("Name", "oeil 1");
plot(reshape(y1, Ns1, length(y1) / Ns1));

% chaine 2
diracs2 = diracs1;
h2 = h1;
hr2 = ones(1, Ns1 / 2);
g2 = conv(h2, hr2);
x2 = filter(h2, 1, diracs2);

TEB_2 = zeros(1, 9);
TEB12 = zeros(1, 9);

for EbN0dB = 0:8

    Px2 = mean(abs(x2) .^ 2);
    sigma12 = (Px2 * Ns1) / (2 * log2(M1) * (10 ^ (EbN0dB / 10)));
    sigma1 = sqrt(sigma12);
    bruit1 = sigma1 * randn(1, length(x2));

    x2_boucle = x2 + bruit1;
    hr1 = ones(1, Ns/2);
    y1 = filter(hr1, 1, x2_boucle);

    n0 = 8;
    y_echant = y1(n0:Ns1:end);
    BitsRecuperes = (sign(y_echant) + 1) / 2; % demapping
    erreur = abs(bits - BitsRecuperes);
    TEB12(EbN0dB + 1) = qfunc(sqrt(10 ^ (EbN0dB / 10)));
    TEB_2(EbN0dB + 1) = mean(erreur);
end

figure("Name", "TEB2")
semilogy(TEB_2);
hold on
semilogy(TEB12);
legend('TEB observé', 'TEB théorique')
xlabel('Eb/N0(dB)')
ylabel('TEB')

figure("Name", "oeil 2");
plot(reshape(y1, Ns1, length(y1) / Ns1));

%chaine 3;
M3 = 4; % ordre de modulation
L3 = log2(M3); % nombre de bits par symbole
Ts3 = L3 * Tb; % période du symbole
Rs3 = 1 / Ts3; % débit du symbole
Ns3 = Ts3 / Te; % nb bits par symbole
long3 = N * Ns3 / L3; % longuer du signal
T3 = long3 * Te; % duréé du signal

% MAPPING 4-AIRE
Mapping3 = zeros(1, length(bits) / 2);

for i = 1:length(Mapping3)

    if (bits(2 * i - 1:2 * i) == [1 1])
        Mapping3(i) = 3;
    elseif (bits(2 * i - 1:2 * i) == [0 1])
        Mapping3(i) = 1;
    elseif (bits(2 * i - 1:2 * i) == [0 0])
        Mapping3(i) = -1;
    else
        Mapping3(i) = -3;
    end

end

diracs3 = kron(Mapping3, [1 zeros(1, Ns3 -1)]);
h3 = ones(1, Ns3);
hr3 = h3;
g3 = conv(h3, hr3);

x3 = filter(h3, 1, diracs3);
TEB_3 = zeros(1, 9);
TEB13 = zeros(1, 9);

for EbN0dB = 0:8

    Px3 = mean(abs(x3) .^ 2);
    sigma12 = (Px3 * Ns3) / (2 * log2(M3) * (10 ^ (EbN0dB / 10)));
    sigma1 = sqrt(sigma12);
    bruit1 = sigma1 * randn(1, length(x3));

    x3_boucle = x3 + bruit1;
    hr3 = h3;
    y3 = filter(hr3, 1, x3_boucle);

    n0 = 16;
    y_echant = y3(n0:Ns3:end) / Ns3;

    % DEMAPPING
    BitsRecuperes = zeros(1, N);

    for i = 1:length(y_echant)

        if (y_echant(i) > 2)
            BitsRecuperes(2 * i - 1:2 * i) = [1 1];
        elseif (y_echant(i) > 0)
            BitsRecuperes(2 * i - 1:2 * i) = [0 1];
        elseif (y_echant(i) > -2)
            BitsRecuperes(2 * i - 1:2 * i) = [0 0];
        else
            BitsRecuperes(2 * i - 1:2 * i) = [1 0];
        end

    end

    % Calcul TEB
    erreur = abs(bits - BitsRecuperes);
    TEB13(EbN0dB + 1) = (3/4) * qfunc(sqrt((12/15) * 10 ^ (EbN0dB / 10)));
    TEB_3(EbN0dB + 1) = mean(erreur);

end

figure("Name", "TEB3")
semilogy(TEB_3);
hold on
semilogy(TEB13);
figure("Name", "oeil 3");
plot(reshape(y3, Ns3, length(y3) / Ns3));
legend('TEB observé', 'TEB théorique')
xlabel('Eb/N0(dB)')
ylabel('TEB')

figure("Name", "filtres globaux")
plot(g1)
hold on
plot(g2)
hold on
plot(g3)
legend("g1", "g2", "g3")
