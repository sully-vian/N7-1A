close all

% données constantes:
Fe = 24000;  % fréquence d'échantillonnage
Te = 1/Fe;   % période d'échantillonnage
Rb = 3000;   % débit binaire
Tb = 1/3000; % période binaire 

%Répartition des bits
N = 1000;    % nombre de bits
bits = randi([0 1],1,N); % message binaire

% Données Chaine 1:
M1 = 2;           % ordre de modulation
L1 = log2(M1);     % nombre de bits par symbole
Ts1 = L1 * Tb;     % période du symbole
Rs1 = 1/Ts1;       % débit du symbole
Ns1 = Ts1/Te;      % nb bits par symbole
long1 = N * Ns1/L1; % longuer du signal
T1 = long1 * Te;   % duréé du signal

%mapping binaire
Mapping1 = 2*bits - 1;
diracs1 = kron(Mapping1,[1 zeros(1,Ns1 -1)]);

%filtrage de mise en forme:
h1 = ones(1,Ns1); % réponse inpulsionelle 
x1 = filter(h1,1,diracs1);

%bruit
EbN0 = 100;
Px1 = mean(abs(x1).^2);
sigma12 = (Px1*Ns1) / (2*log2(M1)*EbN0);
sigma1 = sqrt(sigma12);
bruit1 = sigma1 * randn(1,length(x1));

x1 = x1 + bruit1;
hr1 = h1;
y1 = filter(hr1,1,x1);

g1 = conv(h1,hr1);



figure("Name","oeil 1");
plot(reshape(y1,Ns1,length(y1)/Ns1));

% chaine 2
diracs2 = diracs1;
h2 = h1;
hr2 = ones(1,Ns1/2);
g2 = conv(h2,hr2);
x2 = filter(g2,1,diracs2);
figure("Name","oeil 3");
plot(reshape(x2,Ns1,length(x2)/Ns1));

%chaine 3;
M3 = 4;             % ordre de modulation
L3 = log2(M3);      % nombre de bits par symbole
Ts3 = L3 * Tb;      % période du symbole
Rs3 = 1/Ts3;        % débit du symbole
Ns3 = Ts3/Te;       % nb bits par symbole
long3 = N * Ns3/L3; % longuer du signal
T3 = long3 * Te;    % duréé du signal

%mapping binaire
Mapping3 = reshape(bits,2,N/2);
Mapping3 = bi2de(Mapping3');
Mapping3 = Mapping3' * 2 - 3;

diracs3 = kron(Mapping3,[1 zeros(1,Ns3 -1)]);
h3 = ones(1, Ns3);
hr3 = h3;
g3 =conv(h3, hr3);
x3 = filter(g3,1,diracs3);
figure("Name","oeil 3");
plot(reshape(x3,Ns3,length(x3)/Ns3));

figure("Name", "filtres globaux")
plot(g1)
hold on
plot(g2)
hold on
plot(g3)
legend("g1", "g2", "g3")














