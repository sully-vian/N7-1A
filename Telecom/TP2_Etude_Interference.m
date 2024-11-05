%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TP2 - Étude des interférences entre symbole - Critère de Nyquist %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% constantes
Fe = 24000; % feq ech en Hz
Te = 1 / Fe; % période ech en s
Rb = 3000; % debit binaire en bit/s
Tb = 1 / Rb; % s/bit
N = 100; % nb ech

duree = N / Rb;

Ns = Tb / Te; % symb / bit

% Ns "temporel" = 0.0003s

close all

% émission
bits = randi([0, 1], 1, N);
mapping = [-1, 1];
a = mapping(bits + 1);
diracs = kron(a, [1 zeros(1, Ns - 1)]); % somme diracs
h = ones(1, Ns);
s = filter(h, 1, diracs);
tps = linspace(0, duree, length(s)); % échelle temporelle

figure( ...
    "Name", "Signaux émis et en sortie du filtre de réception", ...
    "NumberTitle", "off")
plot(s, "blue");

% sortie de réception
hr = h;
s = filter(hr, 1, s);

hold on
plot(s, "red");
xlabel("temps")
ylabel("valeur du symbole")
str = {"retard de Ts", "Ts entre chaque instant d'échantillonnage"};
text(0.0005, 6, str)

% décodage
n0 = 3;
y_echant = s(n0:Ns:end);
BitsRecuperes = (sign(y_echant) + 1) / 2; % demapping
erreur = abs(bits - BitsRecuperes);
TEB = mean(erreur);

figure( ...
    "Name", "Réponse impulsionnelle globale de la chaine de transmission", ...
    "NumberTitle", "off")
g = conv(h, h);
plot(g);
xlabel("????")
ylabel("filtre global h*hr")

% diagramme de l'oeil
figure("Name", "diagramme de l'oeil", "NumberTitle", "off")
s_tronque = s(Ns+1:end);
plot(reshape(s_tronque, Ns, length(s_tronque)/Ns))

% explication: 4 valeurs possibles pour deuxc bits (symboles) donc 4 transitions

% instant optimal: 8 parce qu'alors, 1 seul symbole pris en compte

% décodage
