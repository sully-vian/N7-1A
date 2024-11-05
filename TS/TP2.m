clear all
close all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1 Génération du signal à filtrer %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1
N = 1000; % nb échantillons
A = 1; % amplitude
f1 = 1000;
f2 = 3000;
Fe = 10000;
Te = 1 / Fe;
temps = (0 : N-1) * Te;
x = cos(temps * f1 * 2 * pi) + cos(temps * f2 * 2 * pi);

% 2
figure("name", "1 Génération du signal à filtrer")
subplot(2,1,1)
plot(temps, x)
grid
title("Somme de deux $\cos$, de fr\'equences $f1=1000$Hz et $f2=3000$Hz","Interpreter","latex")
xlabel("temps (s)")
ylabel("signal")

% 3
X = fft(x);
echelle_freq = (0:N-1) * Fe/N;
subplot(2,1,2)
plot(echelle_freq, abs(X))
grid
title("repr\'esentation fr\'equentielle du signal", "Interpreter", "latex")
xlabel("fréquence (Hz)")
ylabel("|TFD|")

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2 Sythèse du filtre passe-bas %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1

funtion ordre = passeBas(x, ordre, N)
    fff
end


% 2

ordre = 11;
fc = 1500; % freq de coupure
pts_shift = (0 : ordre) - (ordre-1)/2;
h = (2 / fc/Fe) * sinc(2*fc/Fe * pts_shift);
y = filter(h, 1, x);
Y = fft(y);

figure
plot(echelle_freq, abs(Y));

% 3

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3 Réalisation du filtrage %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

