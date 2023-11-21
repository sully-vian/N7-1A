% Fonction parametres_correlation (exercice_1.m)

function [r,a,b] = parametres_correlation(Vd,Vg)

% Calcul des variables nécessaires
m_Vd = mean(Vd);
v_Vd = mean(Vd.^2) - m_Vd^2;
m_Vg = mean(Vg);
v_Vg = mean(Vg.^2) - m_Vg^2;
cov = mean(Vd.*Vg) - m_Vd * m_Vg;

% calul de r,a et b
r = cov / sqrt(v_Vd * v_Vg);
a = cov / v_Vd;
b = m_Vd - a * m_Vg;

end