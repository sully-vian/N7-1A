# Décomposition en Valeurs Singulières d'une matrice et applications

## I - SVD d'une matrice

Soit $A\in M_{m, n}(\mathbb{R})$ tq $\text{rg}(A) \geqslant 1$

### 1) Propriétés de $A^TA \in M_n(\mathbb{R})$

- $A^TA$ est symétrique semi-def positive
- $A^TA$ est orthodiagonalisable et $\text{Sp}(A^TA) \subset \mathbb{R}_+^*$ ($\lambda_i \geqslant 0$)
- $\begin{cases}\text{Ker}(A^TA) = \text{Ker}(A) \\ \text{Im}(A^TA) = \text{Im}(A) \end{cases}$

  - $\text{Ker}(A)\subset \text{Ker}(A^TA)$ : OK \
      Soit $x \in \text{Ker}(A^TA)$, alors, $A^TAx=0$ et $x^TA^TAx = 0 = ||Ax||^2_2$ donc $Ax=0$ et $x \in \text{Ker}(A)$

  - $\text{Im}(A^TA) \subset \text{Im}(A)$ : OK \
      D'paprès le théorème du rang, $\text{rg}(A^TA) + \text{dim}(\text{Ker}(A^TA)) = n$ donc $\text{rg}(A^TA) = n - \text{dim}(\text{Ker}(A)) = \text{rg}(A) = \text{rg}(A^T)$

- $A^TA \in GL_n(\mathbb{R}) \Longleftrightarrow \text{A}=n$
  - $A^TA \in GL_n(\mathbb{R}) \Longleftrightarrow \text{Ker}(A^TA) = \{0\} \Longleftrightarrow \text{Ker}(A) = \{0\} \Longleftrightarrow\text{rg}(A) = n$

### 2) Construction de la SVD de $A$

#### Objectif

Trouver des bases de $\mathbb{R}^m$ et $\mathbb{R}^n$ pour obtenir une représentation de l'application associée à A qui ressemble à

$$
A = U
\left(\begin{array}{c|c}
    \begin{matrix}
        \ddots & (0) \\
        (0) & \ddots
    \end{matrix} & (0) \\ \hline
    (0) & (0)
\end{array}\right)
V^T
$$

Avec $U \in O_m(\mathbb{R})$, $V \in O_n(\mathbb{R})$ \
On pose $r = \text{rg}(A) \leqslant 1$. Alors, $\text{rg}(A^TA) = \text{rg}(A) = r$ \
D'où $0$ valeur propre de $A^TA$ de multiplicité $n-r$ donc $r$ valeurs propres non nulles ($>0$).

Notons $(\lambda_i)_{i\in[\![1; r]\!]}$ les $r$ valeurs propres non nulles de $A^TA$, classées par ordre décroissant : $\lambda_1 > \lambda_2 > \dots > \lambda_r > \lambda_{r+1} = \lambda_n = 0$

Soit $(v_i)_{i\in[\![1; n]\!]}$ BON de vecteurs propres de $A^TA$ associés à $(\lambda_i)_{i\in[\![1; n]\!]}$ \
Soit $\Sigma = (v_1 \dots v_r\ v_{r+1} \dots v_n)$ BON de $\mathbb{R}^n$

On pose $\forall i\in[\![1; r]\!], u_i = Av_i \frac{1}{\sqrt{\lambda_i}}$ (on normalise) \
Donc, $\forall (i, j)\in[\![1; r]\!]^2, u_i^Tu_j = \frac{1}{\sqrt{\lambda_i\lambda_j}}v_i^TAA^Tv_j = \frac{1}{\sqrt{\lambda_i\lambda_j}}v_i^T\lambda_jv_j = \delta_{ij}$ \
Donc, $(u_i)_{i\in[\![1; r]\!]}\in(\mathbb{R}^m)^r$ est une famille orthonormale de $\mathbb{R}^m$ \
De plus, $\forall i\in[\![1; r]\!], u_i\in\text{Im}(A)$ d'où $\text{Vect}(u_i)\subset\text{Im}(A)$ \
or, $\text{rg}(u_i) = r = \text{rg}(A)$ donc $\text{Vect}(\underset{i\in[\![1; r]\!]}{u_i}) = \text{Im}(A)$ \
on complète $(u_i)_{i\in[\![1; r]\!]}$ en $(u_i)_{i\in[\![1; m]\!]}$ BON de $\mathbb{R}^m$ \
d'où la base $\mathcal{F_i} = (u_1 \dots u_r\ u_{r+1} \dots u_m)$

Aussi, $\forall i\in[\![1; r]\!], AA^Tu_i = \frac{1}{\sqrt{\lambda_i}}AA^TAv_i = \frac{1}{\sqrt{\lambda_i}}A\lambda_iv_i = \lambda_i \left(\frac{1}{\sqrt{\lambda_i}}Av_i\right) = \lambda_iu_i$ \
Donc, $u_i$ vecteur propre de $AA^T$ associé à $\lambda_i$

On pose

$$
V = (v_1 \dots v_n) \in O_n(\mathbb{R}) \\
U = (u_1 \dots u_m) \in O_m(\mathbb{R})
$$

d'où

$$
U^TAV =
\left(\begin{matrix}
    u_1^T \\
    \vdots \\
    u_r^T \\
    u_{r+1}^T \\
    \vdots \\
    u_m^T
\end{matrix}\right)
A\
\left(\begin{matrix}
    v_1 & \dots & v_r & v_{r+1} & \dots & v_n
\end{matrix}\right)
$$

$$
U^TAV =
\left(\begin{matrix}
    u_1^T \\
    \vdots \\
    u_r^T \\
    u_{r+1}^T \\
    \vdots \\
    u_m^T
\end{matrix}\right)
\begin{matrix}

    \left(\begin{matrix}
        Av_1 & \dots & Av_r & Av_{r+1} & \dots & Av_n
    \end{matrix}\right) \\ \\
    \left(\begin{array}{c|c}
        \begin{matrix}
            \sqrt{\lambda_1}u_1^Tv_1 & \dots & \sqrt{\lambda_r}u_1^Tv_r \\
            \vdots & & \vdots \\
            \sqrt{\lambda_1}u_r^Tv_1 & \dots & \sqrt{\lambda_r}u_r^Tv_r \\
        \end{matrix} & (0) \\ \hline
        \begin{matrix}
            \sqrt{\lambda_1}u_{r+1}^Tv_1 & \dots & \sqrt{\lambda_r}u_{r+1}^Tv_r \\
            \vdots & & \vdots \\
            \sqrt{\lambda_1}u_m^Tv_1 & \dots & \sqrt{\lambda_r}u_m^Tv_r \\
        \end{matrix} & (0)
    \end{array}\right)

\end{matrix}
$$

Or,

$$
\forall (i,j)\in[\![r+1;m]\!]\times[\![1;r]\!], u_j^Tu_i = 0 \\
\forall (i,j)\in[\![r+1;m]\!]^2, \sqrt{\lambda_j}u_i^Tu_j = \sqrt{\lambda_i}\ \delta_{i,j}
$$

Finalement,

$$
UAV = \left(\begin{array}{c|c}
    \Sigma_r & (0) \\ \hline
    (0) & (0)
\end{array}\right)
\text{ avec }
\Sigma_r = \left(\begin{matrix}
    \sqrt{\lambda_1} & & (0) \\
    & \ddots & \\
    (0) & & \sqrt{\lambda_r}
    \end{matrix}\right) \in M_r(\mathbb{R})
$$

Donc,

$$
A = U \Sigma V^T \text{ avec }
\Sigma = \left(\begin{matrix}
    \Sigma_r & (0) \\
    (0) & (0)
\end{matrix}\right)
$$

#### Définition

Soit $A\in M_{m, n}(\mathbb{R})$ tq $r = \text{rg}(A) \geqslant 1$ \
On appelle SVD de $A$ toute factorisation du type

$$
A = U\Sigma V^T
\text{ avec }
\begin{cases}
    U \in O_m(\mathbb{R}) \\
    V \in O_n(\mathbb{R})
\end{cases}
\text{ et }
\Sigma =
\left(\begin{array}{c|c}
    \begin{matrix}
        \sigma_1 & & (0) \\
        & \ddots & \\
        (0) & & \sigma_r
    \end{matrix} & (0) \\ \hline
    (0) & (0)
\end{array}\right)
$$

avec $a_i = \sigma_i = \sqrt{\lambda_i} > 0, \forall i\in[\![1; r]\!]$ les valeurs simplifiées de $A$ et $(\lambda_i)_{i\in[\![1; r]\!]}$ les valeurs propres strictement positives de $A^TA$.

#### Propriété: formes réduites

- $A=U_1\ \Sigma_r\ V_1^T$ : libraires d'algèbre linéaire, fonction SVD : choix de la forme de la SVD
- $A = (U_1 \dots U_r)
\begin{pmatrix}
    \sigma_1 & & (0) \\
    & \ddots & \\
    (0) & & \sigma_r
\end{pmatrix}
\begin{pmatrix}
    V_1^T \\
    \vdots \\
    V_r^T
\end{pmatrix}$
Donc $A = \sum\limits_{i=1}^r \sigma_i\ U_i\ V_i^T$

## II Matrice pseudo-inverse

### 1) Définition et quelques propriétés

#### Définition

Soit $A\in M_{m,n}(\mathbb{R})$ tq $rg(A) = r \geqslant 1$ \
Soit $A = U\Sigma V^T$ une SVD de $A$ avec $U\in O_m(\mathbb{R})$, $V\in O_n(\mathbb{R})$ \
On appelle matrice pseudo-inverse (ou inverse généralisée) de $A$, notée $A^+$ la matrice $V\Sigma^+U^T$ \
