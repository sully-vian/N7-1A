% Compte-rendu minimal du mini-projet SDA : LCA et TH
% Auteur : NOM Prénom
% Groupe de TP : ...

**Consigne :** Vous devez écrire vos réponse à la place des ... en laissant
une ligne vide avant et deux après votre réponse.

**Remarque :** Ce document utilise le langage Markdown. On peut en engendrer
une version PDF en faisant par exemple :

~~~bash
pandoc --toc -N -o LISEZ-MOI.pdf LISEZ-MOI.txt
~~~

# Exercice 1

## Question 1.4

**Indiquer les inconvénients/avantages d'une implantation par listes chaînées
d'une SDA.**

On a une SDA de taille variable.
C'est d'abord un avantage pour l'utilisateur : il n'a pas à se soucier de la taille qu'il prend puisque sa seule limite est fixée par la mémoire de l'ordinateur.
C'est aussi un avantage pour la machine : L'espace utilisé n'est pas fixe mais libéré dès qu'inutile.

D'un  autre côté, si l'ajout d'élément est réalisable, il ne s'effectue pas en temps constant puisqu'on doit vérifier que la structure ne comporte pas d'autre élément de clé identique.

# Évaluation expérimentale

## Performance comparée de LCA et TH

Indiquer ici les résultats obtenus.

...

## Qualité du générateur aléatoire

Indiquer les conclusions quant à la qualité du générateur aléatoire.

...

# Principales difficultés rencontrées

Indiquer ici les principales difficultés rencontrées lors de la réalisation de
ce projet et comment elles ont été surmontées ou contournéeS.

...

# Informations complémentaires

Indiquer ici les informations qui pourraient aider à la compréhension du
travail réalisé.

Cette partie peut être vide.

...

# Bilan personnel

Quel bilan personnel tirez-vous de ce mini-projet ?

...
