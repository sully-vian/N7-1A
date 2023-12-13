Require Import Naturelle.
Section Session1_2019_Logique_Exercice_2.

Variable A B : Prop.

Theorem Exercice_2_Coq : (~A) \/ B -> (~A) \/ (A /\ B).
Proof.
intro AouB.
elim AouB.
intro NonA.
left.
exact NonA.
intro HB.
cut (A \/ ~A).
intro AouNonA.
destruct AouNonA as [HA | NonA].
right.
split.
exact HA.
exact HB.
left.
exact NonA.
apply (classic A).
Qed.

Theorem Exercice_2_Naturelle : (~A) \/ B -> (~A) \/ (A /\ B).
Proof.
I_imp NonAOuB.
E_ou A (~A).


Qed.

Theorem Exercice_2_Coq_bis : (~A) \/ B -> (~A) \/ (A /\ B).
Proof.
intro AouB.
destruct AouB as [NonA | HB].
left.
exact NonA.
cut (A /\ ~A).
intro AEtNonA.
destruct AEtNonA as (HA,NonA).
left.
exact NonA.
pass.
Qed.

End Session1_2019_Logique_Exercice_2.

