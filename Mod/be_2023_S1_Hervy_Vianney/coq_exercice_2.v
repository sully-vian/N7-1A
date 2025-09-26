Require Import Naturelle.
Section Session1_2023_Logique_Exercice_2.

Variable A B : Prop.

Theorem Exercice_2_Naturelle : ((A -> B) -> A) -> A.
Proof.
I_imp H.
E_ou (A) (~A).
TE.
I_imp HA.
Hyp HA.
I_imp HNA.
E_imp (A -> B).
Hyp H.
I_imp HA.
E_antiT.
I_antiT A.
Hyp HA.
Hyp HNA.
Qed.

Theorem Exercice_2_Coq : ((A -> B) -> A) -> A.
Proof.
intro H.
cut (A \/ ~A).
intro H1.
elim (A ->B).

cut (A -> B).
exact H.
intro HA.

Admitted.

End Session1_2023_Logique_Exercice_2.

