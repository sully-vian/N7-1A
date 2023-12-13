Require Import Naturelle.
Section Session1_2023_Logique_Exercice_1.

Variable A B : Prop.

Theorem Exercice_1_Naturelle :  ((A \/ B) /\ (~A)) -> (B /\ (~A)).
Proof.
I_imp H.
I_et.
E_ou (A) (B).
E_et_g (~A).
Hyp H.
I_imp HA.
E_antiT.
I_antiT (A).
Hyp HA.
E_et_d (A \/ B).
Hyp H.
I_imp HB.
Hyp HB.
E_et_d (A \/ B).
Hyp H.
Qed.

Theorem Exercice_1_Coq : ((A \/ B) /\ (~A)) -> (B /\ (~A)).
Proof.
intro H.
destruct  H as (H1, H2).
split.
destruct H1 as [HA|HB].
absurd A.
exact H2.
exact HA.
exact HB.
exact H2.
Qed.



End Session1_2023_Logique_Exercice_1.

