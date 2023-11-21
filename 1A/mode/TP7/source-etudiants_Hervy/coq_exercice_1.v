Require Import Naturelle.
Section Session1_2019_Logique_Exercice_1.

Variable A B C : Prop.

Theorem Exercice_1_Naturelle :  (A -> B -> C) -> ((A /\ B) -> C).
Proof.
I_imp H_Impl.
I_imp H_Et.
E_imp B.
E_imp A.
Hyp H_Impl.
E_et_g B.
Hyp H_Et.
E_et_d A.
Hyp H_Et.
Qed.

Theorem Exercice_1_Coq :  (A -> B -> C) -> ((A /\ B) -> C).
Proof.
intro H_Impl.
intro H_Et.
destruct H_Et as (H_A,H_B).
cut B.
cut A.
exact H_Impl.
exact H_A.
exact H_B.
Qed.

End Session1_2019_Logique_Exercice_1.