(* Ouverture d’une section *)
Section LogiquePredicats.

(* Définition de 2 domaines pour les prédicats *) 
Variable A B : Type.

(* Formule du second ordre : Quantification des prédicats P et Q *) 
Theorem Thm_8 : forall (P Q : A -> Prop), (* Quantification du second ordre : P et Q sont des prédicats *)
     (forall x1 : A, (P x1) /\ (Q x1))    (* Quantification du premier ordre : x1, x2 et x3 sont des données de type A *)
     -> 
     (forall x2 : A, (P x2)) /\ (forall x3 : A, (Q x3)).
intro HP.
intro HQ.
intro Big.
split.
apply Big.
apply Big.
Qed.

(* Formule du second ordre : Quantification des prédicats P et Q *) 
Theorem Thm_8b : forall (P Q : A -> Prop), (* Quantification du second ordre : P et Q sont des prédicats *)
     (forall x1 : A, (P x1) /\ (Q x1))    (* Quantification du premier ordre : x1, x2 et x3 sont des données de type A *)
     -> 
     (forall x2 : A, (P x2)) /\ (forall x3 : A, (Q x3)).
intro P.
intro Q.
intro Big.
split.
- (* Pt x2. P(x2) *) 
  intro x2.
  cut (P x2 /\ Q x2).
  + (* Px2 /\ Qx2 -> Px2 *)
    intro x3.
    destruct x3 as (Px2, Qx2).
    exact Px2.
  + (* Px2 /\ Qx2 *)
    generalize x2.
    exact Big.
- (* Pt x3. Q x3 *)
  intro x3.
  cut (P x3 /\ Q x3).
  + (* Px3 /\ Qx3 -> Px3 *)
    intro x4.
    destruct x4 as (Px3, Qx3).
    exact Qx3.
  + (* Px3 /\ Qx3 *)
    generalize x3.
    exact Big.
Qed.

(* Formule du second ordre : Quantification du prédicat P *) 
Theorem Thm_9 : forall (P : A -> (B -> Prop)), (* P est un prédicat de 2 variables de type A pour la première et B pour la seconde *)
     (exists x1 : A, forall y1 : B, (P x1 y1)) (* hypothèse : il existe une valeur de A notée x1, telle que P est vrai pour toute valeur y1 de A -- même x1 pour tous les y1 *)
     -> 
     forall y2:B, exists x2:A, (P x2 y2).      (* conclusion : pour toute valeur y2 de A, il existe une valeur de A notée x2 telle que P(x2 , y2) -- prenons x2 = x1 et donc y1 = y2 *) 
intro P.
intro Big.
intro y2.
generalize y2.
destruct Big as (x, Hx). (* Big c'est un genre de /\ : x témoin et Hx preuve que c'est témoin *)
exists x.
generalize y0.
exact Hx.
Qed.

(* Formule du second ordre : Quantification des prédicats P et Q *) 
Theorem Thm_10 : forall (P Q : A -> Prop),
     (exists x1 : A, (P x1) -> (Q x1)) (* hyp 1 : il existe une valeur de A notée x1, telle que si P est vraie pour cette valeur alors Q est vraie pour cette valeur *)
     -> 
     (forall x2 : A, (P x2))           (* hyp 2 : P est vraie pour toutes valeurs notée x2 de A *)
     -> 
     exists x3 : A, (Q x3).
intro P.
intro Q.
intro Big.
intro Pt_P.
destruct Big as (x, Hx).
exists x.
cut (P x).
exact Hx.
generalize x.
exact Pt_P.
Qed.
(* conclusion : il y a au moins une valeur de A notée x 3 pour laquelle Q est vraie : x1 car  P est vraie pour toutes les valeurs de A donc pour x1 en particulier *)

Variable H : Type.

Variable P : H -> H -> Prop.

Variable GPP : H -> H -> Prop.

Theorem T : (forall (e : H), exists (p : H), (P e p))    (* Tout enfant a un père *)
     /\                                                  (* H0et *)
     (forall (e : H), forall (gpp : H), ((GPP e gpp)     (* Le grand père paternel *)
          <-> exists (p : H), (P e p) /\ (P p gpp)))     (* est le père du père *)
     -> (forall (e : H), exists (gpp : H), (GPP e gpp)). (* alors tout enfant a un grand père paternel *)
intro.
destruct H0.
intro.
destruct H0 with e as (p, H2).
destruct H0 with p as (gpp, H3).
destruct H1 with e gpp as (_,H5). (* double implication, on garde la seconde*)
exists gpp.
cut (exists p : H, P e p /\ P p gpp).
exact H5.
exists p.
split.
exact H2.
exact H3.
Qed.


End LogiquePredicats.







