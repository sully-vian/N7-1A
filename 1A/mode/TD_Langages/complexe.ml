type m = Moins
       | Vide
;;

type nz = Un
            | Deux
            | Trois
            | Quatre
            | Cinq
            | Six
            | Sept
            | Huit
            | Neuf
;;

type n = Zero * n
       | nz * n
       | Vide
;;

type c = Zero
       | m * nz * n * i
       | m * nz * n * s * nz * n * "i"
;;
