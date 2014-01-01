
(* BibliothÃ¨que pour produire du code MIPS

   2008 Jean-Christophe FilliÃ¢tre (CNRS)
   - version initiale

   2013 Kim Nguyen (UniversitÃ© Paris Sud)
   - sous-types text et data
   - types fantÃ´mes pour oreg et oi
   - plus d'opÃ©rations et de directives
   - manipulation de la pile
   - ocamldoc
*)

(** {0 BibliothÃ¨que pour l'Ã©criture de programmes MIPS } *)


(** Le module {!Mips} permet l'Ã©criture de code MIPS dans du code
    OCaml, sans utiliser un prÃ©processeur.  Un exemple complet est
    donnÃ© {{:#1_Exemple}ci-dessous, dans la section exemple}. *)

type 'a asm
(** type abstrait pour reprÃ©senter du code assembleur. Le paramÃ¨tre
    ['a] est utilisÃ© comme type fantÃ´me. *)

type text = [ `text ] asm
(** type reprÃ©sentant du code assembleur se trouvant dans la zone de
    texte *)

type data = [ `data ] asm
(** type reprÃ©sentant du code assembleur se trouvant dans la zone de
    donnÃ©es *)

type program = {
  text : text;
  data : data;
}
(** un programme est constituÃ© d'une zone de texte et d'une zone de
    donnÃ©e *)

val print_program : Format.formatter -> program -> unit
  (** [print_program fmt p] imprime le code du programme [p] dans le
      formatter [fmt] *)

val print_in_file: file:string -> program -> unit

type register
(** Type abstrait pour les registres *)

val v0 : register
val v1 : register
val a0 : register
val a1 : register
val a2 : register
val a3 : register
val t0 : register
val t1 : register
val t2 : register
val t3 : register
val s0 : register
val s1 : register
val ra : register
val sp : register
val fp : register
val gp : register
val zero : register
(** Constantes reprÃ©sentant les registres manipulables. [zero] est
    cablÃ© Ã  0 *)


type label = string
(** Les Ã©tiquettes d'addresses sont des chaines de caractÃ¨res *)

type 'a operand
val oreg : register operand
val oi : int operand
val oi32 : int32 operand

(** type abstrait pour reprÃ©senter la derniÃ¨re opÃ©rande d'une
    expression arithmÃ©tique ainsi que 3 constantes (soit un registre,
    soit un entier, soit un entier 32 bits)
*)



(** {1 OpÃ©rations arithmÃ©tiques } *)


val li : register -> int -> text
val li32 : register -> int32 -> text
(** Chargement des constantes entiÃ¨res *)

val abs : register -> register -> text
(** [abs r1 r2] stocke dans r1 la valeur absolue de r2 *)

val neg : register -> register -> text
(** [neg r1 r2] stocke dans r1 l'opposÃ© de r2 *)

val add : register -> register -> 'a operand -> 'a -> text
val sub : register -> register -> 'a operand -> 'a -> text
val mul : register -> register -> 'a operand -> 'a -> text
val rem : register -> register -> 'a operand -> 'a -> text
val div : register -> register -> 'a operand -> 'a -> text

(** Les 5 opÃ©rations arithmÃ©tique de base: [add rdst rsrc1 ospec o]
   stocke dans rdst le rÃ©sultat de l'opÃ©ration entre rsrc1 et o. La
   constant ospec spÃ©cifie si o est un immÃ©diat, immÃ©diat sur 32 bits
   ou un registre.
   Exemple:

   [add v0 v1 oreg v2]

   [div v0 v1 oi 424]

   [sub t0 a0 oi32 2147483647l]
 *)

(** {1 OpÃ©rations logiques } *)

val and_ : register -> register -> register -> text
val or_ : register -> register -> register -> text
val not_ : register -> register -> text
val clz : register -> register -> text
(** OpÃ©rations de manipulation de bits. "et" bit Ã  bit, "ou" bit Ã 
    bit, "not" bit Ã  bit et clz (count leading zero) *)


(** {1 Comparaisons } *)

val seq : register -> register -> register -> text
val sge : register -> register -> register -> text
val sgt : register -> register -> register -> text
val sle : register -> register -> register -> text
val slt : register -> register -> register -> text
val sne : register -> register -> register -> text
  (** conditionnelles [sop ra rb rc] met [ra] Ã  1 si [rb op rc] et Ã  0
      dans le cas contraire (eq : ==, ge : >=, gt : >, le : <=, lt : <=,
      ne : !=) *)

(** {1 Sauts } *)

val b : label -> text
(** saut inconditionnel *)

val beq : register -> register -> label -> text
val bne : register -> register -> label -> text
val bge : register -> register -> label -> text
val bgt : register -> register -> label -> text
val ble : register -> register -> label -> text
val blt : register -> register -> label -> text
(** [bop ra rb label] branche vers le label [label] si [ra op rb] *)

val beqz : register -> label -> text
val bnez : register -> label -> text
val bgez : register -> label -> text
val bgtz : register -> label -> text
val blez : register ->  label -> text
val bltz : register ->  label -> text
(** [bopz ra rb label] branche vers le label [label] si [ra op 0] *)


val jr : register -> text
(** [jr r] Continue l'exÃ©cution Ã  l'adresse spÃ©cifiÃ©e dans le registre
    [r] *)
val jal : label -> text
(** [jal l] Continue l'exÃ©cution Ã  l'adresse spÃ©cifiÃ©e par le label [l],
    sauve l'adresse de retour dans $ra.
*)
val jalr : register -> text
(** [jalr r] Continue l'exÃ©cution Ã  l'adresse spÃ©cifiÃ©e par le
    registre [r], sauve l'adresse de retour dans $ra.
*)

(** {1 Lecture / Ã©criture en mÃ©moire } *)

type 'a address
(** type abstrait pour reprÃ©senter des adresses *)

val alab : label address
val areg : (int * register) address
(** Les adresses sont soit donnÃ©es par un label, soit par une paire
    dÃ©calage, registre *)

val la : register -> 'a address -> 'a -> text
(** [la reg alab "foo"] charge dans [reg] l'adresse du label "foo"
    [la reg1 areg (x, reg2)] charge dans [reg1] l'adresse contenue dans
    [reg2] dÃ©callÃ©e de [x] octets
 *)

val lbu : register -> 'a address -> 'a -> text
(** charge l'octet Ã  l'adresse donnÃ©e sans extension de signe (valeur
    entre 0 et 255) *)
val lw : register -> 'a address -> 'a -> text
(** charge l'entier 32bits Ã  l'adresse donnÃ©e *)
val sb : register -> 'a address -> 'a -> text
(** Ã©crit les 8 bits de poid faible du registre donnÃ©e Ã  l'adresse
    donnÃ©e *)
val sw : register -> 'a address -> 'a -> text
(** Ã©crit le contenu du registre Ã  l'adresse donnÃ©e *)
val move : register -> register -> text

(** {1 Divers } *)

val nop : [> ] asm
(** l'instruction vide. Peut se trouver dans du text ou du data *)

val label : label ->  [> ] asm
(** un label. Peut se retrouver dans du text ou du data *)

val syscall : text
(** l'instruction syscall *)

val comment : string -> [> ] asm
(** place un commentaire dans le code gÃ©nÃ©rÃ©. Peut se retrouver dans
    du text ou du data *)

val align : int ->  [> ] asm
(** [align n] aligne le code suivant l'instruction sur 2^n octets *)

val asciiz : string -> data
(** place une constante chaÃ®ne de carctÃ¨res (terminÃ©es par 0) dans a
    zone data *)

val dword : int list -> data
(** place une liste de mots mÃ©moires dans la zone data *)

val address : label list -> data
(** place une liste d'adresses (dÃ©notÃ©es par des labels) dans la zone
    data *)

val ( ++ ) : ([< `text|`data ] asm as 'a)-> 'a -> 'a
(** concatÃ¨ne deux bouts de codes (soit text avec text, soit data avec
    data) *)

(** {1 Manipulation de la pile} *)

val push : register -> text
(** [push r] place le contenu de [r] au sommet de la pile.
    Rappel : $sp pointe sur l'adresse de la derniÃ¨re case occupÃ©e *)

val pop : register -> text
(** [pop r] place le mot en sommet de pile dans [r] et dÃ©pile *)

val popn: int -> text
(** [popn n] dÃ©pile [n] octets *)

val peek : register -> text
(** [peek r] place le mot en sommet de pile dans [r] sans dÃ©piler *)

(** {1 Exemple } *)

(** Le programme ci-dessous, donnÃ© Ã  gauche en pur MIPS et Ã  droite en
    OCaml, charge deux constantes, effectue quelques opÃ©rations
    arithÃ©tiques et affiche le rÃ©sultat Ã  l'Ã©cran

    {[
        .text                                                |  { text =
        main:                                                |        label "main"
         #charge 42 dans $a0 et 23 dans $a1                  |    ++  comment "charge 42 dans $a0 et 23 dans $a1"
         li $a0,  42                                         |    ++  li  a0 42
         li $a1,  23                                         |    ++  li  a1 23
         mul $a0, $a0, $a1                                   |    ++  mul a0 a0 oreg a1 (* on utilise oreg pour dire que la derniÃ¨re
                                                             |                             operande est un registre *)
         #place le contenu de $a0 sur la pile                |    ++  comment "place le contenu de $a0 sur la pile"
         sub $sp, $sp, 4                                     |    ++  sub sp sp oi 4
         sw  $a0,  0($sp)                                    |    ++  sw a0 areg (0, sp)
                                                             |
         #appelle une routine d'affichage                    |    ++  comment "appelle la routine d'affichage"
         jal print_int                                       |    ++  jal "print_int"
                                                             |
         #termine                                            |    ++  comment "termine"
         li $v0, 10                                          |    ++  li v0 10
         syscall                                             |    ++  syscall
                                                             |
      print_int:                                             |    ++  label "print_int"
         lw $a0,  0($sp)                                     |    ++  lw a0 areg (0, sp)
         add $sp, $sp, 4                                     |    ++  add sp sp oi 4
         li $v0, 1                                           |    ++  li v0 1
         syscall                                             |    ++  syscall
         #affiche un retour chariot                          |    ++  comment "affiche un retour chariot"
         la $a0, newline                                     |    ++  la a0 alab "newline"
         li $v0, 4                                           |    ++  li v0  4
         syscall                                             |    ++  syscall
         jr $ra                                              |    ++  jr ra  ; (* fin du label text *)
                                                             |
        .data                                                |    data =
       newline:                                              |        label "newline"
        .asciiz  "\n"                                        |    ++  asciiz "\n" ;
                                                             |  } (* fin du record *)
    ]}
*)

