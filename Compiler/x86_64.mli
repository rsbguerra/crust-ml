
(** {0 Biblioteca para produzir código Assembly X86-64}

    Trata-se aqui somente dum fragmente relativamente modesto do assembly X86-64.

    @author Jean-Christophe Filliâtre (CNRS)
    @author Kim Nguyen (Université Paris Sud)



    [Comments translation to PT - Simão Melo de Sousa]

*)

(** {1 Codígo } *)

type 'a asm
  (** tipo abstracto para representar código assembly. O parâmetro
      ['a] é acrescentado como um tipo fantasma. *)

type text = [ `text ] asm
  (** tipo representando código assembly que se encontra na zona de texto *)

type data = [ `data ] asm
  (** tipo representando código assembly que se encontra na zona de dados *)

type label = string
  (** as etiquetas/rótulos (labels) de endereços são strings *)

val nop : [> ] asm
  (** a instrução vazia. Pode encontrar-se no texto ou nos dados *)

val ( ++ ) : ([< `text|`data ] asm as 'a)-> 'a -> 'a
  (** junta dois pedaços de código (text com text, data com data) *)

val inline: string -> [> ] asm
  (** [inline s] copia a string [s] tal como está no ficheiro assembly *)

type program = {
  text : text;
  data : data;
}
  (** um programa é constituído de uma zona de texto e de uam zona de dados *)

val print_program : Format.formatter -> program -> unit
   (** [print_program fmt p] imprime o código do programa [p] no formatter [fmt] *)

val print_in_file: file:string -> program -> unit

(** {1 Registros } *)

type size = [`B | `W | `L | `Q]

type 'size register
  (** Tipo abstracto para os registos *)

val rax: [`Q] register
val rbx: [`Q] register
val rcx: [`Q] register
val rdx: [`Q] register
val rsi: [`Q] register
val rdi: [`Q] register
val rbp: [`Q] register
val rsp: [`Q] register
val r8 : [`Q] register
val r9 : [`Q] register
val r10: [`Q] register
val r11: [`Q] register
val r12: [`Q] register
val r13: [`Q] register
val r14: [`Q] register
val r15: [`Q] register
  (** registros 64 bits *)

val eax: [`L] register
val ebx: [`L] register
val ecx: [`L] register
val edx: [`L] register
val esi: [`L] register
val edi: [`L] register
val ebp: [`L] register
val esp: [`L] register
val r8d: [`L] register
val r9d: [`L] register
val r10d: [`L] register
val r11d: [`L] register
val r12d: [`L] register
val r13d: [`L] register
val r14d: [`L] register
val r15d: [`L] register
  (** registros 32 bits *)

val ax: [`W] register
val bx: [`W] register
val cx: [`W] register
val dx: [`W] register
val si: [`W] register
val di: [`W] register
val bp: [`W] register
val sp: [`W] register
val r8w: [`W] register
val r9w: [`W] register
val r10w: [`W] register
val r11w: [`W] register
val r12w: [`W] register
val r13w: [`W] register
val r14w: [`W] register
val r15w: [`W] register
  (** registros 16 bits *)

val al: [`B] register
val bl: [`B] register
val cl: [`B] register
val dl: [`B] register
val ah: [`B] register
val bh: [`B] register
val ch: [`B] register
val dh: [`B] register
val sil: [`B] register
val dil: [`B] register
val bpl: [`B] register
val spl: [`B] register
val r8b: [`B] register
val r9b: [`B] register
val r10b: [`B] register
val r11b: [`B] register
val r12b: [`B] register
val r13b: [`B] register
val r14b: [`B] register
val r15b: [`B] register
  (** registros 8 bits *)

(** {1 Operandas } *)

type 'size operand
  (** O tipo abstracto das operandas *)

val imm: int -> [>] operand
  (* operanda directa $i *)
val imm32: int32 -> [>] operand
  (* operande directa $i *)
val imm64: int64 -> [>] operand
  (* operanda directa $i *)
val reg: 'size register -> 'size operand
val (!%): 'size register -> 'size operand
  (* registro *)
val ind: ?ofs:int -> ?index:'size1 register -> ?scale:int ->
  'size2 register -> [>] operand
  (* operanda indirecta ofs(register, index, scale) *)
val lab: label -> [>] operand
  (* etiqueta L  *)
val ilab: label -> [`Q] operand
  (* etiqueta directa $L *)

(** {1 Instruções } *)

(** {2 Transferência } *)

val movb: [`B] operand -> [`B] operand -> text
val movw: [`W] operand -> [`W] operand -> text
val movl: [`L] operand -> [`L] operand -> text
val movq: [`Q] operand -> [`Q] operand -> text
  (* Cuidado : nem todas as combinações de operandas são permitidas *)
val movsbw: [`B] operand -> [`W] register -> text
val movsbl: [`B] operand -> [`L] register -> text
val movsbq: [`B] operand -> [`Q] register -> text
val movswl: [`W] operand -> [`L] register -> text
val movswq: [`W] operand -> [`Q] register -> text
val movslq: [`L] operand -> [`Q] register -> text
  (* 8->64 bit, com extensão de sinal *)
val movzbw: [`B] operand -> [`W] register -> text
val movzbl: [`B] operand -> [`L] register -> text
val movzbq: [`B] operand -> [`Q] register -> text
val movzwl: [`W] operand -> [`L] register -> text
val movzwq: [`W] operand -> [`Q] register -> text
  (* 8->64 bit, com extensão por zero *)

val movabsq: [`Q] operand -> [`Q] register -> text
  (** copia um valor directo de 64 bits num registro *)

(** {2 Arithmética } *)

val leab: [`B] operand -> [`B] register -> text
val leaw: [`W] operand -> [`W] register -> text
val leal: [`L] operand -> [`L] register -> text
val leaq: [`Q] operand -> [`Q] register -> text

val incb: [`B] operand -> text
val incw: [`W] operand -> text
val incl: [`L] operand -> text
val incq: [`Q] operand -> text

val decb: [`B] operand -> text
val decw: [`W] operand -> text
val decl: [`L] operand -> text
val decq: [`Q] operand -> text

val negb: [`B] operand -> text
val negw: [`W] operand -> text
val negl: [`L] operand -> text
val negq: [`Q] operand -> text

val addb: [`B] operand -> [`B] operand -> text
val addw: [`W] operand -> [`W] operand -> text
val addl: [`L] operand -> [`L] operand -> text
val addq: [`Q] operand -> [`Q] operand -> text

val subb: [`B] operand -> [`B] operand -> text
val subw: [`W] operand -> [`W] operand -> text
val subl: [`L] operand -> [`L] operand -> text
val subq: [`Q] operand -> [`Q] operand -> text

val imulw: [`W] operand -> [`W] operand -> text
val imull: [`L] operand -> [`L] operand -> text
val imulq: [`Q] operand -> [`Q] operand -> text

val idivq: [`Q] operand -> text
val cqto: text

(** {2 Operações lógicas } *)

val notb: [`B] operand -> text
val notw: [`W] operand -> text
val notl: [`L] operand -> text
val notq: [`Q] operand -> text

val andb: [`B] operand -> [`B] operand -> text
val andw: [`W] operand -> [`W] operand -> text
val andl: [`L] operand -> [`L] operand -> text
val andq: [`Q] operand -> [`Q] operand -> text

val orb : [`B] operand -> [`B] operand -> text
val orw : [`W] operand -> [`W] operand -> text
val orl : [`L] operand -> [`L] operand -> text
val orq : [`Q] operand -> [`Q] operand -> text

val xorb: [`B] operand -> [`B] operand -> text
val xorw: [`W] operand -> [`W] operand -> text
val xorl: [`L] operand -> [`L] operand -> text
val xorq: [`Q] operand -> [`Q] operand -> text
  (** Operações de processamento de bits. "e" bitwise, "ou" bitwise, "not" bitwise *)

(** {2 Offsets } *)

val shlb: [`B] operand -> [`B] operand -> text
val shlw: [`W] operand -> [`W] operand -> text
val shll: [`L] operand -> [`L] operand -> text
val shlq: [`Q] operand -> [`Q] operand -> text
  (** nota: shl é idêntico a  sal *)

val shrb: [`B] operand -> [`B] operand -> text
val shrw: [`W] operand -> [`W] operand -> text
val shrl: [`L] operand -> [`L] operand -> text
val shrq: [`Q] operand -> [`Q] operand -> text

val sarb: [`B] operand -> [`B] operand -> text
val sarw: [`W] operand -> [`W] operand -> text
val sarl: [`L] operand -> [`L] operand -> text
val sarq: [`Q] operand -> [`Q] operand -> text

(** {2 Saltos } *)

val call: label -> text
val call_star: [`Q] operand -> text
val leave: text
val ret: text
  (** chamada de função e retorno *)

val jmp : label -> text
  (* salto incondicional *)
val jmp_star: [`Q] operand -> text
  (** salto para um endereço calculado *)

val je : label -> text  (* =  0 *)
val jz : label -> text  (* =  0 *)
val jne: label -> text  (* <> 0 *)
val jnz: label -> text  (* <> 0 *)
val js : label -> text  (* <  0 *)
val jns: label -> text  (* >= 0 *)
val jg : label -> text  (* >   com sinal *)
val jge: label -> text  (* >=  com sinal *)
val jl : label -> text  (* <   com sinal *)
val jle: label -> text  (* <=  com sinaç *)
val ja : label -> text  (* >   sem sinal *)
val jae: label -> text  (* >=  sem sinal *)
val jb : label -> text  (* <   sem sinal *)
val jbe: label -> text  (* <=  sem sinal *)
  (** saltos condicionais *)

(** {2 Condições } *)

val cmpb: [`B] operand -> [`B] operand -> text
val cmpw: [`W] operand -> [`W] operand -> text
val cmpl: [`L] operand -> [`L] operand -> text
val cmpq: [`Q] operand -> [`Q] operand -> text

val testb: [`B] operand -> [`B] operand -> text
val testw: [`W] operand -> [`W] operand -> text
val testl: [`L] operand -> [`L] operand -> text
val testq: [`Q] operand -> [`Q] operand -> text

val sete : [`B] operand -> text  (* =  0 *)
val setne: [`B] operand -> text  (* <> 0 *)
val sets : [`B] operand -> text  (* <  0 *)
val setns: [`B] operand -> text  (* >= 0 *)
val setg : [`B] operand -> text  (* >  com sinal *)
val setge: [`B] operand -> text  (* >= com sinal *)
val setl : [`B] operand -> text  (* <  com sinal *)
val setle: [`B] operand -> text  (* <= com sinal *)
val seta : [`B] operand -> text  (* >  sem sinal *)
val setae: [`B] operand -> text  (* >= sem sinal *)
val setb : [`B] operand -> text  (* <  sem sinal *)
val setbe: [`B] operand -> text  (* <= sem sinal *)
  (** posiciona o byte operanda em 1 ou 0 conforme o teste der verdade ou não *)

(** {2 Gestão da pilha} *)

val pushq : [`Q] operand -> text
  (** [pushq r] coloca o conteúdo de [r] no topo da pilha.
      Lembrete : %rsp aponta para o endereço da última célula ocupada *)

val popq : [`Q] register -> text
  (** [popq r] coloca a palavra presente no topo da pilha em [r] e desempilha *)

(** {2 Diversos } *)

val label : label -> [> ] asm
  (* uma etiqueta. Pode ser encontrada em text ou em data *)
val globl : label -> [> ] asm
  (** declaração .globl (para main, tipicamente) *)

val comment : string -> [> ] asm
  (** coloca um comentário no código gerado. Pode ser encontrada em text ou em data *)

(** {2 Dados } *)

val string : string -> data
  (* uma constante string  (que termina com um 0) *)
val dbyte  : int list -> data
val dword  : int list -> data
val dint   : int list -> data
val dquad  : int list -> data
  (* coloca uma lista de valores em  1/2/4/8 bytes na zona data *)
val address: label list -> data
  (* coloca uma lista de endereços na zona data (com .quad) *)
val space  : int -> data
  (* [space n] aloca [n] bytes (com valor 0) no segmento dos dados *)
