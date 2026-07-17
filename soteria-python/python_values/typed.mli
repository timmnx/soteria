(** {2 Phantom types} *)

module T : sig
  (** A symbolic integer known to be non-zero. *)
  type nonzero = [ `NonZero ]

  (** A symbolic integer known to be zero. *)
  type zero = [ `Zero ]

  (** A symbolic integer; can either be [`NonZero] if it is known to not be 0,
      [`Zero] if it is 0. *)
  type sint = [ `Nonzero | `Zero ]

  (** A symbolic boolean. *)
  type sbool = [ `Bool ]

  (** A symbolic float. *)
  type sfloat = [ `Float ]

  type snumeric = [ `Nonzero | `Zero | `Float ]
  type sstr = [ `Str ]
  type sothers = [ `Others ]
  type snot_yet_implemented = [ `NotYetImplemented ]

  type any =
    [ `Bool | `Nonzero | `Zero | `Float | `Str | `Others | `NotYetImplemented ]

  val pp_sint : Format.formatter -> sint -> unit
  val pp_nonzero : Format.formatter -> nonzero -> unit
  val pp_zero : Format.formatter -> zero -> unit
  val pp_sbool : Format.formatter -> sbool -> unit
  val pp_any : Format.formatter -> any -> unit
end

open T

(** {2 Types} *)
type +'a ty

val pp_ty :
  (Format.formatter -> 'a ty -> unit) -> Format.formatter -> 'a ty -> unit

val ppa_ty : Format.formatter -> 'a ty -> unit
val equal_ty : 'a ty -> 'b ty -> bool
val t_bool : [> sbool ] ty
val t_int : [> sint ] ty
val t_float : [> sfloat ] ty
val t_str : [> sstr ] ty
val t_others : [> sothers ] ty
val t_not_yet_implemented : [> snot_yet_implemented ] ty

(** {2 Typed svalues} *)

type +'a t
type sbool = T.sbool

val is_bool_ty : 'a ty -> bool

(** Basic value operations *)

val get_ty : 'a t -> Svalue.ty
val type_type : Svalue.ty -> 'a ty
val untype_type : 'a ty -> Svalue.ty
val kind : 'a t -> Svalue.t_kind
val mk_var : Svalue.Var.t -> 'a ty -> 'a t
val iter_vars : 'a t -> (Svalue.Var.t * 'b ty -> unit) -> unit
val type_ : Svalue.t -> 'a t
val type_checked : Svalue.t -> 'a ty -> 'a t option
val cast : 'a t -> 'b t
val cast_checked : 'a t -> 'b ty -> 'b t option
val cast_checked2 : 'a t -> 'b t -> ('c t * 'c t * 'c ty) option
val cast_float : 'a t -> [> sfloat ] t option
val cast_int : 'a t -> [> sint ] t option
val is_float : 'a ty -> bool
val untyped : 'a t -> Svalue.t
val untyped_list : 'a t list -> Svalue.t list
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val ppa : Format.formatter -> 'a t -> unit
val equal : ([< any ] as 'a) t -> 'a t -> bool
val compare : ([< any ] as 'a) t -> 'a t -> int
val hash : [< any ] t -> int
val hasha : 'a t -> int
val unique_tag : [< any ] t -> int

(** Typed constructors *)

val sem_eq : 'a t -> 'a t -> sbool t
val sem_eq_untyped : 'a t -> 'b t -> sbool t

(** Boolean operations *)

module SBool : sig
  val v_true : [> sbool ] t
  val v_false : [> sbool ] t
  val of_bool : bool -> [> sbool ] t
  val to_bool : 'a t -> bool option
  val and_ : [< sbool ] t -> [< sbool ] t -> [> sbool ] t

  (** Similar to [and_], but the rhs is only evaluated if the lhs is not the
      concrete false. In other words, this is a short-circuiting and. Avoids
      some errors, like a division by zero in [0 != x && n / x] when [x] is [0].
  *)
  val and_lazy : [< sbool ] t -> (unit -> [< sbool ] t) -> [> sbool ] t

  val conj : [< sbool ] t list -> [> sbool ] t
  val split_ands : [< sbool ] t -> ([> sbool ] t -> unit) -> unit
  val or_ : [< sbool ] t -> [< sbool ] t -> [> sbool ] t

  (** Similar to [or_], but the rhs is only evaluated if the lhs is not the
      concrete true. In other words, this is a short-circuiting or. Avoids some
      errors, like a division by zero in [0 == x || n / x] when [x] is [0]. *)
  val or_lazy : [< sbool ] t -> (unit -> [< sbool ] t) -> [> sbool ] t

  val not_ : [< sbool ] t -> [> sbool ] t
  val distinct : 'a t list -> [> sbool ] t
  val distinct_seq : 'a t Seq.t -> [> sbool ] t
  val ite : [< sbool ] t -> 'a t -> 'a t -> 'a t
end

(** Integer operations *)
module SNumeric : sig
  (* constructor *)
  val int_z : Z.t -> [> sint ] t
  val int_float : float -> [> sint ] t
  val int : int -> [> sint ] t
  val nonzero_z : Z.t -> [> nonzero ] t
  val nonzero : int -> [> nonzero ] t
  val zero : [> zero ] t
  val one : [> nonzero ] t
  val float : float -> [> sfloat ] t

  (* arithmetic *)
  val add : [< sint ] t -> [< sint ] t -> [> sint ] t
  val sub : [< sint ] t -> [< sint ] t -> [> sint ] t
  val mul : [< sint ] t -> [< sint ] t -> [> sint ] t
  val floor_div : [< sint ] t -> [< sint ] t -> [> sint ] t
  val mod_ : [< sint ] t -> [< sint ] t -> [> sint ] t
  val pow : [< sint ] t -> [< sint ] t -> [> sint ] t
  val neg : [< sint ] t -> [> sint ] t
  val invert : [< sint ] t -> [> sint ] t

  (* inequalities *)
  val lt : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val le : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val sem_eq : [< sint ] t -> [< sint ] t -> [> sbool ] t
  val of_bool : [< sbool ] t -> [> sint ] t
  val to_bool : [< sint ] t -> [> sbool ] t
  val of_float : [< sfloat ] t -> [> sint ] t
  val to_float : [< sint ] t -> [> sfloat ] t
end

module SStr : sig
  val str : string -> [> sstr ] t
  val get_str : [< any ] t -> string option
end

module SOthers : sig
  val none_ : [> sothers ] t
  val null : [> sothers ] t
  val mk_ref : int -> [> sothers ] t
  val mk_builtin : string -> [> sothers ] t
  val not_implemented : [> sothers ] t
  val ellipsis : [> sothers ] t
  val null : [> sothers ] t
  val is_none_ : [< any ] t -> bool
  val is_null : [< any ] t -> bool
  val get_ref : [< any ] t -> int option
  val get_builtin : [< any ] t -> string option
end

val to_bool : 'a t -> bool option
val of_bool : bool -> sbool t

(** All operations *)
val neg : [< any ] t -> [> any ] t
val not_ : [< any ] t -> [> sbool ] t
val invert : [< any ] t -> [> any ] t
val to_bool_ : [< any ] t -> [> sbool ] t

val not : [< any ] t -> [> sbool ] t

(* val to_bool : [< any ] t -> [> sbool ] t *)
val add : [< any ] t -> [< any ] t -> [> snumeric ] t
val sub : [< any ] t -> [< any ] t -> [> snumeric ] t
val mul : [< any ] t -> [< any ] t -> [> snumeric ] t
val div : [< any ] t -> [< any ] t -> [> sfloat ] t
val floor_div : [< any ] t -> [< any ] t -> [> snumeric ] t
val mod_ : [< any ] t -> [< any ] t -> [> snumeric ] t
val pow : [< any ] t -> [< any ] t -> [> snumeric ] t
val mat_mul : [< any ] t -> [< any ] t -> [> any ] t
val and_ : [< any ] t -> [< any ] t -> [> sbool ] t
val or_ : [< any ] t -> [< any ] t -> [> sbool ] t
val bit_and : [< any ] t -> [< any ] t -> [> any ] t
val bit_or : [< any ] t -> [< any ] t -> [> any ] t
val bit_xor : [< any ] t -> [< any ] t -> [> any ] t
val bit_lshift : [< any ] t -> [< any ] t -> [> any ] t
val bit_rshift : [< any ] t -> [< any ] t -> [> any ] t
val sem_eq : [< any ] t -> [< any ] t -> [> any ] t
val sem_ne : [< any ] t -> [< any ] t -> [> any ] t
val lt : [< any ] t -> [< any ] t -> [> sbool ] t
val leq : [< any ] t -> [< any ] t -> [> sbool ] t
val gt : [< any ] t -> [< any ] t -> [> sbool ] t
val geq : [< any ] t -> [< any ] t -> [> sbool ] t

module Infix : sig
  (* val int_z : Z.t -> t val int : int -> t *)
  val ( +@ ) : [< any ] t -> [< any ] t -> [> snumeric ] t
  val ( -@ ) : [< any ] t -> [< any ] t -> [> snumeric ] t
  val ( ~- ) : [< any ] t -> [> snumeric ] t
  val ( ~! ) : [< any ] t -> [> sbool ] t
  val ( ~~ ) : [< any ] t -> [> snumeric ] t
  val ( *@ ) : [< any ] t -> [< any ] t -> [> snumeric ] t
  val ( /@ ) : [< any ] t -> [< any ] t -> [> sfloat ] t
  val ( //@ ) : [< any ] t -> [< any ] t -> [> snumeric ] t
  val ( %@ ) : [< any ] t -> [< any ] t -> [> snumeric ] t
  val ( **@ ) : [< any ] t -> [< any ] t -> [> snumeric ] t
  val ( @@ ) : [< any ] t -> [< any ] t -> [> any ] t
  val ( &&@ ) : [< any ] t -> [< any ] t -> [> sbool ] t
  val ( ||@ ) : [< any ] t -> [< any ] t -> [> sbool ] t
  val ( &@ ) : [< any ] t -> [< any ] t -> [> any ] t
  val ( |@ ) : [< any ] t -> [< any ] t -> [> any ] t
  val ( ^@ ) : [< any ] t -> [< any ] t -> [> any ] t
  val ( >>@ ) : [< any ] t -> [< any ] t -> [> any ] t
  val ( <<@ ) : [< any ] t -> [< any ] t -> [> any ] t
  val ( ==@ ) : [< any ] t -> [< any ] t -> [> sbool ] t
  val ( !=@ ) : [< any ] t -> [< any ] t -> [> sbool ] t
  val ( >@ ) : [< any ] t -> [< any ] t -> [> sbool ] t
  val ( >=@ ) : [< any ] t -> [< any ] t -> [> sbool ] t
  val ( <@ ) : [< any ] t -> [< any ] t -> [> sbool ] t
  val ( <=@ ) : [< any ] t -> [< any ] t -> [> sbool ] t
  (* val ( ==?@ ) : 'a t -> 'b t -> [> sbool ] t *)
end

module Syntax : sig
  module Sym_int_syntax : sig
    val mk_nonzero : int -> [> nonzero ] t
    val zero : unit -> [> sint ] t
    val one : unit -> [> sint ] t
  end
end

val are_addable : 'a t -> 'b t -> ('a t * 'b t) option

module Expr :
  Soteria.Symex.Value.Expr
    with type 'a v := 'a t
     and type 'a ty := 'a ty
     and type t = Svalue.t
