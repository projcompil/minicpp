exception Error

type token = 
  | WHILE
  | VOID
  | VIRTUAL
  | TRUE
  | TIMES
  | TIDENT of (
# 12 "parser.mly"
       (string)
# 13 "parser.ml"
)
  | THIS
  | STDCOUT
  | SEMICOLON
  | SDEREF
  | RPAR
  | RETURN
  | RACC
  | PUBLIC
  | PLUS
  | OR
  | NULL
  | NOT
  | NEW
  | NEQ
  | MODULO
  | MINUS
  | LT
  | LPAR
  | LE
  | LAND
  | LACC
  | INTEGER of (
# 10 "parser.mly"
       (int)
# 39 "parser.ml"
)
  | INT
  | INCR
  | INCLUDEIOS
  | IF
  | IDENT of (
# 11 "parser.mly"
       (string)
# 48 "parser.ml"
)
  | GT
  | GE
  | FOR
  | FALSE
  | EQ
  | EOF
  | ELSE
  | DOT
  | DIV
  | DECR
  | COMMA
  | COLON
  | CLASS
  | CHEVRON
  | CHAINE of (
# 13 "parser.mly"
       (string)
# 67 "parser.ml"
)
  | ASSIGN
  | AND

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState205
  | MenhirState200
  | MenhirState199
  | MenhirState198
  | MenhirState197
  | MenhirState196
  | MenhirState189
  | MenhirState187
  | MenhirState183
  | MenhirState176
  | MenhirState171
  | MenhirState168
  | MenhirState166
  | MenhirState155
  | MenhirState153
  | MenhirState150
  | MenhirState149
  | MenhirState147
  | MenhirState146
  | MenhirState144
  | MenhirState142
  | MenhirState140
  | MenhirState135
  | MenhirState130
  | MenhirState126
  | MenhirState125
  | MenhirState123
  | MenhirState110
  | MenhirState108
  | MenhirState106
  | MenhirState104
  | MenhirState102
  | MenhirState100
  | MenhirState98
  | MenhirState96
  | MenhirState94
  | MenhirState92
  | MenhirState90
  | MenhirState88
  | MenhirState86
  | MenhirState84
  | MenhirState78
  | MenhirState73
  | MenhirState68
  | MenhirState65
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState57
  | MenhirState55
  | MenhirState53
  | MenhirState51
  | MenhirState49
  | MenhirState48
  | MenhirState45
  | MenhirState40
  | MenhirState34
  | MenhirState29
  | MenhirState28
  | MenhirState25
  | MenhirState22
  | MenhirState17
  | MenhirState8
  | MenhirState7
  | MenhirState6
  | MenhirState3
  | MenhirState0


# 4 "parser.mly"
  
  open Ast
 (* open Lexer*)
  (*open Lexerhack*)

# 158 "parser.ml"
let _eRR =
  Error

let rec _menhir_goto_nonempty_list_vinstruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_vinstruction_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState130 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1083 * _menhir_state * 'tv_vinstruction) * _menhir_state * 'tv_nonempty_list_vinstruction_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1081 * _menhir_state * 'tv_vinstruction) * _menhir_state * 'tv_nonempty_list_vinstruction_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_nonempty_list_vinstruction_ = 
# 126 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 175 "parser.ml"
         in
        _menhir_goto_nonempty_list_vinstruction_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1082)) : 'freshtv1084)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1093 * _menhir_state) * _menhir_state * 'tv_nonempty_list_vinstruction_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1091 * _menhir_state) * _menhir_state * 'tv_nonempty_list_vinstruction_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1087 * _menhir_state) * _menhir_state * 'tv_nonempty_list_vinstruction_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1085 * _menhir_state) * _menhir_state * 'tv_nonempty_list_vinstruction_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, l) = _menhir_stack in
            let _v : 'tv_instruction = 
# 182 "parser.mly"
                                                        ( Cout l )
# 197 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1086)) : 'freshtv1088)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1089 * _menhir_state) * _menhir_state * 'tv_nonempty_list_vinstruction_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1090)) : 'freshtv1092)) : 'freshtv1094)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_instruction_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1075 * _menhir_state) * _menhir_state * 'tv_list_instruction_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1073 * _menhir_state) * _menhir_state * 'tv_list_instruction_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RACC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1069 * _menhir_state) * _menhir_state * 'tv_list_instruction_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1067 * _menhir_state) * _menhir_state * 'tv_list_instruction_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
            let _v : 'tv_bloc = 
# 193 "parser.mly"
                                 ( Bloc x )
# 233 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1065) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_bloc) = _v in
            ((match _menhir_s with
            | MenhirState49 | MenhirState176 | MenhirState123 | MenhirState142 | MenhirState171 | MenhirState168 | MenhirState149 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv1059) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_bloc) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv1057) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (b : 'tv_bloc) = _v in
                ((let _v : 'tv_instruction = 
# 181 "parser.mly"
           ( Ibloc b )
# 252 "parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1058)) : 'freshtv1060)
            | MenhirState48 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv1063 * _menhir_state * 'tv_proto) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_bloc) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv1061 * _menhir_state * 'tv_proto) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let (y : 'tv_bloc) = _v in
                ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
                let _v : 'tv_decl = 
# 64 "parser.mly"
                       ( Db (x,y))
# 268 "parser.ml"
                 in
                _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1062)) : 'freshtv1064)
            | _ ->
                _menhir_fail ()) : 'freshtv1066)) : 'freshtv1068)) : 'freshtv1070)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1071 * _menhir_state) * _menhir_state * 'tv_list_instruction_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1072)) : 'freshtv1074)) : 'freshtv1076)
    | MenhirState176 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1079 * _menhir_state * 'tv_instruction) * _menhir_state * 'tv_list_instruction_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1077 * _menhir_state * 'tv_instruction) * _menhir_state * 'tv_list_instruction_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_list_instruction_ = 
# 116 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 289 "parser.ml"
         in
        _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1078)) : 'freshtv1080)
    | _ ->
        _menhir_fail ()

and _menhir_run126 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1055 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CHAINE _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1053) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState126 in
        let (_v : (
# 13 "parser.mly"
       (string)
# 310 "parser.ml"
        )) = _v in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1051) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (x : (
# 13 "parser.mly"
       (string)
# 319 "parser.ml"
        )) = _v in
        ((let _v : 'tv_expr_str = 
# 189 "parser.mly"
             ( Estring x)
# 324 "parser.ml"
         in
        _menhir_goto_expr_str _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1052)) : 'freshtv1054)
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126) : 'freshtv1056)

and _menhir_goto_expr_str : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr_str -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1049 * _menhir_state) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_expr_str) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1047 * _menhir_state) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    let (e : 'tv_expr_str) = _v in
    ((let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _v : 'tv_vinstruction = 
# 163 "parser.mly"
                                     ( e )
# 378 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1045) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_vinstruction) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1043 * _menhir_state * 'tv_vinstruction) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1041 * _menhir_state * 'tv_vinstruction) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CHEVRON ->
        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1039 * _menhir_state * 'tv_vinstruction) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_nonempty_list_vinstruction_ = 
# 124 "/usr/share/menhir/standard.mly"
    ( [ x ] )
# 402 "parser.ml"
         in
        _menhir_goto_nonempty_list_vinstruction_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1040)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130) : 'freshtv1042)) : 'freshtv1044)) : 'freshtv1046)) : 'freshtv1048)) : 'freshtv1050)

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState166 | MenhirState155 | MenhirState147 | MenhirState144 | MenhirState60 | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1033) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1031) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (x : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 59 "/usr/share/menhir/standard.mly"
    ( x )
# 425 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1032)) : 'freshtv1034)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1037 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1035 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 146 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 441 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1036)) : 'freshtv1038)
    | _ ->
        _menhir_fail ()

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1029 * _menhir_state * 'tv_expr) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78) : 'freshtv1030)

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1027 * _menhir_state * 'tv_expr) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv1028)

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1025 * _menhir_state * 'tv_expr) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90) : 'freshtv1026)

and _menhir_run92 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1023 * _menhir_state * 'tv_expr) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92) : 'freshtv1024)

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1021 * _menhir_state * 'tv_expr) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86) : 'freshtv1022)

and _menhir_run94 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1019 * _menhir_state * 'tv_expr) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94) : 'freshtv1020)

and _menhir_run96 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1017 * _menhir_state * 'tv_expr) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv1018)

and _menhir_run98 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1015 * _menhir_state * 'tv_expr) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98) : 'freshtv1016)

and _menhir_run100 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1013 * _menhir_state * 'tv_expr) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100) : 'freshtv1014)

and _menhir_run102 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1011 * _menhir_state * 'tv_expr) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102) : 'freshtv1012)

and _menhir_run104 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1009 * _menhir_state * 'tv_expr) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104) : 'freshtv1010)

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1007 * _menhir_state * 'tv_expr) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88) : 'freshtv1008)

and _menhir_run110 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1005 * _menhir_state * 'tv_expr) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110) : 'freshtv1006)

and _menhir_run106 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1003 * _menhir_state * 'tv_expr) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106) : 'freshtv1004)

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1001 * _menhir_state * 'tv_expr) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv997 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        let (_v : (
# 11 "parser.mly"
       (string)
# 1076 "parser.ml"
        )) = _v in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv995 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        let (y : (
# 11 "parser.mly"
       (string)
# 1084 "parser.ml"
        )) = _v in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_expr = 
# 141 "parser.mly"
                              ( Sderef (x,y) )
# 1090 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv996)) : 'freshtv998)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv999 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1000)) : 'freshtv1002)

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv993 * _menhir_state * 'tv_expr) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | RPAR ->
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73) : 'freshtv994)

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv991 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
    ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
    let _v : 'tv_expr = 
# 144 "parser.mly"
                 ( Rincr x )
# 1156 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv992)

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv989 * _menhir_state * 'tv_expr) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv985 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        let (_v : (
# 11 "parser.mly"
       (string)
# 1173 "parser.ml"
        )) = _v in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv983 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        let (y : (
# 11 "parser.mly"
       (string)
# 1181 "parser.ml"
        )) = _v in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_expr = 
# 140 "parser.mly"
                           ( Attr (x,y) )
# 1187 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv984)) : 'freshtv986)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv987 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv988)) : 'freshtv990)

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv981 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
    ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
    let _v : 'tv_expr = 
# 145 "parser.mly"
                 ( Rdecr x )
# 1207 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv982)

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv919 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv917 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv913 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv911 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x), _, xs0) = _menhir_stack in
            let _v : 'tv_expr = let y =
              let xs = xs0 in
              
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 1236 "parser.ml"
              
            in
            
# 143 "parser.mly"
                                                         ( Fcall (x,y) )
# 1242 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv912)) : 'freshtv914)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv915 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv916)) : 'freshtv918)) : 'freshtv920)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv929 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 1257 "parser.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv927 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 1265 "parser.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv923 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 1274 "parser.ml"
            )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv921 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 1281 "parser.ml"
            )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), x), _, xs0) = _menhir_stack in
            let _v : 'tv_expr = let y =
              let xs = xs0 in
              
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 1289 "parser.ml"
              
            in
            
# 153 "parser.mly"
                                                                ( New (x,y) )
# 1295 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv922)) : 'freshtv924)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv925 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 1305 "parser.ml"
            )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv926)) : 'freshtv928)) : 'freshtv930)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv943 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv941 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv937 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv935 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | SEMICOLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv933 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState146 in
                ((let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv931 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | DECR ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | FALSE ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | IDENT _v ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
                | INCR ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | INTEGER _v ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
                | LAND ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | LPAR ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | MINUS ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | NEW ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | NOT ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | NULL ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | PLUS ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | THIS ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | TIDENT _v ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
                | TIMES ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | TRUE ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | RPAR ->
                    _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147) : 'freshtv932)) : 'freshtv934)
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | TIDENT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState146
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146) : 'freshtv936)) : 'freshtv938)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv939 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv940)) : 'freshtv942)) : 'freshtv944)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv953 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv951 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv947 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv945 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | FOR ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | IF ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | LACC ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | RETURN ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | SEMICOLON ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | STDCOUT ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | TIDENT _v ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | WHILE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149) : 'freshtv946)) : 'freshtv948)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv949 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv950)) : 'freshtv952)) : 'freshtv954)
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv969 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1500 "parser.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv967 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1508 "parser.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv963 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1517 "parser.ml"
            )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv961 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1524 "parser.ml"
            )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | SEMICOLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv957 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1533 "parser.ml"
                )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
                ((let _ = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv955 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1540 "parser.ml"
                )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s, t), _, v), _, s), _, xs0) = _menhir_stack in
                let _v : 'tv_instruction = let e =
                  let xs = xs0 in
                  
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 1548 "parser.ml"
                  
                in
                
# 172 "parser.mly"
(Aidecl (t,v,s,e) )
# 1554 "parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv956)) : 'freshtv958)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv959 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1564 "parser.ml"
                )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv960)) : 'freshtv962)) : 'freshtv964)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv965 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1575 "parser.ml"
            )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv966)) : 'freshtv968)) : 'freshtv970)
    | MenhirState166 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv979 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv977 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv973 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv971 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | FOR ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
            | IF ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
            | LACC ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | RETURN ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | SEMICOLON ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | STDCOUT ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | TIDENT _v ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | WHILE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168) : 'freshtv972)) : 'freshtv974)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv975 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv976)) : 'freshtv978)) : 'freshtv980)
    | _ ->
        _menhir_fail ()

and _menhir_reduce65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_instruction_ = 
# 114 "/usr/share/menhir/standard.mly"
    ( [] )
# 1665 "parser.ml"
     in
    _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv909 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv905 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv903 * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | FALSE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | IDENT _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | INCR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | INTEGER _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | LAND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LPAR ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NEW ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NULL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | THIS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | TIDENT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | TIMES ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | TRUE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51) : 'freshtv904)) : 'freshtv906)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv907 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv908)) : 'freshtv910)

and _menhir_run124 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (string)
# 1732 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv901 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1741 "parser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COLON ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
    | IDENT _ | LAND | TIMES ->
        _menhir_reduce97 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv899 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1756 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv900)) : 'freshtv902)

and _menhir_run125 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv897 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CHEVRON ->
        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125) : 'freshtv898)

and _menhir_run134 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv895) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_instruction = 
# 167 "parser.mly"
            ( Nothing )
# 1785 "parser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv896)

and _menhir_run135 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv893 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv891 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState135 in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv889 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : 'tv_instruction = 
# 184 "parser.mly"
                     ( Areturn )
# 1833 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv890)) : 'freshtv892)
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135) : 'freshtv894)

and _menhir_run139 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv887 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv883 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv881 * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | FALSE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | IDENT _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
        | INCR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | INTEGER _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
        | LAND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | LPAR ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | NEW ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | NOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | NULL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | THIS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | TIDENT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
        | TIMES ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | TRUE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState140
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140) : 'freshtv882)) : 'freshtv884)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv885 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv886)) : 'freshtv888)

and _menhir_run143 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv879 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv875 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv873 * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | FALSE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | IDENT _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
        | INCR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | INTEGER _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
        | LAND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | LPAR ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | NEW ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | NOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | NULL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | THIS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | TIDENT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
        | TIMES ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | TRUE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | SEMICOLON ->
            _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144) : 'freshtv874)) : 'freshtv876)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv877 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv878)) : 'freshtv880)

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv621 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv619 * _menhir_state) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv615 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
            let _v : 'tv_expr = 
# 139 "parser.mly"
                 ( Ldecr x )
# 2001 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv616)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv617 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv618)) : 'freshtv620)) : 'freshtv622)
    | MenhirState144 | MenhirState166 | MenhirState147 | MenhirState155 | MenhirState60 | MenhirState108 | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv633 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv631 * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv625 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv623 * _menhir_state * 'tv_expr) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | TIDENT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState108
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108) : 'freshtv624)) : 'freshtv626)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv627 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 144 "/usr/share/menhir/standard.mly"
    ( [ x ] )
# 2109 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv628)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv629 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv630)) : 'freshtv632)) : 'freshtv634)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv641 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv639 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv635 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x), _, z) = _menhir_stack in
            let _v : 'tv_expr = let y =
              
# 158 "parser.mly"
                                   (Mul)
# 2146 "parser.ml"
              
            in
            
# 132 "parser.mly"
                                    ( Op (y,x,z))
# 2152 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv636)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv637 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv638)) : 'freshtv640)) : 'freshtv642)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv649 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv647 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | MINUS | NEQ | OR | PLUS | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv643 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x), _, z) = _menhir_stack in
            let _v : 'tv_expr = let y =
              
# 158 "parser.mly"
       (Add)
# 2195 "parser.ml"
              
            in
            
# 132 "parser.mly"
                                    ( Op (y,x,z))
# 2201 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv644)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv645 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv646)) : 'freshtv648)) : 'freshtv650)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv657 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv655 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv651 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x), _, z) = _menhir_stack in
            let _v : 'tv_expr = let y =
              
# 158 "parser.mly"
                                                              (Mod)
# 2238 "parser.ml"
              
            in
            
# 132 "parser.mly"
                                    ( Op (y,x,z))
# 2244 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv652)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv653 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv654)) : 'freshtv656)) : 'freshtv658)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv665 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv663 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv659 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x), _, z) = _menhir_stack in
            let _v : 'tv_expr = let y =
              
# 158 "parser.mly"
                                               (Div)
# 2281 "parser.ml"
              
            in
            
# 132 "parser.mly"
                                    ( Op (y,x,z))
# 2287 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv660)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv661 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv662)) : 'freshtv664)) : 'freshtv666)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv673 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv671 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | CHEVRON | COMMA | OR | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv667 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x), _, z) = _menhir_stack in
            let _v : 'tv_expr = let y =
              
# 159 "parser.mly"
                 (Or)
# 2348 "parser.ml"
              
            in
            
# 132 "parser.mly"
                                    ( Op (y,x,z))
# 2354 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv668)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv669 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv670)) : 'freshtv672)) : 'freshtv674)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv681 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv679 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | EQ | NEQ | OR | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv675 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x), _, z) = _menhir_stack in
            let _v : 'tv_expr = let y =
              
# 157 "parser.mly"
               (Neq)
# 2409 "parser.ml"
              
            in
            
# 132 "parser.mly"
                                    ( Op (y,x,z))
# 2415 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv676)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv677 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv678)) : 'freshtv680)) : 'freshtv682)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv689 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv687 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | MINUS | NEQ | OR | PLUS | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv683 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x), _, z) = _menhir_stack in
            let _v : 'tv_expr = let y =
              
# 158 "parser.mly"
                     (Sub)
# 2458 "parser.ml"
              
            in
            
# 132 "parser.mly"
                                    ( Op (y,x,z))
# 2464 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv684)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv685 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv686)) : 'freshtv688)) : 'freshtv690)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv697 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv695 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv691 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x), _, z) = _menhir_stack in
            let _v : 'tv_expr = let y =
              
# 157 "parser.mly"
                         (Lt)
# 2511 "parser.ml"
              
            in
            
# 132 "parser.mly"
                                    ( Op (y,x,z))
# 2517 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv692)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv693 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv694)) : 'freshtv696)) : 'freshtv698)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv705 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv703 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv699 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x), _, z) = _menhir_stack in
            let _v : 'tv_expr = let y =
              
# 157 "parser.mly"
                                   (Le)
# 2564 "parser.ml"
              
            in
            
# 132 "parser.mly"
                                    ( Op (y,x,z))
# 2570 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv700)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv701 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv702)) : 'freshtv704)) : 'freshtv706)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv713 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv711 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv707 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x), _, z) = _menhir_stack in
            let _v : 'tv_expr = let y =
              
# 157 "parser.mly"
                                             (Gt)
# 2617 "parser.ml"
              
            in
            
# 132 "parser.mly"
                                    ( Op (y,x,z))
# 2623 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv708)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv709 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv710)) : 'freshtv712)) : 'freshtv714)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv721 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv719 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv715 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x), _, z) = _menhir_stack in
            let _v : 'tv_expr = let y =
              
# 157 "parser.mly"
                                                       (Ge)
# 2670 "parser.ml"
              
            in
            
# 132 "parser.mly"
                                    ( Op (y,x,z))
# 2676 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv716)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv717 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv718)) : 'freshtv720)) : 'freshtv722)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv729 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv727 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | EQ | NEQ | OR | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv723 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x), _, z) = _menhir_stack in
            let _v : 'tv_expr = let y =
              
# 157 "parser.mly"
     (Eq)
# 2731 "parser.ml"
              
            in
            
# 132 "parser.mly"
                                    ( Op (y,x,z))
# 2737 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv724)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv725 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv726)) : 'freshtv728)) : 'freshtv730)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv737 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv735 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | OR | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv731 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x), _, z) = _menhir_stack in
            let _v : 'tv_expr = let y =
              
# 159 "parser.mly"
      (And)
# 2796 "parser.ml"
              
            in
            
# 132 "parser.mly"
                                    ( Op (y,x,z))
# 2802 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv732)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv733 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv734)) : 'freshtv736)) : 'freshtv738)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv745 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv743 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | CHEVRON | COMMA | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv739 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x), _, y) = _menhir_stack in
            let _v : 'tv_expr = 
# 142 "parser.mly"
                             ( Assign (x,y) )
# 2866 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv740)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv741 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv742)) : 'freshtv744)) : 'freshtv746)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv753 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv751 * _menhir_state) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv747 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
            let _v : 'tv_expr = 
# 138 "parser.mly"
                 ( Lincr x )
# 2902 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv748)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv749 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv750)) : 'freshtv752)) : 'freshtv754)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv761 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv759 * _menhir_state) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv755 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
            let _v : 'tv_expr = 
# 133 "parser.mly"
                 ( Land x )
# 2938 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv756)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv757 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv758)) : 'freshtv760)) : 'freshtv762)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv771 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv769 * _menhir_state) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv765 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv763 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
            let _v : 'tv_expr = 
# 152 "parser.mly"
                       ( Par x )
# 3001 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv764)) : 'freshtv766)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv767 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv768)) : 'freshtv770)) : 'freshtv772)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv779 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv777 * _menhir_state) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv773 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
            let _v : 'tv_expr = 
# 135 "parser.mly"
                               ( Op(Sub, Eint 0, x) )
# 3041 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv774)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv775 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv776)) : 'freshtv778)) : 'freshtv780)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv787 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv785 * _menhir_state) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv781 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
            let _v : 'tv_expr = 
# 134 "parser.mly"
                ( Not x )
# 3077 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv782)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv783 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv784)) : 'freshtv786)) : 'freshtv788)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv795 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv793 * _menhir_state) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv789 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
            let _v : 'tv_expr = 
# 136 "parser.mly"
                              ( x )
# 3113 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv790)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv791 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv792)) : 'freshtv794)) : 'freshtv796)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv803 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv801 * _menhir_state) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv797 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
            let _v : 'tv_expr = 
# 137 "parser.mly"
                               ( Pointeur (x) )
# 3149 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv798)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv799 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv800)) : 'freshtv802)) : 'freshtv804)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv813 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv811 * _menhir_state) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv807 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv805 * _menhir_state) * _menhir_state * 'tv_expr) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | FOR ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | IF ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | LACC ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | RETURN ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | SEMICOLON ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | STDCOUT ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | TIDENT _v ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | WHILE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123) : 'freshtv806)) : 'freshtv808)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv809 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv810)) : 'freshtv812)) : 'freshtv814)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv821 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv819 * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | CHEVRON | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv815 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_expr_str = 
# 188 "parser.mly"
           ( Esexpr x )
# 3329 "parser.ml"
             in
            _menhir_goto_expr_str _menhir_env _menhir_stack _menhir_s _v) : 'freshtv816)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv817 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv818)) : 'freshtv820)) : 'freshtv822)
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv831 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv829 * _menhir_state) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv825 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv823 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
            let _v : 'tv_instruction = 
# 183 "parser.mly"
                                  ( Return e )
# 3394 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv824)) : 'freshtv826)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv827 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv828)) : 'freshtv830)) : 'freshtv832)
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv841 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv839 * _menhir_state) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv835 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv833 * _menhir_state) * _menhir_state * 'tv_expr) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | FOR ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
            | IF ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
            | LACC ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | RETURN ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | SEMICOLON ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | STDCOUT ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | TIDENT _v ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | WHILE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142) : 'freshtv834)) : 'freshtv836)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv837 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv838)) : 'freshtv840)) : 'freshtv842)
    | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv851 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv849 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv845 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv843 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, t), _, v), _, e) = _menhir_stack in
            let _v : 'tv_instruction = 
# 170 "parser.mly"
                                                ( Idecl (t,v,e) )
# 3577 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv844)) : 'freshtv846)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv847 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv848)) : 'freshtv850)) : 'freshtv852)
    | MenhirState49 | MenhirState176 | MenhirState123 | MenhirState142 | MenhirState171 | MenhirState168 | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv861 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv859 * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv855 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv853 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, e) = _menhir_stack in
            let _v : 'tv_instruction = 
# 168 "parser.mly"
                       ( Iexpr e )
# 3644 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv854)) : 'freshtv856)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv857 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv858)) : 'freshtv860)) : 'freshtv862)
    | MenhirState146 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv871 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv869 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | MODULO ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv865 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv863 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | TIDENT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | RPAR ->
                _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState166
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166) : 'freshtv864)) : 'freshtv866)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv867 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv868)) : 'freshtv870)) : 'freshtv872)
    | _ ->
        _menhir_fail ()

and _menhir_goto_qvar : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_qvar -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState200 | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv599 * _menhir_state) * _menhir_state * 'tv_qvar) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv597 * _menhir_state) * _menhir_state * 'tv_qvar) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
        let _v : 'tv_qvar = 
# 122 "parser.mly"
                  ( Qad x  )
# 3772 "parser.ml"
         in
        _menhir_goto_qvar _menhir_env _menhir_stack _menhir_s _v) : 'freshtv598)) : 'freshtv600)
    | MenhirState199 | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv603 * _menhir_state) * _menhir_state * 'tv_qvar) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv601 * _menhir_state) * _menhir_state * 'tv_qvar) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
        let _v : 'tv_qvar = 
# 121 "parser.mly"
                   ( Qpo x )
# 3784 "parser.ml"
         in
        _menhir_goto_qvar _menhir_env _menhir_stack _menhir_s _v) : 'freshtv602)) : 'freshtv604)
    | MenhirState198 | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv613 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_qvar) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv611 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_qvar) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv607 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_qvar) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv605 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_qvar) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | TIDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | RPAR ->
                _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv606)) : 'freshtv608)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv609 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_qvar) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv610)) : 'freshtv612)) : 'freshtv614)
    | _ ->
        _menhir_fail ()

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_instruction -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv567 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv565 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s), _, xs0), _), _, xs1), _, i) = _menhir_stack in
        let _v : 'tv_instruction = let f =
          let xs = xs1 in
          
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 3841 "parser.ml"
          
        in
        let e =
          let xs = xs0 in
          
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 3849 "parser.ml"
          
        in
        
# 180 "parser.mly"
                                                            ( Afor (e,f,i) )
# 3855 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv566)) : 'freshtv568)
    | MenhirState168 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv571 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv569 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s), _, xs0), _, x), _, xs1), _, i) = _menhir_stack in
        let _v : 'tv_instruction = let f =
          let xs = xs1 in
          
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 3869 "parser.ml"
          
        in
        let e =
          let xs = xs0 in
          
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 3877 "parser.ml"
          
        in
        
# 178 "parser.mly"
                                                            ( For (e,x,f,i) )
# 3883 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv570)) : 'freshtv572)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv583 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv581 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_instruction) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv575 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv573 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_instruction) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | FOR ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
            | IF ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
            | LACC ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | RETURN ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | SEMICOLON ->
                _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | STDCOUT ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | TIDENT _v ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | WHILE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState171
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState171) : 'freshtv574)) : 'freshtv576)
        | DECR | FALSE | FOR | IDENT _ | IF | INCR | INT | INTEGER _ | LACC | LAND | LPAR | MINUS | NEW | NOT | NULL | PLUS | RACC | RETURN | SEMICOLON | STDCOUT | THIS | TIDENT _ | TIMES | TRUE | VOID | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv577 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, e), _, i) = _menhir_stack in
            let _v : 'tv_instruction = 
# 173 "parser.mly"
                                                          ( If (e,i) )
# 3964 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv578)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv579 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv580)) : 'freshtv582)) : 'freshtv584)
    | MenhirState171 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv587 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_instruction) * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv585 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_instruction) * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, e), _, i), _, y) = _menhir_stack in
        let _v : 'tv_instruction = 
# 175 "parser.mly"
(Ifelse (e,i,y) )
# 3983 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv586)) : 'freshtv588)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv591 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv589 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, e), _, i) = _menhir_stack in
        let _v : 'tv_instruction = 
# 176 "parser.mly"
                                                    ( While (e,i) )
# 3995 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv590)) : 'freshtv592)
    | MenhirState176 | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv595 * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv593 * _menhir_state * 'tv_instruction) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | FALSE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | FOR ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | IDENT _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
        | IF ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | INCR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | INT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | INTEGER _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
        | LACC ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | LAND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | LPAR ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | NEW ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | NOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | NULL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | RETURN ->
            _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | SEMICOLON ->
            _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | STDCOUT ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | THIS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | TIDENT _v ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
        | TIMES ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | TRUE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | WHILE ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | RACC ->
            _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176) : 'freshtv594)) : 'freshtv596)
    | _ ->
        _menhir_fail ()

and _menhir_reduce71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 57 "/usr/share/menhir/standard.mly"
    ( [] )
# 4071 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv563) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_expr = 
# 149 "parser.mly"
       ( Ebool true )
# 4084 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv564)

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv561 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53) : 'freshtv562)

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv559) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_expr = 
# 147 "parser.mly"
       ( This )
# 4142 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv560)

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv557 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55) : 'freshtv558)

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv555) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_expr = 
# 150 "parser.mly"
       ( Null )
# 4200 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv556)

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv553 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57) : 'freshtv554)

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv551 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | TIDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv547 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 12 "parser.mly"
       (string)
# 4263 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv545 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 4271 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv541 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 4280 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv539 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 4287 "parser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | TIDENT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | RPAR ->
                _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv540)) : 'freshtv542)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv543 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 4336 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv544)) : 'freshtv546)) : 'freshtv548)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv549 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv550)) : 'freshtv552)

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv537 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61) : 'freshtv538)

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv535 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62) : 'freshtv536)

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv533 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63) : 'freshtv534)

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "parser.mly"
       (int)
# 4486 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv531) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (x : (
# 10 "parser.mly"
       (int)
# 4496 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 146 "parser.mly"
              ( Eint x )
# 4501 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv532)

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv529 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv530)

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv527) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_expr = 
# 148 "parser.mly"
        ( Ebool false)
# 4559 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv528)

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv525 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv526)

and _menhir_goto_separated_nonempty_list_COMMA_var_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_var_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv503 * _menhir_state * 'tv_var) * _menhir_state * 'tv_separated_nonempty_list_COMMA_var_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv501 * _menhir_state * 'tv_var) * _menhir_state * 'tv_separated_nonempty_list_COMMA_var_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_var_ = 
# 146 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 4621 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_var_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv502)) : 'freshtv504)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv523 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_separated_nonempty_list_COMMA_var_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv521 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_separated_nonempty_list_COMMA_var_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv517 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_separated_nonempty_list_COMMA_var_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv515 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_separated_nonempty_list_COMMA_var_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x), _, y) = _menhir_stack in
            let _v : 'tv_decl_vars = 
# 68 "parser.mly"
                                                               ( Declv (x,y) )
# 4643 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv513) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_decl_vars) = _v in
            ((match _menhir_s with
            | MenhirState0 | MenhirState183 | MenhirState25 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv507) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_decl_vars) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv505) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (x : 'tv_decl_vars) = _v in
                ((let _v : 'tv_decl = 
# 62 "parser.mly"
                ( Dv (x) )
# 4662 "parser.ml"
                 in
                _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv506)) : 'freshtv508)
            | MenhirState196 | MenhirState205 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv511) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_decl_vars) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv509) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (x : 'tv_decl_vars) = _v in
                ((let _v : 'tv_member = 
# 89 "parser.mly"
                ( Mvar (x) )
# 4677 "parser.ml"
                 in
                _menhir_goto_member _menhir_env _menhir_stack _menhir_s _v) : 'freshtv510)) : 'freshtv512)
            | _ ->
                _menhir_fail ()) : 'freshtv514)) : 'freshtv516)) : 'freshtv518)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv519 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_separated_nonempty_list_COMMA_var_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv520)) : 'freshtv522)) : 'freshtv524)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_argument_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_argument_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState45 | MenhirState22 | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv495) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_argument_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv493) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (x : 'tv_separated_nonempty_list_COMMA_argument_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_argument__ = 
# 59 "/usr/share/menhir/standard.mly"
    ( x )
# 4707 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv494)) : 'freshtv496)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv499 * _menhir_state * 'tv_argument) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_argument_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv497 * _menhir_state * 'tv_argument) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_separated_nonempty_list_COMMA_argument_) = _v in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_argument_ = 
# 146 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 4723 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_argument_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv498)) : 'freshtv500)
    | _ ->
        _menhir_fail ()

and _menhir_goto_member : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_member -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv491 * _menhir_state * 'tv_member) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv489 * _menhir_state * 'tv_member) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState205
    | TIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | VIRTUAL ->
        _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState205
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState205
    | RACC ->
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState205
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205) : 'freshtv490)) : 'freshtv492)

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv487 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | FOR ->
        _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | IF ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LACC ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | RETURN ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | SEMICOLON ->
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | STDCOUT ->
        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TIDENT _v ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | WHILE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | RACC ->
        _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49) : 'freshtv488)

and _menhir_goto_qident : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_qident -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState198 | MenhirState199 | MenhirState200 | MenhirState28 | MenhirState29 | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv481) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_qident) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv479) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (x : 'tv_qident) = _v in
        ((let _v : 'tv_qvar = 
# 120 "parser.mly"
             ( Qvar x )
# 4835 "parser.ml"
         in
        _menhir_goto_qvar _menhir_env _menhir_stack _menhir_s _v) : 'freshtv480)) : 'freshtv482)
    | MenhirState176 | MenhirState49 | MenhirState123 | MenhirState171 | MenhirState142 | MenhirState168 | MenhirState166 | MenhirState146 | MenhirState149 | MenhirState153 | MenhirState155 | MenhirState147 | MenhirState144 | MenhirState140 | MenhirState135 | MenhirState126 | MenhirState51 | MenhirState53 | MenhirState55 | MenhirState57 | MenhirState60 | MenhirState61 | MenhirState62 | MenhirState63 | MenhirState65 | MenhirState110 | MenhirState108 | MenhirState106 | MenhirState104 | MenhirState102 | MenhirState100 | MenhirState98 | MenhirState96 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState78 | MenhirState73 | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv485) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_qident) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv483) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (x : 'tv_qident) = _v in
        ((let _v : 'tv_expr = 
# 151 "parser.mly"
             ( Eqident x )
# 4850 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv484)) : 'freshtv486)
    | _ ->
        _menhir_fail ()

and _menhir_goto_var : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_var -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState34 | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv419 * _menhir_state) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv417 * _menhir_state) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
        let _v : 'tv_var = 
# 116 "parser.mly"
                 ( Ad x )
# 4869 "parser.ml"
         in
        _menhir_goto_var _menhir_env _menhir_stack _menhir_s _v) : 'freshtv418)) : 'freshtv420)
    | MenhirState29 | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv423 * _menhir_state) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv421 * _menhir_state) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
        let _v : 'tv_var = 
# 115 "parser.mly"
                  ( Po x )
# 4881 "parser.ml"
         in
        _menhir_goto_var _menhir_env _menhir_stack _menhir_s _v) : 'freshtv422)) : 'freshtv424)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv441 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv439 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, y) = _menhir_stack in
        let _v : 'tv_argument = 
# 110 "parser.mly"
                            ( Arg (x,y) )
# 4893 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv437) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_argument) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv435 * _menhir_state * 'tv_argument) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv433 * _menhir_state * 'tv_argument) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv427 * _menhir_state * 'tv_argument) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv425 * _menhir_state * 'tv_argument) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | TIDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState17
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17) : 'freshtv426)) : 'freshtv428)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv429 * _menhir_state * 'tv_argument) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_argument_ = 
# 144 "/usr/share/menhir/standard.mly"
    ( [ x ] )
# 4933 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_argument_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv430)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv431 * _menhir_state * 'tv_argument) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv432)) : 'freshtv434)) : 'freshtv436)) : 'freshtv438)) : 'freshtv440)) : 'freshtv442)
    | MenhirState40 | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv453 * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv451 * _menhir_state * 'tv_var) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv445 * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv443 * _menhir_state * 'tv_var) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | IDENT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | LAND ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | TIMES ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40) : 'freshtv444)) : 'freshtv446)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv447 * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_var_ = 
# 144 "/usr/share/menhir/standard.mly"
    ( [ x ] )
# 4977 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_var_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv448)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv449 * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv450)) : 'freshtv452)) : 'freshtv454)
    | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv477 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv475 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv467 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv465 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | TIDENT _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv463 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState153 in
                let (_v : (
# 12 "parser.mly"
       (string)
# 5037 "parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv461 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5045 "parser.ml"
                )) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | COLON ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
                | LPAR ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv457 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5056 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _tok = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv455 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5063 "parser.ml"
                    )) = _menhir_stack in
                    let (_tok : token) = _tok in
                    ((match _tok with
                    | DECR ->
                        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                    | FALSE ->
                        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                    | IDENT _v ->
                        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
                    | INCR ->
                        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                    | INTEGER _v ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
                    | LAND ->
                        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                    | LPAR ->
                        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                    | MINUS ->
                        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                    | NEW ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                    | NOT ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                    | NULL ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                    | PLUS ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                    | THIS ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                    | TIDENT _v ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
                    | TIMES ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                    | TRUE ->
                        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                    | RPAR ->
                        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155) : 'freshtv456)) : 'freshtv458)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv459 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5112 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv460)) : 'freshtv462)) : 'freshtv464)
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState153
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153) : 'freshtv466)) : 'freshtv468)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv471 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv469 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, t), _, v) = _menhir_stack in
            let _v : 'tv_instruction = 
# 169 "parser.mly"
                               ( Idecls (t,v) )
# 5134 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v) : 'freshtv470)) : 'freshtv472)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv473 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv474)) : 'freshtv476)) : 'freshtv478)
    | _ ->
        _menhir_fail ()

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_decl -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv415 * _menhir_state * 'tv_decl) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv413 * _menhir_state * 'tv_decl) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CLASS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | TIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | EOF ->
        _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState183
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183) : 'freshtv414)) : 'freshtv416)

and _menhir_goto_proto : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_proto -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState183 | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv391 * _menhir_state * 'tv_proto) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv389 * _menhir_state * 'tv_proto) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | LACC ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48) : 'freshtv390)) : 'freshtv392)
    | MenhirState197 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv401 * _menhir_state) * _menhir_state * 'tv_proto) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv399 * _menhir_state) * _menhir_state * 'tv_proto) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv395 * _menhir_state) * _menhir_state * 'tv_proto) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv393 * _menhir_state) * _menhir_state * 'tv_proto) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
            let _v : 'tv_member = 
# 90 "parser.mly"
                                 ( Mvir (true,x) )
# 5211 "parser.ml"
             in
            _menhir_goto_member _menhir_env _menhir_stack _menhir_s _v) : 'freshtv394)) : 'freshtv396)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv397 * _menhir_state) * _menhir_state * 'tv_proto) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv398)) : 'freshtv400)) : 'freshtv402)
    | MenhirState205 | MenhirState196 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv411 * _menhir_state * 'tv_proto) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv409 * _menhir_state * 'tv_proto) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv405 * _menhir_state * 'tv_proto) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv403 * _menhir_state * 'tv_proto) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_member = 
# 91 "parser.mly"
                        ( Mvir (false,x) )
# 5240 "parser.ml"
             in
            _menhir_goto_member _menhir_env _menhir_stack _menhir_s _v) : 'freshtv404)) : 'freshtv406)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv407 * _menhir_state * 'tv_proto) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv408)) : 'freshtv410)) : 'freshtv412)
    | _ ->
        _menhir_fail ()

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5256 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv387 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5264 "parser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv383 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5273 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv381 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5280 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv377 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5289 "parser.ml"
            )) = Obj.magic _menhir_stack in
            let (_v : (
# 11 "parser.mly"
       (string)
# 5294 "parser.ml"
            )) = _v in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv375 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5301 "parser.ml"
            )) = Obj.magic _menhir_stack in
            let (y : (
# 11 "parser.mly"
       (string)
# 5306 "parser.ml"
            )) = _v in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : 'tv_qident = 
# 128 "parser.mly"
                                         ( Double (x,y) )
# 5312 "parser.ml"
             in
            _menhir_goto_qident _menhir_env _menhir_stack _menhir_s _v) : 'freshtv376)) : 'freshtv378)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv379 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5322 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv380)) : 'freshtv382)) : 'freshtv384)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv385 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5333 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv386)) : 'freshtv388)

and _menhir_reduce81 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 11 "parser.mly"
       (string)
# 5341 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x) = _menhir_stack in
    let _v : 'tv_qident = 
# 127 "parser.mly"
            ( Qident x )
# 5348 "parser.ml"
     in
    _menhir_goto_qident _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce98 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 11 "parser.mly"
       (string)
# 5355 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x) = _menhir_stack in
    let _v : 'tv_var = 
# 114 "parser.mly"
            ( Ident x)
# 5362 "parser.ml"
     in
    _menhir_goto_var _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_member_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_member_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState205 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv351 * _menhir_state * 'tv_member) * _menhir_state * 'tv_list_member_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv349 * _menhir_state * 'tv_member) * _menhir_state * 'tv_list_member_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_list_member_ = 
# 116 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 5379 "parser.ml"
         in
        _menhir_goto_list_member_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv350)) : 'freshtv352)
    | MenhirState196 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv373 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) * _menhir_state * 'tv_list_member_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv371 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) * _menhir_state * 'tv_list_member_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RACC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv367 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) * _menhir_state * 'tv_list_member_) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv365 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) * _menhir_state * 'tv_list_member_) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | SEMICOLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv361 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) * _menhir_state * 'tv_list_member_) = Obj.magic _menhir_stack in
                ((let _ = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv359 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) * _menhir_state * 'tv_list_member_) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _menhir_s, z), x), _, y) = _menhir_stack in
                let _v : 'tv_decl_class = 
# 80 "parser.mly"
(  Class (x,z,y) )
# 5409 "parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv357) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_decl_class) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv355) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_decl_class) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv353) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (x : 'tv_decl_class) = _v in
                ((let _v : 'tv_decl = 
# 63 "parser.mly"
                 ( Dc (x) )
# 5426 "parser.ml"
                 in
                _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv354)) : 'freshtv356)) : 'freshtv358)) : 'freshtv360)) : 'freshtv362)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv363 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) * _menhir_state * 'tv_list_member_) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv364)) : 'freshtv366)) : 'freshtv368)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv369 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) * _menhir_state * 'tv_list_member_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv370)) : 'freshtv372)) : 'freshtv374)
    | _ ->
        _menhir_fail ()

and _menhir_goto_fichier : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 49 "parser.mly"
       (Ast.file)
# 5449 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv347) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 49 "parser.mly"
       (Ast.file)
# 5458 "parser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv345) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 49 "parser.mly"
       (Ast.file)
# 5466 "parser.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv346)) : 'freshtv348)

and _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_argument__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv323 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5479 "parser.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv321 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5487 "parser.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv317 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5496 "parser.ml"
            )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv315 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5503 "parser.ml"
            )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x), _, xs0) = _menhir_stack in
            let _v : 'tv_proto = let z =
              let xs = xs0 in
              
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 5511 "parser.ml"
              
            in
            
# 99 "parser.mly"
 ( Pshort (x , z))
# 5517 "parser.ml"
             in
            _menhir_goto_proto _menhir_env _menhir_stack _menhir_s _v) : 'freshtv316)) : 'freshtv318)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv319 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5527 "parser.ml"
            )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)) : 'freshtv322)) : 'freshtv324)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv333 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5536 "parser.ml"
        )) * (
# 12 "parser.mly"
       (string)
# 5540 "parser.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv331 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5548 "parser.ml"
        )) * (
# 12 "parser.mly"
       (string)
# 5552 "parser.ml"
        )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv327 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5561 "parser.ml"
            )) * (
# 12 "parser.mly"
       (string)
# 5565 "parser.ml"
            )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv325 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5572 "parser.ml"
            )) * (
# 12 "parser.mly"
       (string)
# 5576 "parser.ml"
            )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, x), y), _, xs0) = _menhir_stack in
            let _v : 'tv_proto = let z =
              let xs = xs0 in
              
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 5584 "parser.ml"
              
            in
            
# 101 "parser.mly"
   ( Pdouble ( x, y, z) )
# 5590 "parser.ml"
             in
            _menhir_goto_proto _menhir_env _menhir_stack _menhir_s _v) : 'freshtv326)) : 'freshtv328)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv329 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5600 "parser.ml"
            )) * (
# 12 "parser.mly"
       (string)
# 5604 "parser.ml"
            )) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv330)) : 'freshtv332)) : 'freshtv334)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv343 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_qvar) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv341 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_qvar) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv337 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_qvar) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv335 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_qvar) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, x), _, y), _, xs0) = _menhir_stack in
            let _v : 'tv_proto = let z =
              let xs = xs0 in
              
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 5629 "parser.ml"
              
            in
            
# 97 "parser.mly"
 ( Plong ( x, y, z) )
# 5635 "parser.ml"
             in
            _menhir_goto_proto _menhir_env _menhir_stack _menhir_s _v) : 'freshtv336)) : 'freshtv338)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv339 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_qvar) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv340)) : 'freshtv342)) : 'freshtv344)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run199 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv313 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v
    | LAND ->
        _menhir_run200 _menhir_env (Obj.magic _menhir_stack) MenhirState199
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v
    | TIMES ->
        _menhir_run199 _menhir_env (Obj.magic _menhir_stack) MenhirState199
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState199) : 'freshtv314)

and _menhir_run200 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv311 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState200 _v
    | LAND ->
        _menhir_run200 _menhir_env (Obj.magic _menhir_stack) MenhirState200
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState200 _v
    | TIMES ->
        _menhir_run199 _menhir_env (Obj.magic _menhir_stack) MenhirState200
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState200) : 'freshtv312)

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 5698 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv309 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LAND ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | TIMES ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv310)

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (string)
# 5729 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv307 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5738 "parser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COLON ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv305 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5751 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv306)) : 'freshtv308)

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv303 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | LAND ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | TIMES ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv304)

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 5780 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv301 * _menhir_state * (
# 11 "parser.mly"
       (string)
# 5789 "parser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COMMA | SEMICOLON ->
        _menhir_reduce98 _menhir_env (Obj.magic _menhir_stack)
    | LPAR ->
        _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv299 * _menhir_state * (
# 11 "parser.mly"
       (string)
# 5804 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv300)) : 'freshtv302)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv297 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | LAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | TIMES ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv298)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv295 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | LAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | TIMES ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8) : 'freshtv296)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 5850 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce98 _menhir_env (Obj.magic _menhir_stack)

and _menhir_reduce67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_member_ = 
# 114 "/usr/share/menhir/standard.mly"
    ( [] )
# 5862 "parser.ml"
     in
    _menhir_goto_list_member_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run197 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv293 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState197
    | TIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState197
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState197) : 'freshtv294)

and _menhir_goto_separated_nonempty_list_COMMA_TIDENT_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_TIDENT_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState189 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv281 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5893 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_TIDENT_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv279 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5901 "parser.ml"
        )) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_separated_nonempty_list_COMMA_TIDENT_) = _v in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_TIDENT_ = 
# 146 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 5909 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_TIDENT_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv280)) : 'freshtv282)
    | MenhirState187 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_TIDENT_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv289) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (z : 'tv_separated_nonempty_list_COMMA_TIDENT_) = _v in
        ((let _v : 'tv_supers = 
# 84 "parser.mly"
                                                           ( Super z )
# 5924 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv287) = _menhir_stack in
        let (_v : 'tv_supers) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv285) = Obj.magic _menhir_stack in
        let (_v : 'tv_supers) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv283) = Obj.magic _menhir_stack in
        let (_ : 'tv_supers) = _v in
        ((let _v : 'tv_boption_supers_ = 
# 50 "/usr/share/menhir/standard.mly"
    ( true )
# 5938 "parser.ml"
         in
        _menhir_goto_boption_supers_ _menhir_env _menhir_stack _v) : 'freshtv284)) : 'freshtv286)) : 'freshtv288)) : 'freshtv290)) : 'freshtv292)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_decl_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv263 * _menhir_state) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv261 * _menhir_state) * _menhir_state * 'tv_list_decl_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv257 * _menhir_state) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv255 * _menhir_state) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
            let _v : (
# 49 "parser.mly"
       (Ast.file)
# 5966 "parser.ml"
            ) = 
# 56 "parser.mly"
                                 ( {bincludeios = true ; declarations =  x} )
# 5970 "parser.ml"
             in
            _menhir_goto_fichier _menhir_env _menhir_stack _menhir_s _v) : 'freshtv256)) : 'freshtv258)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv259 * _menhir_state) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv260)) : 'freshtv262)) : 'freshtv264)
    | MenhirState183 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv267 * _menhir_state * 'tv_decl) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv265 * _menhir_state * 'tv_decl) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_list_decl_ = 
# 116 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 5989 "parser.ml"
         in
        _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv266)) : 'freshtv268)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv277 * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv275 * _menhir_state * 'tv_list_decl_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv271 * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv269 * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (
# 49 "parser.mly"
       (Ast.file)
# 6010 "parser.ml"
            ) = 
# 57 "parser.mly"
                   ( {bincludeios = false ; declarations =  x })
# 6014 "parser.ml"
             in
            _menhir_goto_fichier _menhir_env _menhir_stack _menhir_s _v) : 'freshtv270)) : 'freshtv272)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv273 * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv274)) : 'freshtv276)) : 'freshtv278)
    | _ ->
        _menhir_fail ()

and _menhir_reduce97 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6030 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, s) = _menhir_stack in
    let _v : 'tv_typ = 
# 107 "parser.mly"
              ( Tid s )
# 6037 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_argument__ = 
# 57 "/usr/share/menhir/standard.mly"
    ( [] )
# 6046 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (string)
# 6053 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce97 _menhir_env (Obj.magic _menhir_stack)

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_typ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState45 | MenhirState22 | MenhirState17 | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv241 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv239 * _menhir_state * 'tv_typ) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | LAND ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | TIMES ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv240)) : 'freshtv242)
    | MenhirState0 | MenhirState205 | MenhirState196 | MenhirState183 | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv245 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv243 * _menhir_state * 'tv_typ) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | LAND ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | TIDENT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28) : 'freshtv244)) : 'freshtv246)
    | MenhirState176 | MenhirState49 | MenhirState123 | MenhirState171 | MenhirState142 | MenhirState168 | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv247 * _menhir_state * 'tv_typ) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
        | LAND ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | TIMES ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState150
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150) : 'freshtv248)) : 'freshtv250)
    | MenhirState197 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv253 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv251 * _menhir_state * 'tv_typ) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENT _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
        | LAND ->
            _menhir_run200 _menhir_env (Obj.magic _menhir_stack) MenhirState198
        | TIDENT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v
        | TIMES ->
            _menhir_run199 _menhir_env (Obj.magic _menhir_stack) MenhirState198
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState198) : 'freshtv252)) : 'freshtv254)
    | _ ->
        _menhir_fail ()

and _menhir_goto_boption_supers_ : _menhir_env -> 'ttv_tail -> 'tv_boption_supers_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv237 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv235 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LACC ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv231 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv229 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | PUBLIC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv225 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv223 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv219 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv217 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | INT ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                | TIDENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _v
                | VIRTUAL ->
                    _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                | VOID ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                | RACC ->
                    _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState196
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState196) : 'freshtv218)) : 'freshtv220)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv221 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv222)) : 'freshtv224)) : 'freshtv226)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv227 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)) : 'freshtv230)) : 'freshtv232)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv233 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv234)) : 'freshtv236)) : 'freshtv238)

and _menhir_run188 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (string)
# 6221 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv215 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6230 "parser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv209 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6239 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv207 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6246 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | TIDENT _v ->
            _menhir_run188 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState189) : 'freshtv208)) : 'freshtv210)
    | LACC ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6261 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_TIDENT_ = 
# 144 "/usr/share/menhir/standard.mly"
    ( [ x ] )
# 6267 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_TIDENT_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv212)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv213 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6277 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv214)) : 'freshtv216)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState205 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * _menhir_state * 'tv_member) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState200 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState199 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState198 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState197 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState196 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState189 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6320 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState187 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv86)
    | MenhirState183 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state * 'tv_decl) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState176 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState171 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv91 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_instruction) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState168 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv93 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState166 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv95 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState155 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv97 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6358 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState153 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv103 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv105 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState146 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv111 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState130 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state * 'tv_vinstruction) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv123 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState108 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv129 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv143 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv149 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv151 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv153 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv155 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv157 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv161 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv165 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv167 * _menhir_state) * (
# 12 "parser.mly"
       (string)
# 6537 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv178)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179 * _menhir_state * 'tv_proto) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv181 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_qvar) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183 * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv185 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv187 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv189 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv193 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6606 "parser.ml"
        )) * (
# 12 "parser.mly"
       (string)
# 6610 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv195 * _menhir_state * 'tv_argument) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv197 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv199 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv201 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv203 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6639 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv205) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv206)

and _menhir_reduce63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_decl_ = 
# 114 "/usr/share/menhir/standard.mly"
    ( [] )
# 6653 "parser.ml"
     in
    _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv69) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_typ = 
# 105 "parser.mly"
       ( Void )
# 6666 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv70)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (string)
# 6673 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv67 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6682 "parser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6691 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6698 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv53 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6707 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv51 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6714 "parser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | TIDENT _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv47 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6723 "parser.ml"
                )) = Obj.magic _menhir_stack in
                let (_v : (
# 12 "parser.mly"
       (string)
# 6728 "parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv45 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6736 "parser.ml"
                )) * (
# 12 "parser.mly"
       (string)
# 6740 "parser.ml"
                )) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | LPAR ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv41 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6749 "parser.ml"
                    )) * (
# 12 "parser.mly"
       (string)
# 6753 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _tok = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv39 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6760 "parser.ml"
                    )) * (
# 12 "parser.mly"
       (string)
# 6764 "parser.ml"
                    )) = _menhir_stack in
                    let (_tok : token) = _tok in
                    ((match _tok with
                    | INT ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | TIDENT _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
                    | VOID ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | RPAR ->
                        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22) : 'freshtv40)) : 'freshtv42)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv43 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6787 "parser.ml"
                    )) * (
# 12 "parser.mly"
       (string)
# 6791 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)) : 'freshtv46)) : 'freshtv48)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv49 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6802 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)) : 'freshtv52)) : 'freshtv54)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv55 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6813 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)) : 'freshtv58)) : 'freshtv60)
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6822 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6829 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | INT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | TIDENT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | RPAR ->
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv62)) : 'freshtv64)
    | IDENT _ | LAND | TIDENT _ | TIMES ->
        _menhir_reduce97 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6854 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)) : 'freshtv68)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv37) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_typ = 
# 106 "parser.mly"
      ( Int )
# 6868 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv38)

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv35 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv31 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 11 "parser.mly"
       (string)
# 6898 "parser.ml"
        )) = _v in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state) = Obj.magic _menhir_stack in
        let (z : (
# 11 "parser.mly"
       (string)
# 6906 "parser.ml"
        )) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : 'tv_debut_decl_class = 
# 76 "parser.mly"
                     ( Hashtbl.add (Lexer.table) z () ; z )
# 6912 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_debut_decl_class) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state * 'tv_debut_decl_class) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state * 'tv_debut_decl_class) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv15) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | PUBLIC ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv11) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv9) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | TIDENT _v ->
                    _menhir_run188 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187) : 'freshtv10)) : 'freshtv12)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv13) = Obj.magic _menhir_stack in
                (raise _eRR : 'freshtv14)) : 'freshtv16)) : 'freshtv18)
        | LACC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv19) = Obj.magic _menhir_stack in
            ((let _v : 'tv_boption_supers_ = 
# 48 "/usr/share/menhir/standard.mly"
    ( false )
# 6961 "parser.ml"
             in
            _menhir_goto_boption_supers_ _menhir_env _menhir_stack _v) : 'freshtv20)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv21 * _menhir_state * 'tv_debut_decl_class) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)) : 'freshtv24)) : 'freshtv26)) : 'freshtv28)) : 'freshtv30)) : 'freshtv32)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)) : 'freshtv36)

and fichier : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 49 "parser.mly"
       (Ast.file)
# 6982 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = lexer lexbuf in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_startp = lexbuf.Lexing.lex_start_p;
        _menhir_endp = lexbuf.Lexing.lex_curr_p;
        _menhir_shifted = max_int;
        }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv7) = () in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv5) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CLASS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INCLUDEIOS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1 * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | CLASS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | INT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | TIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | EOF ->
            _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25) : 'freshtv2)) : 'freshtv4)
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv6)) : 'freshtv8))



