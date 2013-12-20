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
  | MenhirState206
  | MenhirState201
  | MenhirState200
  | MenhirState199
  | MenhirState198
  | MenhirState197
  | MenhirState190
  | MenhirState188
  | MenhirState184
  | MenhirState177
  | MenhirState172
  | MenhirState169
  | MenhirState167
  | MenhirState156
  | MenhirState154
  | MenhirState151
  | MenhirState150
  | MenhirState148
  | MenhirState147
  | MenhirState145
  | MenhirState143
  | MenhirState141
  | MenhirState136
  | MenhirState131
  | MenhirState127
  | MenhirState126
  | MenhirState124
  | MenhirState111
  | MenhirState109
  | MenhirState107
  | MenhirState105
  | MenhirState103
  | MenhirState101
  | MenhirState99
  | MenhirState97
  | MenhirState95
  | MenhirState93
  | MenhirState91
  | MenhirState89
  | MenhirState87
  | MenhirState85
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

let rec _menhir_goto_nonempty_list_vinst_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_vinst_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1089 * _menhir_state * 'tv_vinst) * _menhir_state * 'tv_nonempty_list_vinst_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1087 * _menhir_state * 'tv_vinst) * _menhir_state * 'tv_nonempty_list_vinst_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_nonempty_list_vinst_ = 
# 126 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 175 "parser.ml"
         in
        _menhir_goto_nonempty_list_vinst_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1088)) : 'freshtv1090)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1099 * _menhir_state) * _menhir_state * 'tv_nonempty_list_vinst_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1097 * _menhir_state) * _menhir_state * 'tv_nonempty_list_vinst_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1093 * _menhir_state) * _menhir_state * 'tv_nonempty_list_vinst_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1091 * _menhir_state) * _menhir_state * 'tv_nonempty_list_vinst_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, l) = _menhir_stack in
            let _v : 'tv_inst = 
# 187 "parser.mly"
                                                 ( Cout l )
# 197 "parser.ml"
             in
            _menhir_goto_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1092)) : 'freshtv1094)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1095 * _menhir_state) * _menhir_state * 'tv_nonempty_list_vinst_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1096)) : 'freshtv1098)) : 'freshtv1100)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_inst_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_inst_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1081 * _menhir_state) * _menhir_state * 'tv_list_inst_) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1079 * _menhir_state) * _menhir_state * 'tv_list_inst_) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RACC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1075 * _menhir_state) * _menhir_state * 'tv_list_inst_) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1073 * _menhir_state) * _menhir_state * 'tv_list_inst_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
            let _v : 'tv_bloc = 
# 198 "parser.mly"
                          ( Bloc x )
# 233 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1071) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_bloc) = _v in
            ((match _menhir_s with
            | MenhirState49 | MenhirState177 | MenhirState124 | MenhirState143 | MenhirState172 | MenhirState169 | MenhirState150 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv1065) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_bloc) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv1063) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (b : 'tv_bloc) = _v in
                ((let _v : 'tv_inst = 
# 186 "parser.mly"
           ( Ibloc b )
# 252 "parser.ml"
                 in
                _menhir_goto_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1064)) : 'freshtv1066)
            | MenhirState48 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv1069 * _menhir_state * 'tv_proto) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_bloc) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv1067 * _menhir_state * 'tv_proto) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let (y : 'tv_bloc) = _v in
                ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
                let _v : 'tv_decl = 
# 64 "parser.mly"
                       ( Db (x,y))
# 268 "parser.ml"
                 in
                _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1068)) : 'freshtv1070)
            | _ ->
                _menhir_fail ()) : 'freshtv1072)) : 'freshtv1074)) : 'freshtv1076)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1077 * _menhir_state) * _menhir_state * 'tv_list_inst_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1078)) : 'freshtv1080)) : 'freshtv1082)
    | MenhirState177 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1085 * _menhir_state * 'tv_inst) * _menhir_state * 'tv_list_inst_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1083 * _menhir_state * 'tv_inst) * _menhir_state * 'tv_list_inst_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_list_inst_ = 
# 116 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 289 "parser.ml"
         in
        _menhir_goto_list_inst_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1084)) : 'freshtv1086)
    | _ ->
        _menhir_fail ()

and _menhir_run127 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1061 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CHAINE _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1059) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState127 in
        let (_v : (
# 13 "parser.mly"
       (string)
# 310 "parser.ml"
        )) = _v in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1057) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (x : (
# 13 "parser.mly"
       (string)
# 319 "parser.ml"
        )) = _v in
        ((let _v : 'tv_expr_str = 
# 194 "parser.mly"
             ( Estring x)
# 324 "parser.ml"
         in
        _menhir_goto_expr_str _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1058)) : 'freshtv1060)
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127) : 'freshtv1062)

and _menhir_goto_expr_str : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr_str -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1055 * _menhir_state) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_expr_str) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1053 * _menhir_state) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    let (e : 'tv_expr_str) = _v in
    ((let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _v : 'tv_vinst = 
# 168 "parser.mly"
                              ( e )
# 378 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1051) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_vinst) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1049 * _menhir_state * 'tv_vinst) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1047 * _menhir_state * 'tv_vinst) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CHEVRON ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1045 * _menhir_state * 'tv_vinst) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_nonempty_list_vinst_ = 
# 124 "/usr/share/menhir/standard.mly"
    ( [ x ] )
# 402 "parser.ml"
         in
        _menhir_goto_nonempty_list_vinst_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1046)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131) : 'freshtv1048)) : 'freshtv1050)) : 'freshtv1052)) : 'freshtv1054)) : 'freshtv1056)

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState167 | MenhirState156 | MenhirState148 | MenhirState145 | MenhirState60 | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1039) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1037) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (x : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 59 "/usr/share/menhir/standard.mly"
    ( x )
# 425 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1038)) : 'freshtv1040)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1043 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1041 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 146 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 441 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1042)) : 'freshtv1044)
    | _ ->
        _menhir_fail ()

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1035 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78) : 'freshtv1036)

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1033 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85) : 'freshtv1034)

and _menhir_run91 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1031 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91) : 'freshtv1032)

and _menhir_run93 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1029 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93) : 'freshtv1030)

and _menhir_run87 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1027 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87) : 'freshtv1028)

and _menhir_run95 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1025 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv1026)

and _menhir_run97 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1023 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97) : 'freshtv1024)

and _menhir_run99 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1021 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99) : 'freshtv1022)

and _menhir_run101 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1019 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101) : 'freshtv1020)

and _menhir_run103 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1017 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103) : 'freshtv1018)

and _menhir_run105 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1015 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105) : 'freshtv1016)

and _menhir_run89 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1013 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89) : 'freshtv1014)

and _menhir_run111 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1011 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111) : 'freshtv1012)

and _menhir_run107 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1009 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107) : 'freshtv1010)

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1007 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1003 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        let (_v : (
# 11 "parser.mly"
       (string)
# 1079 "parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_startp in
        let (_endpos : Lexing.position) = _menhir_env._menhir_endp in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1001 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        let (y : (
# 11 "parser.mly"
       (string)
# 1089 "parser.ml"
        )) = _v in
        let (_startpos_y_ : Lexing.position) = _startpos in
        let (_endpos_y_ : Lexing.position) = _endpos in
        ((let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_y_ in
        let _v : 'tv_dexpr = 
# 142 "parser.mly"
                              ( Esderef (x,y) )
# 1099 "parser.ml"
         in
        _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv1002)) : 'freshtv1004)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1005 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1006)) : 'freshtv1008)

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv999 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | RPAR ->
        _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73) : 'freshtv1000)

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv997 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
    let (_startpos__2_ : Lexing.position) = _startpos in
    let (_endpos__2_ : Lexing.position) = _endpos in
    ((let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos__2_ in
    let _v : 'tv_dexpr = 
# 145 "parser.mly"
                 ( Erincr x )
# 1170 "parser.ml"
     in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv998)

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv995 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv991 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        let (_v : (
# 11 "parser.mly"
       (string)
# 1187 "parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_startp in
        let (_endpos : Lexing.position) = _menhir_env._menhir_endp in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv989 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        let (y : (
# 11 "parser.mly"
       (string)
# 1197 "parser.ml"
        )) = _v in
        let (_startpos_y_ : Lexing.position) = _startpos in
        let (_endpos_y_ : Lexing.position) = _endpos in
        ((let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_y_ in
        let _v : 'tv_dexpr = 
# 141 "parser.mly"
                           ( Eattr (x,y) )
# 1207 "parser.ml"
         in
        _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv990)) : 'freshtv992)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv993 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv994)) : 'freshtv996)

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr * Lexing.position * Lexing.position -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv987 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
    let (_startpos__2_ : Lexing.position) = _startpos in
    let (_endpos__2_ : Lexing.position) = _endpos in
    ((let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos__2_ in
    let _v : 'tv_dexpr = 
# 146 "parser.mly"
                 ( Erdecr x )
# 1231 "parser.ml"
     in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv988)

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv925 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv923 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv919 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_endp in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv917 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _startpos__2_), _, xs0) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_dexpr = let y =
              let xs = xs0 in
              
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 1264 "parser.ml"
              
            in
            
# 144 "parser.mly"
                                                         ( Efcall (x,y) )
# 1270 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv918)) : 'freshtv920)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv921 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv922)) : 'freshtv924)) : 'freshtv926)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv935 * _menhir_state * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 1285 "parser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv933 * _menhir_state * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 1293 "parser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv929 * _menhir_state * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 1302 "parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_endp in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv927 * _menhir_state * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 1310 "parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            let (_endpos__5_ : Lexing.position) = _endpos in
            ((let ((((_menhir_stack, _menhir_s, _startpos__1_), x, _startpos_x_), _startpos__3_), _, xs0) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__5_ in
            let _v : 'tv_dexpr = let y =
              let xs = xs0 in
              
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 1321 "parser.ml"
              
            in
            
# 154 "parser.mly"
                                                                ( Enew (x,y) )
# 1327 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv928)) : 'freshtv930)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv931 * _menhir_state * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 1337 "parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv932)) : 'freshtv934)) : 'freshtv936)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv949 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv947 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv943 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv941 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | SEMICOLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv939 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState147 in
                ((let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv937 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | DECR ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | FALSE ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | IDENT _v ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | INCR ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | INTEGER _v ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | LAND ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
                | LPAR ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
                | MINUS ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
                | NEW ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
                | NOT ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
                | NULL ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | PLUS ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
                | THIS ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | TIDENT _v ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v _menhir_env._menhir_startp
                | TIMES ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
                | TRUE ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | RPAR ->
                    _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148) : 'freshtv938)) : 'freshtv940)
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147) : 'freshtv942)) : 'freshtv944)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv945 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv946)) : 'freshtv948)) : 'freshtv950)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv959 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv957 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv953 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_endp in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv951 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * Lexing.position) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | SEMICOLON ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | STDCOUT ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | WHILE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState150
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150) : 'freshtv952)) : 'freshtv954)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv955 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv956)) : 'freshtv958)) : 'freshtv960)
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv975 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1534 "parser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv973 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1542 "parser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv969 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1551 "parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_endp in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv967 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1560 "parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * Lexing.position) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | SEMICOLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv963 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1569 "parser.ml"
                ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * Lexing.position) = Obj.magic _menhir_stack in
                ((let _ = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv961 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1576 "parser.ml"
                ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * Lexing.position) = Obj.magic _menhir_stack in
                ((let ((((((_menhir_stack, _menhir_s, t), _, v), _, s, _startpos_s_), _startpos__5_), _, xs0), _endpos__7_) = _menhir_stack in
                let _v : 'tv_inst = let e =
                  let xs = xs0 in
                  
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 1584 "parser.ml"
                  
                in
                
# 177 "parser.mly"
(Aidecl (t,v,s,e) )
# 1590 "parser.ml"
                 in
                _menhir_goto_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv962)) : 'freshtv964)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv965 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1600 "parser.ml"
                ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * Lexing.position) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv966)) : 'freshtv968)) : 'freshtv970)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv971 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1611 "parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv972)) : 'freshtv974)) : 'freshtv976)
    | MenhirState167 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv985 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv983 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv979 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_endp in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv977 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * Lexing.position) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | SEMICOLON ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | STDCOUT ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState169 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | WHILE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState169
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169) : 'freshtv978)) : 'freshtv980)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv981 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv982)) : 'freshtv984)) : 'freshtv986)
    | _ ->
        _menhir_fail ()

and _menhir_reduce66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_inst_ = 
# 114 "/usr/share/menhir/standard.mly"
    ( [] )
# 1703 "parser.ml"
     in
    _menhir_goto_list_inst_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv915 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv911 * _menhir_state) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_startp in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv909 * _menhir_state) * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FALSE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IDENT _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INCR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INTEGER _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp
        | LPAR ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp
        | THIS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51) : 'freshtv910)) : 'freshtv912)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv913 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv914)) : 'freshtv916)

and _menhir_run125 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (string)
# 1772 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv907 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1781 "parser.ml"
    ) * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COLON ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
    | IDENT _ | LAND | TIMES ->
        _menhir_reduce98 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv905 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 1796 "parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv906)) : 'freshtv908)

and _menhir_run126 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv903 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | CHEVRON ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126) : 'freshtv904)

and _menhir_run135 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv901) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_inst = 
# 172 "parser.mly"
            ( Nothing )
# 1825 "parser.ml"
     in
    _menhir_goto_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv902)

and _menhir_run136 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv899 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv897 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState136 in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv895 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : 'tv_inst = 
# 189 "parser.mly"
                     ( Areturn )
# 1873 "parser.ml"
         in
        _menhir_goto_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv896)) : 'freshtv898)
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136) : 'freshtv900)

and _menhir_run140 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv893 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv889 * _menhir_state) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_startp in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv887 * _menhir_state) * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FALSE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IDENT _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INCR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INTEGER _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_startp
        | LPAR ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_startp
        | THIS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141) : 'freshtv888)) : 'freshtv890)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv891 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv892)) : 'freshtv894)

and _menhir_run144 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv885 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv881 * _menhir_state) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_startp in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv879 * _menhir_state) * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FALSE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IDENT _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INCR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INTEGER _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
        | LPAR ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
        | THIS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | SEMICOLON ->
            _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145) : 'freshtv880)) : 'freshtv882)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv883 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv884)) : 'freshtv886)

and _menhir_goto_dexpr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_dexpr -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv877) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_dexpr) = _v in
    let (_startpos : Lexing.position) = _startpos in
    let (_endpos : Lexing.position) = _endpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv875) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (d : 'tv_dexpr) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    let (_endpos_d_ : Lexing.position) = _endpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_expr = 
# 158 "parser.mly"
            ( { dexpr = d ; loc = _startpos, _endpos } )
# 2034 "parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv873) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_expr) = _v in
    let (_startpos : Lexing.position) = _startpos in
    let (_endpos : Lexing.position) = _endpos in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    match _menhir_s with
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv621 * _menhir_state * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv619 * _menhir_state * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv615 * _menhir_state * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_, _endpos__1_), _, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x_ in
            let _v : 'tv_dexpr = 
# 140 "parser.mly"
                 ( Eldecr x )
# 2072 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv616)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv617 * _menhir_state * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv618)) : 'freshtv620)) : 'freshtv622)
    | MenhirState145 | MenhirState167 | MenhirState148 | MenhirState156 | MenhirState60 | MenhirState109 | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv633 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv631 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv625 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv623 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109) : 'freshtv624)) : 'freshtv626)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv627 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 144 "/usr/share/menhir/standard.mly"
    ( [ x ] )
# 2180 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv628)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv629 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv630)) : 'freshtv632)) : 'freshtv634)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv641 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv639 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv635 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _startpos__2_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : 'tv_dexpr = let y =
              
# 163 "parser.mly"
                                   (Mul)
# 2219 "parser.ml"
              
            in
            
# 133 "parser.mly"
                                    ( Eop (y,x,z))
# 2225 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv636)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv637 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv638)) : 'freshtv640)) : 'freshtv642)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv649 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv647 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | MINUS | NEQ | OR | PLUS | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv643 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _startpos__2_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : 'tv_dexpr = let y =
              
# 163 "parser.mly"
       (Add)
# 2270 "parser.ml"
              
            in
            
# 133 "parser.mly"
                                    ( Eop (y,x,z))
# 2276 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv644)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv645 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv646)) : 'freshtv648)) : 'freshtv650)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv657 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv655 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv651 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : 'tv_dexpr = let y =
              
# 163 "parser.mly"
                                                              (Mod)
# 2315 "parser.ml"
              
            in
            
# 133 "parser.mly"
                                    ( Eop (y,x,z))
# 2321 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv652)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv653 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv654)) : 'freshtv656)) : 'freshtv658)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv665 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv663 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv659 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : 'tv_dexpr = let y =
              
# 163 "parser.mly"
                                               (Div)
# 2360 "parser.ml"
              
            in
            
# 133 "parser.mly"
                                    ( Eop (y,x,z))
# 2366 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv660)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv661 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv662)) : 'freshtv664)) : 'freshtv666)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv673 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv671 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | ASSIGN | CHEVRON | COMMA | OR | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv667 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : 'tv_dexpr = let y =
              
# 164 "parser.mly"
                 (Or)
# 2429 "parser.ml"
              
            in
            
# 133 "parser.mly"
                                    ( Eop (y,x,z))
# 2435 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv668)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv669 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv670)) : 'freshtv672)) : 'freshtv674)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv681 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv679 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | NEQ | OR | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv675 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : 'tv_dexpr = let y =
              
# 162 "parser.mly"
               (Neq)
# 2492 "parser.ml"
              
            in
            
# 133 "parser.mly"
                                    ( Eop (y,x,z))
# 2498 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv676)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv677 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv678)) : 'freshtv680)) : 'freshtv682)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv689 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv687 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | MINUS | NEQ | OR | PLUS | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv683 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _startpos__2_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : 'tv_dexpr = let y =
              
# 163 "parser.mly"
                     (Sub)
# 2543 "parser.ml"
              
            in
            
# 133 "parser.mly"
                                    ( Eop (y,x,z))
# 2549 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv684)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv685 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv686)) : 'freshtv688)) : 'freshtv690)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv697 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv695 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv691 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : 'tv_dexpr = let y =
              
# 162 "parser.mly"
                         (Lt)
# 2598 "parser.ml"
              
            in
            
# 133 "parser.mly"
                                    ( Eop (y,x,z))
# 2604 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv692)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv693 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv694)) : 'freshtv696)) : 'freshtv698)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv705 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv703 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv699 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : 'tv_dexpr = let y =
              
# 162 "parser.mly"
                                   (Le)
# 2653 "parser.ml"
              
            in
            
# 133 "parser.mly"
                                    ( Eop (y,x,z))
# 2659 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv700)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv701 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv702)) : 'freshtv704)) : 'freshtv706)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv713 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv711 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv707 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : 'tv_dexpr = let y =
              
# 162 "parser.mly"
                                             (Gt)
# 2708 "parser.ml"
              
            in
            
# 133 "parser.mly"
                                    ( Eop (y,x,z))
# 2714 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv708)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv709 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv710)) : 'freshtv712)) : 'freshtv714)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv721 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv719 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv715 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : 'tv_dexpr = let y =
              
# 162 "parser.mly"
                                                       (Ge)
# 2763 "parser.ml"
              
            in
            
# 133 "parser.mly"
                                    ( Eop (y,x,z))
# 2769 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv716)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv717 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv718)) : 'freshtv720)) : 'freshtv722)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv729 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv727 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | NEQ | OR | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv723 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : 'tv_dexpr = let y =
              
# 162 "parser.mly"
     (Eq)
# 2826 "parser.ml"
              
            in
            
# 133 "parser.mly"
                                    ( Eop (y,x,z))
# 2832 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv724)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv725 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv726)) : 'freshtv728)) : 'freshtv730)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv737 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv735 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | OR | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv731 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : 'tv_dexpr = let y =
              
# 164 "parser.mly"
      (And)
# 2893 "parser.ml"
              
            in
            
# 133 "parser.mly"
                                    ( Eop (y,x,z))
# 2899 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv732)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv733 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv734)) : 'freshtv736)) : 'freshtv738)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv745 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv743 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | CHEVRON | COMMA | RPAR | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv739 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, y, _startpos_y_, _endpos_y_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_y_ in
            let _v : 'tv_dexpr = 
# 143 "parser.mly"
                             ( Eassign (x,y) )
# 2965 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv740)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv741 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv742)) : 'freshtv744)) : 'freshtv746)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv753 * _menhir_state * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv751 * _menhir_state * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv747 * _menhir_state * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_, _endpos__1_), _, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x_ in
            let _v : 'tv_dexpr = 
# 139 "parser.mly"
                 ( Elincr x )
# 3003 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv748)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv749 * _menhir_state * Lexing.position * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv750)) : 'freshtv752)) : 'freshtv754)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv761 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv759 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv755 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x_ in
            let _v : 'tv_dexpr = 
# 134 "parser.mly"
                 ( Eland x )
# 3041 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv756)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv757 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv758)) : 'freshtv760)) : 'freshtv762)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv771 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv769 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv765 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_endp in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv763 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_dexpr = 
# 153 "parser.mly"
                       ( Epar x )
# 3108 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv764)) : 'freshtv766)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv767 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv768)) : 'freshtv770)) : 'freshtv772)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv779 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv777 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv773 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x_ in
            let _v : 'tv_dexpr = 
# 136 "parser.mly"
                               ( Euminus( x) )
# 3150 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv774)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv775 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv776)) : 'freshtv778)) : 'freshtv780)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv787 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv785 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv781 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x_ in
            let _v : 'tv_dexpr = 
# 135 "parser.mly"
                ( Enot x )
# 3188 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv782)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv783 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv784)) : 'freshtv786)) : 'freshtv788)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv795 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv793 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv789 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x_ in
            let _v : 'tv_dexpr = 
# 137 "parser.mly"
                              (Euplus( x) )
# 3226 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv790)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv791 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv792)) : 'freshtv794)) : 'freshtv796)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv803 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv801 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv797 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x_ in
            let _v : 'tv_dexpr = 
# 138 "parser.mly"
                               ( Epointeur (x) )
# 3264 "parser.ml"
             in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv798)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv799 * _menhir_state * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv800)) : 'freshtv802)) : 'freshtv804)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv813 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv811 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv807 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_endp in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv805 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | SEMICOLON ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | STDCOUT ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | WHILE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124) : 'freshtv806)) : 'freshtv808)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv809 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv810)) : 'freshtv812)) : 'freshtv814)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv821 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv819 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | CHEVRON | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv815 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _v : 'tv_expr_str = 
# 193 "parser.mly"
           ( Esexpr x )
# 3446 "parser.ml"
             in
            _menhir_goto_expr_str _menhir_env _menhir_stack _menhir_s _v) : 'freshtv816)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv817 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv818)) : 'freshtv820)) : 'freshtv822)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv831 * _menhir_state) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv829 * _menhir_state) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv825 * _menhir_state) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv823 * _menhir_state) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, e, _startpos_e_, _endpos_e_) = _menhir_stack in
            let _v : 'tv_inst = 
# 188 "parser.mly"
                                  ( Return e )
# 3511 "parser.ml"
             in
            _menhir_goto_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv824)) : 'freshtv826)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv827 * _menhir_state) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv828)) : 'freshtv830)) : 'freshtv832)
    | MenhirState141 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv841 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv839 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv835 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_endp in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv833 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | SEMICOLON ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | STDCOUT ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | WHILE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143) : 'freshtv834)) : 'freshtv836)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv837 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv838)) : 'freshtv840)) : 'freshtv842)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv851 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv849 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv845 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv843 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, t), _, v), _, e, _startpos_e_, _endpos_e_) = _menhir_stack in
            let _v : 'tv_inst = 
# 175 "parser.mly"
                                                ( Idecl (t,v,e) )
# 3696 "parser.ml"
             in
            _menhir_goto_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv844)) : 'freshtv846)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv847 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv848)) : 'freshtv850)) : 'freshtv852)
    | MenhirState49 | MenhirState177 | MenhirState124 | MenhirState143 | MenhirState172 | MenhirState169 | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv861 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv859 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv855 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv853 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, e, _startpos_e_, _endpos_e_) = _menhir_stack in
            let _v : 'tv_inst = 
# 173 "parser.mly"
                       ( Iexpr e )
# 3763 "parser.ml"
             in
            _menhir_goto_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv854)) : 'freshtv856)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv857 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv858)) : 'freshtv860)) : 'freshtv862)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv871 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv869 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | AND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run99 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv865 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv863 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | RPAR ->
                _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState167
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState167) : 'freshtv864)) : 'freshtv866)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv867 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv868)) : 'freshtv870)) : 'freshtv872)
    | _ ->
        _menhir_fail ()) : 'freshtv874)) : 'freshtv876)) : 'freshtv878)

and _menhir_goto_qvar : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_qvar -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState201 | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv599 * _menhir_state * Lexing.position) * _menhir_state * 'tv_qvar) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv597 * _menhir_state * Lexing.position) * _menhir_state * 'tv_qvar) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, x) = _menhir_stack in
        let _v : 'tv_qvar = 
# 122 "parser.mly"
                  ( Qad x  )
# 3891 "parser.ml"
         in
        _menhir_goto_qvar _menhir_env _menhir_stack _menhir_s _v) : 'freshtv598)) : 'freshtv600)
    | MenhirState200 | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv603 * _menhir_state * Lexing.position) * _menhir_state * 'tv_qvar) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv601 * _menhir_state * Lexing.position) * _menhir_state * 'tv_qvar) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, x) = _menhir_stack in
        let _v : 'tv_qvar = 
# 121 "parser.mly"
                   ( Qpo x )
# 3903 "parser.ml"
         in
        _menhir_goto_qvar _menhir_env _menhir_stack _menhir_s _v) : 'freshtv602)) : 'freshtv604)
    | MenhirState199 | MenhirState28 ->
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
            let (_startpos : Lexing.position) = _menhir_env._menhir_startp in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv605 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_qvar) * Lexing.position) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | TIDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v _menhir_env._menhir_startp
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | RPAR ->
                _menhir_reduce70 _menhir_env (Obj.magic _menhir_stack) MenhirState45
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

and _menhir_goto_inst : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_inst -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv567 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * Lexing.position) * _menhir_state * 'tv_inst) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv565 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * Lexing.position) * _menhir_state * 'tv_inst) = Obj.magic _menhir_stack in
        ((let (((((((_menhir_stack, _menhir_s), _startpos__2_), _, xs0), _), _, xs1), _endpos__7_), _, i) = _menhir_stack in
        let _v : 'tv_inst = let f =
          let xs = xs1 in
          
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 3962 "parser.ml"
          
        in
        let e =
          let xs = xs0 in
          
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 3970 "parser.ml"
          
        in
        
# 185 "parser.mly"
                                                     ( Afor (e,f,i) )
# 3976 "parser.ml"
         in
        _menhir_goto_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv566)) : 'freshtv568)
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv571 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * Lexing.position) * _menhir_state * 'tv_inst) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv569 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * Lexing.position) * _menhir_state * 'tv_inst) = Obj.magic _menhir_stack in
        ((let (((((((_menhir_stack, _menhir_s), _startpos__2_), _, xs0), _, x, _startpos_x_, _endpos_x_), _, xs1), _endpos__8_), _, i) = _menhir_stack in
        let _v : 'tv_inst = let f =
          let xs = xs1 in
          
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 3990 "parser.ml"
          
        in
        let e =
          let xs = xs0 in
          
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 3998 "parser.ml"
          
        in
        
# 183 "parser.mly"
                                                     ( For (e,x,f,i) )
# 4004 "parser.ml"
         in
        _menhir_goto_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv570)) : 'freshtv572)
    | MenhirState143 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv583 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_inst) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv581 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_inst) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv575 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_inst) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv573 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_inst) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | SEMICOLON ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | STDCOUT ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | WHILE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172) : 'freshtv574)) : 'freshtv576)
        | DECR | FALSE | FOR | IDENT _ | IF | INCR | INT | INTEGER _ | LACC | LAND | LPAR | MINUS | NEW | NOT | NULL | PLUS | RACC | RETURN | SEMICOLON | STDCOUT | THIS | TIDENT _ | TIMES | TRUE | VOID | WHILE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv577 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_inst) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s), _startpos__2_), _, e, _startpos_e_, _endpos_e_), _endpos__4_), _, i) = _menhir_stack in
            let _v : 'tv_inst = 
# 178 "parser.mly"
                                                   ( If (e,i) )
# 4085 "parser.ml"
             in
            _menhir_goto_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv578)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv579 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_inst) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv580)) : 'freshtv582)) : 'freshtv584)
    | MenhirState172 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv587 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_inst) * _menhir_state * 'tv_inst) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv585 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_inst) * _menhir_state * 'tv_inst) = Obj.magic _menhir_stack in
        ((let ((((((_menhir_stack, _menhir_s), _startpos__2_), _, e, _startpos_e_, _endpos_e_), _endpos__4_), _, i), _, y) = _menhir_stack in
        let _v : 'tv_inst = 
# 180 "parser.mly"
(Ifelse (e,i,y) )
# 4104 "parser.ml"
         in
        _menhir_goto_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv586)) : 'freshtv588)
    | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv591 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_inst) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv589 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_inst) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s), _startpos__2_), _, e, _startpos_e_, _endpos_e_), _endpos__4_), _, i) = _menhir_stack in
        let _v : 'tv_inst = 
# 181 "parser.mly"
                                             ( While (e,i) )
# 4116 "parser.ml"
         in
        _menhir_goto_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv590)) : 'freshtv592)
    | MenhirState177 | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv595 * _menhir_state * 'tv_inst) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv593 * _menhir_state * 'tv_inst) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | DECR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FALSE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FOR ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | IDENT _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | INCR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | INTEGER _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LACC ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | LAND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
        | LPAR ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
        | RETURN ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | SEMICOLON ->
            _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | STDCOUT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | THIS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | WHILE ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | RACC ->
            _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack) MenhirState177
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177) : 'freshtv594)) : 'freshtv596)
    | _ ->
        _menhir_fail ()

and _menhir_reduce72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 57 "/usr/share/menhir/standard.mly"
    ( [] )
# 4192 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv563) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    let (_endpos__1_ : Lexing.position) = _endpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_dexpr = 
# 150 "parser.mly"
       ( Ebool true )
# 4209 "parser.ml"
     in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv564)

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv561 * _menhir_state * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53) : 'freshtv562)

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv559) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    let (_endpos__1_ : Lexing.position) = _endpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_dexpr = 
# 148 "parser.mly"
       ( Ethis )
# 4271 "parser.ml"
     in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv560)

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv557 * _menhir_state * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55) : 'freshtv558)

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv555) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    let (_endpos__1_ : Lexing.position) = _endpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_dexpr = 
# 151 "parser.mly"
       ( Enull )
# 4333 "parser.ml"
     in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv556)

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv553 * _menhir_state * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57) : 'freshtv554)

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv551 * _menhir_state * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | TIDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv547 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_v : (
# 12 "parser.mly"
       (string)
# 4396 "parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_startp in
        ((let _menhir_stack = (_menhir_stack, _v, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv545 * _menhir_state * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 4405 "parser.ml"
        ) * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | LPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv541 * _menhir_state * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 4414 "parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_startp in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv539 * _menhir_state * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 4423 "parser.ml"
            ) * Lexing.position) * Lexing.position) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | RPAR ->
                _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv540)) : 'freshtv542)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv543 * _menhir_state * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 4472 "parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv544)) : 'freshtv546)) : 'freshtv548)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv549 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv550)) : 'freshtv552)

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv537 * _menhir_state * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61) : 'freshtv538)

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv535 * _menhir_state * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62) : 'freshtv536)

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv533 * _menhir_state * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63) : 'freshtv534)

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "parser.mly"
       (int)
# 4622 "parser.ml"
) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv531) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (x : (
# 10 "parser.mly"
       (int)
# 4632 "parser.ml"
    )) = _v in
    let (_startpos_x_ : Lexing.position) = _startpos in
    let (_endpos_x_ : Lexing.position) = _endpos in
    ((let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : 'tv_dexpr = 
# 147 "parser.mly"
              ( Eint x )
# 4641 "parser.ml"
     in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv532)

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv529 * _menhir_state * Lexing.position * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv530)

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv527) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    let (_endpos__1_ : Lexing.position) = _endpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_dexpr = 
# 149 "parser.mly"
        ( Ebool false)
# 4703 "parser.ml"
     in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv528)

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv525 * _menhir_state * Lexing.position * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
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
# 4765 "parser.ml"
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
# 4787 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv513) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_decl_vars) = _v in
            ((match _menhir_s with
            | MenhirState0 | MenhirState184 | MenhirState25 ->
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
# 4806 "parser.ml"
                 in
                _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv506)) : 'freshtv508)
            | MenhirState197 | MenhirState206 ->
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
# 4821 "parser.ml"
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
# 4851 "parser.ml"
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
# 4867 "parser.ml"
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
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState206
    | TIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _v _menhir_env._menhir_startp
    | VIRTUAL ->
        _menhir_run198 _menhir_env (Obj.magic _menhir_stack) MenhirState206
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState206
    | RACC ->
        _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState206
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState206) : 'freshtv490)) : 'freshtv492)

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv487 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FOR ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LACC ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LAND ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | SEMICOLON ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | STDCOUT ->
        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | WHILE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | RACC ->
        _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49) : 'freshtv488)

and _menhir_goto_qident : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_qident -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    match _menhir_s with
    | MenhirState199 | MenhirState200 | MenhirState201 | MenhirState28 | MenhirState29 | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv481) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_qident) = _v in
        let (_startpos : Lexing.position) = _startpos in
        let (_endpos : Lexing.position) = _endpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv479) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (x : 'tv_qident) = _v in
        let (_startpos_x_ : Lexing.position) = _startpos in
        let (_endpos_x_ : Lexing.position) = _endpos in
        ((let _v : 'tv_qvar = 
# 120 "parser.mly"
             ( Qvar x )
# 4983 "parser.ml"
         in
        _menhir_goto_qvar _menhir_env _menhir_stack _menhir_s _v) : 'freshtv480)) : 'freshtv482)
    | MenhirState177 | MenhirState49 | MenhirState124 | MenhirState172 | MenhirState143 | MenhirState169 | MenhirState167 | MenhirState147 | MenhirState150 | MenhirState154 | MenhirState156 | MenhirState148 | MenhirState145 | MenhirState141 | MenhirState136 | MenhirState127 | MenhirState51 | MenhirState53 | MenhirState55 | MenhirState57 | MenhirState60 | MenhirState61 | MenhirState62 | MenhirState63 | MenhirState65 | MenhirState111 | MenhirState109 | MenhirState107 | MenhirState105 | MenhirState103 | MenhirState101 | MenhirState99 | MenhirState97 | MenhirState95 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState85 | MenhirState78 | MenhirState73 | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv485) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_qident) = _v in
        let (_startpos : Lexing.position) = _startpos in
        let (_endpos : Lexing.position) = _endpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv483) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (x : 'tv_qident) = _v in
        let (_startpos_x_ : Lexing.position) = _startpos in
        let (_endpos_x_ : Lexing.position) = _endpos in
        ((let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_dexpr = 
# 152 "parser.mly"
             ( Eqident x )
# 5004 "parser.ml"
         in
        _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv484)) : 'freshtv486)
    | _ ->
        _menhir_fail ()

and _menhir_goto_var : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_var -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState34 | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv419 * _menhir_state * Lexing.position) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv417 * _menhir_state * Lexing.position) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, x) = _menhir_stack in
        let _v : 'tv_var = 
# 116 "parser.mly"
                 ( Ad x )
# 5023 "parser.ml"
         in
        _menhir_goto_var _menhir_env _menhir_stack _menhir_s _v) : 'freshtv418)) : 'freshtv420)
    | MenhirState29 | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv423 * _menhir_state * Lexing.position) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv421 * _menhir_state * Lexing.position) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, x) = _menhir_stack in
        let _v : 'tv_var = 
# 115 "parser.mly"
                  ( Po x )
# 5035 "parser.ml"
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
# 5047 "parser.ml"
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
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v _menhir_env._menhir_startp
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
# 5087 "parser.ml"
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
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_startp
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
# 5131 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_var_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv448)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv449 * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv450)) : 'freshtv452)) : 'freshtv454)
    | MenhirState151 ->
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
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv463 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState154 in
                let (_v : (
# 12 "parser.mly"
       (string)
# 5191 "parser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_startp in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
                let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv461 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5200 "parser.ml"
                ) * Lexing.position) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | COLON ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
                | LPAR ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv457 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5211 "parser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    let (_startpos : Lexing.position) = _menhir_env._menhir_startp in
                    ((let _menhir_stack = (_menhir_stack, _startpos) in
                    let _tok = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv455 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5220 "parser.ml"
                    ) * Lexing.position) * Lexing.position) = _menhir_stack in
                    let (_tok : token) = _tok in
                    ((match _tok with
                    | DECR ->
                        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | FALSE ->
                        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | IDENT _v ->
                        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | INCR ->
                        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | INTEGER _v ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | LAND ->
                        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp
                    | LPAR ->
                        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp
                    | MINUS ->
                        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp
                    | NEW ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp
                    | NOT ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp
                    | NULL ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | PLUS ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp
                    | THIS ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | TIDENT _v ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v _menhir_env._menhir_startp
                    | TIMES ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp
                    | TRUE ->
                        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | RPAR ->
                        _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156) : 'freshtv456)) : 'freshtv458)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv459 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5269 "parser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv460)) : 'freshtv462)) : 'freshtv464)
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154) : 'freshtv466)) : 'freshtv468)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv471 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv469 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, t), _, v) = _menhir_stack in
            let _v : 'tv_inst = 
# 174 "parser.mly"
                               ( Idecls (t,v) )
# 5291 "parser.ml"
             in
            _menhir_goto_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv470)) : 'freshtv472)
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
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState184
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState184
    | TIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _v _menhir_env._menhir_startp
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState184
    | EOF ->
        _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState184
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState184) : 'freshtv414)) : 'freshtv416)

and _menhir_goto_proto : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_proto -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState184 | MenhirState25 ->
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
    | MenhirState198 ->
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
# 5368 "parser.ml"
             in
            _menhir_goto_member _menhir_env _menhir_stack _menhir_s _v) : 'freshtv394)) : 'freshtv396)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv397 * _menhir_state) * _menhir_state * 'tv_proto) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv398)) : 'freshtv400)) : 'freshtv402)
    | MenhirState206 | MenhirState197 ->
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
# 5397 "parser.ml"
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
# 5413 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv387 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5421 "parser.ml"
    ) * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv383 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5430 "parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv381 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5437 "parser.ml"
        ) * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv377 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5446 "parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (_v : (
# 11 "parser.mly"
       (string)
# 5451 "parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_startp in
            let (_endpos : Lexing.position) = _menhir_env._menhir_endp in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv375 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5460 "parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            let (y : (
# 11 "parser.mly"
       (string)
# 5465 "parser.ml"
            )) = _v in
            let (_startpos_y_ : Lexing.position) = _startpos in
            let (_endpos_y_ : Lexing.position) = _endpos in
            ((let (_menhir_stack, _menhir_s, x, _startpos_x_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_y_ in
            let _v : 'tv_qident = 
# 128 "parser.mly"
                                         ( Double (x,y) )
# 5475 "parser.ml"
             in
            _menhir_goto_qident _menhir_env _menhir_stack _menhir_s _v _startpos _endpos) : 'freshtv376)) : 'freshtv378)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv379 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5485 "parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv380)) : 'freshtv382)) : 'freshtv384)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv385 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5496 "parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv386)) : 'freshtv388)

and _menhir_reduce82 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 11 "parser.mly"
       (string)
# 5504 "parser.ml"
) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : 'tv_qident = 
# 127 "parser.mly"
            ( Qident x )
# 5513 "parser.ml"
     in
    _menhir_goto_qident _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce99 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 11 "parser.mly"
       (string)
# 5520 "parser.ml"
) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
    let _v : 'tv_var = 
# 114 "parser.mly"
            ( Ident x)
# 5527 "parser.ml"
     in
    _menhir_goto_var _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_member_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_member_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState206 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv351 * _menhir_state * 'tv_member) * _menhir_state * 'tv_list_member_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv349 * _menhir_state * 'tv_member) * _menhir_state * 'tv_list_member_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_list_member_ = 
# 116 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 5544 "parser.ml"
         in
        _menhir_goto_list_member_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv350)) : 'freshtv352)
    | MenhirState197 ->
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
# 5574 "parser.ml"
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
# 5591 "parser.ml"
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
       (Ast.fichier)
# 5614 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv347) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 49 "parser.mly"
       (Ast.fichier)
# 5623 "parser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv345) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 49 "parser.mly"
       (Ast.fichier)
# 5631 "parser.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv346)) : 'freshtv348)

and _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_argument__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv323 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5644 "parser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv321 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5652 "parser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv317 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5661 "parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_endp in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv315 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5669 "parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, x, _startpos_x_), _startpos__2_), _, xs0) = _menhir_stack in
            let _v : 'tv_proto = let z =
              let xs = xs0 in
              
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 5678 "parser.ml"
              
            in
            
# 99 "parser.mly"
 ( Pshort (x , z))
# 5684 "parser.ml"
             in
            _menhir_goto_proto _menhir_env _menhir_stack _menhir_s _v) : 'freshtv316)) : 'freshtv318)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv319 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5694 "parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)) : 'freshtv322)) : 'freshtv324)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv333 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5703 "parser.ml"
        ) * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 5707 "parser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv331 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5715 "parser.ml"
        ) * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 5719 "parser.ml"
        ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv327 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5728 "parser.ml"
            ) * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 5732 "parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_endp in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv325 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5740 "parser.ml"
            ) * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 5744 "parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
            let (_endpos__7_ : Lexing.position) = _endpos in
            ((let ((((_menhir_stack, _menhir_s, x, _startpos_x_), y, _startpos_y_), _startpos__5_), _, xs0) = _menhir_stack in
            let _v : 'tv_proto = let z =
              let xs = xs0 in
              
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 5753 "parser.ml"
              
            in
            
# 101 "parser.mly"
   ( Pdouble ( x, y, z) )
# 5759 "parser.ml"
             in
            _menhir_goto_proto _menhir_env _menhir_stack _menhir_s _v) : 'freshtv326)) : 'freshtv328)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv329 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5769 "parser.ml"
            ) * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 5773 "parser.ml"
            ) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv330)) : 'freshtv332)) : 'freshtv334)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv343 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_qvar) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv341 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_qvar) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv337 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_qvar) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_endp in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv335 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_qvar) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
            let (_endpos__5_ : Lexing.position) = _endpos in
            ((let ((((_menhir_stack, _menhir_s, x), _, y), _startpos__3_), _, xs0) = _menhir_stack in
            let _v : 'tv_proto = let z =
              let xs = xs0 in
              
# 135 "/usr/share/menhir/standard.mly"
    ( xs )
# 5800 "parser.ml"
              
            in
            
# 97 "parser.mly"
 ( Plong ( x, y, z) )
# 5806 "parser.ml"
             in
            _menhir_goto_proto _menhir_env _menhir_stack _menhir_s _v) : 'freshtv336)) : 'freshtv338)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv339 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_qvar) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_argument__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv340)) : 'freshtv342)) : 'freshtv344)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run200 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv313 * _menhir_state * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState200 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState200 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState200 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run200 _menhir_env (Obj.magic _menhir_stack) MenhirState200 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState200) : 'freshtv314)

and _menhir_run201 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv311 * _menhir_state * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run200 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState201) : 'freshtv312)

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 5869 "parser.ml"
) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv309 * _menhir_state * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv310)

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (string)
# 5900 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv307 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 5909 "parser.ml"
    ) * Lexing.position) = _menhir_stack in
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
# 5922 "parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv306)) : 'freshtv308)

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv303 * _menhir_state * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv304)

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 5951 "parser.ml"
) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv301 * _menhir_state * (
# 11 "parser.mly"
       (string)
# 5960 "parser.ml"
    ) * Lexing.position * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COMMA | SEMICOLON ->
        _menhir_reduce99 _menhir_env (Obj.magic _menhir_stack)
    | LPAR ->
        _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv299 * _menhir_state * (
# 11 "parser.mly"
       (string)
# 5975 "parser.ml"
        ) * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv300)) : 'freshtv302)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv297 * _menhir_state * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv298)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv295 * _menhir_state * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | IDENT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8) : 'freshtv296)

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 6021 "parser.ml"
) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce99 _menhir_env (Obj.magic _menhir_stack)

and _menhir_reduce68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_member_ = 
# 114 "/usr/share/menhir/standard.mly"
    ( [] )
# 6033 "parser.ml"
     in
    _menhir_goto_list_member_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run198 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv293 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | TIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v _menhir_env._menhir_startp
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState198) : 'freshtv294)

and _menhir_goto_separated_nonempty_list_COMMA_TIDENT_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_TIDENT_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState190 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv281 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6064 "parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_TIDENT_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv279 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6072 "parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_separated_nonempty_list_COMMA_TIDENT_) = _v in
        ((let (_menhir_stack, _menhir_s, x, _startpos_x_) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_TIDENT_ = 
# 146 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 6080 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_TIDENT_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv280)) : 'freshtv282)
    | MenhirState188 ->
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
# 6095 "parser.ml"
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
# 6109 "parser.ml"
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
       (Ast.fichier)
# 6137 "parser.ml"
            ) = 
# 56 "parser.mly"
                                 ( {bincludeios = true ; declarations =  x} )
# 6141 "parser.ml"
             in
            _menhir_goto_fichier _menhir_env _menhir_stack _menhir_s _v) : 'freshtv256)) : 'freshtv258)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv259 * _menhir_state) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv260)) : 'freshtv262)) : 'freshtv264)
    | MenhirState184 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv267 * _menhir_state * 'tv_decl) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv265 * _menhir_state * 'tv_decl) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : 'tv_list_decl_ = 
# 116 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 6160 "parser.ml"
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
       (Ast.fichier)
# 6181 "parser.ml"
            ) = 
# 57 "parser.mly"
                   ( {bincludeios = false ; declarations =  x })
# 6185 "parser.ml"
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

and _menhir_reduce98 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6201 "parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, s, _startpos_s_) = _menhir_stack in
    let _v : 'tv_typ = 
# 107 "parser.mly"
              ( Tid s )
# 6208 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_argument__ = 
# 57 "/usr/share/menhir/standard.mly"
    ( [] )
# 6217 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (string)
# 6224 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce98 _menhir_env (Obj.magic _menhir_stack)

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
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv240)) : 'freshtv242)
    | MenhirState0 | MenhirState206 | MenhirState197 | MenhirState184 | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv245 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv243 * _menhir_state * 'tv_typ) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_startp
        | TIDENT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28) : 'freshtv244)) : 'freshtv246)
    | MenhirState177 | MenhirState49 | MenhirState124 | MenhirState172 | MenhirState143 | MenhirState169 | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv247 * _menhir_state * 'tv_typ) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151) : 'freshtv248)) : 'freshtv250)
    | MenhirState198 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv253 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv251 * _menhir_state * 'tv_typ) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IDENT _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_startp
        | TIDENT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run200 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState199) : 'freshtv252)) : 'freshtv254)
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
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState197
                | TIDENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v _menhir_env._menhir_startp
                | VIRTUAL ->
                    _menhir_run198 _menhir_env (Obj.magic _menhir_stack) MenhirState197
                | VOID ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState197
                | RACC ->
                    _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState197
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState197) : 'freshtv218)) : 'freshtv220)
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

and _menhir_run189 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (string)
# 6392 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv215 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6401 "parser.ml"
    ) * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv209 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6410 "parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv207 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6417 "parser.ml"
        ) * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | TIDENT _v ->
            _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState190) : 'freshtv208)) : 'freshtv210)
    | LACC ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6432 "parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, x, _startpos_x_) = _menhir_stack in
        let _v : 'tv_separated_nonempty_list_COMMA_TIDENT_ = 
# 144 "/usr/share/menhir/standard.mly"
    ( [ x ] )
# 6438 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_TIDENT_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv212)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv213 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6448 "parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv214)) : 'freshtv216)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState206 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * _menhir_state * 'tv_member) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState201 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState200 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState199 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState198 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState197 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * 'tv_debut_decl_class) * 'tv_boption_supers_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState190 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6491 "parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState188 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv86)
    | MenhirState184 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state * 'tv_decl) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState177 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state * 'tv_inst) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState172 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv91 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_inst) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv93 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState167 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv95 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv97 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6529 "parser.ml"
        ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv103 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv105 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv107 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv109 * _menhir_state) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState143 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv111 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState141 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv113 * _menhir_state) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState131 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state * 'tv_vinst) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv123 * _menhir_state) * Lexing.position) * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv129 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv141 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv143 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv147 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv149 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv151 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv153 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv155 * _menhir_state * 'tv_expr * Lexing.position * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv157 * _menhir_state * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159 * _menhir_state * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv161 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv165 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv167 * _menhir_state * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 6708 "parser.ml"
        ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _), _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState57 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv175 * _menhir_state) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
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
        let (_menhir_stack : (('freshtv181 * _menhir_state * 'tv_typ) * _menhir_state * 'tv_qvar) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183 * _menhir_state * 'tv_var) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv185 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv187 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        let (_menhir_stack : (('freshtv193 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6777 "parser.ml"
        ) * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 6781 "parser.ml"
        ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _, _), _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv195 * _menhir_state * 'tv_argument) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv197 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv199 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv201 * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv203 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6810 "parser.ml"
        ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv205) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv206)

and _menhir_reduce64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_decl_ = 
# 114 "/usr/share/menhir/standard.mly"
    ( [] )
# 6824 "parser.ml"
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
# 6837 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv70)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (string)
# 6844 "parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv67 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6853 "parser.ml"
    ) * Lexing.position) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6862 "parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6869 "parser.ml"
        ) * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv53 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6878 "parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv51 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6885 "parser.ml"
            ) * Lexing.position) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | TIDENT _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv47 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6894 "parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_v : (
# 12 "parser.mly"
       (string)
# 6899 "parser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_startp in
                ((let _menhir_stack = (_menhir_stack, _v, _startpos) in
                let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv45 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6908 "parser.ml"
                ) * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 6912 "parser.ml"
                ) * Lexing.position) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | LPAR ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv41 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6921 "parser.ml"
                    ) * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 6925 "parser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    let (_startpos : Lexing.position) = _menhir_env._menhir_startp in
                    ((let _menhir_stack = (_menhir_stack, _startpos) in
                    let _tok = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv39 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6934 "parser.ml"
                    ) * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 6938 "parser.ml"
                    ) * Lexing.position) * Lexing.position) = _menhir_stack in
                    let (_tok : token) = _tok in
                    ((match _tok with
                    | INT ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | TIDENT _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v _menhir_env._menhir_startp
                    | VOID ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | RPAR ->
                        _menhir_reduce70 _menhir_env (Obj.magic _menhir_stack) MenhirState22
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
# 6961 "parser.ml"
                    ) * Lexing.position) * (
# 12 "parser.mly"
       (string)
# 6965 "parser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)) : 'freshtv46)) : 'freshtv48)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv49 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6976 "parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)) : 'freshtv52)) : 'freshtv54)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv55 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6987 "parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)) : 'freshtv58)) : 'freshtv60)
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 6996 "parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_startp in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 7005 "parser.ml"
        ) * Lexing.position) * Lexing.position) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | INT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | TIDENT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v _menhir_env._menhir_startp
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | RPAR ->
            _menhir_reduce70 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv62)) : 'freshtv64)
    | IDENT _ | LAND | TIDENT _ | TIMES ->
        _menhir_reduce98 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state * (
# 12 "parser.mly"
       (string)
# 7030 "parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
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
# 7044 "parser.ml"
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
# 7074 "parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_startp in
        let (_endpos : Lexing.position) = _menhir_env._menhir_endp in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state) = Obj.magic _menhir_stack in
        let (z : (
# 11 "parser.mly"
       (string)
# 7084 "parser.ml"
        )) = _v in
        let (_startpos_z_ : Lexing.position) = _startpos in
        let (_endpos_z_ : Lexing.position) = _endpos in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : 'tv_debut_decl_class = 
# 76 "parser.mly"
                     ( Hashtbl.add (Lexer.table) z () ; z )
# 7092 "parser.ml"
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
                    _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v _menhir_env._menhir_startp
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188) : 'freshtv10)) : 'freshtv12)
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
# 7141 "parser.ml"
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
       (Ast.fichier)
# 7162 "parser.ml"
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
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v _menhir_env._menhir_startp
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | EOF ->
            _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25) : 'freshtv2)) : 'freshtv4)
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v _menhir_env._menhir_startp
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv6)) : 'freshtv8))



