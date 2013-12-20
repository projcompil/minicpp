exception Error

type token = 
  | WHILE
  | VOID
  | VIRTUAL
  | TRUE
  | TIMES
  | TIDENT of (string)
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
  | INTEGER of (int)
  | INT
  | INCR
  | INCLUDEIOS
  | IF
  | IDENT of (string)
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
  | CHAINE of (string)
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
  | MenhirState218
  | MenhirState212
  | MenhirState210
  | MenhirState203
  | MenhirState198
  | MenhirState197
  | MenhirState196
  | MenhirState195
  | MenhirState194
  | MenhirState189
  | MenhirState182
  | MenhirState177
  | MenhirState174
  | MenhirState172
  | MenhirState160
  | MenhirState158
  | MenhirState155
  | MenhirState154
  | MenhirState152
  | MenhirState151
  | MenhirState149
  | MenhirState147
  | MenhirState145
  | MenhirState140
  | MenhirState135
  | MenhirState130
  | MenhirState129
  | MenhirState127
  | MenhirState114
  | MenhirState112
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
  | MenhirState81
  | MenhirState76
  | MenhirState71
  | MenhirState68
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState60
  | MenhirState58
  | MenhirState56
  | MenhirState54
  | MenhirState52
  | MenhirState51
  | MenhirState48
  | MenhirState43
  | MenhirState37
  | MenhirState32
  | MenhirState31
  | MenhirState26
  | MenhirState21
  | MenhirState11
  | MenhirState10
  | MenhirState9
  | MenhirState6
  | MenhirState3

  
  open Ast
 (* open Lexer*)
  (*open Lexerhack*)
let _eRR =
  Error

let rec _menhir_goto_nonempty_list_vinst_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr_str list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.expr_str list) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_vinst_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos__3_ = _startpos in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, l) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.dinst) =                                                  ( Cout l ) in
            _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_inst_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.inst list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RACC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, x) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.bloc) =                           ( Bloc x ) in
            (match _menhir_s with
            | MenhirState52 | MenhirState182 | MenhirState127 | MenhirState147 | MenhirState177 | MenhirState174 | MenhirState154 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let b = _v in
                let _startpos_b_ = _startpos in
                let _endpos_b_ = _endpos in
                let _startpos = _startpos_b_ in
                let _endpos = _endpos_b_ in
                let _v : (Ast.dinst) =            ( Ibloc b ) in
                _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
            | MenhirState51 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let y = _v in
                let _startpos_y_ = _startpos in
                let _endpos_y_ = _endpos in
                let (_menhir_stack, _menhir_s, x) = _menhir_stack in
                let _v : (Ast.decl) =                        ( Db (x,y)) in
                _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x, _endpos_x_), _, xs) = _menhir_stack in
        let _v : (Ast.inst list) =     ( x :: xs ) in
        _menhir_goto_list_inst_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run130 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | CHAINE _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState130 in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _startpos_x_ = _startpos in
        let _endpos_x_ = _endpos in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : (Ast.dexpr_str) =              ( Estring x) in
        _menhir_goto_dexpr_str _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130

and _menhir_goto_dexpr_str : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.dexpr_str) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let d = _v in
    let _startpos_d_ = _startpos in
    let _endpos_d_ = _endpos in
    let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : (Ast.expr_str) =                 ( { dexpr_str = d ; loc = _startpos, _endpos } ) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let e = _v in
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _v : (Ast.expr_str) =                               ( e ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CHEVRON ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.expr_str list) =     ( [ x ] ) in
        _menhir_goto_nonempty_list_vinst_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState172 | MenhirState160 | MenhirState152 | MenhirState149 | MenhirState63 | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ast.expr list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
        let _v : (Ast.expr list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run94 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

and _menhir_run96 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_run98 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98

and _menhir_run100 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100

and _menhir_run102 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102

and _menhir_run104 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104

and _menhir_run106 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

and _menhir_run108 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108

and _menhir_run92 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92

and _menhir_run114 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114

and _menhir_run110 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let y = _v in
        let _startpos_y_ = _startpos in
        let _endpos_y_ = _endpos in
        let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_y_ in
        let _v : (Ast.dexpr) =                               ( Esderef (x,y) ) in
        _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | RPAR ->
        _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__2_ = _startpos in
    let _endpos__2_ = _endpos in
    let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos__2_ in
    let _v : (Ast.dexpr) =                  ( Erincr x ) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let y = _v in
        let _startpos_y_ = _startpos in
        let _endpos_y_ = _endpos in
        let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_y_ in
        let _v : (Ast.dexpr) =                            ( Eattr (x,y) ) in
        _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__2_ = _startpos in
    let _endpos__2_ = _endpos in
    let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos__2_ in
    let _v : (Ast.dexpr) =                  ( Erdecr x ) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__4_ = _endpos in
            let (((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _startpos__2_), _, xs0) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos__4_ in
            let _v : (Ast.dexpr) = let y =
              let xs = xs0 in
                  ( xs )
            in
                                                                     ( Efcall (x,y) ) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__5_ = _endpos in
            let ((((_menhir_stack, _menhir_s, _startpos__1_), x, _startpos_x_), _startpos__3_), _, xs0) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__5_ in
            let _v : (Ast.dexpr) = let y =
              let xs = xs0 in
                  ( xs )
            in
                                                                            ( Enew (x,y) ) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _startpos, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState151 in
                let _startpos = _menhir_env._menhir_startp in
                let _endpos = _menhir_env._menhir_endp in
                let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | DECR ->
                    _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | FALSE ->
                    _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | IDENT _v ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | INCR ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | INTEGER _v ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | LAND ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp
                | LPAR ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp
                | MINUS ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp
                | NEW ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp
                | NOT ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp
                | NULL ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | PLUS ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp
                | THIS ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | TIDENT _v ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v _menhir_env._menhir_startp
                | TIMES ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp
                | TRUE ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | RPAR ->
                    _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState152
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152)
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | LAND ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos = _menhir_env._menhir_startp in
                let _endpos = _menhir_env._menhir_endp in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos__8_ = _startpos in
                let _endpos__8_ = _endpos in
                let ((((((_menhir_stack, _menhir_s, t, _startpos_t_), _, v, _endpos_v_), _, s, _startpos_s_), _startpos__5_), _, xs0), _endpos__7_) = _menhir_stack in
                let _startpos = _startpos_t_ in
                let _endpos = _endpos__8_ in
                let _v : (Ast.dinst) = let e =
                  let xs = xs0 in
                      ( xs )
                in
                (Aidecl (t,v,s,e) ) in
                _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp
            | LAND ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.inst list) =     ( [] ) in
    _menhir_goto_list_inst_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | DECR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FALSE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IDENT _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INCR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INTEGER _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
        | LPAR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
        | THIS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run128 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COLON ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
    | IDENT _ | LAND | TIMES ->
        _menhir_reduce103 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run129 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | CHEVRON ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState129
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129

and _menhir_run139 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dinst) =             ( Nothing ) in
    _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run140 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState140 in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos__2_ = _startpos in
        let _endpos__2_ = _endpos in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (Ast.dinst) =                      ( Areturn ) in
        _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140

and _menhir_run144 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | DECR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FALSE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IDENT _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INCR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INTEGER _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
        | LPAR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
        | THIS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run148 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | DECR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FALSE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IDENT _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INCR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INTEGER _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp
        | LPAR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp
        | THIS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | SEMICOLON ->
            _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState149
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_dexpr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.dexpr) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let d = _v in
    let _startpos_d_ = _startpos in
    let _endpos_d_ = _endpos in
    let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : (Ast.expr) =             ( { dexpr = d ; loc = _startpos, _endpos } ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    match _menhir_s with
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_, _endpos__1_), _, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x_ in
            let _v : (Ast.dexpr) =                  ( Eldecr x ) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState149 | MenhirState172 | MenhirState152 | MenhirState160 | MenhirState63 | MenhirState112 | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _v : (Ast.expr list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _startpos__2_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                                                 (Mul)
            in
                                                ( Eop (y,x,z)) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | MINUS | NEQ | OR | PLUS | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _startpos__2_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                     (Add)
            in
                                                ( Eop (y,x,z)) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                                                                            (Mod)
            in
                                                ( Eop (y,x,z)) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                                                             (Div)
            in
                                                ( Eop (y,x,z)) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | ASSIGN | CHEVRON | COMMA | OR | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                               (Or)
            in
                                                ( Eop (y,x,z)) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | NEQ | OR | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                             (Neq)
            in
                                                ( Eop (y,x,z)) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | MINUS | NEQ | OR | PLUS | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _startpos__2_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                                   (Sub)
            in
                                                ( Eop (y,x,z)) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                                       (Lt)
            in
                                                ( Eop (y,x,z)) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                                                 (Le)
            in
                                                ( Eop (y,x,z)) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                                                           (Gt)
            in
                                                ( Eop (y,x,z)) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                                                                     (Ge)
            in
                                                ( Eop (y,x,z)) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | NEQ | OR | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                   (Eq)
            in
                                                ( Eop (y,x,z)) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | OR | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                    (And)
            in
                                                ( Eop (y,x,z)) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | CHEVRON | COMMA | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, y, _startpos_y_, _endpos_y_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_y_ in
            let _v : (Ast.dexpr) =                              ( Eassign (x,y) ) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_, _endpos__1_), _, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x_ in
            let _v : (Ast.dexpr) =                  ( Elincr x ) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x_ in
            let _v : (Ast.dexpr) =                  ( Eland x ) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.dexpr) =                        ( Epar x ) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x_ in
            let _v : (Ast.dexpr) =                                ( Euminus( x) ) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x_ in
            let _v : (Ast.dexpr) =                 ( Enot x ) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x_ in
            let _v : (Ast.dexpr) =                               (Euplus( x) ) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x_ in
            let _v : (Ast.dexpr) =                                ( Epointeur (x) ) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
            | LAND ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | CHEVRON | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_x_ in
            let _v : (Ast.dexpr_str) =            ( Esexpr x ) in
            _menhir_goto_dexpr_str _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos__3_ = _startpos in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, e, _startpos_e_, _endpos_e_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.dinst) =                                   ( Return e ) in
            _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | LAND ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos__5_ = _startpos in
            let _endpos__5_ = _endpos in
            let (((_menhir_stack, _menhir_s, t, _startpos_t_), _, v, _endpos_v_), _, e, _startpos_e_, _endpos_e_) = _menhir_stack in
            let _startpos = _startpos_t_ in
            let _endpos = _endpos__5_ in
            let _v : (Ast.dinst) =                                                 ( Idecl (t,v,e) ) in
            _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState52 | MenhirState182 | MenhirState127 | MenhirState147 | MenhirState177 | MenhirState174 | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos__2_ = _startpos in
            let _endpos__2_ = _endpos in
            let (_menhir_stack, _menhir_s, e, _startpos_e_, _endpos_e_) = _menhir_stack in
            let _startpos = _startpos_e_ in
            let _endpos = _endpos__2_ in
            let _v : (Ast.dinst) =                        ( Iexpr e ) in
            _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _startpos, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | RPAR ->
                _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState172
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172)
        | TIMES ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_qvar : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.qvar) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState198 | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, x) = _menhir_stack in
        let _v : (Ast.qvar) =                   ( Qad x  ) in
        _menhir_goto_qvar _menhir_env _menhir_stack _menhir_s _v
    | MenhirState197 | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, x) = _menhir_stack in
        let _v : (Ast.qvar) =                    ( Qpo x ) in
        _menhir_goto_qvar _menhir_env _menhir_stack _menhir_s _v
    | MenhirState196 | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | INT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp
            | TIDENT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v _menhir_env._menhir_startp
            | VOID ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp
            | RPAR ->
                _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_dinst : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.dinst) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let d = _v in
    let _startpos_d_ = _startpos in
    let _endpos_d_ = _endpos in
    let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : (Ast.inst) =             ( { dinst = d ; loc = _startpos, _endpos } ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _endpos) in
    match _menhir_s with
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, xs0), _startpos__4_, _endpos__4_), _, _startpos__5_, _endpos__5_), _, xs1), _endpos__7_), _, i, _endpos_i_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_i_ in
        let _v : (Ast.dinst) = let f =
          let xs = xs1 in
              ( xs )
        in
        let e =
          let xs = xs0 in
              ( xs )
        in
                                                             ( Afor (e,f,i) ) in
        _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, xs0), _startpos__4_, _endpos__4_), _, x, _startpos_x_, _endpos_x_), _startpos__6_, _endpos__6_), _, xs1), _endpos__8_), _, i, _endpos_i_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_i_ in
        let _v : (Ast.dinst) = let f =
          let xs = xs1 in
              ( xs )
        in
        let e =
          let xs = xs0 in
              ( xs )
        in
                                                             ( For (e,x,f,i) ) in
        _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
            | LAND ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177)
        | DECR | FALSE | FOR | IDENT _ | IF | INCR | INT | INTEGER _ | LACC | LAND | LPAR | MINUS | NEW | NOT | NULL | PLUS | RACC | RETURN | SEMICOLON | STDCOUT | THIS | TIDENT _ | TIMES | TRUE | VOID | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, e, _startpos_e_, _endpos_e_), _endpos__4_), _, i, _endpos_i_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_i_ in
            let _v : (Ast.dinst) =                                                    ( If (e,i) ) in
            _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, e, _startpos_e_, _endpos_e_), _endpos__4_), _, i, _endpos_i_), _, y, _endpos_y_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_y_ in
        let _v : (Ast.dinst) = (Ifelse (e,i,y) ) in
        _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, e, _startpos_e_, _endpos_e_), _endpos__4_), _, i, _endpos_i_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_i_ in
        let _v : (Ast.dinst) =                                              ( While (e,i) ) in
        _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState182 | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FALSE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FOR ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp
        | IDENT _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IF ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp
        | INCR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp
        | INTEGER _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LACC ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp
        | LAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp
        | LPAR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp
        | RETURN ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp
        | SEMICOLON ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | STDCOUT ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp
        | THIS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | VOID ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp
        | WHILE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_startp
        | RACC ->
            _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState182
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182)
    | _ ->
        _menhir_fail ()

and _menhir_reduce77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expr list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dexpr) =        ( Ebool true ) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dexpr) =        ( Ethis ) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dexpr) =        ( Enull ) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | TIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _menhir_stack = (_menhir_stack, _v, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | LPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | RPAR ->
                _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.dexpr) =               ( Eint x ) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dexpr) =         ( Ebool false) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_goto_separated_nonempty_list_COMMA_var_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.var list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x, _endpos_x_), _, xs) = _menhir_stack in
        let _v : (Ast.var list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_var_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos__3_ = _startpos in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_), _, y) = _menhir_stack in
            let _v : (Ast.decl_v) =                                                                ( Declv (x,y) ) in
            (match _menhir_s with
            | MenhirState189 | MenhirState3 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let x = _v in
                let _v : (Ast.decl) =                 ( Dv (x) ) in
                _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
            | MenhirState218 | MenhirState194 | MenhirState203 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let x = _v in
                let _v : (Ast.membre) =                 ( Mvar (x) ) in
                _menhir_goto_member _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_argument_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.arg list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState48 | MenhirState26 | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ast.arg list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.arg list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_argument_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_member : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.membre) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v _menhir_env._menhir_startp
    | VIRTUAL ->
        _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState203
    | VOID ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_startp
    | RACC ->
        _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState203
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState203

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FOR ->
        _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | INCR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | INTEGER _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LACC ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | LAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | SEMICOLON ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STDCOUT ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | VOID ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | WHILE ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
    | RACC ->
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_goto_qident : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.qident) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    match _menhir_s with
    | MenhirState196 | MenhirState197 | MenhirState198 | MenhirState31 | MenhirState32 | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _startpos_x_ = _startpos in
        let _endpos_x_ = _endpos in
        let _v : (Ast.qvar) =              ( Qvar x ) in
        _menhir_goto_qvar _menhir_env _menhir_stack _menhir_s _v
    | MenhirState182 | MenhirState52 | MenhirState127 | MenhirState177 | MenhirState147 | MenhirState174 | MenhirState172 | MenhirState151 | MenhirState154 | MenhirState158 | MenhirState160 | MenhirState152 | MenhirState149 | MenhirState145 | MenhirState140 | MenhirState130 | MenhirState54 | MenhirState56 | MenhirState58 | MenhirState60 | MenhirState63 | MenhirState64 | MenhirState65 | MenhirState66 | MenhirState68 | MenhirState114 | MenhirState112 | MenhirState110 | MenhirState108 | MenhirState106 | MenhirState104 | MenhirState102 | MenhirState100 | MenhirState98 | MenhirState96 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState81 | MenhirState76 | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _startpos_x_ = _startpos in
        let _endpos_x_ = _endpos in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : (Ast.dexpr) =              ( Eqident x ) in
        _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | _ ->
        _menhir_fail ()

and _menhir_goto_dvar : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.dvar) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let d = _v in
    let _startpos_d_ = _startpos in
    let _endpos_d_ = _endpos in
    let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : (Ast.var) =            ( { dvar = d ; loc = _startpos, _endpos } ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _endpos) in
    match _menhir_s with
    | MenhirState37 | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _endpos_x_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_x_ in
        let _v : (Ast.dvar) =                  ( Ad x ) in
        _menhir_goto_dvar _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState32 | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _endpos_x_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_x_ in
        let _v : (Ast.dvar) =                   ( Po x ) in
        _menhir_goto_dvar _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x, _startpos_x_), _, y, _endpos_y_) = _menhir_stack in
        let _v : (Ast.arg) =                             ( Arg (x,y) ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | INT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _menhir_env._menhir_startp
            | TIDENT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v _menhir_env._menhir_startp
            | VOID ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (Ast.arg list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_argument_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState43 | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IDENT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x, _endpos_x_) = _menhir_stack in
            let _v : (Ast.var list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_var_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState158 in
                let _startpos = _menhir_env._menhir_startp in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | COLON ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
                | LPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _startpos = _menhir_env._menhir_startp in
                    let _menhir_stack = (_menhir_stack, _startpos) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | DECR ->
                        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | FALSE ->
                        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | IDENT _v ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | INCR ->
                        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | INTEGER _v ->
                        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | LAND ->
                        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
                    | LPAR ->
                        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
                    | MINUS ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
                    | NEW ->
                        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
                    | NOT ->
                        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
                    | NULL ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | PLUS ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
                    | THIS ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | TIDENT _v ->
                        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v _menhir_env._menhir_startp
                    | TIMES ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
                    | TRUE ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | RPAR ->
                        _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState160
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | TIMES ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos__3_ = _startpos in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, t, _startpos_t_), _, v, _endpos_v_) = _menhir_stack in
            let _startpos = _startpos_t_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.dinst) =                                ( Idecls (t,v) ) in
            _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState189
    | INT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _v _menhir_env._menhir_startp
    | VOID ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _menhir_env._menhir_startp
    | EOF ->
        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState189
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState189

and _menhir_goto_proto : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.proto) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState189 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LACC ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | MenhirState195 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos__3_ = _startpos in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
            let _v : (Ast.membre) =                                  ( Mvir (true,x) ) in
            _menhir_goto_member _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState218 | MenhirState203 | MenhirState194 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos__2_ = _startpos in
            let _endpos__2_ = _endpos in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (Ast.membre) =                         ( Mvir (false,x) ) in
            _menhir_goto_member _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let y = _v in
            let _startpos_y_ = _startpos in
            let _endpos_y_ = _endpos in
            let (_menhir_stack, _menhir_s, x, _startpos_x_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_y_ in
            let _v : (Ast.qident) =                                          ( Double (x,y) ) in
            _menhir_goto_qident _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce87 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.qident) =             ( Qident x ) in
    _menhir_goto_qident _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce62 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.dvar) =             ( Ident x) in
    _menhir_goto_dvar _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_goto_decl_class : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.decl_c) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (Ast.decl) =                  ( Dc (x) ) in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.arg list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__4_ = _endpos in
            let (((_menhir_stack, _menhir_s, x, _startpos_x_), _startpos__2_), _, xs0) = _menhir_stack in
            let _v : (Ast.proto) = let z =
              let xs = xs0 in
                  ( xs )
            in
             ( Pshort (x , z)) in
            _menhir_goto_proto _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__7_ = _endpos in
            let ((((_menhir_stack, _menhir_s, x, _startpos_x_), y, _startpos_y_), _startpos__5_), _, xs0) = _menhir_stack in
            let _v : (Ast.proto) = let z =
              let xs = xs0 in
                  ( xs )
            in
               ( Pdouble ( x, y, z) ) in
            _menhir_goto_proto _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__5_ = _endpos in
            let ((((_menhir_stack, _menhir_s, x, _startpos_x_), _, y), _startpos__3_), _, xs0) = _menhir_stack in
            let _v : (Ast.proto) = let z =
              let xs = xs0 in
                  ( xs )
            in
             ( Plong ( x, y, z) ) in
            _menhir_goto_proto _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run197 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run198 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState197

and _menhir_run198 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run198 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState198

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce87 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COLON ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAR ->
        _menhir_reduce87 _menhir_env (Obj.magic _menhir_stack)
    | COMMA | SEMICOLON ->
        _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack)

and _menhir_goto_list_member_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.membre list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState203 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.membre list) =     ( x :: xs ) in
        _menhir_goto_list_member_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState194 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RACC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos = _menhir_env._menhir_startp in
                let _endpos = _menhir_env._menhir_endp in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos__7_ = _startpos in
                let _endpos__7_ = _endpos in
                let ((((_menhir_stack, _menhir_s, z), _startpos__2_), _, y), _endpos__6_) = _menhir_stack in
                let _v : (Ast.decl_c) = (  Class (z, (Super []),y) ) in
                _menhir_goto_decl_class _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState218 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RACC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos = _menhir_env._menhir_startp in
                let _endpos = _menhir_env._menhir_endp in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos__8_ = _startpos in
                let _endpos__8_ = _endpos in
                let (((((_menhir_stack, _menhir_s, z), l), _startpos__3_), _, y), _endpos__7_) = _menhir_stack in
                let _v : (Ast.decl_c) = (  Class (z,l,y) ) in
                _menhir_goto_decl_class _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_TIDENT_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState212 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x, _startpos_x_) = _menhir_stack in
        let _v : (string list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_TIDENT_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState210 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let z = _v in
        let _v : (Ast.sup) =                                                            ( Super z ) in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LACC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | PUBLIC ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | COLON ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | INT ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _menhir_env._menhir_startp
                    | TIDENT _v ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v _menhir_env._menhir_startp
                    | VIRTUAL ->
                        _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState218
                    | VOID ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _menhir_env._menhir_startp
                    | RACC ->
                        _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState218
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState218)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.decl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, y), _, x) = _menhir_stack in
            let _v : (Ast.fichier) =                                             ( {bincludeios = y ; decls =  x} ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Obj.magic _1
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState189 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.decl list) =     ( x :: xs ) in
        _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce103 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, s, _startpos_s_) = _menhir_stack in
    let _startpos = _startpos_s_ in
    let _v : (Ast.typedef) =               ( Tid s ) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_reduce75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.arg list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce103 _menhir_env (Obj.magic _menhir_stack)

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.typedef) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState48 | MenhirState26 | MenhirState21 | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
    | MenhirState218 | MenhirState203 | MenhirState194 | MenhirState189 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_startp
        | TIDENT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
    | MenhirState182 | MenhirState52 | MenhirState127 | MenhirState177 | MenhirState147 | MenhirState174 | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
    | MenhirState195 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run198 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _menhir_env._menhir_startp
        | TIDENT _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState196)
    | _ ->
        _menhir_fail ()

and _menhir_reduce73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.membre list) =     ( [] ) in
    _menhir_goto_list_member_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run195 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v _menhir_env._menhir_startp
    | VOID ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState195

and _menhir_run211 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | TIDENT _v ->
            _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState212 _v _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState212)
    | LACC ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x, _startpos_x_) = _menhir_stack in
        let _v : (string list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_TIDENT_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState218 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState212 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState210 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState203 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState197 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState196 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState195 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState194 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState189 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _, _), _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.decl list) =     ( [] ) in
    _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (Ast.typedef) =        ( Void ) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | TIDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos = _menhir_env._menhir_startp in
                let _menhir_stack = (_menhir_stack, _v, _startpos) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | LPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _startpos = _menhir_env._menhir_startp in
                    let _menhir_stack = (_menhir_stack, _startpos) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | INT ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_startp
                    | TIDENT _v ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v _menhir_env._menhir_startp
                    | VOID ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_startp
                    | RPAR ->
                        _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | INT ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_startp
        | TIDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v _menhir_env._menhir_startp
        | VOID ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_startp
        | RPAR ->
            _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
    | IDENT _ | LAND | TIDENT _ | TIMES ->
        _menhir_reduce103 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (Ast.typedef) =       ( Int ) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let z = _v in
        let _startpos_z_ = _startpos in
        let _endpos_z_ = _endpos in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (string) =                      ( Hashtbl.add (Lexerhack.table) z () ; z ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | PUBLIC ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | TIDENT _v ->
                    _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v _menhir_env._menhir_startp
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState210)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | LACC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | PUBLIC ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | COLON ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | INT ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_startp
                    | TIDENT _v ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v _menhir_env._menhir_startp
                    | VIRTUAL ->
                        _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                    | VOID ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_startp
                    | RACC ->
                        _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState194
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState194)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_boption_INCLUDEIOS_ : _menhir_env -> 'ttv_tail -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | INT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v _menhir_env._menhir_startp
    | VOID ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _menhir_env._menhir_startp
    | EOF ->
        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

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

and fichier : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.fichier) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = max_int;
      } in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INCLUDEIOS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (bool) =     ( true ) in
        _menhir_goto_boption_INCLUDEIOS_ _menhir_env _menhir_stack _v
    | CLASS | EOF | INT | TIDENT _ | VOID ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (bool) =     ( false ) in
        _menhir_goto_boption_INCLUDEIOS_ _menhir_env _menhir_stack _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR)



