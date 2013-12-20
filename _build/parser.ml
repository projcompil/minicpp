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
  | MenhirState226
  | MenhirState220
  | MenhirState218
  | MenhirState211
  | MenhirState206
  | MenhirState205
  | MenhirState204
  | MenhirState203
  | MenhirState202
  | MenhirState197
  | MenhirState188
  | MenhirState183
  | MenhirState180
  | MenhirState178
  | MenhirState163
  | MenhirState161
  | MenhirState158
  | MenhirState157
  | MenhirState155
  | MenhirState154
  | MenhirState152
  | MenhirState150
  | MenhirState148
  | MenhirState143
  | MenhirState138
  | MenhirState132
  | MenhirState131
  | MenhirState129
  | MenhirState116
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
  | MenhirState83
  | MenhirState78
  | MenhirState73
  | MenhirState70
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState62
  | MenhirState60
  | MenhirState58
  | MenhirState56
  | MenhirState54
  | MenhirState53
  | MenhirState50
  | MenhirState45
  | MenhirState39
  | MenhirState34
  | MenhirState33
  | MenhirState28
  | MenhirState23
  | MenhirState13
  | MenhirState12
  | MenhirState11
  | MenhirState8
  | MenhirState5

  
  open Ast
 (* open Lexer*)
  (*open Lexerhack*)
let _eRR =
  Error

let rec _menhir_goto_nonempty_list_vinst_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr_str list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.expr_str list) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_vinst_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState131 ->
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
    | MenhirState54 ->
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
            let _v : (Ast.dbloc) =                           ( Bloc x ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let x = _v in
            let _startpos_x_ = _startpos in
            let _endpos_x_ = _endpos in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_x_ in
            let _v : (Ast.bloc) =         ( {v = x ; loc=(_startpos, _endpos) }) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let x = _v in
            let _startpos_x_ = _startpos in
            let _endpos_x_ = _endpos in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_x_ in
            let _v : (Ast.bloc) =                       ( x ) in
            (match _menhir_s with
            | MenhirState54 | MenhirState188 | MenhirState129 | MenhirState150 | MenhirState183 | MenhirState180 | MenhirState157 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let b = _v in
                let _startpos_b_ = _startpos in
                let _endpos_b_ = _endpos in
                let _startpos = _startpos_b_ in
                let _endpos = _endpos_b_ in
                let _v : (Ast.dinst) =            ( Ibloc b ) in
                _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
            | MenhirState53 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let y = _v in
                let _startpos_y_ = _startpos in
                let _endpos_y_ = _endpos in
                let (_menhir_stack, _menhir_s, x, _startpos_x_) = _menhir_stack in
                let _startpos = _startpos_x_ in
                let _endpos = _endpos_y_ in
                let _v : (Ast.ddecl) =                        ( Db (x,y)) in
                _menhir_goto_ddecl _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x, _endpos_x_), _, xs) = _menhir_stack in
        let _v : (Ast.inst list) =     ( x :: xs ) in
        _menhir_goto_list_inst_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run132 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | CHAINE _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState132 in
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
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132

and _menhir_goto_dexpr_str : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.dexpr_str) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.expr_str) =         ( {v = x ; loc=(_startpos, _endpos) }) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (Ast.expr_str) =                           ( x ) in
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
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState138
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.expr_str list) =     ( [ x ] ) in
        _menhir_goto_nonempty_list_vinst_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState178 | MenhirState163 | MenhirState155 | MenhirState152 | MenhirState65 | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ast.expr list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
        let _v : (Ast.expr list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_run96 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96

and _menhir_run98 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98

and _menhir_run92 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92

and _menhir_run100 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100

and _menhir_run102 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102

and _menhir_run104 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104

and _menhir_run106 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

and _menhir_run108 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108

and _menhir_run110 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110

and _menhir_run94 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

and _menhir_run116 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116

and _menhir_run112 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
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

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | RPAR ->
        _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> Lexing.position -> 'ttv_return =
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

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
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

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> Lexing.position -> 'ttv_return =
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
    | MenhirState78 ->
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
    | MenhirState65 ->
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
    | MenhirState152 ->
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
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState154 in
                let _startpos = _menhir_env._menhir_startp in
                let _endpos = _menhir_env._menhir_endp in
                let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | DECR ->
                    _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | FALSE ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | IDENT _v ->
                    _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | INCR ->
                    _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | INTEGER _v ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | LAND ->
                    _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_startp
                | LPAR ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_startp
                | MINUS ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_startp
                | NEW ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_startp
                | NOT ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_startp
                | NULL ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | PLUS ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_startp
                | THIS ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | TIDENT _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v _menhir_env._menhir_startp
                | TIMES ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_startp
                | TRUE ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_startp _menhir_env._menhir_endp
                | RPAR ->
                    _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState155
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
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
    | MenhirState155 ->
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
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp
            | LAND ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState163 ->
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
    | MenhirState178 ->
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
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp
            | LAND ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.inst list) =     ( [] ) in
    _menhir_goto_list_inst_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FALSE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IDENT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INCR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INTEGER _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
        | LPAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
        | THIS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run130 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COLON ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
    | IDENT _ | LAND | TIMES ->
        _menhir_reduce113 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run131 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | CHEVRON ->
        _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131

and _menhir_run142 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dinst) =             ( Nothing ) in
    _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run143 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState143 in
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
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143

and _menhir_run147 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FALSE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IDENT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INCR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INTEGER _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
        | LPAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
        | THIS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run151 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FALSE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IDENT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INCR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INTEGER _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp
        | LPAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp
        | THIS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | SEMICOLON ->
            _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152)
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
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.expr) =             ( { dexpr = x ; loc = _startpos, _endpos } ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    match _menhir_s with
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState152 | MenhirState178 | MenhirState155 | MenhirState163 | MenhirState65 | MenhirState114 | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _startpos__2_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                                                 (Mul)
            in
                                                (  Eop (y,x,z) ) in
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
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | MINUS | NEQ | OR | PLUS | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _startpos__2_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                     (Add)
            in
                                                (  Eop (y,x,z) ) in
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
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                                                                            (Mod)
            in
                                                (  Eop (y,x,z) ) in
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
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                                                             (Div)
            in
                                                (  Eop (y,x,z) ) in
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
        | AND ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | ASSIGN | CHEVRON | COMMA | OR | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                               (Or)
            in
                                                (  Eop (y,x,z) ) in
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
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | NEQ | OR | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                             (Neq)
            in
                                                (  Eop (y,x,z) ) in
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
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | MINUS | NEQ | OR | PLUS | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _startpos__2_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                                   (Sub)
            in
                                                (  Eop (y,x,z) ) in
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
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                                       (Lt)
            in
                                                (  Eop (y,x,z) ) in
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
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                                                 (Le)
            in
                                                (  Eop (y,x,z) ) in
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
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                                                           (Gt)
            in
                                                (  Eop (y,x,z) ) in
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
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | GE | GT | LE | LT | NEQ | OR | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                                                                     (Ge)
            in
                                                (  Eop (y,x,z) ) in
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
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | EQ | NEQ | OR | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                   (Eq)
            in
                                                (  Eop (y,x,z) ) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | AND | ASSIGN | CHEVRON | COMMA | OR | RPAR | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, z, _startpos_z_, _endpos_z_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_z_ in
            let _v : (Ast.dexpr) = let y =
                    (And)
            in
                                                (  Eop (y,x,z) ) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
            | LAND ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | LAND ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150)
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 | MenhirState188 | MenhirState129 | MenhirState150 | MenhirState183 | MenhirState180 | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _startpos, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | RPAR ->
                _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState178
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178)
        | TIMES ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
    | MenhirState206 | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, x) = _menhir_stack in
        let _v : (Ast.qvar) =                   ( Qad x  ) in
        _menhir_goto_qvar _menhir_env _menhir_stack _menhir_s _v
    | MenhirState205 | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, x) = _menhir_stack in
        let _v : (Ast.qvar) =                    ( Qpo x ) in
        _menhir_goto_qvar _menhir_env _menhir_stack _menhir_s _v
    | MenhirState204 | MenhirState33 ->
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
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_startp
            | TIDENT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v _menhir_env._menhir_startp
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_startp
            | RPAR ->
                _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
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
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.inst) =         ( {v = x ; loc=(_startpos, _endpos) }) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _endpos_x_ = _endpos in
    let _endpos = _endpos_x_ in
    let _v : (Ast.inst) =                       ( x ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _endpos) in
    match _menhir_s with
    | MenhirState157 ->
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
    | MenhirState180 ->
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
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp
            | LAND ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183)
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
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, e, _startpos_e_, _endpos_e_), _endpos__4_), _, i, _endpos_i_), _, y, _endpos_y_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_y_ in
        let _v : (Ast.dinst) = (Ifelse (e,i,y) ) in
        _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, e, _startpos_e_, _endpos_e_), _endpos__4_), _, i, _endpos_i_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_i_ in
        let _v : (Ast.dinst) =                                              ( While (e,i) ) in
        _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState188 | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FALSE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FOR ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp
        | IDENT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IF ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp
        | INCR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp
        | INTEGER _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LACC ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp
        | LAND ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp
        | LPAR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp
        | RETURN ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp
        | SEMICOLON ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | STDCOUT ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp
        | THIS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp
        | WHILE ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _menhir_env._menhir_startp
        | RACC ->
            _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188)
    | _ ->
        _menhir_fail ()

and _menhir_reduce81 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expr list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dexpr) =        ( Ebool true ) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
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
    let _v : (Ast.dexpr) =        ( Ethis ) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dexpr) =        ( Enull ) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | RPAR ->
                _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
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

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> Lexing.position -> Lexing.position -> 'ttv_return =
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

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dexpr) =         ( Ebool false) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_goto_separated_nonempty_list_COMMA_var_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.var list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x, _endpos_x_), _, xs) = _menhir_stack in
        let _v : (Ast.var list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_var_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState33 ->
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
            let _startpos = _startpos_x_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.decl_v) =                                                                ( Declv (x,y) ) in
            (match _menhir_s with
            | MenhirState197 | MenhirState5 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let x = _v in
                let _startpos_x_ = _startpos in
                let _endpos_x_ = _endpos in
                let _startpos = _startpos_x_ in
                let _endpos = _endpos_x_ in
                let _v : (Ast.ddecl) =                 ( Dv (x) ) in
                _menhir_goto_ddecl _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
            | MenhirState226 | MenhirState202 | MenhirState211 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let x = _v in
                let _startpos_x_ = _startpos in
                let _endpos_x_ = _endpos in
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
    | MenhirState50 | MenhirState28 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ast.arg list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState23 ->
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
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState211 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState211 _v _menhir_env._menhir_startp
    | VIRTUAL ->
        _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState211
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState211 _menhir_env._menhir_startp
    | RACC ->
        _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState211
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState211

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FOR ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run147 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | INCR ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | INTEGER _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LACC ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | LAND ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | SEMICOLON ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STDCOUT ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | WHILE ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | RACC ->
        _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_goto_qident : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.qident) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    match _menhir_s with
    | MenhirState204 | MenhirState205 | MenhirState206 | MenhirState33 | MenhirState34 | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _startpos_x_ = _startpos in
        let _endpos_x_ = _endpos in
        let _v : (Ast.qvar) =              ( Qvar x ) in
        _menhir_goto_qvar _menhir_env _menhir_stack _menhir_s _v
    | MenhirState188 | MenhirState54 | MenhirState129 | MenhirState183 | MenhirState150 | MenhirState180 | MenhirState178 | MenhirState154 | MenhirState157 | MenhirState161 | MenhirState163 | MenhirState155 | MenhirState152 | MenhirState148 | MenhirState143 | MenhirState132 | MenhirState56 | MenhirState58 | MenhirState60 | MenhirState62 | MenhirState65 | MenhirState66 | MenhirState67 | MenhirState68 | MenhirState70 | MenhirState116 | MenhirState114 | MenhirState112 | MenhirState110 | MenhirState108 | MenhirState106 | MenhirState104 | MenhirState102 | MenhirState100 | MenhirState98 | MenhirState96 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState83 | MenhirState78 | MenhirState73 ->
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
    | MenhirState39 | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _endpos_x_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_x_ in
        let _v : (Ast.dvar) =                  ( Ad x ) in
        _menhir_goto_dvar _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState34 | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _endpos_x_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_x_ in
        let _v : (Ast.dvar) =                   ( Po x ) in
        _menhir_goto_dvar _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState11 ->
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
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _menhir_env._menhir_startp
            | TIDENT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v _menhir_env._menhir_startp
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
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
    | MenhirState45 | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IDENT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
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
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState161 in
                let _startpos = _menhir_env._menhir_startp in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | COLON ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
                | LPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _startpos = _menhir_env._menhir_startp in
                    let _menhir_stack = (_menhir_stack, _startpos) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | DECR ->
                        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | FALSE ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | IDENT _v ->
                        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | INCR ->
                        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | INTEGER _v ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | LAND ->
                        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_startp
                    | LPAR ->
                        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_startp
                    | MINUS ->
                        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_startp
                    | NEW ->
                        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_startp
                    | NOT ->
                        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_startp
                    | NULL ->
                        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | PLUS ->
                        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_startp
                    | THIS ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | TIDENT _v ->
                        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v _menhir_env._menhir_startp
                    | TIMES ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_startp
                    | TRUE ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | RPAR ->
                        _menhir_reduce81 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | TIMES ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161)
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

and _menhir_goto_ddecl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ddecl) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.decl) =         ( {v = x ; loc=(_startpos, _endpos) }) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (Ast.decl) =                       ( x ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _menhir_env._menhir_startp
    | INT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v _menhir_env._menhir_startp
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _menhir_env._menhir_startp
    | EOF ->
        _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState197
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState197

and _menhir_goto_proto : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.proto) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState197 | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LACC ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | MenhirState203 ->
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
            let ((_menhir_stack, _menhir_s), _, x, _startpos_x_) = _menhir_stack in
            let _v : (Ast.membre) =                                  ( Mvir (true,x) ) in
            _menhir_goto_member _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState226 | MenhirState211 | MenhirState202 ->
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
            let (_menhir_stack, _menhir_s, x, _startpos_x_) = _menhir_stack in
            let _v : (Ast.membre) =                         ( Mvir (false,x) ) in
            _menhir_goto_member _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position -> 'ttv_return =
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

and _menhir_reduce97 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.qident) =             ( Qident x ) in
    _menhir_goto_qident _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce66 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.dvar) =             ( Ident x) in
    _menhir_goto_dvar _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_goto_ddecl_class : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ddecl_c) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.decl_c) =         ( {v = x ; loc=(_startpos, _endpos) }) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.decl_c) =                             (x) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.ddecl) =                  ( Dc (x) ) in
    _menhir_goto_ddecl _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.arg list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
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
            let _startpos = _startpos_x_ in
            let _v : (Ast.proto) = let z =
              let xs = xs0 in
                  ( xs )
            in
             ( Pshort (x , z)) in
            _menhir_goto_proto _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
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
            let _startpos = _startpos_x_ in
            let _v : (Ast.proto) = let z =
              let xs = xs0 in
                  ( xs )
            in
               ( Pdouble ( x, y, z) ) in
            _menhir_goto_proto _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState50 ->
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
            let _startpos = _startpos_x_ in
            let _v : (Ast.proto) = let z =
              let xs = xs0 in
                  ( xs )
            in
             ( Plong ( x, y, z) ) in
            _menhir_goto_proto _menhir_env _menhir_stack _menhir_s _v _startpos
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

and _menhir_run205 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run206 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205

and _menhir_run206 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run206 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState206

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce97 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COLON ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAR ->
        _menhir_reduce97 _menhir_env (Obj.magic _menhir_stack)
    | COMMA | SEMICOLON ->
        _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack)

and _menhir_goto_list_member_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.membre list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState211 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.membre list) =     ( x :: xs ) in
        _menhir_goto_list_member_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState202 ->
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
                let ((((_menhir_stack, _menhir_s, z, _startpos_z_), _startpos__2_), _, y), _endpos__6_) = _menhir_stack in
                let _startpos = _startpos_z_ in
                let _endpos = _endpos__7_ in
                let _v : (Ast.ddecl_c) = (  Class (z, (Super []),y) ) in
                _menhir_goto_ddecl_class _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
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
    | MenhirState226 ->
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
                let (((((_menhir_stack, _menhir_s, z, _startpos_z_), l), _startpos__3_), _, y), _endpos__7_) = _menhir_stack in
                let _startpos = _startpos_z_ in
                let _endpos = _endpos__8_ in
                let _v : (Ast.ddecl_c) = (  Class (z,l,y) ) in
                _menhir_goto_ddecl_class _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
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
    | MenhirState220 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x, _startpos_x_) = _menhir_stack in
        let _v : (string list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_TIDENT_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState218 ->
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
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _menhir_env._menhir_startp
                    | TIDENT _v ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v _menhir_env._menhir_startp
                    | VIRTUAL ->
                        _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState226
                    | VOID ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _menhir_env._menhir_startp
                    | RACC ->
                        _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState226
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState226)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s, _, _), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, _, _), _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.decl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, y, _startpos_y_), _, x) = _menhir_stack in
            let _startpos = _startpos_y_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.dfichier) =                                             ( {bincludeios = y ; decls =  x} ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let x = _v in
            let _startpos_x_ = _startpos in
            let _endpos_x_ = _endpos in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_x_ in
            let _v : (Ast.fichier) =         ( {v = x ; loc=(_startpos, _endpos) }) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let x = _v in
            let _v : (Ast.fichier) =                          ( x ) in
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
    | MenhirState197 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.decl list) =     ( x :: xs ) in
        _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce113 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, s, _startpos_s_) = _menhir_stack in
    let _startpos = _startpos_s_ in
    let _v : (Ast.typedef) =               ( Tid s ) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_reduce79 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.arg list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce113 _menhir_env (Obj.magic _menhir_stack)

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.typedef) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState50 | MenhirState28 | MenhirState23 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
    | MenhirState226 | MenhirState211 | MenhirState202 | MenhirState197 | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_startp
        | TIDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
    | MenhirState188 | MenhirState54 | MenhirState129 | MenhirState183 | MenhirState150 | MenhirState180 | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158)
    | MenhirState203 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState204 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run206 _menhir_env (Obj.magic _menhir_stack) MenhirState204 _menhir_env._menhir_startp
        | TIDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState204 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState204 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState204)
    | _ ->
        _menhir_fail ()

and _menhir_reduce77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.membre list) =     ( [] ) in
    _menhir_goto_list_member_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run203 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v _menhir_env._menhir_startp
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState203

and _menhir_run219 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | TIDENT _v ->
            _menhir_run219 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState220)
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
    | MenhirState226 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState220 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState218 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState211 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState206 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState204 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState203 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState202 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState197 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState178 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
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
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
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
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _, _), _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.decl list) =     ( [] ) in
    _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (Ast.typedef) =        ( Void ) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
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
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_startp
                    | TIDENT _v ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v _menhir_env._menhir_startp
                    | VOID ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_startp
                    | RPAR ->
                        _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
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
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
        | TIDENT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v _menhir_env._menhir_startp
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
        | RPAR ->
            _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
    | IDENT _ | LAND | TIDENT _ | TIMES ->
        _menhir_reduce113 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (Ast.typedef) =       ( Int ) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
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
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _v : (string) =                      ( Hashtbl.add (Lexerhack.table) z () ; z ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
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
                    _menhir_run219 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v _menhir_env._menhir_startp
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState218)
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
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState202 _menhir_env._menhir_startp
                    | TIDENT _v ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState202 _v _menhir_env._menhir_startp
                    | VIRTUAL ->
                        _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState202
                    | VOID ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState202 _menhir_env._menhir_startp
                    | RACC ->
                        _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState202
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState202)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_boption_INCLUDEIOS_ : _menhir_env -> 'ttv_tail -> (bool) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _v _startpos ->
    let _menhir_stack = (_menhir_stack, _v, _startpos) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_startp
    | INT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v _menhir_env._menhir_startp
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_startp
    | EOF ->
        _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

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
        let _startpos = _menhir_env._menhir_startp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos__1_ = _startpos in
        let _startpos = _startpos__1_ in
        let _v : (bool) =     ( true ) in
        _menhir_goto_boption_INCLUDEIOS_ _menhir_env _menhir_stack _v _startpos
    | CLASS | EOF | INT | TIDENT _ | VOID ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _v : (bool) =     ( false ) in
        _menhir_goto_boption_INCLUDEIOS_ _menhir_env _menhir_stack _v _startpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR)



