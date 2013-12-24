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
  | ADDR

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState228
  | MenhirState222
  | MenhirState221
  | MenhirState220
  | MenhirState219
  | MenhirState218
  | MenhirState210
  | MenhirState208
  | MenhirState204
  | MenhirState192
  | MenhirState187
  | MenhirState172
  | MenhirState170
  | MenhirState167
  | MenhirState166
  | MenhirState164
  | MenhirState162
  | MenhirState160
  | MenhirState158
  | MenhirState156
  | MenhirState151
  | MenhirState146
  | MenhirState140
  | MenhirState139
  | MenhirState137
  | MenhirState124
  | MenhirState122
  | MenhirState120
  | MenhirState118
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
  | MenhirState91
  | MenhirState86
  | MenhirState80
  | MenhirState79
  | MenhirState76
  | MenhirState74
  | MenhirState73
  | MenhirState72
  | MenhirState69
  | MenhirState67
  | MenhirState65
  | MenhirState63
  | MenhirState61
  | MenhirState60
  | MenhirState57
  | MenhirState52
  | MenhirState43
  | MenhirState37
  | MenhirState36
  | MenhirState31
  | MenhirState26
  | MenhirState14
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
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.expr_str list) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_vinst_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState139 ->
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
    | MenhirState61 ->
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
            | MenhirState61 | MenhirState192 | MenhirState137 | MenhirState158 | MenhirState187 | MenhirState166 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let b = _v in
                let _startpos_b_ = _startpos in
                let _endpos_b_ = _endpos in
                let _startpos = _startpos_b_ in
                let _endpos = _endpos_b_ in
                let _v : (Ast.dinst) =            ( Ibloc b ) in
                _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
            | MenhirState60 ->
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
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x, _endpos_x_), _, xs) = _menhir_stack in
        let _v : (Ast.inst list) =     ( x :: xs ) in
        _menhir_goto_list_inst_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run140 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
    | CHAINE _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState140 in
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
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140

and _menhir_reduce97 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expr option) =     ( None ) in
    _menhir_goto_option_expr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState151 ->
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
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, ex) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.dinst) =                                            ( match ex with
						| None -> Areturn
						| Some e -> Return e ) in
            _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState162 ->
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
            | ADDR ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_startp
            | DECR ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAR ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIMES ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | RPAR ->
                _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState164
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

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
        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState146
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.expr_str list) =     ( [ x ] ) in
        _menhir_goto_nonempty_list_vinst_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState172 | MenhirState164 | MenhirState160 | MenhirState72 | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ast.expr list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
        let _v : (Ast.expr list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run91 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_run98 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98

and _menhir_run104 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104

and _menhir_run106 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

and _menhir_run100 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100

and _menhir_run108 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108

and _menhir_run110 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110

and _menhir_run112 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112

and _menhir_run114 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114

and _menhir_run116 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116

and _menhir_run118 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118

and _menhir_run102 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102

and _menhir_run124 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124

and _menhir_run120 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120

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
        let _v : (Ast.dexpr) =                               ( Esderef (x,y) ) in
        _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | RPAR ->
        _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_run93 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> Lexing.position -> 'ttv_return =
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

and _menhir_run94 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
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

and _menhir_run96 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> Lexing.position -> 'ttv_return =
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
    | MenhirState86 ->
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
    | MenhirState72 ->
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
            let ((((_menhir_stack, _menhir_s, _startpos__1_), x, _startpos_x_, _endpos_x_), _startpos__3_), _, xs0) = _menhir_stack in
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
    | MenhirState160 ->
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
            | ADDR ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
            | DECR ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAR ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIMES ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | SEMICOLON ->
                _menhir_reduce97 _menhir_env (Obj.magic _menhir_stack) MenhirState162
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState164 ->
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
            | ADDR ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp
            | DECR ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIMES ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166)
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
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos = _menhir_env._menhir_startp in
                let _endpos = _menhir_env._menhir_endp in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos__8_ = _startpos in
                let _endpos__8_ = _endpos in
                let ((((((_menhir_stack, _menhir_s, t, _startpos_t_), _, v, _endpos_v_), _, s, _startpos_s_, _endpos_s_), _startpos__5_), _, xs0), _endpos__7_) = _menhir_stack in
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
    | _ ->
        _menhir_fail ()

and _menhir_reduce86 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.inst list) =     ( [] ) in
    _menhir_goto_list_inst_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
        | ADDR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
        | DECR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FALSE ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IDENT _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INCR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INTEGER _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | PLUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
        | THIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIMES ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run138 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COLON ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
    | ADDR | IDENT _ | TIMES ->
        _menhir_reduce131 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run139 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | CHEVRON ->
        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139

and _menhir_run150 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dinst) =             ( Nothing ) in
    _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run151 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | SEMICOLON ->
        _menhir_reduce97 _menhir_env (Obj.magic _menhir_stack) MenhirState151
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151

and _menhir_run155 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
        | ADDR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp
        | DECR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FALSE ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IDENT _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INCR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INTEGER _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | PLUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp
        | THIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIMES ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run159 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
        | ADDR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
        | DECR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FALSE ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IDENT _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INCR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INTEGER _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | PLUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
        | THIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIMES ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | SEMICOLON ->
            _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState160
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160)
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
    let _v : (Ast.expr) =         ( {v = x ; loc=(_startpos, _endpos) }) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.expr) =                       ( x ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    match _menhir_s with
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CHEVRON | COMMA | DIV | EQ | GE | GT | LE | LT | MINUS | MODULO | NEQ | OR | PLUS | RPAR | SEMICOLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_x_ in
            let _v : (Ast.dexpr) =                  ( Eaddr x ) in
            _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState160 | MenhirState164 | MenhirState172 | MenhirState72 | MenhirState122 | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ADDR ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp
            | DECR ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAR ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIMES ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ADDR ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
            | DECR ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIMES ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137)
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
    | MenhirState162 | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _v : (Ast.expr option) =     ( Some x ) in
            _menhir_goto_option_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ADDR ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | DECR ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIMES ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158)
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState61 | MenhirState192 | MenhirState137 | MenhirState158 | MenhirState187 | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack)
        | DECR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | DIV ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | INCR ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LE ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack)
        | LPAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | LT ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | MODULO ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | SDEREF ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_dqvar : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.dqvar) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.qvar) =         ( {v = x ; loc=(_startpos, _endpos) }) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _endpos_x_ = _endpos in
    let _endpos = _endpos_x_ in
    let _v : (Ast.qvar) =                       ( x ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _endpos) in
    match _menhir_s with
    | MenhirState222 | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _endpos_x_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_x_ in
        let _v : (Ast.dqvar) =                   ( Qad x  ) in
        _menhir_goto_dqvar _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState221 | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _endpos_x_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_x_ in
        let _v : (Ast.dqvar) =                    ( Qpo x ) in
        _menhir_goto_dqvar _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState220 | MenhirState36 ->
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
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_startp
            | TIDENT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_startp
            | RPAR ->
                _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
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
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, xs0), _startpos__4_, _endpos__4_), _, x), _startpos__6_, _endpos__6_), _, xs1), _endpos__8_), _, i, _endpos_i_) = _menhir_stack in
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
                                                             ( match x with
							| None -> Afor (e,f,i)
							| Some ex -> For (e,ex,f,i) ) in
        _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ADDR ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp
            | DECR ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIMES ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187)
        | ADDR | DECR | FALSE | FOR | IDENT _ | IF | INCR | INT | INTEGER _ | LACC | LPAR | MINUS | NEW | NOT | NULL | PLUS | RACC | RETURN | SEMICOLON | STDCOUT | THIS | TIDENT _ | TIMES | TRUE | VOID | WHILE ->
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
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, e, _startpos_e_, _endpos_e_), _endpos__4_), _, i, _endpos_i_), _, y, _endpos_y_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_y_ in
        let _v : (Ast.dinst) = (Ifelse (e,i,y) ) in
        _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, e, _startpos_e_, _endpos_e_), _endpos__4_), _, i, _endpos_i_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_i_ in
        let _v : (Ast.dinst) =                                              ( While (e,i) ) in
        _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState192 | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADDR ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp
        | DECR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FALSE ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FOR ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp
        | IDENT _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IF ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp
        | INCR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INT ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp
        | INTEGER _v ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LACC ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp
        | LPAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | PLUS ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp
        | RETURN ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp
        | SEMICOLON ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | STDCOUT ->
            _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp
        | THIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIMES ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp
        | WHILE ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp
        | RACC ->
            _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState192)
    | _ ->
        _menhir_fail ()

and _menhir_reduce92 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expr list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dexpr) =        ( Ebool true ) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dexpr) =        ( Ethis ) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dexpr) =        ( Enull ) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | TIDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | LPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ADDR ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp
            | DECR ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAR ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIMES ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | RPAR ->
                _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> Lexing.position -> Lexing.position -> 'ttv_return =
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

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dexpr) =         ( Ebool false) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run80 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_goto_separated_nonempty_list_COMMA_var_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.var list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x, _endpos_x_), _, xs) = _menhir_stack in
        let _v : (Ast.var list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_var_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState36 ->
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
            let _v : (Ast.ddecl_v) =                                                                ( Declv (x,y) ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let x = _v in
            let _startpos_x_ = _startpos in
            let _endpos_x_ = _endpos in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_x_ in
            let _v : (Ast.decl_v) =         ( {v = x ; loc=(_startpos, _endpos) }) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let x = _v in
            let _startpos_x_ = _startpos in
            let _endpos_x_ = _endpos in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_x_ in
            let _v : (Ast.decl_v) =                            ( x ) in
            (match _menhir_s with
            | MenhirState204 | MenhirState5 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let x = _v in
                let _startpos_x_ = _startpos in
                let _endpos_x_ = _endpos in
                let _startpos = _startpos_x_ in
                let _endpos = _endpos_x_ in
                let _v : (Ast.ddecl) =                 ( Dv (x) ) in
                _menhir_goto_ddecl _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
            | MenhirState218 | MenhirState228 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let x = _v in
                let _startpos_x_ = _startpos in
                let _endpos_x_ = _endpos in
                let _startpos = _startpos_x_ in
                let _endpos = _endpos_x_ in
                let _v : (Ast.dmembre) =                 ( Mvar (x) ) in
                _menhir_goto_dmember _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
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
    | MenhirState57 | MenhirState31 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ast.arg list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.arg list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_argument_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_dmember : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.dmembre) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.membre) =         ( {v = x ; loc=(_startpos, _endpos) }) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (Ast.membre) =                         ( x ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | VIRTUAL ->
        _menhir_run219 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _menhir_env._menhir_startp
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _menhir_env._menhir_startp
    | RACC ->
        _menhir_reduce88 _menhir_env (Obj.magic _menhir_stack) MenhirState228
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState228

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | DECR ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FOR ->
        _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | INCR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | INTEGER _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LACC ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | LPAR ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | MINUS ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | NEW ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | NOT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | PLUS ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | SEMICOLON ->
        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STDCOUT ->
        _menhir_run139 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | WHILE ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp
    | RACC ->
        _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_goto_dqident : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.dqident) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.qident) =         ( {v = x ; loc=(_startpos, _endpos) }) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.qident) =                         ( x ) in
    match _menhir_s with
    | MenhirState220 | MenhirState221 | MenhirState222 | MenhirState36 | MenhirState37 | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _startpos_x_ = _startpos in
        let _endpos_x_ = _endpos in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : (Ast.dqvar) =              ( Qvar x ) in
        _menhir_goto_dqvar _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState192 | MenhirState61 | MenhirState137 | MenhirState187 | MenhirState158 | MenhirState166 | MenhirState170 | MenhirState172 | MenhirState164 | MenhirState162 | MenhirState160 | MenhirState156 | MenhirState151 | MenhirState140 | MenhirState63 | MenhirState65 | MenhirState67 | MenhirState69 | MenhirState72 | MenhirState73 | MenhirState74 | MenhirState76 | MenhirState79 | MenhirState124 | MenhirState122 | MenhirState120 | MenhirState118 | MenhirState116 | MenhirState114 | MenhirState112 | MenhirState110 | MenhirState108 | MenhirState106 | MenhirState104 | MenhirState102 | MenhirState100 | MenhirState98 | MenhirState91 | MenhirState86 | MenhirState80 ->
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
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.var) =         ( {v = x ; loc=(_startpos, _endpos) }) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _endpos_x_ = _endpos in
    let _endpos = _endpos_x_ in
    let _v : (Ast.var) =                      ( x ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _endpos) in
    match _menhir_s with
    | MenhirState43 | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _endpos_x_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_x_ in
        let _v : (Ast.dvar) =                  ( Ad x ) in
        _menhir_goto_dvar _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState37 | MenhirState12 ->
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
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_y_ in
        let _v : (Ast.darg) =                     ( Arg (x,y) ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _startpos_x_ = _startpos in
        let _endpos_x_ = _endpos in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : (Ast.arg) =         ( {v = x ; loc=(_startpos, _endpos) }) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ast.arg) =                           ( x ) in
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
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_startp
            | TIDENT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
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
    | MenhirState52 | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ADDR ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIMES ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
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
    | MenhirState167 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ADDR ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | DECR ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAR ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState170 in
                let _startpos = _menhir_env._menhir_startp in
                let _endpos = _menhir_env._menhir_endp in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | COLON ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
                | LPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _startpos = _menhir_env._menhir_startp in
                    let _menhir_stack = (_menhir_stack, _startpos) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | ADDR ->
                        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
                    | DECR ->
                        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | FALSE ->
                        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | IDENT _v ->
                        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | INCR ->
                        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | INTEGER _v ->
                        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | LPAR ->
                        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
                    | MINUS ->
                        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
                    | NEW ->
                        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
                    | NOT ->
                        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
                    | NULL ->
                        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | PLUS ->
                        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
                    | THIS ->
                        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | TIDENT _v ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | TIMES ->
                        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp
                    | TRUE ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | RPAR ->
                        _menhir_reduce92 _menhir_env (Obj.magic _menhir_stack) MenhirState172
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | TIMES ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170)
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
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState204 _menhir_env._menhir_startp
    | INT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState204 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState204 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState204 _menhir_env._menhir_startp
    | EOF ->
        _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState204
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState204

and _menhir_goto_dproto : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.dproto) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.proto) =         ( {v = x ; loc=(_startpos, _endpos) }) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _startpos = _startpos_x_ in
    let _v : (Ast.proto) =                        ( x ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState204 | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LACC ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState219 ->
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
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, x, _startpos_x_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.dmembre) =                                  ( Mvir (true,x) ) in
            _menhir_goto_dmember _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState228 | MenhirState218 ->
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
            let _startpos = _startpos_x_ in
            let _endpos = _endpos__2_ in
            let _v : (Ast.dmembre) =                         ( Mvir (false,x) ) in
            _menhir_goto_dmember _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _menhir_stack = (_menhir_stack, _startpos) in
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
            let (((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _startpos__2_), _startpos__3_) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_y_ in
            let _v : (Ast.dqident) =                                          (  Double (x,y) ) in
            _menhir_goto_dqident _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _, _, _), _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce71 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.dqident) =             ( Qident x ) in
    _menhir_goto_dqident _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce77 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.dvar) =             ( Ident x) in
    _menhir_goto_dvar _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_goto_list_member_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.membre list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState228 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.membre list) =     ( x :: xs ) in
        _menhir_goto_list_member_ _menhir_env _menhir_stack _menhir_s _v
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
                let ((((((_menhir_stack, _menhir_s, z, _startpos_z_), l), _startpos__3_), _startpos__5_), _, y), _endpos__7_) = _menhir_stack in
                let _startpos = _startpos_z_ in
                let _endpos = _endpos__8_ in
                let _v : (Ast.ddecl_c) = (  Class (z,l,y) ) in
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
            let (((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _startpos__2_), _, xs0) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos__4_ in
            let _v : (Ast.dproto) = let z =
              let xs = xs0 in
                  ( xs )
            in
             ( Pshort (x , z)) in
            _menhir_goto_dproto _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
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
            let ((((((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _startpos__2_), _startpos__3_), y, _startpos_y_, _endpos_y_), _startpos__5_), _, xs0) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos__7_ in
            let _v : (Ast.dproto) = let z =
              let xs = xs0 in
                  ( xs )
            in
               ( Pstatic ( x, y, z) ) in
            _menhir_goto_dproto _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState57 ->
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
            let ((((_menhir_stack, _menhir_s, x, _startpos_x_), _, y, _endpos_y_), _startpos__3_), _, xs0) = _menhir_stack in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos__5_ in
            let _v : (Ast.dproto) = let z =
              let xs = xs0 in
                  ( xs )
            in
             ( Plong ( x, y, z) ) in
            _menhir_goto_dproto _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
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

and _menhir_run221 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run222 _menhir_env (Obj.magic _menhir_stack) MenhirState221 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState221 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState221 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState221 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState221

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run222 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run222 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState222

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COLON ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COMMA | SEMICOLON ->
        _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack)
    | LPAR ->
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ADDR ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIMES ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_reduce88 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.membre list) =     ( [] ) in
    _menhir_goto_list_member_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run219 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState219 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState219

and _menhir_goto_separated_nonempty_list_COMMA_TIDENT_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _endpos ->
    match _menhir_s with
    | MenhirState210 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let _endpos_xs_ = _endpos in
        let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
        let _endpos = _endpos_xs_ in
        let _v : (string list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_TIDENT_ _menhir_env _menhir_stack _menhir_s _v _endpos
    | MenhirState208 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let z = _v in
        let _endpos_z_ = _endpos in
        let (_menhir_stack, _startpos__1_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_z_ in
        let _v : (Ast.dsupers) =                                                            ( Super z ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _startpos_x_ = _startpos in
        let _endpos_x_ = _endpos in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : (Ast.supers) =         ( {v = x ; loc=(_startpos, _endpos) }) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _startpos_x_ = _startpos in
        let _endpos_x_ = _endpos in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : (Ast.supers) =                         ( x ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _startpos_x_ = _startpos in
        let _endpos_x_ = _endpos in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : (Ast.supers option) =     ( Some x ) in
        _menhir_goto_option_supers_ _menhir_env _menhir_stack _v _startpos _endpos
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
    | MenhirState204 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.decl list) =     ( x :: xs ) in
        _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce131 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, s, _startpos_s_, _endpos_s_) = _menhir_stack in
    let _startpos = _startpos_s_ in
    let _v : (Ast.typedef) =               ( Tid s ) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_reduce90 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.arg list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce131 _menhir_env (Obj.magic _menhir_stack)

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.typedef) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState57 | MenhirState31 | MenhirState26 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADDR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp
        | IDENT _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIMES ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
    | MenhirState228 | MenhirState218 | MenhirState204 | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADDR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | IDENT _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIMES ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | MenhirState192 | MenhirState61 | MenhirState137 | MenhirState187 | MenhirState158 | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADDR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_startp
        | IDENT _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIMES ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState167 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState167)
    | MenhirState219 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADDR ->
            _menhir_run222 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_startp
        | IDENT _v ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIMES ->
            _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState220)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_supers_ : _menhir_env -> 'ttv_tail -> (Ast.supers option) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _v _startpos _endpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _startpos_x_ = _startpos in
    let _endpos_x_ = _endpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.supers) =                      ( match x with | None -> { v = (Super []) ; loc=(_startpos,_endpos) }
				    | Some a -> a  
		     ) in
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
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
                let _startpos = _menhir_env._menhir_startp in
                let _menhir_stack = (_menhir_stack, _startpos) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | INT ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _menhir_env._menhir_startp
                | TIDENT _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | VIRTUAL ->
                    _menhir_run219 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _menhir_env._menhir_startp
                | VOID ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _menhir_env._menhir_startp
                | RACC ->
                    _menhir_reduce88 _menhir_env (Obj.magic _menhir_stack) MenhirState218
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState218)
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run209 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | TIDENT _v ->
            _menhir_run209 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState210)
    | LACC ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
        let _endpos = _endpos_x_ in
        let _v : (string list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_TIDENT_ _menhir_env _menhir_stack _menhir_s _v _endpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState228 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState222 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState221 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState220 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState219 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState218 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s, _, _), _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState210 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState208 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState204 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState167 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
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
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
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
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, _, _, _), _), _), _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
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
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce84 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | TIDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos = _menhir_env._menhir_startp in
                let _endpos = _menhir_env._menhir_endp in
                let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | LPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _startpos = _menhir_env._menhir_startp in
                    let _menhir_stack = (_menhir_stack, _startpos) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | INT ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_startp
                    | TIDENT _v ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                    | VOID ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_startp
                    | RPAR ->
                        _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((((_menhir_stack, _menhir_s, _, _, _), _), _), _, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, _, _, _), _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
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
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | VOID ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
        | RPAR ->
            _menhir_reduce90 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
    | ADDR | IDENT _ | TIDENT _ | TIMES ->
        _menhir_reduce131 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (Ast.typedef) =       ( Int ) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | PUBLIC ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | TIDENT _v ->
                    _menhir_run209 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState208)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | LACC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _startpos in
            let _v : (Ast.supers option) =     ( None ) in
            _menhir_goto_option_supers_ _menhir_env _menhir_stack _v _startpos _endpos
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
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_startp
    | INT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | VOID ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_startp
    | EOF ->
        _menhir_reduce84 _menhir_env (Obj.magic _menhir_stack) MenhirState5
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



