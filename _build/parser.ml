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
  | MenhirState214
  | MenhirState208
  | MenhirState206
  | MenhirState199
  | MenhirState194
  | MenhirState193
  | MenhirState192
  | MenhirState191
  | MenhirState190
  | MenhirState185
  | MenhirState178
  | MenhirState173
  | MenhirState170
  | MenhirState168
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

  
  open Ast
 (* open Lexer*)
  (*open Lexerhack*)
let _eRR =
  Error

let rec _menhir_goto_nonempty_list_vinst_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr_str list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.expr_str list) =     ( x :: xs ) in
        _menhir_goto_nonempty_list_vinst_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState126 ->
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
    | MenhirState49 ->
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
            | MenhirState49 | MenhirState178 | MenhirState124 | MenhirState143 | MenhirState173 | MenhirState170 | MenhirState150 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let b = _v in
                let _startpos_b_ = _startpos in
                let _endpos_b_ = _endpos in
                let _startpos = _startpos_b_ in
                let _endpos = _endpos_b_ in
                let _v : (Ast.dinst) =            ( Ibloc b ) in
                _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
            | MenhirState48 ->
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
    | MenhirState178 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x, _endpos_x_), _, xs) = _menhir_stack in
        let _v : (Ast.inst list) =     ( x :: xs ) in
        _menhir_goto_list_inst_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run127 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | CHAINE _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState127 in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ast.expr_str) =              ( Estring x) in
        _menhir_goto_expr_str _menhir_env _menhir_stack _menhir_s _v
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127

and _menhir_goto_expr_str : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr_str) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
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
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.expr_str list) =     ( [ x ] ) in
        _menhir_goto_nonempty_list_vinst_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState168 | MenhirState156 | MenhirState148 | MenhirState145 | MenhirState60 | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ast.expr list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
        let _v : (Ast.expr list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run91 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_run93 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_run87 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run95 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95

and _menhir_run97 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_run99 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99

and _menhir_run101 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101

and _menhir_run103 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_run105 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_run89 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run111 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111

and _menhir_run107 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
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

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> Lexing.position -> 'ttv_return =
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

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
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

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> Lexing.position -> Lexing.position -> 'ttv_return =
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
    | MenhirState73 ->
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
    | MenhirState60 ->
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
    | MenhirState145 ->
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
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState147 in
                let _startpos = _menhir_env._menhir_startp in
                let _endpos = _menhir_env._menhir_endp in
                let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
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
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148)
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState148 ->
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
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
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
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState156 ->
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
                let ((((((_menhir_stack, _menhir_s, t, _startpos_t_), _, v), _, s, _startpos_s_), _startpos__5_), _, xs0), _endpos__7_) = _menhir_stack in
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
    | MenhirState168 ->
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
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.inst list) =     ( [] ) in
    _menhir_goto_list_inst_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run125 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COLON ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
    | IDENT _ | LAND | TIMES ->
        _menhir_reduce98 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run126 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | CHEVRON ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126

and _menhir_run135 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dinst) =             ( Nothing ) in
    _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run136 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState136 in
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136

and _menhir_run140 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
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
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState145 | MenhirState168 | MenhirState148 | MenhirState156 | MenhirState60 | MenhirState109 | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
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
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
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
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
            let _v : (Ast.expr_str) =            ( Esexpr x ) in
            _menhir_goto_expr_str _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
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
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
        | SDEREF ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos__5_ = _startpos in
            let _endpos__5_ = _endpos in
            let (((_menhir_stack, _menhir_s, t, _startpos_t_), _, v), _, e, _startpos_e_, _endpos_e_) = _menhir_stack in
            let _startpos = _startpos_t_ in
            let _endpos = _endpos__5_ in
            let _v : (Ast.dinst) =                                                 ( Idecl (t,v,e) ) in
            _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState49 | MenhirState178 | MenhirState124 | MenhirState143 | MenhirState173 | MenhirState170 | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _startpos, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | RPAR ->
                _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState168
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168)
        | TIMES ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
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
    | MenhirState194 | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, x) = _menhir_stack in
        let _v : (Ast.qvar) =                   ( Qad x  ) in
        _menhir_goto_qvar _menhir_env _menhir_stack _menhir_s _v
    | MenhirState193 | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, x) = _menhir_stack in
        let _v : (Ast.qvar) =                    ( Qpo x ) in
        _menhir_goto_qvar _menhir_env _menhir_stack _menhir_s _v
    | MenhirState192 | MenhirState28 ->
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
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_startp
            | TIDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v _menhir_env._menhir_startp
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_startp
            | RPAR ->
                _menhir_reduce70 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
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
    | MenhirState150 ->
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
    | MenhirState170 ->
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
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DECR ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FALSE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | FOR ->
                _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
            | IDENT _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
            | INCR ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
            | INTEGER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LACC ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
            | LAND ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
            | LPAR ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
            | MINUS ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
            | NEW ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
            | NOT ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | PLUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
            | SEMICOLON ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | STDCOUT ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
            | THIS ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TIDENT _v ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState173)
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
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, e, _startpos_e_, _endpos_e_), _endpos__4_), _, i, _endpos_i_), _, y, _endpos_y_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_y_ in
        let _v : (Ast.dinst) = (Ifelse (e,i,y) ) in
        _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, e, _startpos_e_, _endpos_e_), _endpos__4_), _, i, _endpos_i_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_i_ in
        let _v : (Ast.dinst) =                                              ( While (e,i) ) in
        _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState178 | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DECR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FALSE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | FOR ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
        | IDENT _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IF ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
        | INCR ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
        | INTEGER _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LACC ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
        | LAND ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
        | LPAR ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
        | NEW ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
        | NOT ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | PLUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
        | RETURN ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
        | SEMICOLON ->
            _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | STDCOUT ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
        | THIS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | TIDENT _v ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
        | TRUE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp _menhir_env._menhir_endp
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
        | WHILE ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_startp
        | RACC ->
            _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178)
    | _ ->
        _menhir_fail ()

and _menhir_reduce72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expr list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dexpr) =        ( Ebool true ) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dexpr) =        ( Ethis ) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dexpr) =        ( Enull ) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
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
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
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

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> Lexing.position -> Lexing.position -> 'ttv_return =
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

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.dexpr) =         ( Ebool false) in
    _menhir_goto_dexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_goto_separated_nonempty_list_COMMA_var_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.var list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.var list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_var_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState28 ->
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
            | MenhirState0 | MenhirState185 | MenhirState25 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let x = _v in
                let _v : (Ast.decl) =                 ( Dv (x) ) in
                _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
            | MenhirState214 | MenhirState190 | MenhirState199 ->
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
    | MenhirState45 | MenhirState22 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ast.arg list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState17 ->
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
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v _menhir_env._menhir_startp
    | VIRTUAL ->
        _menhir_run191 _menhir_env (Obj.magic _menhir_stack) MenhirState199
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _menhir_env._menhir_startp
    | RACC ->
        _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState199
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState199

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DECR ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FALSE ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | FOR ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | INCR ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | INTEGER _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LACC ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
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
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | SEMICOLON ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | STDCOUT ->
        _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | THIS ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | TIDENT _v ->
        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | TRUE ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | WHILE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_startp
    | RACC ->
        _menhir_reduce66 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_goto_qident : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.qident) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    match _menhir_s with
    | MenhirState192 | MenhirState193 | MenhirState194 | MenhirState28 | MenhirState29 | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _startpos_x_ = _startpos in
        let _endpos_x_ = _endpos in
        let _v : (Ast.qvar) =              ( Qvar x ) in
        _menhir_goto_qvar _menhir_env _menhir_stack _menhir_s _v
    | MenhirState178 | MenhirState49 | MenhirState124 | MenhirState173 | MenhirState143 | MenhirState170 | MenhirState168 | MenhirState147 | MenhirState150 | MenhirState154 | MenhirState156 | MenhirState148 | MenhirState145 | MenhirState141 | MenhirState136 | MenhirState127 | MenhirState51 | MenhirState53 | MenhirState55 | MenhirState57 | MenhirState60 | MenhirState61 | MenhirState62 | MenhirState63 | MenhirState65 | MenhirState111 | MenhirState109 | MenhirState107 | MenhirState105 | MenhirState103 | MenhirState101 | MenhirState99 | MenhirState97 | MenhirState95 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState85 | MenhirState78 | MenhirState73 | MenhirState68 ->
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

and _menhir_goto_var : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.var) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState34 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, x) = _menhir_stack in
        let _v : (Ast.var) =                  ( Ad x ) in
        _menhir_goto_var _menhir_env _menhir_stack _menhir_s _v
    | MenhirState29 | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, x) = _menhir_stack in
        let _v : (Ast.var) =                   ( Po x ) in
        _menhir_goto_var _menhir_env _menhir_stack _menhir_s _v
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x, _startpos_x_), _, y) = _menhir_stack in
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
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
            | TIDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v _menhir_env._menhir_startp
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
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
    | MenhirState40 | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IDENT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LAND ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_startp
            | TIMES ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (Ast.var list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_var_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
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
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState154 in
                let _startpos = _menhir_env._menhir_startp in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | COLON ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
                | LPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _startpos = _menhir_env._menhir_startp in
                    let _menhir_stack = (_menhir_stack, _startpos) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
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
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | TIMES ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp
            | TRUE ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos__3_ = _startpos in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, t, _startpos_t_), _, v) = _menhir_stack in
            let _startpos = _startpos_t_ in
            let _endpos = _endpos__3_ in
            let _v : (Ast.dinst) =                                ( Idecls (t,v) ) in
            _menhir_goto_dinst _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState185
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v _menhir_env._menhir_startp
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_startp
    | EOF ->
        _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState185
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185

and _menhir_goto_proto : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.proto) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState185 | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LACC ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
    | MenhirState191 ->
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
    | MenhirState214 | MenhirState199 | MenhirState190 ->
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

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position -> 'ttv_return =
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

and _menhir_reduce82 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (Ast.qident) =             ( Qident x ) in
    _menhir_goto_qident _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce99 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
    let _v : (Ast.var) =             ( Ident x) in
    _menhir_goto_var _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_decl_class : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.decl_c) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (Ast.decl) =                  ( Dc (x) ) in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_fichier : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.fichier) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    Obj.magic _1

and _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.arg list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
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
    | MenhirState22 ->
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
    | MenhirState45 ->
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

and _menhir_run193 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run194 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState193

and _menhir_run194 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run194 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState194

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COLON ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COMMA | SEMICOLON ->
        _menhir_reduce99 _menhir_env (Obj.magic _menhir_stack)
    | LPAR ->
        _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LAND ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | TIMES ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce99 _menhir_env (Obj.magic _menhir_stack)

and _menhir_goto_list_member_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.membre list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState199 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.membre list) =     ( x :: xs ) in
        _menhir_goto_list_member_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState190 ->
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
    | MenhirState214 ->
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
    | MenhirState208 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x, _startpos_x_) = _menhir_stack in
        let _v : (string list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_TIDENT_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState206 ->
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
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState214 _menhir_env._menhir_startp
                    | TIDENT _v ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState214 _v _menhir_env._menhir_startp
                    | VIRTUAL ->
                        _menhir_run191 _menhir_env (Obj.magic _menhir_stack) MenhirState214
                    | VOID ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState214 _menhir_env._menhir_startp
                    | RACC ->
                        _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState214
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState214)
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
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, x) = _menhir_stack in
            let _v : (Ast.fichier) =                                  ( {bincludeios = true ; decls =  x} ) in
            _menhir_goto_fichier _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ast.decl list) =     ( x :: xs ) in
        _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (Ast.fichier) =                    ( {bincludeios = false ; decls =  x }) in
            _menhir_goto_fichier _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce98 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, s, _startpos_s_) = _menhir_stack in
    let _startpos = _startpos_s_ in
    let _v : (Ast.typedef) =               ( Tid s ) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_reduce70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.arg list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_argument__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce98 _menhir_env (Obj.magic _menhir_stack)

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.typedef) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState45 | MenhirState22 | MenhirState17 | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
    | MenhirState0 | MenhirState214 | MenhirState199 | MenhirState190 | MenhirState185 | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
    | MenhirState178 | MenhirState49 | MenhirState124 | MenhirState173 | MenhirState143 | MenhirState170 | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151)
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LAND ->
            _menhir_run194 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp
        | TIDENT _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _v _menhir_env._menhir_startp
        | TIMES ->
            _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState192)
    | _ ->
        _menhir_fail ()

and _menhir_reduce68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.membre list) =     ( [] ) in
    _menhir_goto_list_member_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run191 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v _menhir_env._menhir_startp
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191

and _menhir_run207 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | TIDENT _v ->
            _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState208)
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
    | MenhirState214 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState208 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState206 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState199 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState194 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState193 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState178 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _, _), _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.decl list) =     ( [] ) in
    _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (Ast.typedef) =        ( Void ) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
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
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_startp
                    | TIDENT _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v _menhir_env._menhir_startp
                    | VOID ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_startp
                    | RPAR ->
                        _menhir_reduce70 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
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
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _menhir_env._menhir_startp
        | TIDENT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v _menhir_env._menhir_startp
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _menhir_env._menhir_startp
        | RPAR ->
            _menhir_reduce70 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
    | IDENT _ | LAND | TIDENT _ | TIMES ->
        _menhir_reduce98 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (Ast.typedef) =       ( Int ) in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v _startpos

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
                    _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _v _menhir_env._menhir_startp
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState206)
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
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _menhir_env._menhir_startp
                    | TIDENT _v ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v _menhir_env._menhir_startp
                    | VIRTUAL ->
                        _menhir_run191 _menhir_env (Obj.magic _menhir_stack) MenhirState190
                    | VOID ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _menhir_env._menhir_startp
                    | RACC ->
                        _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack) MenhirState190
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState190)
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
    | CLASS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INCLUDEIOS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | CLASS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | INT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_startp
        | TIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v _menhir_env._menhir_startp
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _menhir_env._menhir_startp
        | EOF ->
            _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
    | INT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | TIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v _menhir_env._menhir_startp
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | EOF ->
        _menhir_reduce64 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



