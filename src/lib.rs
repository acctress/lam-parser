use iota::{
    IotaParser, Expression, ParseValue,
    action, alpha, alphanum, between, ch, digit, ignore, lazy,
    lit, one_or_more, optional, values, whitespace,
    ws, zero_or_more,
};

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard:   Option<Box<Node>>,
    pub body:    Box<Node>,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Literal(f64),
    StringLit(String),
    Wildcard,
    Var(String),
    List(Vec<Pattern>, Option<String>),
}

#[derive(Debug, Clone)]
pub enum Node {
    Literal(f64),
    Bool(bool),
    Atom(String),
    Partial { op: String, arg: Box<Node> },
    Application { func: Box<Node>, arg: Box<Node> },
    List(Vec<Node>),
    Let { name: String, value: Box<Node>, body: Box<Node> },
    If  { cond: Box<Node>, then: Box<Node>, els: Box<Node> },
    FnDef { name: String, params: Vec<String>, body: Box<Node> },
    Lambda { params: Vec<String>, body: Box<Node> },
    Match  { expr: Box<Node>, arms: Vec<MatchArm> },
    Pipe   { lhs: Box<Node>, rhs: Box<Node> },
    UseModule { path: String },
}

#[derive(Debug, Clone)]
pub enum LamVal {
    Raw(String),
    Node(Node),
    Pattern(Pattern),
    Params(Vec<String>),
    Arms(Vec<MatchArm>),
    ListPattern(Vec<Pattern>, Option<String>)
}

impl ParseValue for LamVal {
    fn from_str(s: String) -> Self { LamVal::Raw(s) }
    fn as_str(&self) -> String {
        match self {
            LamVal::Raw(s) => s.clone(),
            _               => String::new()
        }
    }
}

fn clean(vals: Vec<LamVal>) -> Vec<LamVal> {
    values(vals)
        .into_iter()
        .filter(|v| match v {
            LamVal::Raw(s) => !s.is_empty(),
            _              => true,
        })
        .collect()
}

fn unwrap_node(v: &LamVal) -> Node {
    match v {
        LamVal::Node(n) => n.clone(),
        LamVal::Raw(s)  => Node::Atom(s.clone()),
        _               => panic!("Expected Node, but got {v:?}")
    }
}

fn unwrap_pattern(v: &LamVal) -> Pattern {
    match v {
        LamVal::Pattern(p) => p.clone(),
        LamVal::Raw(s) if s == "_" => Pattern::Wildcard,
        LamVal::Raw(s) => {
            if let Ok(n) = s.parse::<f64>() {
                Pattern::Literal(n)
            } else {
                Pattern::Var(s.clone())
            }
        }

        _ => panic!("Expected pattern, but got {v:?}")
    }
}

fn operators() -> Expression<LamVal> {
    ch('+') | ch('-') | ch('*') | ch('/') | ch('%')
        | ch('<') | ch('>') | ch('=') | ch('!')
        | ch('&') | ch('|') | ch('^') | ch('~')
}

fn spaces() -> Expression<LamVal> {
    ignore(one_or_more(whitespace))
}

fn optional_ws() -> Expression<LamVal> {
    ws()
}

fn identifier() -> Expression<LamVal> {
    action(
        (alpha() | ch('_'))
            + zero_or_more(
                alphanum() | ch('_') | ch('-') | ch('?') | ch('!')
            ),
        |vals| {
            let s: String = values(vals)
                .iter().map(|v| v.as_str()).collect();
            LamVal::Raw(s)
        }
    )
}

fn operator() -> Expression<LamVal> {
    action(one_or_more(operators), |vals| LamVal::Raw(values(vals).iter().map(|v| v.as_str()).collect()))
}

fn number_literal() -> Expression<LamVal> {
    action(
        one_or_more(digit)
            + optional(|| ch('.') + one_or_more(digit)),
        |vals| {
            let s: String = values(vals)
                .iter().map(|v| v.as_str()).collect();
            LamVal::Raw(s)
        }
    )
}

fn string_literal() -> Expression<LamVal> {
    action(
        between(
            ch('"'), ch('"'),
            zero_or_more(
                action(
                    Expression::Not(Box::new(ch::<LamVal>('"')))
                        + iota::any(),
                    |vals| {
                        let v = values(vals);
                        LamVal::Raw(v.last().map(|x| x.as_str()).unwrap_or_default())
                    },
                )
            ),
        ),
        |vals| {
            let s: String = values(vals).iter().map(|v| v.as_str()).collect();
            LamVal::Raw(s)
        },
    )
}

fn pattern() -> Expression<LamVal> {
    number_pattern() | string_pattern() | wildcard_pattern()
        | list_pattern_expr() | var_pattern()
}

fn number_pattern() -> Expression<LamVal> {
    action(number_literal(), |vals| {
        let s = values(vals)[0].as_str();
        LamVal::Pattern(Pattern::Literal(s.parse().unwrap()))
    })
}

fn string_pattern() -> Expression<LamVal> {
    action(string_literal(), |vals| {
        LamVal::Pattern(Pattern::StringLit(values(vals)[0].as_str()))
    })
}

fn wildcard_pattern() -> Expression<LamVal> {
    action(lit("_"), |_| LamVal::Pattern(Pattern::Wildcard))
}

fn var_pattern() -> Expression<LamVal> {
    action(identifier(), |vals| {
        LamVal::Pattern(Pattern::Var(values(vals)[0].as_str()))
    })
}

fn list_pattern_expr() -> Expression<LamVal> {
    action(
        between(
            ch('['),
            ch(']'),
            optional(|| list_pattern_items()),
        ),
        |vals| {
            let parts = clean(vals);
            if parts.is_empty() {
                return LamVal::Pattern(Pattern::List(vec![], None));
            }
            match &parts[0] {
                LamVal::ListPattern(pats, rest) =>
                    LamVal::Pattern(Pattern::List(pats.clone(), rest.clone())),
                _ => LamVal::Pattern(Pattern::List(vec![], None)),
            }
        },
    )
}

fn list_pattern_items() -> Expression<LamVal> {
    action(
        lazy(pattern)
            + zero_or_more(
            ignore(optional_ws())
                + ignore(ch(','))
                + ignore(optional_ws())
                + lazy(pattern)
        )
            + optional(||
            ignore(optional_ws())
                + ignore(lit(","))
                + ignore(optional_ws())
                + ignore(lit("..."))
                + identifier()
        ),
        |vals| {
            let parts = clean(vals);
            let mut pats  = vec![];
            let mut rest  = None;

            for v in &parts {
                match v {
                    LamVal::Pattern(p) => pats.push(p.clone()),
                    LamVal::Raw(s)     => rest = Some(s.clone()), // the ...name
                    _                  => {}
                }
            }
            LamVal::ListPattern(pats, rest)
        },
    )
}

pub fn expr() -> Expression<LamVal> {
    lazy(pipe_expr)
}

fn pipe_expr() -> Expression<LamVal> {
    action(
        lazy(app_expr)
            + zero_or_more(
            ignore(optional_ws())
                + ignore(lit("|>"))
                + ignore(optional_ws())
                + lazy(app_expr)
        ),
        |vals| {
            let parts = clean(vals);
            let mut iter = parts.into_iter();
            let first = unwrap_node(&iter.next().unwrap());
            let node = iter.fold(first, |acc, v| {
                Node::Pipe { lhs: Box::new(acc), rhs: Box::new(unwrap_node(&v)) }
            });

            LamVal::Node(node)
        },
    )
}

fn empty_or_tab() -> Expression<LamVal> {
    ch(' ') | ch('\t')
}

fn spaces_no_newline() -> Expression<LamVal> {
    ignore(one_or_more(empty_or_tab))
}

fn app_expr() -> Expression<LamVal> {
    action(
        lazy(primary) + zero_or_more(ignore(spaces_no_newline()) + lazy(primary)),
        |vals| {
            let parts = clean(vals);
            let mut iter = parts.into_iter();
            let first = unwrap_node(&iter.next().unwrap());
            let result = iter.fold(first, |acc, v| {
                Node::Application {
                    func: Box::new(acc),
                    arg:  Box::new(unwrap_node(&v)),
                }
            });
            LamVal::Node(result)
        },
    )
}

fn primary() -> Expression<LamVal> {
    number_expr()
        | string_expr()
        | bool_expr()
        | lambda_expr()
        | list_expr()
        | paren_expr()
        | atom_expr()
}

fn number_expr() -> Expression<LamVal> {
    action(number_literal(), |vals| {
        let s = values(vals)[0].as_str();
        LamVal::Node(Node::Literal(s.parse().unwrap()))
    })
}

fn string_expr() -> Expression<LamVal> {
    action(string_literal(), |vals| {
        LamVal::Node(Node::Atom(values(vals)[0].as_str()))
    })
}

fn bool_expr() -> Expression<LamVal> {
    action(lit("true") | lit("false"), |vals| {
        LamVal::Node(Node::Bool(values(vals)[0].as_str() == "true"))
    })
}

fn atom_expr() -> Expression<LamVal> {
    action(identifier(), |vals| {
        LamVal::Node(Node::Atom(values(vals)[0].as_str()))
    })
}

fn list_expr() -> Expression<LamVal> {
    action(
        ignore(ch('['))
            + ignore(optional_ws())
            + optional(|| range_body() | plain_list_body())
            + ignore(optional_ws())
            + ignore(ch(']')),
        |vals| {
            let parts = clean(vals);
            if parts.is_empty() {
                return LamVal::Node(Node::List(vec![]));
            }
            parts[0].clone()
        },
    )
}

fn range_body() -> Expression<LamVal> {
    action(
        lazy(expr)
            + ignore(optional_ws())
            + ignore(lit(".."))
            + ignore(optional_ws())
            + lazy(expr)
            + optional(||
            ignore(optional_ws())
                + ignore(ch(';'))
                + ignore(optional_ws())
                + lazy(expr)
        ),
        |vals| {
            let parts = clean(vals);
            let from = unwrap_node(&parts[0]);
            let to   = unwrap_node(&parts[1]);
            let step = parts.get(2).map(|v| unwrap_node(v))
                .unwrap_or(Node::Literal(1.0));
            build_range(from, to, step)
        },
    )
}

fn plain_list_body() -> Expression<LamVal> {
    action(
        lazy(expr)
            + zero_or_more(
            ignore(optional_ws())
                + ignore(ch(','))
                + ignore(optional_ws())
                + lazy(expr)
        ),
        |vals| {
            let nodes = clean(vals).iter().map(|v| unwrap_node(v)).collect();
            LamVal::Node(Node::List(nodes))
        },
    )
}

fn build_range(from: Node, to: Node, step: Node) -> LamVal {
    match (&from, &to, &step) {
        (Node::Literal(f), Node::Literal(t), Node::Literal(s)) => {
            let mut v = *f;
            let mut items = vec![];
            while v < *t {
                items.push(Node::Literal(v));
                v += s;
            }
            LamVal::Node(Node::List(items))
        }

        _ => LamVal::Node(Node::List(vec![from, to, step])),
    }
}

fn lambda_expr() -> Expression<LamVal> {
    action(
        ignore(ch('\\'))
            + one_or_more(|| ignore(optional_ws()) + identifier())
            + ignore(optional_ws())
            + ignore(lit("->"))
            + ignore(optional_ws())
            + lazy(primary),
        |vals| {
            let parts = clean(vals);
            let params: Vec<String> = parts[..parts.len()-1]
                .iter().map(|v| v.as_str()).collect();
            let body = unwrap_node(parts.last().unwrap());
            LamVal::Node(Node::Lambda { params, body: Box::new(body) })
        },
    )
}

fn paren_expr() -> Expression<LamVal> {
    action(
        ignore(ch('('))
            + ignore(optional_ws())
            + paren_body()
            + ignore(optional_ws())
            + ignore(ch(')')),
        |vals| values(vals).into_iter().find(|v| matches!(v, LamVal::Node(_))).unwrap(),
    )
}

fn paren_body() -> Expression<LamVal> {
    special_let()
        | special_fn()
        | special_if()
        | special_match()
        | special_use()
        | op_section()
        | inner_expr()
}

fn inner_expr() -> Expression<LamVal> { lazy(expr) }

fn special_let() -> Expression<LamVal> {
    action(
        ignore(lit("let"))
            + ignore(spaces())
            + ignore(ch('('))
            + ignore(optional_ws())
            + identifier()
            + ignore(spaces())
            + lazy(expr)
            + ignore(optional_ws())
            + ignore(ch(')'))
            + ignore(spaces())
            + lazy(expr),
        |vals| {
            let parts = clean(vals);
            let name  = parts[0].as_str();
            let value = unwrap_node(&parts[1]);
            let body  = unwrap_node(&parts[2]);
            LamVal::Node(Node::Let { name, value: Box::new(value), body: Box::new(body) })
        },
    )
}

fn special_fn() -> Expression<LamVal> {
    action(
        ignore(lit("fn"))
            + ignore(spaces())
            + ignore(ch('('))
            + ignore(optional_ws())
            + identifier()
            + zero_or_more(ignore(spaces()) + identifier())
            + ignore(optional_ws())
            + ignore(ch(')'))
            + ignore(spaces())
            + lazy(expr),
        |vals| {
            let parts = clean(vals);
            let name   = parts[0].as_str();
            let params = parts[1..parts.len()-1].iter().map(|v| v.as_str()).collect();
            let body   = unwrap_node(parts.last().unwrap());
            LamVal::Node(Node::FnDef { name, params, body: Box::new(body) })
        },
    )
}

fn special_if() -> Expression<LamVal> {
    action(
        ignore(lit("if"))
            + ignore(spaces())
            + lazy(primary)
            + ignore(spaces())
            + lazy(primary)
            + ignore(spaces())
            + lazy(primary),
        |vals| {
            let parts = clean(vals);
            LamVal::Node(Node::If {
                cond: Box::new(unwrap_node(&parts[0])),
                then: Box::new(unwrap_node(&parts[1])),
                els:  Box::new(unwrap_node(&parts[2])),
            })
        },
    )
}

fn special_match() -> Expression<LamVal> {
    action(
        ignore(lit("match"))
            + ignore(spaces())
            + lazy(primary)
            + zero_or_more(
            ignore(optional_ws())
                + match_arm()
        ),
        |vals| {
            let parts = clean(vals);
            let scrutinee = unwrap_node(&parts[0]);
            let arms: Vec<MatchArm> = parts[1..].iter().filter_map(|v| {
                match v { LamVal::Arms(a) => a.first().cloned(), _ => None }
            }).collect();
            LamVal::Node(Node::Match { expr: Box::new(scrutinee), arms })
        },
    )
}

fn match_arm() -> Expression<LamVal> {
    action(
        ignore(ch('('))
            + ignore(optional_ws())
            + lazy(pattern)
            + optional(||
            ignore(spaces())
                + ignore(lit("if"))
                + ignore(spaces())
                + lazy(primary)
        )
            + ignore(spaces())
            + lazy(primary)
            + ignore(optional_ws())
            + ignore(ch(')')),
        |vals| {
            let parts = clean(vals);
            // parts: [Pattern, (optional guard Node), body Node]
            let pat  = unwrap_pattern(&parts[0]);
            let (guard, body) = if parts.len() == 3 {
                (Some(Box::new(unwrap_node(&parts[1]))), unwrap_node(&parts[2]))
            } else {
                (None, unwrap_node(&parts[1]))
            };
            LamVal::Arms(vec![MatchArm { pattern: pat, guard, body: Box::new(body) }])
        },
    )
}

fn special_use() -> Expression<LamVal> {
    action(
        ignore(lit("use"))
            + ignore(spaces())
            + string_literal(),
        |vals| {
            let path = values(vals)[0].as_str();
            LamVal::Node(Node::UseModule { path })
        },
    )
}

fn op_section() -> Expression<LamVal> {
    action(
        operator()
            + zero_or_more(ignore(spaces()) + lazy(primary)),
        |vals| {
            let parts = clean(vals);
            let op   = parts[0].as_str();
            let args: Vec<Node> = parts[1..].iter().map(|v| unwrap_node(v)).collect();
            let node = match args.len() {
                0 => Node::Atom(op),
                1 => Node::Partial { op, arg: Box::new(args[0].clone()) },
                2 => Node::Application {
                    func: Box::new(Node::Partial { op, arg: Box::new(args[0].clone()) }),
                    arg:  Box::new(args[1].clone()),
                },
                _ => panic!("op section takes 0–2 arguments"),
            };
            LamVal::Node(node)
        },
    )
}

pub fn parse_expr(source: &str) -> Option<Node> {
    let mut parser = IotaParser::<LamVal>::new(source.to_string());
    parser.parse(&expr(), 0).map(|(_, vals)| {
        clean(vals).into_iter().find_map(|v| match v {
            LamVal::Node(n) => Some(n),
            _ => None,
        })
    })?
}

pub fn parse_source(source: &str) -> Vec<Node> {
    let mut parser = IotaParser::<LamVal>::new(source.to_string());
    let program = zero_or_more(ignore(optional_ws()) + lazy(expr) + ignore(optional_ws()));

    match parser.parse(&program, 0) {
        Some((_, vals)) => clean(vals).into_iter().filter_map(|v| match v {
            LamVal::Node(n) => Some(n),
            _ => None,
        }).collect(),
        None => vec![],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(src: &str) -> Node {
        parse_expr(src).unwrap_or_else(|| panic!("parse failed: {src}"))
    }

    #[test]
    fn test_literal() {
        assert!(matches!(parse("42"), Node::Literal(n) if n == 42.0));
        assert!(matches!(parse("3.14"), Node::Literal(n) if (n - 3.14).abs() < 1e-9));
    }

    #[test]
    fn test_bool() {
        assert!(matches!(parse("true"),  Node::Bool(true)));
        assert!(matches!(parse("false"), Node::Bool(false)));
    }

    #[test]
    fn test_atom() {
        assert!(matches!(parse("foo"), Node::Atom(s) if s == "foo"));
    }

    #[test]
    fn test_list_literal() {
        assert!(matches!(parse("[1, 2, 3]"), Node::List(v) if v.len() == 3));
        assert!(matches!(parse("[]"), Node::List(v) if v.is_empty()));
    }

    #[test]
    fn test_range() {
        let Node::List(items) = parse("[1..5]") else { panic!() };
        assert_eq!(items.len(), 4);
    }

    #[test]
    fn test_range_with_step() {
        let Node::List(items) = parse("[0..10;2]") else { panic!() };
        assert_eq!(items.len(), 5);
    }

    #[test]
    fn test_application() {
        assert!(matches!(
            parse("(f x)"),
            Node::Application { .. }
        ));
    }

    #[test]
    fn test_lambda() {
        let Node::Lambda { params, .. } = parse(r"\x -> x") else { panic!() };
        assert_eq!(params, vec!["x"]);
    }

    #[test]
    fn test_let() {
        assert!(matches!(parse("(let (x 5) x)"), Node::Let { .. }));
    }

    #[test]
    fn test_fn() {
        assert!(matches!(parse("(fn (double x) ((* 2) x))"), Node::FnDef { .. }));
    }

    #[test]
    fn test_if() {
        assert!(matches!(parse("(if true 1 0)"), Node::If { .. }));
    }

    #[test]
    fn test_op_section() {
        assert!(matches!(parse("(+ 1)"), Node::Partial { .. }));
    }

    #[test]
    fn test_pipe() {
        assert!(matches!(parse("x |> f"), Node::Pipe { .. }));
    }

    #[test]
    fn test_match() {
        let src = r#"(match x (1 "one") (_ "other"))"#;
        assert!(matches!(parse(src), Node::Match { .. }));
    }

    #[test]
    fn test_use() {
        assert!(matches!(parse(r#"(use "std/list")"#), Node::UseModule { .. }));
    }

    #[test]
    fn test_parse_source_multi() {
        let nodes = parse_source("(fn (id x) x)\n(id 42)");
        assert_eq!(nodes.len(), 2);
    }

    #[test]
    fn test_nested_application() {
        assert!(matches!(parse("(f (g x))"), Node::Application { .. }));
    }

    #[test]
    fn test_multi_param_lambda() {
        let Node::Lambda { params, .. } = parse(r"\x y z -> x") else { panic!() };
        assert_eq!(params, vec!["x", "y", "z"]);
    }

    #[test]
    fn test_let_with_expression() {
        assert!(matches!(
        parse("(let (x (+ 1 2)) x)"),
        Node::Let { .. }
    ));
    }

    #[test]
    fn test_fn_multi_param() {
        let Node::FnDef { name, params, .. } = parse("(fn (add x y) (+ x y))") else { panic!() };
        assert_eq!(name, "add");
        assert_eq!(params, vec!["x", "y"]);
    }

    #[test]
    fn test_pipe_chain() {
        // x |> f |> g  →  Pipe(Pipe(x, f), g)
        let node = parse("x |> f |> g");
        assert!(matches!(node, Node::Pipe { .. }));
        let Node::Pipe { lhs, .. } = node else { panic!() };
        assert!(matches!(*lhs, Node::Pipe { .. }));
    }

    #[test]
    fn test_match_with_guard() {
        let src = "(match x (n if (> n 0) \"pos\") (_ \"other\"))";
        assert!(matches!(parse(src), Node::Match { .. }));
        let Node::Match { arms, .. } = parse(src) else { panic!() };
        assert!(arms[0].guard.is_some());
        assert!(arms[1].guard.is_none());
    }

    #[test]
    fn test_match_list_pattern() {
        let src = r#"(match xs ([x, ...rest] x) ([] 0))"#;
        let Node::Match { arms, .. } = parse(src) else { panic!() };
        assert!(matches!(&arms[0].pattern, Pattern::List(pats, Some(_)) if pats.len() == 1));
        assert!(matches!(&arms[1].pattern, Pattern::List(pats, None) if pats.is_empty()));
    }

    #[test]
    fn test_nested_let() {
        let src = "(let (x 1) (let (y 2) (+ x y)))";
        assert!(matches!(parse(src), Node::Let { .. }));
        let Node::Let { body, .. } = parse(src) else { panic!() };
        assert!(matches!(*body, Node::Let { .. }));
    }

    #[test]
    fn test_op_section_applied() {
        // (* 2) applied to 5 — should be Application { Partial, Literal }
        assert!(matches!(parse("((* 2) 5)"), Node::Application { .. }));
    }

    #[test]
    fn test_parse_source_with_blank_lines() {
        let nodes = parse_source("(fn (id x) x)\n\n\n(id 42)");
        assert_eq!(nodes.len(), 2);
    }

    #[test]
    fn test_string_literal() {
        assert!(matches!(parse(r#""hello""#), Node::Atom(s) if s == "hello"));
    }

    #[test]
    fn test_empty_list() {
        assert!(matches!(parse("[]"), Node::List(v) if v.is_empty()));
    }

    #[test]
    fn test_float() {
        assert!(matches!(parse("3.14"), Node::Literal(n) if (n - 3.14).abs() < 1e-9));
        assert!(matches!(parse("0.5"),  Node::Literal(n) if (n - 0.5).abs()  < 1e-9));
        assert!(matches!(parse("1.0"),  Node::Literal(n) if (n - 1.0).abs()  < 1e-9));
    }

    #[test]
    fn test_integer_no_decimal() {
        assert!(matches!(parse("0"),   Node::Literal(n) if n == 0.0));
        assert!(matches!(parse("100"), Node::Literal(n) if n == 100.0));
    }

    #[test]
    fn test_negative_number() {
        // lam represents negatives as (- 5), not a literal -5
        assert!(matches!(parse("(- 5)"), Node::Partial { op, .. } if op == "-"));
    }

    #[test]
    fn test_negative_in_expression() {
        // (- x) is a partial, applied: ((- 1) x) subtracts 1 from x
        assert!(matches!(parse("((- 1) x)"), Node::Application { .. }));
    }

    #[test]
    fn test_float_in_list() {
        let Node::List(items) = parse("[1.5, 2.5, 3.5]") else { panic!() };
        assert_eq!(items.len(), 3);
        assert!(matches!(items[0], Node::Literal(n) if (n - 1.5).abs() < 1e-9));
    }

    #[test]
    fn test_float_range() {
        // ranges work with floats too
        let Node::List(items) = parse("[0.0..1.0;0.5]") else { panic!() };
        assert_eq!(items.len(), 2); // 0.0, 0.5
    }
}