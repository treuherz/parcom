#![type_length_limit = "1137933"]

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;

    fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
        where
            Self: Sized + 'a,
            Output: 'a,
            NewOutput: 'a,
            F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn and_then<F, NextParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput>
        where
            Self: Sized + 'a,
            Output: 'a,
            NewOutput: 'a,
            NextParser: Parser<'a, NewOutput> + 'a,
            F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(and_then(self, f))
    }


    fn pred<F>(self, pred_fn: F) -> BoxedParser<'a, Output>
        where Self: Sized + 'a,
              Output: 'a,
              F: Fn(&Output) -> bool + 'a,
    {
        BoxedParser::new(pred(self, pred_fn))
    }

    fn zero_or_one(self) -> BoxedParser<'a, Option<Output>>
        where Self: Sized + 'a,
              Output: 'a,
    {
        BoxedParser::new(zero_or_one(self))
    }

    fn zero_or_more(self) -> BoxedParser<'a, Vec<Output>>
        where Self: Sized + 'a,
              Output: 'a,
    {
        BoxedParser::new(zero_or_more(self))
    }

    fn one_or_more(self) -> BoxedParser<'a, Vec<Output>>
        where Self: Sized + 'a,
              Output: 'a,
    {
        BoxedParser::new(one_or_more(self))
    }
}

impl<'a, F, Output> Parser<'a, Output> for F
    where
        F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

fn the_letter_a(input: &str) -> Result<(&str, ()), &str> {
    match input.chars().next() {
        Some('a') => Ok((&input['a'.len_utf8()..], ())),
        _ => Err(input),
    }
}

fn literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

fn identifier(input: &str) -> ParseResult<String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            matched.push(next);
        } else {
            break;
        }
    }

    let next_index = matched.len();
    Ok((&input[next_index..], matched))
}

fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2
                .parse(next_input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
    }
}

fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
    where
        P: Parser<'a, A>,
        F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
    where
        P1: Parser<'a, R1>,
        P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_, right)| right)
}

fn one_or_more<'a, P: Parser<'a, A>, A>(parser: P) -> impl Parser<'a, Vec<A>> {
    move |mut input| {
        let mut result = Vec::new();
        if let Ok((next_input, first_item)) = parser.parse(input) {
            input = next_input;
            result.push(first_item);
        } else {
            return Err(input);
        }
        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }
        Ok((input, result))
    }
}

fn zero_or_more<'a, P: Parser<'a, A>, A>(parser: P) -> impl Parser<'a, Vec<A>> {
    move |mut input| {
        let mut result = Vec::new();
        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }
        Ok((input, result))
    }
}

fn zero_or_one<'a, P: Parser<'a, A>, A>(parser: P) -> impl Parser<'a, Option<A>> {
    move |input| {
        match parser.parse(input) {
            Ok((next, item)) => {
                Ok((next, Some(item)))
            }
            Err(input) => Ok((input, None))
        }
    }
}

fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(input),
    }
}

fn pred<'a, P, A, F>(parser: P, predicate: F) -> impl Parser<'a, A>
    where
        P: Parser<'a, A>,
        F: Fn(&A) -> bool,
{
    move |input| {
        if let Ok((next_input, value)) = parser.parse(input) {
            if predicate(&value) {
                return Ok((next_input, value));
            }
        }
        Err(input)
    }
}

fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

fn quoted_string<'a>() -> impl Parser<'a, String> {
    right(
        literal("\""),
        left(
            any_char
                .pred(|c| *c != '"')
                .zero_or_more(),
            literal("\""),
        ),
    ).map(|chars| chars.into_iter().collect())
}

fn attribute_pair<'a>() -> impl Parser<'a, (String, String)> {
    pair(identifier, right(literal("="), quoted_string()))
}

fn attributes<'a>() -> impl Parser<'a, Vec<(String, String)>> {
    right(
        whitespace_char().one_or_more(),
        attribute_pair(),
    ).zero_or_more()
}

struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new<P>(parser: P) -> Self
        where P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}


fn element_start<'a>() -> impl Parser<'a, (String, Vec<(String, String)>)> {
    right(literal("<"), pair(identifier, attributes()))
}

fn self_closing_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), pair(literal(" ").zero_or_one(), literal("/>")))
        .map(|(name, attributes)| Element {
            name,
            attributes,
            children: vec![],
        })
}

fn open_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), literal(">"))
        .map(|(name, attributes)| Element {
            name,
            attributes,
            children: vec![],
        })
}

fn either<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
    where P1: Parser<'a, A>,
          P2: Parser<'a, A>,
{
    move |input| match parser1.parse(input) {
        ok @ Ok(_) => ok,
        Err(_) => parser2.parse(input)
    }
}

fn element<'a>() -> impl Parser<'a, Element> {
    either(self_closing_element(), open_element())
}

fn close_element<'a>(expected_name: String) -> impl Parser<'a, String> {
    right(literal("/"), left(identifier, literal(">")))
        .pred(move |name| name == &expected_name)
}

fn and_then<'a, P, F, A, B, Next>(parser: P, f: F) -> impl Parser<'a, B>
    where P: Parser<'a, A>,
          Next: Parser<'a, B>,
          F: Fn(A) -> Next,
{
    move |input| match parser.parse(input) {
        Ok((next_input, result)) => f(result).parse(next_input),
        Err(err) => Err(err),
    }
}

fn parent_element<'a>() -> impl Parser<'a, Element> {
    pair(
        open_element().and_then(|el| {
        left(element().zero_or_more(), close_element()),
    )
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn self_closing_element_parser() {
        assert_eq!(
            self_closing_element().parse("<div class=\"float\" />"),
            Ok(("", Element {
                name: "div".to_string(),
                attributes: vec![("class".to_string(), "float".to_string())],
                children: vec![],
            }))
        )
    }

    #[test]
    fn zero_or_one_combinator() {
        let parser = zero_or_one(literal("o"));
        assert_eq!(
            parser.parse("o"),
            Ok(("", Some(()))),
        );
        assert_eq!(
            parser.parse("a"),
            Ok(("a", None)),
        );
    }

    #[test]
    fn attribute_parser() {
        assert_eq!(
            attributes().parse(" one=\"1\" two=\"2\""),
            Ok((
                "",
                vec![
                    ("one".to_string(), "1".to_string()),
                    ("two".to_string(), "2".to_string())
                ]
            )),
        )
    }

    #[test]
    fn quoted_string_parser() {
        assert_eq!(
            Ok(("", "Hello Joe!".to_string())),
            quoted_string().parse("\"Hello Joe!\""),
        )
    }

    #[test]
    fn predicate_combinator() {
        let parser = pred(any_char, |c| *c == 'o');
        assert_eq!(Ok(("mg", 'o')), parser.parse("omg"));
        assert_eq!(Err("lol"), parser.parse("lol"));
    }

    #[test]
    fn one_or_more_combinator() {
        let parser = one_or_more(literal("ha"));
        assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
        assert_eq!(Err("ahah"), parser.parse("ahah"));
        assert_eq!(Err(""), parser.parse(""));
    }

    #[test]
    fn zero_or_more_combinator() {
        let parser = zero_or_more(literal("ha"));
        assert_eq!(Ok(("", vec![(), (), ()])), parser.parse("hahaha"));
        assert_eq!(Ok(("ahah", vec![])), parser.parse("ahah"));
        assert_eq!(Ok(("", vec![])), parser.parse(""));
    }

    #[test]
    fn literal_parser() {
        let parse_joe = literal("Hello Joe!");
        assert_eq!(parse_joe.parse("Hello Joe!"), Ok(("", ())), );
        assert_eq!(
            parse_joe.parse("Hello Joe! Hello Robert!"),
            Ok((" Hello Robert!", ())),
        );
        assert_eq!(parse_joe.parse("Hello Mike!"), Err("Hello Mike!"), );
    }

    #[test]
    fn identifier_parser() {
        assert_eq!(
            identifier("i-am-an-identifier"),
            Ok(("", "i-am-an-identifier".to_string())),
        );
        assert_eq!(
            identifier("not entirely an identifier"),
            Ok((" entirely an identifier", "not".to_string())),
        );
        assert_eq!(
            identifier("!not at all an identifier"),
            Err("!not at all an identifier"),
        );
    }

    #[test]
    fn pair_combinator() {
        let tag_opener = pair(literal("<"), identifier);
        assert_eq!(
            Ok(("/>", ((), "my-first-element".to_string()))),
            tag_opener.parse("<my-first-element/>")
        );
        assert_eq!(Err("oops"), tag_opener.parse("oops"));
        assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
    }

    #[test]
    fn right_combinator() {
        let tag_opener = right(literal("<"), identifier);
        assert_eq!(
            Ok(("/>", "my-first-element".to_string())),
            tag_opener.parse("<my-first-element/>")
        );
        assert_eq!(Err("oops"), tag_opener.parse("oops"));
        assert_eq!(Err("!oops"), tag_opener.parse("!oops"))
    }
}
