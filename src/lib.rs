use std::collections::HashMap;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_until, take_while, take_while1};
use nom::multi::{many0, many_till};

#[derive(Debug)]
pub struct Document {
    pub version: i32,
    pub encoding: Option<String>,
    pub root: Element,
}

#[derive(Debug)]
pub enum Node {
    CharData(String),
    Element(Element),
}

#[derive(Debug)]
pub struct Element {
    pub name: String,
    pub attributes: HashMap<String, String>,
    pub children: Vec<Node>,
}

pub trait QuerySupport<'a, T> {
    fn only(&self) -> Option<&'a T>;
    fn first(&self) -> Option<&'a T>;
    fn nth(&self, index: usize) -> Option<&'a T>;
    fn last(&self) -> Option<&'a T>;
    fn elem_name(&self, name: &str) -> Vec<&'a Element>;
}

impl<'a> QuerySupport<'a, Node> for Vec<&'a Node> {
    fn only(&self) -> Option<&'a Node> {
        if self.len() == 1 {
            Some(self[0])
        } else {
            None
        }
    }

    fn first(&self) -> Option<&'a Node> {
        self.nth(0)
    }

    fn nth(&self, index: usize) -> Option<&'a Node> {
        if index < self.len() {
            Some(self[index])
        } else {
            None
        }
    }

    fn last(&self) -> Option<&'a Node> {
        self.nth(self.len() - 1)
    }

    fn elem_name(&self, name: &str) -> Vec<&'a Element> {
        let mut v = vec![];
        for x in self {
            if let Node::Element(element) = &x {
                if element.name.eq_ignore_ascii_case(name) {
                    v.push(element);
                } else {
                    v.append(&mut element.children().elem_name(name));
                }
            }
        }
        v
    }
}
impl<'a> QuerySupport<'a, Element> for Vec<&'a Element> {
    fn only(&self) -> Option<&'a Element> {
        if self.len() == 1 {
            Some(self[0])
        } else {
            None
        }
    }

    fn first(&self) -> Option<&'a Element> {
        self.nth(0)
    }

    fn nth(&self, index: usize) -> Option<&'a Element> {
        if index < self.len() {
            Some(self[index])
        } else {
            None
        }
    }

    fn last(&self) -> Option<&'a Element> {
        if self.len() == 0 {
            None
        } else {
            Some(&self[self.len() - 1])
        }
    }

    fn elem_name(&self, name: &str) -> Vec<&'a Element> {
        let mut v = vec![];
        for x in self {
            let element = *x;
            if element.name.eq_ignore_ascii_case(name) {
                v.push(element);
            } else {
                v.append(&mut element.children().elem_name(name));
            }
        }
        v
    }
}

impl Node {
    pub fn as_cdata(&self) -> Option<&String> {
        match self {
            Node::CharData(data) => Some(data),
            _ => None
        }
    }
    pub fn into_cdata(self) -> Option<String> {
        match self {
            Node::CharData(data) => Some(data),
            _ => None
        }
    }
    pub fn is_cdata(&self) -> bool {
        matches!(self, Node::CharData(_))
    }
    pub fn as_element(&self) -> Option<&Element> {
        match self {
            Node::Element(element) => Some(element),
            _ => None
        }
    }
    pub fn into_element(self) -> Option<Element> {
        match self {
            Node::Element(element) => Some(element),
            _ => None
        }
    }
    pub fn is_element(&self) -> bool {
        matches!(self, Node::Element(_))
    }
}

#[derive(Debug, Default)]
pub struct Parser {
    pub allow_no_close: Vec<String>
}

pub type IResult<'a, T> = nom::IResult<&'a str, T>;

fn name_char(ch: char) -> bool {
    ch == ':' || ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '!'
}

macro_rules! ws {
    ($input: ident) => {
        let ($input, _) = take_while(char::is_whitespace)($input)?;
    }
}

fn identifier<'a>(input: &'a str) -> IResult<&'a str> {
    take_while1(name_char)(input)
}

fn attribute_value<'a>(input: &'a str) -> IResult<&'a str> {
    let (input, quote) = alt((tag("\""), tag("\'")))(input)?;
    let (input, data) = take_while(|ch| format!("{}", ch) != quote)(input)?;
    let (input, _) = tag(quote)(input)?;
    Ok((input, data))
}

fn eq(input: &str) -> IResult<()> {
    ws!(input);
    let (input, _) = tag("=")(input)?;
    ws!(input);
    Ok((input, ()))
}

fn quoted<'a, T, F: Fn(&'a str) -> IResult<'a, T>>(f: impl Fn(&'a str) -> F) -> impl Fn(&'a str) -> IResult<'a, T> {
    move |input| {
        let (input, quote) = alt((tag("\""), tag("\'")))(input)?;
        let (input, res) = f(quote)(input)?;
        let (input, _) = tag(quote)(input)?;
        Ok((input, res))
    }
}

fn attribute<'a>(input: &'a str) -> IResult<(String, &'a str)> {
    ws!(input);
    let (input, key) = identifier(input)?;
    let key = key.to_ascii_lowercase();
    let (input, _) = eq(input)?;
    let (input, value) = attribute_value(input)?;
    ws!(input);
    Ok((input, (key, value)))
}

impl Parser {
    pub fn complete_element(&self, input: &str) -> Option<Element> {
        let (input, mut element) = self.element(input).ok()?;
        // let (input, mut element) = self.element(input).unwrap();
        if input.len() == 0 {
            element.strip_whitespace();
            Some(element)
        } else {
            None
        }
    }
    pub fn complete_document(&self, input: &str) -> Option<Document> {
        let (input, mut document) = self.document(input).ok()?;
        if input.len() == 0 {
            document.root.strip_whitespace();
            Some(document)
        } else {
            None
        }
    }

    pub fn element<'a>(&self, input: &'a str) -> IResult<'a, Element> {
        let (input, _) = tag("<")(input)?;
        let (input, name) = identifier(input)?;
        if name == "!DOCTYPE" {
            let (mut input, _) = take_until(">")(input)?;
            if input.len() > 0 {
                input = &input[1..];
            } else {
                return Err(nom::Err::Error(nom::error::Error {
                    // "expected shit after > in doctype decl"
                    input: "expected shit after > in doctype decl",
                    code: nom::error::ErrorKind::Tag
                }))
            }
            return Ok((input, Element {
                name: "doctype_decl".to_string(),
                attributes: Default::default(),
                children: vec![]
            }))
        }
        let name = name.to_ascii_lowercase();
        ws!(input);
        let (input, attributes) = many0(attribute)(input)?;
        let (input, children) = alt((|input| {
            if self.allow_no_close.contains(&name) {
                if let Ok((input, _)) = tag::<_, _, nom::error::Error<&str>>(">")(input) {
                    return Ok((input, vec![]))
                }
            }
            let (input, _) = tag("/>")(input)?;
            Ok((input, vec![]))
        }, |input| {
            let (input, _) = tag(">")(input)?;
            ws!(input);
            let (input, (children, _)) = many_till(|input| self.node(input), tag("</"))(input)?;
            ws!(input);
            let (input, res) = identifier(input)?;
            let res = res.to_ascii_lowercase();
            if name == res {
                ws!(input);
                let (input, _) = tag(">")(input)?;
                Ok((input, children))
            } else {
                Err(nom::Err::Failure(nom::error::Error{ input, code: nom::error::ErrorKind::Tag }))
            }
        }))(input)?;
        ws!(input);
        Ok((input, Element {
            name,
            attributes: {
                let mut map = HashMap::new();
                for (key, value) in &attributes {
                    if map.contains_key(key) {
                        return Err(nom::Err::Error(nom::error::Error {
                            input: "duplicate attribute",
                            code: nom::error::ErrorKind::Verify,
                        }));
                    }
                    map.insert(key.to_string(), value.to_string());
                }
                map
            },
            children,
        }))
    }

    pub fn element_into_node<'a>(&self, input: &'a str) -> IResult<'a, Node> {
        let (input, element) = self.element(input)?;
        Ok((input, Node::Element(element)))
    }

    pub fn node<'a>(&self, input: &'a str) -> IResult<'a, Node> {
        alt((|input| self.element_into_node(input), char_data_into_node))(input)
    }

    pub fn document<'a>(&self, input: &'a str) -> IResult<'a, Document> {
        ws!(input);
        let (input, _) = tag("<?xml")(input)?;
        ws!(input);
        let (input, _) = tag("version")(input)?;
        let (input, _) = eq(input)?;
        let (input, version) = quoted(|_| version_num)(input)?;
        ws!(input);
        let (input, encoding) = encoding(input)?;
        ws!(input);
        let (input, _) = tag("?>")(input)?;
        ws!(input);
        let (input, root) = self.element(input)?;
        ws!(input);
        Ok((input, Document {
            version,
            encoding: encoding.map(&str::to_string),
            root,
        }))
    }
}

fn is_char(x: char) -> bool {
    x != '<' && x != '>'
}

fn cdata_section(input: &str) -> IResult<String> {
    let (input, _) = tag("<![CDATA[")(input)?;
    let (input, data) = take_until("]]>")(input)?;
    let (input, _) = tag("]]>")(input)?;
    Ok((input, data.to_string()))
}

fn text_data(input: &str) -> IResult<String> {
    let (input, data) = take_while(is_char)(input)?;
    Ok((input, data.to_string()))
}

pub fn char_data(input: &str) -> IResult<String> {
    alt((cdata_section, text_data))(input)
}

pub fn char_data_into_node(input: &str) -> IResult<Node> {
    let (input, data) = char_data(input)?;
    Ok((input, Node::CharData(data)))
}

fn version_num(input: &str) -> IResult<i32> {
    let (input, _) = tag("1.")(input)?;
    let (input, data) = take_while(|ch: char| ch.is_ascii_digit())(input)?;
    Ok((input, data.parse().unwrap()))
}

fn encoding<'a>(input: &'a str) -> IResult<Option<&'a str>> {
    if let Ok((input, _)) = tag::<_, _, nom::error::Error<&str>>("encoding")(input) {
        let (input, _) = eq(input)?;
        let (input, data) = attribute_value(input)?;
        Ok((input, Some(data)))
    } else {
        Ok((input, None))
    }
}

impl Element {
    pub fn strip_whitespace(&mut self) {
        self.children.retain(|e| if let Node::CharData(data) = e { data.trim().len() > 0 } else { true });
        for x in &mut self.children {
            if let Node::Element(y) = x {
                y.strip_whitespace();
            }
        }
    }
    pub fn children(&self) -> Vec<&Node> {
        let mut v = vec![];
        for x in &self.children {
            v.push(x);
        }
        v
    }
}

pub fn strip_whitespace(node: Node) -> Node {
    match node {
        Node::CharData(data) => Node::CharData(data.trim().to_string()),
        Node::Element(data) => Node::Element(Element {
            name: data.name,
            attributes: data.attributes,
            children: {
                let mut v = vec![];
                for x in data.children {
                    let x = strip_whitespace(x);
                    if let Node::CharData(d) = &x {
                        if d.len() == 0 {
                            continue;
                        }
                    }
                    v.push(x);
                }
                v
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::*;

    fn e(x: &str) {
        let parser: Parser = Parser {
            allow_no_close: vec!["img".to_string()]
        };
        let res = parser.complete_element(x).unwrap();
        println!("{}\n{:#?}", x, res);
    }

    #[test]
    fn it_works() {
        e(&std::fs::read_to_string("test.xml").unwrap());
    }
}
