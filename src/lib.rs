use std::collections::HashMap;

use nom::bytes::complete::{tag, take_until, take_while, take_while1};
use nom::multi::{many_till, many0};
use nom::branch::alt;

#[derive(Debug)]
pub struct Document {
    version: i32,
    encoding: Option<String>,
    root: Element
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
    pub contents: Vec<Node>,
}

pub type IResult<'a, T> = nom::IResult<&'a str, T>;

fn name_char(ch: char) -> bool {
    macro_rules! r {
        ($a: literal, $b: literal) => {$a < ch && ch < $b};
    }
    ch == ':' || r!('A', 'Z') || r!('a', 'z')
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

fn attribute<'a>(input: &'a str) -> IResult<(&'a str, &'a str)> {
    ws!(input);
    let (input, key) = identifier(input)?;
    let (input, _) = eq(input)?;
    let (input, value) = attribute_value(input)?;
    ws!(input);
    Ok((input, (key, value)))
}

fn start_element<'a>(input: &'a str) -> IResult<(&'a str, Vec<(&'a str, &'a str)>)> {
    let (input, _) = tag("<")(input)?;
    let (input, name) = identifier(input)?;
    let (input, attributes) = many0(attribute)(input)?;
    let (input, _) = tag(">")(input)?;
    Ok((input, (name, attributes)))
}

fn end_element<'a>(name: &'a str) -> impl Fn(&'a str) -> IResult<()> {
    move |input| {
        let (input, _) = tag("</")(input)?;
        let (input, found_name) = identifier(input)?;
        ws!(input);
        let (input, _) = tag(">")(input)?;
        if found_name == name {
            Ok((input, ()))
        } else {
            Err(nom::Err::Error(nom::error::Error {
                input: "invalid end element",
                code: nom::error::ErrorKind::Verify
            }))
        }
    }
}

pub fn element(input: &str) -> IResult<Element> {
    let (input, element) = start_element(input)?;
    let (input, (contents, _)) = many_till(node, end_element(element.0))(input)?;
    Ok((input, Element { name: element.0.to_owned(), attributes: {
        let mut map = HashMap::new();
        for (key, value) in element.1 {
            if map.contains_key(key) {
                return Err(nom::Err::Error(nom::error::Error {
                    input: "duplicate attribute",
                    code: nom::error::ErrorKind::Verify
                }))
            }
            map.insert(key.to_owned(), value.to_owned());
        }
        map
    }, contents }))
}

pub fn element_into_node(input: &str) -> IResult<Node> {
    let (input, element) = element(input)?;
    Ok((input, Node::Element(element)))
}

fn is_char(x: char) -> bool {
    x != '<' && x != '>'
}

fn cdata_section(input: &str) -> IResult<String> {
    let (input, _) = tag("<![CDATA[")(input)?;
    let (input, data) = take_until("]]>")(input)?;
    let (input, _) = tag("]]>")(input)?;
    Ok((input, data.to_owned()))
}

fn text_data(input: &str) -> IResult<String> {
    let (input, data) = take_while(is_char)(input)?;
    Ok((input, data.to_owned()))
}

pub fn char_data(input: &str) -> IResult<String> {
    alt((cdata_section, text_data))(input)
}

pub fn char_data_into_node(input: &str) -> IResult<Node> {
    let (input, data) = char_data(input)?;
    Ok((input, Node::CharData(data)))
}

pub fn node(input: &str) -> IResult<Node> {
    alt((element_into_node, char_data_into_node))(input)
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

pub fn document(input: &str) -> IResult<Document> {
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
    let (input, root) = element(input)?;
    Ok((input, Document {
        version,
        encoding: encoding.map(&str::to_owned),
        root
    }))
}

impl Element {
    pub fn strip_whitespace(&mut self) {
        self.contents.retain(|e| !matches!(e, Node::CharData(data) if data.trim().len() == 0));
        for x in &mut self.contents {
            if let Node::Element(y) = x {
                y.strip_whitespace();
            }
        }
    }
}

pub fn strip_whitespace(node: Node) -> Node {
    match node {
        Node::CharData(data) => Node::CharData(data.trim().to_owned()),
        Node::Element(data) => Node::Element(Element {
            name: data.name,
            attributes: data.attributes,
            contents: {
                let mut v = vec![];
                for x in data.contents {
                    let x = strip_whitespace(x);
                    if let Node::CharData(d) = &x {
                        if d.len() == 0 {
                            continue
                        }
                    }
                    v.push(x);
                }
                v
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::*;

    fn e(x: &'static str) {
        let res = document(x).unwrap();
        let mut doc = res.1;
        doc.root.strip_whitespace();
        println!("{}\n{:#?}", x, doc);
    }
    #[test]
    fn it_works() {
        e("<?xml version='1.5' encoding = \"utf-8\"?> <rss x=\"4\" y=' 10' ><![CDATA[</rss>]]></rss>");
    }
}
