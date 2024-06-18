#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParseError {
    // Parser errors
    ExpectedElement,
    ExpectedTraitElement,
    ExpectedToken(String),
    ExpectedType,
    ExpectedTypeParam,
    ExpectedParams,
    ExpectedParam,
    ExpectedEnumVariants,
    ExpectedEnumVariant,
    ExpectedUsePath,
    ExpectedPattern,
    ExpectedField,
    MisplacedElse,
    ExpectedFactor,
    UnclosedStringTemplate,
    // NOTE: makes no sense ExpectedIdentifier,
    ExpectedExpression,
}

impl ParseError {
    pub fn message(&self) -> String {
        match self {
            // Parser errors
            ParseError::ExpectedElement => {
                format!("expected top-level declaration.")
            }
            ParseError::ExpectedTraitElement => {
                format!("expected trait element.")
            }
            ParseError::ExpectedToken(ref exp) => {
                format!("expected `{}`.", exp)
            }
            ParseError::ExpectedParams => "expected parameters.".into(),
            ParseError::ExpectedParam => "expected param.".into(),
            ParseError::ExpectedTypeParam => "expected type param".into(),
            ParseError::ExpectedEnumVariants => "expected block with enum variants.".into(),
            ParseError::ExpectedEnumVariant => "expected enum variant.".into(),
            ParseError::ExpectedType => format!("type expected."),
            ParseError::ExpectedUsePath => "expected use path.".into(),
            ParseError::ExpectedField => "expected field.".into(),
            ParseError::ExpectedPattern => "expected pattern.".into(),
            ParseError::MisplacedElse => "misplace else.".into(),
            ParseError::ExpectedFactor => format!("factor expected."),
            ParseError::UnclosedStringTemplate => "unclosed string template.".into(),
            //ParseError::ExpectedIdentifier => {
            //    format!("identifier expected.")
            //}
            ParseError::ExpectedExpression => "expected expression.".into(),
        }
    }
}
