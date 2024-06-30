use arkc_hir::hir::HirId;
use arkc_hir::ty;
use arkc_hir::ty::PrimitiveType;
use parser::Span;

use crate::error::ErrorMessage;
use crate::Sema;

fn parse_lit_int(mut value: &str) -> (u32, String, String) {
    let base = if value.starts_with("0b") {
        value = &value[2..];
        2
    } else if value.starts_with("0x") {
        value = &value[2..];
        16
    } else {
        10
    };

    let mut it = value.chars().peekable();
    let mut filtered_value = String::new();

    while let Some(&ch) = it.peek() {
        if ch.is_digit(base) {
            filtered_value.push(ch);
            it.next();
        } else if ch == '_' {
            it.next();
        } else {
            break;
        }
    }

    let mut suffix = String::new();

    for ch in it {
        suffix.push(ch);
    }

    (base, filtered_value, suffix)
}

fn determine_suffix_type_int_literal(suffix: &str) -> Option<ty::Type> {
    match suffix {
        //"u8" => Some(ty::Type::UInt8),
        //"i32" => Some(ty::Type::Int32),
        //"i64" => Some(ty::Type::Int64),
        //"f32" => Some(ty::Type::Float32),
        //"f64" => Some(ty::Type::Float64),
        "" => None,
        _ => {
            panic!("{:?}", ErrorMessage::UnknownSuffix);
            // sa.report(file, span, ErrorMessage::UnknownSuffix);
            None
        }
    }
}

pub fn check_lit_int(
    //sa: &Sema,
    value: &String,
    negate: bool,
    expected_type: Box<ty::Type>,
) -> (ty::Type, i64, f64) {
    let (base, value, suffix) = parse_lit_int(value);
    let suffix_type = determine_suffix_type_int_literal(&suffix);

    let ty = suffix_type.unwrap_or_else(|| match *expected_type {
        // ty::Type::UInt8 if !negate => ty::Type::UInt8,
        ty::Type::Primitive(PrimitiveType::Int32) => ty::Type::Primitive(PrimitiveType::Int32),
        ty::Type::Primitive(PrimitiveType::Int64) => ty::Type::Primitive(PrimitiveType::Int64),
        _ => ty::Type::Primitive(PrimitiveType::Int64),
    });

    if ty.is_float() {
        let value = value.parse::<f64>().expect("unparsable float");
        let value = if negate { -value } else { value };

        if base != 10 {
            panic!("{:?}", ErrorMessage::InvalidNumberFormat)
            //sa.report(file, e.span, ErrorMessage::InvalidNumberFormat);
        }

        return (ty, 0, value);
    }

    //if negate && ty == ty::Type::UInt8 {
    //    panic!("{}", ErrorMessage::NegativeUnsigned)
    //sa.report(file, e.span, ErrorMessage::NegativeUnsigned);
    //}

    let ty_name = ty.name();
    let parsed_value = u64::from_str_radix(&value, base);

    let value = match parsed_value {
        Ok(value) => value,
        Err(_) => {
            panic!("{:?}", ErrorMessage::NumberLimitOverflow);
            //sa.report(file, e.span, ErrorMessage::NumberLimitOverflow);
            return (ty, 0, 0.0);
        }
    };

    if base == 10 {
        let max = match ty {
            //ty::Type::Primitive(PrimitiveType::UInt8) => 256,
            ty::Type::Primitive(PrimitiveType::Int32) => 1u64 << 31,
            ty::Type::Primitive(PrimitiveType::Int64) => 1u64 << 63,
            _ => unreachable!(),
        };

        if (negate && value > max) || (!negate && value >= max) {
            panic!("{:?}", ErrorMessage::NumberOverflow(ty_name.into()));
            //sa.report(file, e.span, ErrorMessage::NumberOverflow(ty_name.into()));
        }

        let value = if negate {
            (value as i64).wrapping_neg()
        } else {
            value as i64
        };

        (ty, value, 0.0)
    } else {
        assert!(!negate);

        let max = match ty {
            //ty::Type::UInt8 => 256 as u64,
            ty::Type::Primitive(PrimitiveType::Int32) => u32::max_value() as u64,
            ty::Type::Primitive(PrimitiveType::Int64) => u64::max_value() as u64,
            _ => unreachable!(),
        };

        if value > max {
            panic!("{:?}", ErrorMessage::NumberOverflow(ty_name.into()));
            //sa.report(file, e.span, ErrorMessage::NumberOverflow(ty_name.into()));
        }

        (ty, value as i64, 0.0)
    }
}

pub fn check_lit_float(
    // sa: &Sema,
    lit_id: HirId,
    //span: Span,
    value: &String,
    negate: bool,
) -> (ty::Type, f64) {
    let (base, value, suffix) = parse_lit_float(&value);

    if base != 10 {
        panic!("{:?}", ErrorMessage::InvalidNumberFormat)
        // sa.report(span, ErrorMessage::InvalidNumberFormat);
    }

    let ty = match suffix.as_str() {
        //"f32" => ty::Type::Float32,
        //"f64" => ty::Type::Float64,
        "" => ty::Type::Primitive(ty::PrimitiveType::Float64),
        _ => {
            panic!("{:?}", ErrorMessage::UnknownSuffix);
            //sa.report(span, ErrorMessage::UnknownSuffix);
            ty::Type::Primitive(ty::PrimitiveType::Float64)
        }
    };

    let (min, max) = match ty {
        // ty::Type::Float32 => (f32::MIN as f64, f32::MAX as f64),
        ty::Type::Primitive(ty::PrimitiveType::Float64) => (f64::MIN, f64::MAX),
        _ => unreachable!(),
    };

    let value = value.parse::<f64>().expect("unparsable float");
    let value = if negate { -value } else { value };

    if value < min || value > max {
        let name = match ty {
            //ty::Type::Float32 => "Float32",
            ty::Type::Primitive(PrimitiveType::Float64) => "Float64",
            _ => unreachable!(),
        };
        panic!("{:?}", ErrorMessage::NumberOverflow(name.into()))
        //sa.report(file, e.span, ErrorMessage::NumberOverflow(name.into()));
    }

    (ty, value)
}

fn parse_lit_float(mut value: &str) -> (u32, String, String) {
    let base = if value.starts_with("0b") {
        value = &value[2..];
        2
    } else if value.starts_with("0x") {
        value = &value[2..];
        16
    } else {
        10
    };

    let mut it = value.chars().peekable();
    let mut filtered_value = String::new();
    let mut allow_scientific = true;

    while let Some(&ch) = it.peek() {
        if ch.is_digit(base) || ch == '.' || ch == '-' || ch == '+' {
            filtered_value.push(ch);
            it.next();
        } else if ch == '_' {
            it.next();
        } else if (ch == 'e' || ch == 'E') && allow_scientific {
            filtered_value.push(ch);
            it.next();
            allow_scientific = false;
        } else {
            break;
        }
    }

    let mut suffix = String::new();

    for ch in it {
        suffix.push(ch);
    }

    (base, filtered_value, suffix)
}
