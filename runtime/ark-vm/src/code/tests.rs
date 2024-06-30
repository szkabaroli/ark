#[cfg(test)]
mod test {
    use crate::builder::{Builder, EncodedInstruction};
    use crate::code::Code;
    use crate::instruction::OpCode;
    use crate::operand::Operand;

    #[test]
    fn encode() {
        let mut builder: Builder = Builder::new();

        let fn_label = builder.create_named_label("some_function");

        builder.push(Operand::Int(123));
        builder.push(Operand::Int(456));
        builder.label(fn_label);
        builder.pop();
        let code = Code::from(builder);

        let actual = bincode::encode_to_vec(code, bincode::config::standard()).unwrap();

        let expected = [
            3, 0, 4, 110, 111, 111, 112, 1, 4, 112, 117, 115, 104, 2, 3, 112, 111, 112, 10, 0, 0,
            1, 1, 0, 1, 1, 1, 2, 0, 2, 123, 251, 200, 1, 2, 0, 4, 109, 97, 105, 110, 8, 13, 115,
            111, 109, 101, 95, 102, 117, 110, 99, 116, 105, 111, 110,
        ];

        assert_eq!(&actual[..], &expected[..]);
    }

    #[test]
    fn decode() {
        let bytecode: [u8; 56] = [
            3, 0, 4, 110, 111, 111, 112, 1, 4, 112, 117, 115, 104, 2, 3, 112, 111, 112, 10, 0, 0,
            1, 1, 0, 1, 1, 1, 2, 0, 2, 123, 251, 200, 1, 2, 0, 4, 109, 97, 105, 110, 8, 13, 115,
            111, 109, 101, 95, 102, 117, 110, 99, 116, 105, 111, 110,
        ];

        let (code, _): (Code, usize) =
            bincode::decode_from_slice(&mut &bytecode[..], bincode::config::standard()).unwrap();

        assert_eq!(
            code.code,
            [
                EncodedInstruction(OpCode::Push, 0, [0, 0]),
                EncodedInstruction(OpCode::Push, 0, [0, 0]),
                EncodedInstruction(OpCode::Push, 0, [0, 0]),
                EncodedInstruction(OpCode::Push, 0, [0, 0]),
                EncodedInstruction(OpCode::Push, 0, [0, 0]),
            ]
        );
        assert_eq!(code.data, [Operand::Int(123), Operand::Int(456)]);
        assert_eq!(
            code.debug_labels,
            [
                (0 as usize, "main".to_string()),
                (8 as usize, "some_function".to_string())
            ].into()
        )
    }
}
