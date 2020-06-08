let () =
  Alcotest.run "Evm_analyzer" [
    ("opcode_parser", TestOpcodeParser.parser_tests);
    ("trace_parser", TestTraceParser.trace_parser_tests);
  ]
