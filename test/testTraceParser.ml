open Evm_analyzer


let parse_regular_format () =
  let traces = TraceParser.parse_string Fixtures.regular_trace_0xdd77a7d375e in
  Alcotest.(check int) "has all traces" 165 (List.length traces)

let parse_stripped_format () =
  let regular_traces = TraceParser.parse_string Fixtures.regular_trace_0xdd77a7d375e in
  let stripped_traces = TraceParser.parse_string Fixtures.stripped_trace_0xdd77a7d375e in
  Alcotest.(check (list Testable.trace)) "regular and stripped yield same result"
    regular_traces stripped_traces

let trace_parser_tests = [
  ("parse_regular_format", `Quick, parse_regular_format);
  ("parse_stripped_format", `Quick, parse_stripped_format);
]
