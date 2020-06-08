open Evm_analyzer

let opcode = Alcotest.testable Op.pp Op.equal
let trace = Alcotest.testable Trace.pp Trace.equal
