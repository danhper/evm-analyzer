# evm-analyzer

Set of analysis tools for EVM bytecode and in particular, for EVM execution traces.

## Build & Installation

The tool is written in OCaml and will therefore requires an OCaml compiler.
The software is tested using OCaml 4.09, which we recommend.

Installation steps

```
git clone https://github.com/danhper/evm-analyzer.git
cd evm-analyzer
opam switch create ./ 4.09.1
eval $(opam config env) # activate the switch if needed
```

If everything succeeded, the command `evm-analyzer` should be available.

## Usage

The available commands can be displayed using `evm-analyzer help`


### `opcodes` command

Opcodes can be shown from raw bytecode.
An improvement over some other decompilers is that this accepts `solc` combined JSON format and can output where the instruction comes from using the compiler sourcemap.
The file needs to be compiled with: `solc --combined-json bin-runtime,srcmap-runtime Contract.sol > contract.json`. The format outputted by Truffle is also supported.
Opcodes can then been viewed with

```
evm-analyzer opcodes -contract-name Contract contract.json -show-sourcemap
```

Note that the `-contract-name` option is only needed when multiple contracts are defined in the same file.
`-show-pc` option can also be passed to view the program counter next to the bytecode.

The output should look similar to the following:

```
0 PUSH1 0x80 contracts/Treasury.sol:7:1-23:0
2 PUSH1 0x40 contracts/Treasury.sol:7:1-23:0
4 MSTORE contracts/Treasury.sol:7:1-23:0
5 CALLVALUE contracts/Treasury.sol:7:1-23:0
6 DUP1 contracts/Treasury.sol:7:1-23:0
7 ISZERO contracts/Treasury.sol:7:1-23:0
8 PUSH2 0x10 contracts/Treasury.sol:7:1-23:0
11 JUMPI contracts/Treasury.sol:7:1-23:0
12 PUSH1 0x0 contracts/Treasury.sol:7:1-23:0
14 DUP1 contracts/Treasury.sol:7:1-23:0
15 REVERT contracts/Treasury.sol:7:1-23:0
16 JUMPDEST contracts/Treasury.sol:7:1-23:0
```

### `analyze-traces` command

`analyze-traces` allows to analyze traces returned by `debug.traceTransaction`.
A couple of sample traces are available in [`test/fixtures`](./test/fixtures).
Traces for a particular transaction can be generated using the following command.

```
geth attach GETH_URL --exec 'console.log(JSON.stringify(debug.traceTransaction("TX_ID")))' | head -n-1 > traces.json
```

Note that GETH_URL must point to a node with archive data.

The traces can then be analyzed using the following command:

```
evm-analyzer analyze-traces traces.json 'QUERY'
```

The query must be either the name of a vulnerability or a Datalog query.
The available vulnerabilities are:

* integer-overflow
* unhandled-exception
* locked-ether
* reentrancy
* tod
* unrestricted-action

The different Datalog queries available can be found in [`datalog/clauses.pl`](./datalog/clauses.pl).

For example, to show the reentrancy issues in The DAO logs, the following command can be used:

```
evm-analyzer analyze-traces test/fixtures/0xb717a196ef8a20dd1cd91835fd2287704b5edbea7f15483d95f8a231c050118d-dao.json.gz reentrancy -contract-address 0x969837498944ae1dc0dcac2d0c65634c88729b2d > results.json
```

Note that for reentrancy, the contract address should be passed explicitly as it is not present in the traces.


### `analyze-results` command

This command is used to analyze the output of `analyze-traces`, as it can sometimes be quite verbose.
For example, to get an aggregate value of the analysis run with `analyze-traces`, the following command can be used:

```
evm-analyzer analyze-result reentrancy results.json -min-value 1
```

Note that this is currently only available for `reentrancy` and `unhandled-exception`.


The other commands are mostly wrappers around `analyze-traces`.

## Academic work

This has been developed as part of the paper [Smart Contract Vulnerabilities: Vulnerable Does Not Imply Exploited](https://arxiv.org/abs/1902.06710), to appear at [USENIX Security'21](https://www.usenix.org/conference/usenixsecurity21).
If you find this work is useful, we would be grateful if you could cite it:

```
@article{DBLP:journals/corr/abs-1902-06710,
  author    = {Daniel Perez and
               Benjamin Livshits},
  title     = {Smart Contract Vulnerabilities: Does Anyone Care?},
  journal   = {CoRR},
  volume    = {abs/1902.06710},
  year      = {2019},
  url       = {http://arxiv.org/abs/1902.06710},
  archivePrefix = {arXiv},
  eprint    = {1902.06710},
  timestamp = {Thu, 18 Jun 2020 10:16:16 +0200},
  biburl    = {https://dblp.org/rec/journals/corr/abs-1902-06710.bib},
  bibsource = {dblp computer science bibliography, https://dblp.org}
}
```
