% Facts
% is_output(A, B) - A is an output of B
% uses_storage(A) - A uses the storage, i.e. is loaded through SLOAD
% is_signed_operand(A) - A is used as an operand which requires a signed value
% is_unsigned_overflow(A) - unsigned integer overflow, i.e. AND n mask w/mask > n
% is_signed_overflow(A) - signed integer overflow
% has_int_size(A, N) - A has an int_size of N

% Clauses
% depends(A, B) - A depends directly or indirectly on B
% depends_on_storage(A) - A depends on storage
% is_signed(A) - A is signed
% is_overflow(A, S, X, Y) - A is either a signed or unsigned (S) overflow. Result should be X but is Y
% modified_by_overflow(A) - A depends on a value which overflowed
% path_modified_by_overflow(A) - the program path is changed by a value modified by an overflow
% int_size(A, N) - A has an inferred int_size of N

depends(A, B) :- is_output(A, B).
depends(A, B) :- is_output(A, C), depends(C, B).
depends(A, B) :- mdepends(A, B).
depends(A, B) :- mdepends(A, C), depends(C, B).

depends_on_storage(A) :- uses_storage(A).
depends_on_storage(A) :- uses_storage(B), depends(A, B).

modified_by_overflow(A) :- is_overflow(A, S, X, Y).
modified_by_overflow(A) :- is_overflow(B, S, X, Y), depends(A, B).

influences_condition(A) :- used_in_condition(A).
influences_condition(A) :- used_in_condition(B), depends(B, A).

negated_const(B) :- is_output(B, A), const(A), not(B).

path_modified_by_overflow(A) :- modified_by_overflow(A), used_in_condition(A).

is_signed(A) :- is_signed_operand(A).
% is_signed(A) :- is_output(B, A), is_signed_operand(B).
% is_signed(A) :- depends(A, B), is_signed_operand(B).
is_unsigned(A) :- ~is_signed(A).

int_size(A, N) :- has_int_size(A, N).
% int_size(A, N) :- is_output(B, A), has_int_size(B, N).
% int_size(A, N) :- depends(A, B), has_int_size(B, N).

uint_size(A, N) :- has_uint_size(A, N).
% uint_size(A, N) :- is_output(B, A), has_uint_size(B, N).
% uint_size(A, N) :- depends(A, B), has_uint_size(B, N).

unhandled_exception(I, A, V) :- failed_call(I, A, V), ~influences_condition(I).

call(I, A, B, V) :- direct_call(I, IV, A, B, V).
call(I, A, B, V) :- direct_call(I, IV, A, C, V), call(I2, C, B, V2).
reentrant_call(I, A, B, V, V2) :- call(I, A, B, V), call(I2, B, A, V2), A != B, gt(I, I2).

empty_delegate(A) :- call_entry(V, A), call_exit(V2), successor(V2, V).

% tx_sstore(Block, Address, Tx, Index, Key).
% tx_sload(Block, Address, Tx, Index, Key).
tod(B, A, T, T2, K) :- tx_sstore(B, A, T, I, K), tx_sload(B, A, T2, I2, K), T != T2.


caller_influences_condition_before(I) :- caller(I2, A), influences_condition(I2), lt(I2, I).
unsafe_selfdestruct(I, A) :- selfdestruct(I, A), ~caller_influences_condition_before(I).

depends_on_caller(I) :- caller(I2, A), depends(I, I2).
unsafe_sstore(I, K) :- tx_sstore(B, A, T, I, K), lt(I, 1000),
                       ~depends_on_caller(I),
                       ~caller_influences_condition_before(I).


% mdepends_r(I, KStart, Kend)
% mdepends_w(I, KStart, KEnd)

memory_overlap(ReadStart, WriteStart, WriteEnd) :-
    gte_bi(ReadStart, WriteStart),
    lt_bi(ReadStart, WriteEnd).

mdepends(I, I2) :- mdepends_r(I, ReadStart, ReadEnd),
                   mdepends_w(I2, WriteStart, WriteEnd),
                   memory_overlap(ReadStart, WriteStart, WriteEnd),
                   gt(I, I2).

depends_on_data(I) :- data_load(I2), depends(I, I2).
depends_on_data(I) :- depends_on_data(I2), depends(I, I2).
depends_on_data(I) :- mdepends_r(I, ReadStart, ReadEnd),
                      data_load_m(WriteStart, WriteEnd),
                      memory_overlap(ReadStart, WriteStart, WriteEnd).

any_depends_on_data(I1, I2) :- depends_on_data(I1).
any_depends_on_data(I1, I2) :- depends_on_data(I2).

unsafe_call(IA, A, V) :- direct_call(IA, IV, A, B, V),
                         any_depends_on_data(IA, IV),
                         ~caller_influences_condition_before(IA),
                         ~depends_on_caller(IV),
                         ~depends_on_caller(IA).

unsafe_delegate_call(I, A) :-
    delegate_call(I, A),
    depends_on_data(I),
    ~caller_influences_condition_before(I).
