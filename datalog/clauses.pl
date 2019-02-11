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
% is_overflow(A) - A is either a signed or unsigned overflow
% modified_by_overflow(A) - A depends on a value which overflowed
% path_modified_by_overflow(A) - the program path is changed by a value modified by an overflow
% int_size(A, N) - A has an inferred int_size of N

depends(A, B) :- is_output(A, B).
depends(A, B) :- depends(A, B), is_output(B, C).

depends_on_storage(A) :- uses_storage(A).
depends_on_storage(A) :- uses_storage(B), depends(A, B).

is_overflow(A) :- is_unsigned_overflow(A), ~is_signed(A).
is_overflow(A) :- is_signed_overflow(A), is_signed(A).

modified_by_overflow(A) :- is_overflow(A).
modified_by_overflow(A) :- is_overflow(B), depends(A, B).

influences_condition(A) :- used_in_condition(A).
influences_condition(A) :- depends(B, A), used_in_condition(B).

negated_const(B) :- is_output(B, A), const(A), not(B).

path_modified_by_overflow(A) :- modified_by_overflow(A), used_in_condition(A).

is_signed(A) :- is_signed_operand(A).
is_signed(A) :- depends(A, B), is_signed_operand(B).
is_unsigned(A) :- ~is_signed(A).

int_size(A, N) :- has_int_size(A, N).
int_size(A, N) :- depends(A, B), has_int_size(B, N).

uint_size(A, N) :- has_uint_size(A, N).
uint_size(A, N) :- depends(A, B), has_uint_size(B, N).

unhandled_exception(A) :- failed_call(A), ~influences_condition(A).

call(I, A, B, V) :- direct_call(I, A, B, V).
call(I, A, B, V) :- call(I2, A, C, V), direct_call(I, C, B, V2).
reentrant_call(I, A, B, V, V2) :- call(I, A, B, V), call(I2, B, A, V2), A != B.

empty_delegate(A) :- call_entry(V, A), call_exit(V2), successor(V2, V).

% tx_sstore(B, T, I).
% tx_sload(B, T, I).
tod(B, T, T2, I) :- tx_sstore(B, T, I), tx_sload(B, T2, I), T != T2.
