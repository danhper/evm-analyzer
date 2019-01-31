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
depends(A, B) :- is_output(A, C), depends(C, B).

connected(A, B) :- depends(A, B).
connected(A, B) :- depends(B, A).

depends_on_storage(A) :- uses_storage(A).
depends_on_storage(A) :- uses_storage(B), depends(A, B).

is_overflow(A) :- is_unsigned_overflow(A), ~is_signed(A).
is_overflow(A) :- is_signed_overflow(A), is_signed(A).

modified_by_overflow(A) :- is_overflow(A).
modified_by_overflow(A) :- is_overflow(B), depends(A, B).

path_modified_by_overflow(A) :- modified_by_overflow(A), used_in_condition(A).

is_signed(A) :- is_signed_operand(A).
is_signed(A) :- is_signed_operand(B), connected(A, B).
is_unsigned(A) :- ~is_signed(A).

int_size(A, N) :- has_int_size(A, N).
int_size(A, N) :- has_int_size(B, N), connected(A, B).

uint_size(A, N) :- has_uint_size(A, N).
uint_size(A, N) :- has_uint_size(B, N), connected(A, B).
