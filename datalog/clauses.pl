% Facts
% is_output(A, B) - A is an output of B
% uses_storage(A) - A uses the storage, i.e. is loaded through SLOAD
% is_unsigned_overflow(A) - unsigned integer overflow, i.e. AND n mask w/mask > n
% is_signed_overflow(A) - signed integer overflow

% Clauses
% depends(A, B) - A depends directly or indirectly on B
% depends_on_storage(A) - A depends on storage
% is_overflow(A) - A is either a signed or unsigned overflow
% modified_by_overflow(A) - A depends on a value which overflowed
% path_modified_by_overflow(A) - the program path is changed by a value modified by an overflow

depends(A, B) :- is_output(A, B).
depends(A, B) :- is_output(A, C), depends(C, B).
depends_on_storage(A) :- uses_storage(A).
depends_on_storage(A) :- uses_storage(B), depends(A, B).
is_overflow(A) :- ~is_signed(A), is_unsigned_overflow(A), ~depends_on_storage(A).
is_overflow(A) :- is_signed(A), is_signed_overflow(A), ~depends_on_storage(A).
modified_by_overflow(A) :- is_overflow(A).
modified_by_overflow(A) :- is_overflow(B), depends(A, B).
path_modified_by_overflow(A) :- modified_by_overflow(A), used_in_condition(A).
