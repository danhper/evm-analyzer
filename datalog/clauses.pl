depends(A, B) :- is_output(A, B).
depends(A, B) :- is_output(A, C), depends(C, B).
depends_on_storage(A) :- uses_storage(A).
depends_on_storage(A) :- uses_storage(B), depends(A, B).
can_overflow(A) :- is_overflow(A).
can_overflow(A) :- is_overflow(B), depends(A, B).
actual_overflow(A) :- can_overflow(A), ~depends_on_storage(A).
is_dangerous_overflow(A) :- actual_overflow(A), used_in_condition(A).
