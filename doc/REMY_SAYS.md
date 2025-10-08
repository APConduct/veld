# Syntactically enforce var naming scm
no follow -> error


decl fun -> decl names, thus also return type

while you decl vars and assign, ydk what to name it sumtimes and it be bad.
So....... It should just be derived from how it's made.
write it with explicit but can also be derived (poss via tooling) from the name of the vars themselves (at least initially).

to avoid collisions, express an index of the name to not have confusion.

Future generic function implementations should make the witness search strategy user-defined via a library (The stdlib should implement useful ones)
