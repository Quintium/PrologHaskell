person(jgi).
person(dcl).
person(jck).
person(nlo).
person(eme).
person(dbl).
person(spe).
person(fai).
person(ner).
person(mus).

hatRang(jgi, professor).
hatRang(dcl, assistent).
hatRang(jck, assistent).
hatRang(nlo, assistent).
hatRang(eme, assistent).
hatRang(dbl, hiwi).
hatRang(spe, hiwi).
hatRang(fai, student).
hatRang(ner, student).
hatRang(mus, student).

bossVon(A, B) :- hatRang(A, professor), hatRang(B, assistent).
bossVon(A, B) :- hatRang(A, assistent), hatRang(B, hiwi).
bossVon(A, B) :- hatRang(A, hiwi), hatRang(B, student).

hatGleichenRang(A, B) :- person(A), person(B), hatRang(A, C), hatRang(B, C).

vorgesetzt(A, B) :- bossVon(A, B).
vorgesetzt(A, B) :- bossVon(A, C), vorgesetzt(C, B).