MFP
===

Repozitorij za projekt pri predmetu matematika s funkcijskim programiranjem.
Tema projekta: reševanje diferencialnih enačb s pomočjo zlepkov (angl. spline).
Vir in osnova za projekt: http://gbaz.github.io/slides/ode-draft-2009.pdf
Avtorja: Grega Gašperšič, Tanja Malić

Za uporabo je potrebno paket namestiti s programom Cabal.

Podatkovna struktura:
p    ... polinom podan s seznamom koeficientov: [a0, a1, ... an]
ps   ... segment polinoma, ki je par (d, p), kjer d predstavlja dolžino polinoma p
s    ... zlepek, ki je seznam segmentov [(d1,ps1), (d2, ps2), ... (dn,psn)]

V datoteki DSolve.hs se nahajajo funkcije, s katerimi rešimo dano diferencialno enačbo.

Izboljšave:
- eps  ... določamo lahko parameter d - s tem pridobimo na natančnosti a izgubimo na času
- trim ... določamo lahko stopnjo polinomov vseh segmentov - s tem pridobimo na času, a izgubimo na natančnosti
- higherOrder ... določamo lahko pri katerem segmentu zlapka začnemo


