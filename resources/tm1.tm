start z0
final z6

;; this should accept a^n b^n c^n | n >= 1
(z0, a) -> (z1, $, R)
(z1, a) -> (z1, a, R)
(z1, b) -> (z2, $, R)
(z1, $) -> (z1, $, R)
(z2, b) -> (z2, b, R)
(z2, c) -> (z3, $, R)
(z2, $) -> (z2, $, R)
(z3, c) -> (z3, c, R)
(z3, _) -> (z4, _, L)
(z5, _) -> (z5, _, L)
(z4, $) -> (z4, $, L)
(z4, _) -> (z6, _, R)
(z4, c) -> (z5, c, L)
(z5, c) -> (z5, c, L)
(z5, $) -> (z5, $, L)
(z5, b) -> (z5, b, L)
(z5, a) -> (z5, a, L)
(z5, _) -> (z0, _, R)
(z0, $) -> (z0, $, R)
