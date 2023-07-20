module Params where


-- EMBEDDING DEGREE --

k0 = 6      -- Embedding degree for BLS6_6

-- k0 = 12  -- Embedding degree for BL128


-- FIELD ORDER --

-- BLS6_6
q0 = 43

-- BL128
-- q0 = 21888242871839275222246405745257275088696311157297823662689037894645226208583


-- ELLIPTIC CURVE ORDER  --

-- BLS6_6
r_EC   = 13
r_sign = 1 - 2 * mod r_EC 2


-- IRREDUCIBLE POLYNOMIAL --

-- BLS6_6
poly0 = [6, 0, 0, 0, 0, 0, 1]  -- 6 + x^6
