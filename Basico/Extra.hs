module Basico.Extra where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Interp

(x,y) .+ (x',y') = (x+x',y+y')
s .* (x,y) = (s*x,s*y)
(x,y) .- (x',y') = (x-x',y-y')
negar (x,y) = (-x,-y)

curvita :: FloatingPic
curvita a b c = line $ bezier a (a .+ b .+((1/3) .* c)) (a .+ b .+ c) 10

bezier :: Vector -> Vector -> Vector -> Int -> [Vector]
bezier p0 p1 p2 n = [ p1 .+ (((1-t)^2) .* (p0 .+ (negar p1))) .+ ((t^2) .* (p2 .+ (negar p1))) | t <- ts]
  where ts = 0:map (divF n) [1..n]
        divF :: Int -> Int -> Float
        divF j i = toEnum i / toEnum j

--Extras con Lineas:

--extra1
extra1 :: FloatingPic
extra1 a b c = line $ map (a V.+) [((0/8) V.* b) V.+ ((8/8) V.* c), zero,
                                   ((1/8) V.* b) V.+ ((7/8) V.* c), zero,
                                   ((2/8) V.* b) V.+ ((6/8) V.* c), zero,
                                   ((3/8) V.* b) V.+ ((5/8) V.* c), zero,
                                   ((4/8) V.* b) V.+ ((4/8) V.* c), zero,
                                   ((5/8) V.* b) V.+ ((3/8) V.* c), zero,
                                   ((6/8) V.* b) V.+ ((2/8) V.* c), zero,
                                   ((7/8) V.* b) V.+ ((1/8) V.* c), zero,
                                   ((8/8) V.* b) V.+ ((0/8) V.* c),
                                   ((0/8) V.* b) V.+ ((8/8) V.* c),
                                   ((0/8) V.* b) V.+ ((6/8) V.* c),
                                   ((6/8) V.* b) V.+ ((0/8) V.* c),
                                   ((4/8) V.* b) V.+ ((0/8) V.* c),
                                   ((0/8) V.* b) V.+ ((4/8) V.* c),
                                   ((0/8) V.* b) V.+ ((2/8) V.* c),
                                   ((2/8) V.* b) V.+ ((0/8) V.* c)] 

--extra2
extra2 :: FloatingPic
extra2 a b c = line $ map (a V.+) [((4/6) V.* b) V.+ ((0/6) V.* c),
                                  ((3/6) V.* b) V.+ ((3/6) V.* c),
                                  ((4/6) V.* c),
                                  ((1/6) V.* b) V.+ ((5/6) V.* c), 
                                  zero,
                                  ((3/6) V.* b) V.+ ((3/6) V.* c),
                                  zero,
                                  ((5/6) V.* b) V.+ ((1/6) V.* c),
                                  ((4/6) V.* b)]

--extra3
extra3 :: FloatingPic
extra3 a b c = line $ map (a V.+) [((0/6) V.* b) V.+ ((4/6) V.* c),
                                  ((1/6) V.* b) V.+ ((5/6) V.* c),
                                  ((0/6) V.* b) V.+ ((2/6) V.* c),
                                  ((2/6) V.* b) V.+ ((4/6) V.* c),
                                  zero,
                                  ((3/6) V.* b) V.+ ((3/6) V.* c),
                                  zero,
                                  ((4/6) V.* b) V.+ ((2/6) V.* c),
                                  ((2/6) V.* b) V.+ ((0/6) V.* c),
                                  ((5/6) V.* b) V.+ ((1/6) V.* c),
                                  ((4/6) V.* b) V.+ ((0/6) V.* c)]                                   

--extra4
extra4 :: FloatingPic
extra4 a b c = pictures [extra40 a b c, extra41 a b c, extra42 a b c, extra43 a b c, extra44 a b c] 

extra40 :: FloatingPic
extra40 a b c = line $ map (a V.+) [((2/8) V.* b) V.+ ((6/8) V.* c),
                                  ((0/8) V.* b) V.+ ((4/8) V.* c),
                                  ((2/8) V.* b) V.+ ((4/8) V.* c),
                                  ((3/8) V.* b) V.+ ((5/8) V.* c),
                                  ((2/8) V.* b) V.+ ((4/8) V.* c),
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((0/8) V.* b) V.+ ((2/8) V.* c),
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((2/8) V.* b) V.+ ((0/8) V.* c),
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((4/8) V.* b) V.+ ((2/8) V.* c),
                                  ((5/8) V.* b) V.+ ((3/8) V.* c),
                                  ((4/8) V.* b) V.+ ((2/8) V.* c),
                                  ((4/8) V.* b) V.+ ((0/8) V.* c),
                                  ((6/8) V.* b) V.+ ((2/8) V.* c)] 

extra41 :: FloatingPic
extra41 a b c = line $ map (a V.+) [((0/8) V.* b) V.+ ((6/8) V.* c),
                                  ((1/8) V.* b) V.+ ((7/8) V.* c)] 

extra42 :: FloatingPic
extra42 a b c = line $ map (a V.+) [((6/8) V.* b) V.+ ((0/8) V.* c),
                                  ((7/8) V.* b) V.+ ((1/8) V.* c)]

extra43 :: FloatingPic
extra43 a b c = line $ map (a V.+) [((0/8) V.* b) V.+ ((1/8) V.* c),
                                  ((1/8) V.* b) V.+ ((0/8) V.* c)]

extra44 :: FloatingPic
extra44 a b c = line $ map (a V.+) [((7/16) V.* b) V.+ ((9/16) V.* c),
                                  ((7/16) V.* b) V.+ ((7/16) V.* c),
                                  ((9/16) V.* b) V.+ ((7/16) V.* c)] 

--extra5
extra5 :: FloatingPic
extra5 a b c = pictures [extra50 a b c, extra51 a b c]

extra50 :: FloatingPic
extra50 a b c = line $ map (a V.+) [zero,
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((0/8) V.* b) V.+ ((2/8) V.* c),
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((0/8) V.* b) V.+ ((4/8) V.* c),
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((0/8) V.* b) V.+ ((6/8) V.* c),
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((0/8) V.* b) V.+ ((8/8) V.* c),
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((2/8) V.* b) V.+ ((6/8) V.* c),
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((4/8) V.* b) V.+ ((4/8) V.* c),
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((6/8) V.* b) V.+ ((2/8) V.* c),
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((8/8) V.* b) V.+ ((0/8) V.* c),
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((6/8) V.* b) V.+ ((0/8) V.* c),
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((4/8) V.* b) V.+ ((0/8) V.* c),
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((2/8) V.* b) V.+ ((0/8) V.* c),
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((3/8) V.* b) V.+ ((5/8) V.* c),
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((5/8) V.* b) V.+ ((3/8) V.* c),
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((7/8) V.* b) V.+ ((1/8) V.* c),
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((1/8) V.* b) V.+ ((7/8) V.* c),
                                  ((2/8) V.* b) V.+ ((2/8) V.* c),
                                  ((0/8) V.* b) V.+ ((8/8) V.* c),
                                  ((8/8) V.* b) V.+ ((0/8) V.* c),
                                  ((0/8) V.* b) V.+ ((6/8) V.* c),
                                  ((6/8) V.* b) V.+ ((0/8) V.* c),
                                  ((2/8) V.* b) V.+ ((0/8) V.* c),
                                  ((0/8) V.* b) V.+ ((2/8) V.* c)] 

extra51 :: FloatingPic
extra51 a b c = line $ map (a V.+) [((0/8) V.* b) V.+ ((2/8) V.* c),
                                   ((2/8) V.* b) V.+ ((4/8) V.* c),
                                   ((4/8) V.* b) V.+ ((2/8) V.* c),
                                   ((2/8) V.* b) V.+ ((0/8) V.* c),
                                   ((0/8) V.* b) V.+ ((2/8) V.* c),
                                   ((0/8) V.* b) V.+ ((4/8) V.* c),
                                   ((2/8) V.* b) V.+ ((6/8) V.* c),
                                   ((6/8) V.* b) V.+ ((2/8) V.* c),
                                   ((4/8) V.* b) V.+ ((0/8) V.* c),
                                   ((6/8) V.* b) V.+ ((0/8) V.* c),
                                   ((7/8) V.* b) V.+ ((1/8) V.* c),
                                   ((1/8) V.* b) V.+ ((7/8) V.* c),
                                   ((0/8) V.* b) V.+ ((6/8) V.* c),
                                   zero,
                                   ((8/8) V.* b) V.+ ((0/8) V.* c)]

--Extras2

--extra6
extra6 :: FloatingPic
extra6 a b c = pictures [(inv a b c), (b1 a b c), (b2 a b c),
                         color doradolight (estrella1 a b c), color dorado (estrella2 a b c), color doradolight (estrella3 a b c),
                         color doradolight (estrella4 a b c), color dorado (estrella5 a b c), color doradolight (estrella6 a b c),
                         color dorado (estrella7 a b c), color doradolight (estrella8 a b c),
                         color verde (o1 a b c), color verde (o2 a b c), color rojo (bo a b c), (c1 a b c), (c2 a b c)]
                         where dorado = makeColorI 239 184 16 255
                               doradolight = makeColorI 255 204 79 255
                               rojo = makeColorI 91 11 18 255
                               verde = makeColorI 63 102 0 255

inv :: FloatingPic
inv a b c = line $ map (a V.+) [((10/38) V.* c),
                                   ((2/38) V.* b) V.+ ((10/38) V.* c),
                                   ((2/38) V.* b) V.+ ((14/38) V.* c),
                                   ((6/38) V.* b) V.+ ((14/38) V.* c),
                                   ((6/38) V.* b) V.+ ((16/38) V.* c),
                                   ((16/38) V.* b) V.+ ((16/38) V.* c),
                                   ((16/38) V.* b) V.+ ((6/38) V.* c),
                                   ((14/38) V.* b) V.+ ((6/38) V.* c),
                                   ((14/38) V.* b) V.+ ((2/38) V.* c),
                                   ((10/38) V.* b) V.+ ((2/38) V.* c),
                                   ((10/38) V.* b),
                                   ((8/38) V.* b),
                                   ((8/38) V.* b) V.+ ((4/38) V.* c),
                                   ((10/38) V.* b) V.+ ((4/38) V.* c),
                                   ((10/38) V.* b) V.+ ((6/38) V.* c),
                                   ((12/38) V.* b) V.+ ((6/38) V.* c),
                                   ((12/38) V.* b) V.+ ((8/38) V.* c),
                                   ((8/38) V.* b) V.+ ((8/38) V.* c),
                                   ((8/38) V.* b) V.+ ((12/38) V.* c),
                                   ((6/38) V.* b) V.+ ((12/38) V.* c),
                                   ((6/38) V.* b) V.+ ((10/38) V.* c),
                                   ((4/38) V.* b) V.+ ((10/38) V.* c),
                                   ((4/38) V.* b) V.+ ((8/38) V.* c),
                                   ((8/38) V.* c),
                                   ((10/38) V.* c)]

b1 :: FloatingPic
b1 a b c = line $ map (a V.+) [((6/38) V.* b) V.+ ((16/38) V.* c),
                                   ((6/38) V.* b) V.+ ((18/38) V.* c),
                                   ((12/38) V.* b) V.+ ((18/38) V.* c),
                                   ((12/38) V.* b) V.+ ((20/38) V.* c),
                                   ((13/38) V.* b) V.+ ((20/38) V.* c),
                                   ((13/38) V.* b) V.+ ((17/38) V.* c),
                                   ((7/38) V.* b) V.+ ((17/38) V.* c),
                                   ((7/38) V.* b) V.+ ((16/38) V.* c),
                                   ((6/38) V.* b) V.+ ((16/38) V.* c)]

b2 :: FloatingPic
b2 a b c = line $ map (a V.+) [((16/38) V.* b) V.+ ((7/38) V.* c),
                                   ((17/38) V.* b) V.+ ((7/38) V.* c),
                                   ((17/38) V.* b) V.+ ((13/38) V.* c),
                                   ((20/38) V.* b) V.+ ((13/38) V.* c),
                                   ((20/38) V.* b) V.+ ((12/38) V.* c),
                                   ((18/38) V.* b) V.+ ((12/38) V.* c),
                                   ((18/38) V.* b) V.+ ((6/38) V.* c),
                                   ((16/38) V.* b) V.+ ((6/38) V.* c),
                                   ((16/38) V.* b) V.+ ((7/38) V.* c)]                                 

estrella1 :: FloatingPic
estrella1 a b c = polygon $ map (a V.+) [zero,
                                   ((6/38) V.* c),
                                   ((2/38) V.* b),
                                   zero] 

estrella2 :: FloatingPic
estrella2 a b c = polygon $ map (a V.+) [zero,
                                   ((2/38) V.* c),
                                   ((4/38) V.* b) V.+ ((4/38) V.* c),
                                   ((2/38) V.* b),
                                   zero] 

estrella3 :: FloatingPic
estrella3 a b c = polygon $ map (a V.+) [zero,
                                   ((6/38) V.* b),
                                   ((2/38) V.* c),
                                   zero]

estrella4 :: FloatingPic
estrella4 a b c = polygon $ map (a V.+) [((16/38) V.* b) V.+ ((22/38) V.* c),
                                      ((19/38) V.* b) V.+ ((19/38) V.* c),
                                      ((18/38) V.* b) V.+ ((18/38) V.* c),
                                      ((16/38) V.* b) V.+ ((22/38) V.* c)]

estrella5 :: FloatingPic
estrella5 a b c = polygon $ map (a V.+) [((15/38) V.* b) V.+ ((19/38) V.* c),
                                      ((18/38) V.* b) V.+ ((20/38) V.* c),
                                      ((19/38) V.* b) V.+ ((19/38) V.* c),
                                      ((18/38) V.* b) V.+ ((18/38) V.* c),
                                      ((15/38) V.* b) V.+ ((19/38) V.* c)]

estrella6 :: FloatingPic
estrella6 a b c = polygon $ map (a V.+) [((16/38) V.* b) V.+ ((16/38) V.* c),
                                      ((18/38) V.* b) V.+ ((20/38) V.* c),
                                      ((20/38) V.* b) V.+ ((18/38) V.* c),
                                      ((16/38) V.* b) V.+ ((16/38) V.* c)]

estrella7 :: FloatingPic
estrella7 a b c = polygon $ map (a V.+) [((18/38) V.* b) V.+ ((18/38) V.* c),
                                         ((19/38) V.* b) V.+ ((19/38) V.* c),
                                         ((20/38) V.* b) V.+ ((18/38) V.* c),
                                         ((19/38) V.* b) V.+ ((15/38) V.* c),
                                         ((18/38) V.* b) V.+ ((18/38) V.* c)]

estrella8 :: FloatingPic
estrella8 a b c = polygon $ map (a V.+) [((19/38) V.* b) V.+ ((19/38) V.* c),
                                         ((22/38) V.* b) V.+ ((16/38) V.* c),
                                         ((18/38) V.* b) V.+ ((18/38) V.* c),
                                         ((19/38) V.* b) V.+ ((19/38) V.* c)] 

o1 :: FloatingPic
o1 a b c = polygon $ map (a V.+) [((11/38) V.* b) V.+ ((15/38) V.* c),
                                    ((12/38) V.* b) V.+ ((15/38) V.* c),
                                    ((12/38) V.* b) V.+ ((14/38) V.* c),
                                    ((11/38) V.* b) V.+ ((15/38) V.* c)]
o2 :: FloatingPic
o2 a b c = polygon $ map (a V.+) [((14/38) V.* b) V.+ ((12/38) V.* c),
                                    ((15/38) V.* b) V.+ ((12/38) V.* c),
                                    ((15/38) V.* b) V.+ ((11/38) V.* c),
                                    ((15/38) V.* b) V.+ ((12/38) V.* c)]

bo :: FloatingPic
bo a b c = polygon $ map (a V.+) [((9/38) V.* b) V.+ ((12/38) V.* c),
                                    ((12/38) V.* b) V.+ ((12/38) V.* c),
                                    ((12/38) V.* b) V.+ ((9/38) V.* c),
                                    ((9/38) V.* b) V.+ ((12/38) V.* c)]

c1 :: FloatingPic
c1 a b c = line $ map (a V.+) [((13/38) V.* b) V.+ ((15/38) V.* c),
                                  ((13/38) V.* b) V.+ ((14/38) V.* c)]

c2 :: FloatingPic
c2 a b c = line $ map (a V.+) [((14/38) V.* b) V.+ ((13/38) V.* c),
                                  ((15/38) V.* b) V.+ ((13/38) V.* c)]

-- extra7 
extra7 :: FloatingPic
extra7 a b c = pictures [color tucoise (a1 a b c),color amarillito  (a2 a b c),
                         color ladrido (a3 a b c),color tucoise (a4 a b c),
                         color naranjudo (a5 a b c),color amarillito (a6 a b c),
                         color tucoise (a7 a b c),color amarillito (a8 a b c),
                         color tucoise (a9 a b c),color amarillito (a10 a b c)]
               where ladrido = makeColorI 189 39 0 255
                     naranjudo = makeColorI 255 77 0 255
                     tucoise = makeColorI 0 168 123 255
                     mgreen = makeColorI 46 88 14 255
                     amarillito = makeColorI 249 177 32 255


a1 :: FloatingPic
a1 a b c = polygon $ map (a V.+) [((0/24) V.* b) V.+ ((4/24) V.* c),
                                  ((0/24) V.* b) V.+ ((12/24) V.* c),
                                  ((4/24) V.* b) V.+ ((8/24) V.* c),
                                  ((0/24) V.* b) V.+ ((4/24) V.* c)]
a2 :: FloatingPic
a2 a b c = polygon $ map (a V.+) [((4/24) V.* b) V.+ ((0/24) V.* c),
                                  ((8/24) V.* b) V.+ ((4/24) V.* c),
                                  ((12/24) V.* b) V.+ ((0/24) V.* c),
                                  ((4/24) V.* b) V.+ ((0/24) V.* c)]                                
a3 :: FloatingPic
a3 a b c = polygon $ map (a V.+) [((2/24) V.* b) V.+ ((2/24) V.* c),
                                  ((2/24) V.* b) V.+ ((14/24) V.* c),
                                  ((14/24) V.* b) V.+ ((2/24) V.* c),
                                  ((2/24) V.* b) V.+ ((2/24) V.* c)]
a4 :: FloatingPic
a4 a b c = polygon $ map (a V.+) [((0/24) V.* b) V.+ ((14/24) V.* c),
                                  ((0/24) V.* b) V.+ ((18/24) V.* c),
                                  ((6/24) V.* b) V.+ ((18/24) V.* c),
                                  ((10/24) V.* b) V.+ ((14/24) V.* c),
                                  ((0/24) V.* b) V.+ ((14/24) V.* c)]
a5 :: FloatingPic
a5 a b c = polygon $ map (a V.+) [((6/24) V.* b) V.+ ((6/24) V.* c),
                                  ((6/24) V.* b) V.+ ((10/24) V.* c),
                                  ((10/24) V.* b) V.+ ((10/24) V.* c),
                                  ((10/24) V.* b) V.+ ((6/24) V.* c),
                                  ((6/24) V.* b) V.+ ((6/24) V.* c)]
a6 :: FloatingPic
a6 a b c = polygon $ map (a V.+) [((14/24) V.* b) V.+ ((0/24) V.* c),
                                  ((14/24) V.* b) V.+ ((10/24) V.* c),
                                  ((18/24) V.* b) V.+ ((6/24) V.* c),
                                  ((18/24) V.* b) V.+ ((0/24) V.* c),
                                  ((14/24) V.* b) V.+ ((0/24) V.* c)]
a7 :: FloatingPic
a7 a b c = polygon $ map (a V.+) [((3/24) V.* b) V.+ ((19/24) V.* c),
                                  ((3/24) V.* b) V.+ ((21/24) V.* c),
                                  ((5/24) V.* b) V.+ ((19/24) V.* c),
                                  ((3/24) V.* b) V.+ ((19/24) V.* c)]
a8 :: FloatingPic
a8 a b c = polygon $ map (a V.+) [((19/24) V.* b) V.+ ((5/24) V.* c),
                                  ((21/24) V.* b) V.+ ((3/24) V.* c),
                                  ((19/24) V.* b) V.+ ((3/24) V.* c),
                                  ((19/24) V.* b) V.+ ((5/24) V.* c)]
a9 :: FloatingPic
a9 a b c = polygon $ map (a V.+) [((0/24) V.* b) V.+ ((4/24) V.* c),
                                  ((2/24) V.* b) V.+ ((6/24) V.* c),
                                  ((2/24) V.* b) V.+ ((2/24) V.* c),
                                  ((0/24) V.* b) V.+ ((4/24) V.* c)]
a10 :: FloatingPic
a10 a b c = polygon $ map (a V.+) [((4/24) V.* b) V.+ ((0/24) V.* c),
                                  ((2/24) V.* b) V.+ ((2/24) V.* c),
                                  ((6/24) V.* b) V.+ ((2/24) V.* c),
                                  ((4/24) V.* b) V.+ ((0/24) V.* c)]

-- extra8
extra8 :: FloatingPic
extra8 a b c = line $ map (a V.+) [((0/10) V.* b) V.+ ((0/10) V.* c), ((10/10) V.* b) V.+ ((0/10) V.* c),
                                  ((10/10) V.* b) V.+ ((9/10) V.* c), ((1/10) V.* b) V.+ ((9/10) V.* c),
                                  ((1/10) V.* b) V.+ ((2/10) V.* c), ((8/10) V.* b) V.+ ((2/10) V.* c),
                                  ((8/10) V.* b) V.+ ((7/10) V.* c), ((3/10) V.* b) V.+ ((7/10) V.* c),
                                  ((3/10) V.* b) V.+ ((4/10) V.* c), ((6/10) V.* b) V.+ ((4/10) V.* c),
                                  ((6/10) V.* b) V.+ ((5/10) V.* c), ((4/10) V.* b) V.+ ((5/10) V.* c),
                                  ((4/10) V.* b) V.+ ((6/10) V.* c), ((7/10) V.* b) V.+ ((6/10) V.* c),
                                  ((7/10) V.* b) V.+ ((3/10) V.* c), ((2/10) V.* b) V.+ ((3/10) V.* c),
                                  ((2/10) V.* b) V.+ ((8/10) V.* c), ((9/10) V.* b) V.+ ((8/10) V.* c),
                                  ((9/10) V.* b) V.+ ((1/10) V.* c), ((0/10) V.* b) V.+ ((1/10) V.* c),
                                  ((0/10) V.* b) V.+ ((0/10) V.* c)]

-- extra9
extra9 :: FloatingPic
extra9 a b c = line $ map (a V.+) [((10/100) V.* b) V.+ ((10/100) V.* c), ((0/100) V.* b) V.+ ((30/100) V.* c),
                                  ((10/100) V.* b) V.+ ((10/100) V.* c), ((30/100) V.* b) V.+ ((0/100) V.* c),
                                  zero, ((0/100) V.* b) V.+ ((50/100) V.* c), ((10/100) V.* b) V.+ ((30/100) V.* c),
                                  ((30/100) V.* b) V.+ ((10/100) V.* c), ((50/100) V.* b) V.+ ((0/100) V.* c),
                                  ((80/100) V.* b) V.+ ((0/100) V.* c), ((60/100) V.* b) V.+ ((10/100) V.* c),
                                  ((40/100) V.* b) V.+ ((20/100) V.* c), ((20/100) V.* b) V.+ ((40/100) V.* c),
                                  ((10/100) V.* b) V.+ ((60/100) V.* c), ((0/100) V.* b) V.+ ((80/100) V.* c),
                                  zero, ((0/100) V.* b) V.+ c, ((20/100) V.* b) V.+ c, ((30/100) V.* b) V.+ ((80/100) V.* c),
                                  ((50/100) V.* b) V.+ ((60/100) V.* c), ((50/100) V.* b) V.+ ((50/100) V.* c),
                                  ((60/100) V.* b) V.+ ((50/100) V.* c), ((80/100) V.* b) V.+ ((30/100) V.* c),
                                  b V.+ ((20/100) V.* c), b V.+ ((0/100) V.* c), zero, ((0/100) V.* b) V.+ c,
                                  ((10/100) V.* b) V.+ ((80/100) V.* c), ((30/100) V.* b) V.+ ((50/100) V.* c),
                                  ((50/100) V.* b) V.+ ((30/100) V.* c), ((80/100) V.* b) V.+ ((10/100) V.* c),
                                  b V.+ ((0/100) V.* c)]