{-# LANGUAGE NoImplicitPrelude #-}
import BasicPrelude
import qualified Numeric.LinearAlgebra as H
import qualified Data.Set as S

type Point = H.Vector Double
type Direction = H.Vector Double
type Face = [Point]
type Polyhedron = [Face]

cross::H.Vector Double->H.Vector Double->H.Vector Double
cross a b = 3 H.|> [ay*bz-az*by, az*bx-ax*bz, ax*by-ay*bx]
  where ax = a H.@> 0
        ay = a H.@> 1
        az = a H.@> 2
        bx = b H.@> 0
        by = b H.@> 1
        bz = b H.@> 2

ordNub::Ord a=>[a]->[a]
ordNub = S.toList . S.fromList

--translation of polyhedrons
translatePolyhedron::Point->Polyhedron->Polyhedron
translatePolyhedron p h = map (map trans) h
  where trans = H.add p

--scaling of polyhedrons
scalePolyhedron::Double->Polyhedron->Polyhedron
scalePolyhedron s h = map (map scale) h
  where scale = H.scale s

--test whether a given point is inside the hemispace defined by splitting
--space along a given plane
inHemiSpace::Direction->Point->Point->Bool
inHemiSpace d p x= d `H.dot` p <= d `H.dot` x

--Find a normal vector to a face
surfaceNormal::Face->Direction
surfaceNormal f = cross (first - second) (first - third)
  where first = head f
        second = head . tail $ f
        third = head . tail . tail $ f

vectorAverage::[H.Vector Double]->H.Vector Double
vectorAverage f = H.scale (1/(fromIntegral $ length f)) (foldr1 (+) f)

--Find the outward normal vectors of a convex polyhedron
convexOutwardNormals::Polyhedron->[Direction]
convexOutwardNormals h = map outwardNormal h
  where centroid = vectorAverage $ map vectorAverage h
        outwardNormal f = let v=surfaceNormal f in
                              if inHemiSpace v centroid (head f)
                                 then v
                                 else (-v)

--Check whether a point is inside a convex polyhedron
inConvexPolyhedron::Point->Polyhedron->Bool
inConvexPolyhedron p h =  and inSpaces || not (or inSpaces)
  where normals = convexOutwardNormals h
        inSpaces = map (\(f, n)->inHemiSpace n p (head f)) (zip h normals)

--Perform a sierpinski-style IFS replacement with translation and scaling
sierpinskiIFS::Polyhedron->Double->Double->[Polyhedron]
sierpinskiIFS h tscale hscale = map (flip translatePolyhedron scaled) translations
  where vertices = ordNub . concat $ h
        translations = map (H.scale tscale) vertices
        scaled = scalePolyhedron hscale h

sierpinskiTetrahedron::Integer->[Polyhedron]
sierpinskiTetrahedron = loop [tet0]
  where loop polys n
          | n <= 0 = polys
          | otherwise = loop (((\x->sierpinskiIFS x tscale hscale)) =<< polys) (n-1)
        tet0 = map (map H.fromList) [[p1, p2, p3],
                                     [p1, p2, p4],
                                     [p2, p3, p4],
                                     [p3, p1, p4]]
        p1 = [0, -1/(sqrt 3), 0]
        p2 = [0.5, 0.5/(sqrt 3), 0]
        p3 = [-0.5, 0.5/(sqrt 3), 0]
        p4 = [0, 0, sqrt (2/3)]
        tscale = 0.5
        hscale = 0.5

