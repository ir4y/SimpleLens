import Test.HUnit
import SimpleLens
import Control.Applicative

-- Tests 
test_x'_lens = TestCase (do { assertEqual "x' getter" ((Point 1.0 2.0) ^. x') 1.0
                            ; assertEqual "x' setter" ((x' ^= 10) (Point 1.0 2.0)) (Point 10.0 2.0)
                            })

test_y'_lens = TestCase (do { assertEqual "y' getter" ((Point 1.0 2.0) ^. y') 2.0
                            ; assertEqual "y' setter" ((y' ^= 10) (Point 1.0 2.0)) (Point 1.0 10.0)
                            })

test_center'_lens = TestCase (do { assertEqual "center' getter" ((Circle (Point 1.0 2.0) 3.0) ^. center') (Point 1.0 2.0)
                                 ; assertEqual "center' setter" ((center' ^= (Point 10.0 10.0)) (Circle (Point 1.0 2.0) 3.0)) (Circle (Point 10.0 10.0) 3.0)
                                 })
 
test_radius'_lens = TestCase (do { assertEqual "radius' getter" ((Circle (Point 1.0 2.0) 3.0) ^. radius') 3.0
                                 ; assertEqual "radius' setter" ((radius' ^= 10.0) (Circle (Point 1.0 2.0) 3.0)) (Circle (Point 1.0 2.0) 10.0)
                                 })

test_points'_lens = TestCase (do { assertEqual "points' getter" ((Surface [(Point 1.0 2.0), (Point 3.0 4.0)]) ^. points') [(Point 1.0 2.0), (Point 3.0 4.0)]
                                 ; assertEqual "points' setter" ((points' ^= [(Point 0.0 0.0)]) (Surface [(Point 1.0 2.0), (Point 3.0 4.0)]))  (Surface [(Point 0.0 0.0)])
                                 })
                                   
test_composition = TestCase (do { assertEqual "get x' from Circle center'" ((Circle (Point 1.0 2.0) 3.0) ^. (center' <.> x')) 1.0
                                ; assertEqual "set x' of Circle center'" (((center' <.> x') ^= 10.0) (Circle (Point 1.0 2.0) 3.0)) (Circle (Point 10.0 2.0) 3.0)
                                ; assertEqual "increase x' of Circle center'" (((center' <.> x') %= (+10.0)) (Circle (Point 1.0 2.0) 3.0)) (Circle (Point 11.0 2.0) 3.0)
                                ; assertEqual "get y' of second Surface point" ((Surface [(Point 0.0 0.0), (Point 1.0 2.0)]) ^. (points' <.>  (at' 1) <.> y')) 2.0
                                ; assertEqual "set y' of second Surface point" (((points' <.>  (at' 1) <.> y') ^= 10.0) (Surface [(Point 0.0 0.0), (Point 1.0 2.0)])) (Surface [(Point 0.0 0.0), (Point 1.0 10.0)])
                                })

test_traversed = TestCase (do { assertEqual "get traversed x' over points'" ((Surface [(Point 0.0 0.0), (Point 1.0 2.0)]) ^. (points' `traversed` x')) [0.0, 1.0]
                              ; assertEqual "set traversed x' over points'" (((points' `traversed` x') ^= [9, 9]) (Surface [(Point 0.0 0.0), (Point 1.0 2.0)])) (Surface [(Point 9.0 0.0), (Point 9.0 2.0)])
                              ; assertEqual "increase traversed x' over points'" (((points' `traversed` x') %= ((<*>) [(+1)])) (Surface [(Point 0.0 0.0), (Point 1.0 2.0)])) (Surface [(Point 1.0 0.0), (Point 2.0 2.0)])
                              })

test_filtered = TestCase (
                let x_eq_0 = filtred (\p -> p^.x' == 0.0)
                    s = (Surface [(Point 0.0 0.0), (Point 1.0 2.0), (Point 0.0 9.0), (Point 5.0 5.0)])
                in
                do { assertEqual "get filtered x' over points'" (s ^. (points' `x_eq_0` x')) [0.0, 0.0]
                   ; assertEqual "set filtered x' over points'" (((points' `x_eq_0` x') ^= [9, 9]) s) (Surface [(Point 9.0 0.0), (Point 1.0 2.0), (Point 9.0 9.0), (Point 5.0 5.0)])
                   ; assertEqual "increase filtered x' over points'" (((points' `x_eq_0` x') %= ((<*>) [(+1)])) s) (Surface [(Point 1.0 0.0), (Point 1.0 2.0), (Point 1.0 9.0), (Point 5.0 5.0)])
                   })

tests = TestList [ TestLabel "Test x' lens" test_x'_lens
                 , TestLabel "Test y' lens" test_y'_lens
                 , TestLabel "Test center' lens" test_center'_lens
                 , TestLabel "Test radius' lens" test_radius'_lens
                 , TestLabel "Test points' lens" test_points'_lens
                 , TestLabel "Test composion" test_composition
                 , TestLabel "Test traversed" test_traversed
                 , TestLabel "Test filtered" test_filtered
                 ]

main = runTestTT tests
