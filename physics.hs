import Control.Arrow
import Control.Monad
import Physics.Hipmunk

groundShape = LineSegment (Vector (-10) 0.5) (Vector 10 (-0.5)) 0.1
addedShape  = Circle 1

main = do
    space  <- makeSpace
    body   <- newBody 1 1
    circle <- newShape body addedShape 0

    setPosition body (Vector 0 10)
    spaceAdd space body
    spaceAdd space circle

    forever $ do
        (Vector x y) <- getPosition body
        putStr . show $ (x, y)
        step space 0.01
        getLine

makeSpace = do
    initChipmunk
    space  <- newSpace
    ground <- newBody infinity infinity
    line   <- newShape ground groundShape 0
    setGravity space (Vector 0 (-9.8))
    setPosition ground 0
    spaceAdd space line
    return space
