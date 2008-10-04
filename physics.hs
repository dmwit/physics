import Control.Arrow
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Time
import Graphics.UI.Gtk
import Physics.Hipmunk hiding (scale)
import Util
import View

groundShape = LineSegment (Vector (-10) 0.5) (Vector 10 (-0.5)) 0.1
addedShape  = Circle 1

main = do
    mSpace  <- makeSpace
    mBodies <- newMVar []
    view    <- makeView (addBody mSpace mBodies)
    now     <- getCurrentTime

    timeoutAddFull (redraw view mBodies >> yield >> return True) priorityLow 30
    forkIO . forever $ physics mSpace now >> yield
    mainGUI

makeSpace = do
    initChipmunk
    space  <- newSpace
    ground <- newBody infinity infinity
    line   <- newShape ground groundShape 0
    setGravity space (Vector 0 (-9.8))
    setPosition ground 0
    setFriction line 1
    spaceAdd space line
    newMVar space

addBody mSpace mBodies = do
    body   <- newBody 1 1
    circle <- newShape body addedShape 0

    setPosition body (Vector 0 10)
    setFriction circle 1

    space <- takeMVar mSpace
    spaceAdd space body
    spaceAdd space circle
    putMVar mSpace space

    modifyMVar_ mBodies (return . (body:))

numSteps start now old = floor (diffUTCTime now start / 0.01) - old
physics mSpace start = do
    space <- takeMVar mSpace
    steps <- liftM2 (numSteps start) getCurrentTime (getTimeStamp space)
    replicateM_ (min 100 $ castInt steps) $ step space 0.01
    putMVar mSpace space

redraw view mBodies = do
    bodies <- takeMVar mBodies
    ps     <- mapM (fmap castVector . getPosition) bodies
    thetas <- mapM (fmap castFloat  . getAngle   ) bodies
    putMVar mBodies bodies
    redrawView ps thetas view
