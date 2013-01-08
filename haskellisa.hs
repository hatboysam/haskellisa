import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Random
import Control.Monad
import Control.Monad.State

type Coord a = (a,a)
type Tri a = (Coord a, Coord a, Coord a)
type GenState = State StdGen

-- Random Tuples
instance (Random a, Random b) => Random (a,b) where
	randomR ((loX,loY), (hiX,hiY)) gen1 =
		let (x,gen2) = randomR (loX,hiX) gen1;
			(y,gen3) = randomR (loY,hiY) gen2;
		in ((x,y), gen3)
	random gen1 = 
		let (a1, gen2) = random gen1;
			(a2, gen3) = random gen2;
		in ((a1, a2), gen3)

-- Random RGBA Colors
instance Random a => Random (Color4 a) where
	randomR ((Color4 r1 g1 b1 a1),(Color4 r2 g2 b2 a2)) gen0 =
		let (r,gen1) = randomR (r1,r2) gen0;
			(g,gen2) = randomR (g1,g2) gen1;
			(b,gen3) = randomR (b1,b2) gen2;
			(a,gen4) = randomR (a1,a2) gen3;
		in ((Color4 r g b a), gen4)
	random gen0 =
		let (r,gen1) = random gen0;
			(g,gen2) = random gen1;
			(b,gen3) = random gen2;
			(a,gen4) = random gen3;
		in ((Color4 r g b a), gen4)

{-
	(-1,1) ------------------------ (1,1)
	   |							  |
	   |							  |
	   |							  |
	   |							  |
	   |			(0,0)			  |
	   |							  |
	   |							  |
	   |							  |
	   |							  |
	(-1,-1) ------------------------ (1,-1)
-}

{-
Stateful Random Functions (explanation)
	state :: MonadState s m => (s -> (a,s)) -> m a
		Takes a function (s->(a,s)), returns State s a
	random :: (RandomGen g, Random a) => g -> (a, g)
		Takes a generator, returns (a,g)
		Stateful function for State g a
-}
srandom :: Random a => State StdGen a
srandom = state random

srandomR :: Random a => (a, a) -> State StdGen a
srandomR range = state $ randomR range

{-
Generate a list of Random objects
	Args:
		Int - number to generate
		(a,a) - lower bound, upper bound (like randomR)
		StdGen - generator
	Return:
		([a], StdGen) - random list, and new generator
-}
nrandomsR :: Random a => Int -> (a,a) -> StdGen -> ([a], StdGen)
nrandomsR n range gen = runState (replicateM n randWithRange) gen
	where randWithRange = srandomR range

drawTri :: Tri Float -> Color4 GLfloat -> IO ()
drawTri ((x1,y1), (x2,y2), (x3,y3)) col = do
	renderPrimitive Triangles $ do
		color col
		vertex $ (Vertex3 (x1 :: GLfloat) (y1 :: GLfloat) 0)
		vertex $ (Vertex3 (x2 :: GLfloat) (y2 :: GLfloat) 0)
		vertex $ (Vertex3 (x3 :: GLfloat) (y3 :: GLfloat) 0)

drawRandomTri :: StdGen -> IO (StdGen)
drawRandomTri gen0 = do
	let (tri, gen1) = randomTri gen0
	let (col, gen2) = randomColor4 gen1
	drawTri tri col
	return gen2

randomTri :: StdGen -> (Tri Float, StdGen)
randomTri gen = 
	let ([c0, c1, c2], gen1) = nrandomsR 3 ((-1.0,-1.0),(1.0,1.0)) gen
	in ((c0,c1,c2), gen1)

randomTris :: Int -> StdGen -> ([Tri Float], StdGen)
randomTris n gen = (groupThrees coordlist, gen1)
	where
		(coordlist, gen1) = nrandomsR (3*n) ((-1.0,-1.0),(1.0,1.0)) gen
		groupThrees (a:b:c:xs) = (a,b,c):(groupThrees xs)
		groupThrees [] = []


zeros :: Color4 GLfloat
zeros = Color4 0 0 0 0

ones :: Color4 GLfloat
ones = Color4 1 1 1 1

randomColor4 :: StdGen -> (Color4 GLfloat, StdGen)
randomColor4 gen0 = randomR (zeros,ones) gen0

randomColor4s :: Int -> StdGen -> ([Color4 GLfloat], StdGen)
randomColor4s n gen0 = nrandomsR n (zeros,ones) gen0

genRefresh :: GenState Int
genRefresh = do
	gen <- Control.Monad.State.get
	let (val, newGen) = random gen
	put newGen
	return val

main :: IO ()
main = do
	(pname, _) <- getArgsAndInitialize
	createWindow $ "Haskellisa"
	-- TODO set based on command line args
	windowSize $= (Size 640 480)
	blend $= Enabled
	blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
	displayCallback $= display
	keyboardMouseCallback $= Just (keyboardMouse)
	mainLoop

display :: IO ()
display = do
	clear [ ColorBuffer ]
	gen0 <- newStdGen
	let (tris,gen1) = randomTris 10 gen0
	let (cols,gen2) = randomColor4s 10 gen1
	let triColPairs = zip tris cols
	mapM_ (\(tri, col) -> drawTri tri col) triColPairs
	flush

keyboardMouse :: Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse k ks m p = do
	display
