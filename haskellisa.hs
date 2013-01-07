import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Random
import Control.Monad
import Control.Monad.State

type Coord a = (a,a)
type Tri a = (Coord a, Coord a, Coord a)

instance (Random a, Random b) => Random (a,b) where
	randomR ((loX,loY), (hiX,hiY)) gen1 =
		let (x,gen2) = randomR (loX,hiX) gen1;
			(y,gen3) = randomR (loY,hiY) gen2;
		in ((x,y), gen3)
	random gen1 = 
		let (a1, gen2) = random gen1;
			(a2, gen3) = random gen2;
		in ((a1, a2), gen3)

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

--stateful randomness
srandom :: Random a => State StdGen a
srandom = state random

srandomR :: Random a => (a, a) -> State StdGen a
srandomR range = state $ randomR range

-- Draws a red triangle based on 3 points
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

-- Generate n random objects within a certain range, given a generator
nrandomsR :: Random a => Int -> StdGen -> (a,a) -> ([a], StdGen)
nrandomsR n gen range = runState (replicateM n randWithRange) gen
	where randWithRange = srandomR range

randomTri :: StdGen -> (Tri Float, StdGen)
randomTri gen = 
	let ([c0, c1, c2], gen1) = nrandomsR 3 gen ((-1.0,-1.0),(1.0,1.0))
	in ((c0,c1,c2), gen1)

randomColor4 :: StdGen -> (Color4 GLfloat, StdGen)
randomColor4 gen0 = randomR (zeros,ones) gen0
	where
		zeros = Color4 0 0 0 0 :: Color4 GLfloat
		ones = Color4 1 1 1 1 :: Color4 GLfloat

main :: IO ()
main = do
	(pname, _) <- getArgsAndInitialize
	createWindow $ "Haskellisa"
	displayCallback $= display
	mainLoop

display :: IO ()
display = do
	clear [ ColorBuffer ]
	-- Test Random Code
	gen0 <- getStdGen
	gen1 <- drawRandomTri gen0
	gen2 <- drawRandomTri gen1
	-- End Random Code
	flush
