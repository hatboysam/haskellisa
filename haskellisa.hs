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

randomTris :: StdGen -> Int -> ([Tri Float], StdGen)
randomTris gen n = (groupThrees pointlist, gen1)
	where
		(pointlist, gen1) = nrandomsR (3*n) gen ((-1.0,-1.0),(1.0,1.0))
		groupThrees (a:b:c:xs) = (a,b,c):(groupThrees xs)
		groupThrees [] = []

zeros :: Color4 GLfloat
zeros = Color4 0 0 0 0

ones :: Color4 GLfloat
ones = Color4 1 1 1 1

randomColor4 :: StdGen -> (Color4 GLfloat, StdGen)
randomColor4 gen0 = randomR (zeros,ones) gen0

randomColor4s :: StdGen -> Int -> ([Color4 GLfloat], StdGen)
randomColor4s gen0 n = nrandomsR n gen0 (zeros,ones)

main :: IO ()
main = do
	(pname, _) <- getArgsAndInitialize
	createWindow $ "Haskellisa"
	blend $= Enabled
	blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
	displayCallback $= display
	mainLoop

display :: IO ()
display = do
	clear [ ColorBuffer ]
	gen0 <- getStdGen
	let (tris,gen1) = randomTris gen0 10
	let (cols,gen2) = randomColor4s gen1 10
	let triColPairs = zip tris cols
	mapM_ (\(tri, col) -> drawTri tri col) triColPairs
	flush
