import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Random

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


randomCoord :: StdGen -> (Coord Float, StdGen)
randomCoord gen = ((randomR ((-1.0,-1.0),(1.0,1.0)) gen) :: (Coord Float, StdGen))

randomTri :: StdGen -> (Tri Float, StdGen)
randomTri gen0 =
	let (c0, gen1) = randomCoord gen0;
		(c1, gen2) = randomCoord gen1;
		(c2, gen3) = randomCoord gen2;
	in ((c0, c1, c2), gen3)

randomColor3 :: StdGen -> (Color3 GLfloat, StdGen)
randomColor3 gen0 =
	let (r, gen1) = (randomR (0.0, 1.0) gen0) :: (GLfloat, StdGen);
		(g, gen2) = (randomR (0.0, 1.0) gen1) :: (GLfloat, StdGen);
		(b, gen3) = (randomR (0.0, 1.0) gen2) :: (GLfloat, StdGen);
	in ((Color3 r g b), gen3)

randomColor4 :: StdGen -> (Color4 GLfloat, StdGen)
randomColor4 gen0 =
	let ((Color3 r g b), gen1) = randomColor3 gen0;
		(a, gen2) = (randomR (0.0, 1.0) gen1) :: (GLfloat, StdGen);
	in ((Color4 r g b a), gen2)


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
