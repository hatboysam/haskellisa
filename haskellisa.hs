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

drawRandomTri :: StdGen -> IO ()
drawRandomTri gen = do
	(tri, gen) <- randomTri gen
	(col, gen) <- randomColor4 gen
	drawTri tri col


randomCoord :: StdGen -> IO ((Coord Float, StdGen))
randomCoord gen = do
	return ((randomR ((-1.0,-1.0),(1.0,1.0)) gen) :: (Coord Float, StdGen))

randomTri :: StdGen -> IO ((Tri Float, StdGen))
randomTri gen0 = do
	(c0, gen1) <- randomCoord gen0
	(c1, gen2) <- randomCoord gen1
	(c2, gen3) <- randomCoord gen2
	return ((c0, c1, c2), gen3)

randomColor3 :: StdGen -> IO ((Color3 GLfloat, StdGen))
randomColor3 gen0 = do
	let (r, gen1) = (randomR (0.0, 1.0) gen0) :: (GLfloat, StdGen)
	let (g, gen2) = (randomR (0.0, 1.0) gen1) :: (GLfloat, StdGen)
	let (b, gen3) = (randomR (0.0, 1.0) gen2) :: (GLfloat, StdGen)
	return ((Color3 r g b), gen3)

randomColor4 :: StdGen -> IO (Color4 GLfloat, StdGen)
randomColor4 gen0 = do
	((Color3 r g b), gen1) <- randomColor3 gen0
	let (a, gen2) = (randomR (0.0, 1.0) gen1) :: (GLfloat, StdGen)
	return ((Color4 r g b a), gen2)


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
		gen <- getStdGen
		drawRandomTri gen
		-- End Random Code
		flush
