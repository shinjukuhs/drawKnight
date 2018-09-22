{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.Bool
import System.Environment
import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)

debug :: Bool
debug = False

main :: IO ()
main = do
	args <- getArgs
	f <- openField
	topleft f
	t <- newTurtle f
	onkeypress f $ return . (/= 'q')
	case args of
		"hito" : _ -> check t hito
		"yari" : _ -> check t yari
		"tate" : _ -> check t tate
		"monshou" : _ -> check t $ \t p s -> do
			tate t p s
			monshou t p s
		"kote" : _ -> check t kote
		"kabuto" : _ -> check t kabuto
		"yaiba" : _ -> check t $ \t p s -> do
			yari t p s
			yaiba t p s
		"yoroi" : _ -> check t yoroi
		"isu" : _ -> check t isu
		"kura" : _ -> check t kura
		"uma" : _ -> check t uma
		"all" : _ -> forM_ [
			(hito, (135, 145), 50, "hito"),
			(yari, (50, 175), 50, "yari"),
			(tate, (235, 135), 30, "tate"),
			(monshou, (235, 135), 30, "monshou"),
			(kote, (88, 145), 30, "not_use"),
			(kote, (183, 145), 30, "kote"),
			(kabuto, (135, 65), 30, "kabuto"),
			(yaiba, (50, 175), 50, "yaiba"),
			(yoroi, (135, 140), 43, "yoroi"),
			(isu, (235, 240), 43, "isu"),
			(kura, (330, 95), 40, "kura"),
			(uma, (370, 194), 60, "uma") ] $ \(dr, p, s, fp) -> do
				dr t p s
				picture t ("svgs/get_" ++ fp ++ ".svg")
		_ -> return ()
	waitField f

picture :: Turtle -> FilePath -> IO ()
picture t fp = getSVG t >>= writeFile fp . showSVG 515 290 . map ("" ,)

check :: Turtle -> (Turtle -> (Double, Double) -> Double -> IO ()) -> IO ()
check t dr = dr t (100, 170) 50 >> dr t (350, 270) 100

hito :: Turtle -> (Double, Double) -> Double -> IO ()
hito t p@(x0, y0) s = do
	initialize t p s
	reference t p s
	goto t x0 (y0 - s * 100 / 100)
	fill t . circle t $ s * 50 / 100
	goto t (x0 - s * 49.44 / 100) (y0 + s * 80 / 100)
	fill t . replicateM_ 2 $ do
		forward t (s * 98.88 / 100) >> left t 90
		forward t (s * 160 / 100) >> left t 90
	goto t (x0 - s * 110 / 100) (y0 + s * 80 / 100)
	fill t . replicateM_ 2 $ do
		forward t (s * 30 / 100) >> left t 90
		forward t (s * 160 / 100) >> left t 90
	goto t (x0 + s * 80 / 100) (y0 + s * 80 / 100)
	fill t . replicateM_ 2 $ do
		forward t (s * 30 / 100) >> left t 90
		forward t (s * 160 / 100) >> left t 90
	goto t (x0 - s * 49.44 / 100) (y0 + s * 260 / 100)
	fill t . replicateM_ 2 $ do
		forward t (s * 40 / 100) >> left t 90
		forward t (s * 160 / 100) >> left t 90
	goto t (x0 + s * 9.44 / 100) (y0 + s * 260 / 100)
	fill t . replicateM_ 2 $ do
		forward t (s * 40 / 100) >> left t 90
		forward t (s * 160 / 100) >> left t 90
	initialize t p s

yari :: Turtle -> (Double, Double) -> Double -> IO ()
yari t p@(x0, y0) s = do
	initialize t p s
	reference t p s
	goto t (x0 - s * 10 / 100) (y0 + s * 200 / 100)
	fill t . replicateM_ 2 $ do
		forward t (s * 20 / 100) >> left t 90
		forward t (s * 400 / 100) >> left t 90
	goto t (x0 - s * 15 / 100) (y0 - s * 215 / 100)
	setheading t 0
	pendown t
	forward t (s * 30 / 100) >> left t 100
	forward t (s * 86.38 / 100) >> left t 160
	forward t (s * 86.38 / 100) >> left t 100
	initialize t p s

tate :: Turtle -> (Double, Double) -> Double -> IO ()
tate t p@(x0, y0) s = do
	initialize t p s
	reference t p s
	goto t (x0 - s * 100 / 100) (y0 + s * 100 / 100)
	fill t $ do
		setheading t (- 45)
		forward t (s * 141.42 / 100) >> left t 90
		forward t (s * 141.42 / 100) >> left t 45
		forward t (s * 200 / 100) >> left t 90
		forward t (s * 200 / 100) >> left t 90
		forward t (s * 200 / 100) >> left t 90
	initialize t p s

monshou :: Turtle -> (Double, Double) -> Double -> IO ()
monshou t p@(x0, y0) s = do
	initialize t p s
	pencolor t "white"
	forM_ [	(-1, -1),	(0, -1),	(1, -1),
		(-1,  0),			(1,  0),
		(-1,  1),	(0,  1),	(1,  1) ] $ \(dx, dy) -> do
		goto t (x0 + d * dx) (y0 + d * dy)
		fill t . circle t $ s * 10 / 100
	initialize t p s
	where d = s * 45 / 100

kote :: Turtle -> (Double, Double) -> Double -> IO ()
kote t p@(x0, y0) s = do
	initialize t p s
	reference t p s
	goto t (x0 - s * 50 / 100) (y0 + s * 100 / 100)
	nakanuki t s $ do
		forward t (s * 100 / 100) >> left t 90
		forward t (s * 40 / 100) >> left t 90
		forward t (s * 20 / 100) >> right t 90
		forward t (s * 160 / 100) >> left t 90
		forward t (s * 60 / 100) >> left t 90
		forward t (s * 160 / 100) >> right t 90
		forward t (s * 20 / 100) >> left t 90
		forward t (s * 40 / 100) >> left t 90
	initialize t p s

kabuto :: Turtle -> (Double, Double) -> Double -> IO ()
kabuto t p@(x0, y0) s = do
	initialize t p s
	reference t p s
	goto t (x0 - s) (y0 + s / 2)
	nakanuki t s $ do
		forward t $ s * (30 / 100)
		left t 45
		arc t False (s * 57 / 100) 45
		setheading t (- 22.5)
		arc t False (s * 137.5 / 100) 45
		setheading t (- 90)
		arc t False (s * 57 / 100) 45
		setheading t 0
		forward t $ s * (30 / 100)
		setheading t 90
		arc t False (s * 339 / 100) 45
		setheading t (- 135)
		arc t False (s * 339 / 100) 45
	goto t x0 (y0 - s * 190 / 100)
	pendown t
	pencolor t "black"
	goto t x0 (y0 + s * 17 / 100)
	initialize t p s

yaiba :: Turtle -> (Double, Double) -> Double -> IO ()
yaiba t p@(x0, y0) s = do
	initialize t p s
	goto t (x0 - s * 85 / 100) (y0 - s * 200 / 100)
	setheading t (- 10)
	pendown t
	forward t (s * 57.59 / 100) >> left t 100
	forward t (s * 20 / 100) >> left t 100
	forward t (s * 57.59 / 100) >> left t 160
	penup t

	goto t (x0 + s * 85 / 100) (y0 - s * 200 / 100)
	setheading t (- 170)
	pendown t
	forward t (s * 57.59 / 100) >> right t 100
	forward t (s * 20 / 100) >> right t 100
	forward t (s * 57.59 / 100) >> right t 160

	initialize t p s

yoroi :: Turtle -> (Double, Double) -> Double -> IO ()
yoroi t p@(x0, y0) s = do
	initialize t p s
	reference t p s
	goto t (x0 - s * 62 / 100) (y0 + s * 75 / 100)
	pendown t
	nakanuki t s $ do
		arc t True (s * 40 / 100) 90
		setheading t 0
		forward t $ s * 44 / 100
		setheading t 90
		arc t True (s * 40 / 100) 90
		setheading t 90
	--	forward t $ s * 85.2 / 100
		forward t $ s * 122.6 / 100
		setheading t 135
	--	arc t True (s * 105.8 / 100) 45
		arc t True (s * 52.9 / 100) 45
		setheading t 180
	--	forward t $ s * 10.35 / 100
		forward t $ s * 25.85 / 100
		setheading t (- 90)
		arc t True (s * 20.65 / 100) 180
		setheading t 180
	--	forward t $ s * 10.35 / 100
		forward t $ s * 25.85 / 100
		setheading t (- 90)
	--	arc t True (s * 105.8 / 100) 45
		arc t True (s * 52.9 / 100) 45
		setheading t (- 90)
		forward t $ s * 122.6 / 100
	initialize t p s

isu :: Turtle -> (Double, Double) -> Double -> IO ()
isu t p@(x0, y0) s = do
	initialize t p s
	reference t p s
	goto t (x0 - s * 60 / 100) (y0 + s * 20 / 100)
	pendown t
	beginfill t
	replicateM_ 2 $ do
		forward t (s * 110 / 100) >> left t 90
		forward t (s * 20 / 100) >> left t 90
	endfill t
	penup t
	goto t (x0 - s * 50 / 100) y0
	pendown t
	beginfill t
	replicateM_ 2 $ do
		forward t (s * 20 / 100) >> left t 105
		forward t (s * 100 / 100) >> left t 75
	endfill t
	penup t
	goto t (x0 - s * 45 / 100) (y0 + s * 20 / 100)
	pendown t
	beginfill t
	replicateM_ 2 $ do
		forward t (s * 12 / 100) >> right t 105
		forward t (s * 70 / 100) >> right t 75
	endfill t
	penup t
	goto t (x0 + s * 25 / 100) (y0 + s * 20 / 100)
	pendown t
	beginfill t
	replicateM_ 2 $ do
		forward t (s * 12 / 100) >> right t 90
		forward t (s * 70 / 100) >> right t 90
	endfill t
	initialize t p s

kura :: Turtle -> (Double, Double) -> Double -> IO ()
kura t p@(x0, y0) s = do
	initialize t p s
	reference t p s
	goto t (x0 - s) (y0 + s * 50 / 100)
	pendown t
	forward t $ s * 150 / 100
	setheading t 90
	forward t $ s * 50 / 100
	arc t True (s * 100 / 100) 45
	setheading t (- 135)
	arc t True (s * 110 / 100) 90
	setheading t (- 135)
	arc t False (s * 100 / 100) 45
	forward t $ s * 50 / 100
	penup t
	setheading t 90
	goto t (x0 + s * 50 / 100) (y0 + s * 25 / 100)
	pendown t
	beginfill t
	forward t $ s * 25 / 100
	arc t True (s * 100 / 100) 45
	setheading t (- 45)
	arc t True (s * 100 / 100) 45
	forward t $ s * 25 / 100
	right t 90
	forward t $ s * 52 / 100
	endfill t
	initialize t p s

uma :: Turtle -> (Double, Double) -> Double -> IO ()
uma t p@(x0, y0) s = do
	initialize t p s
	reference t p s
	goto t (x0 - s * 100 / 100) (y0 + s * 50 / 100)
	fill t . replicateM_ 2 $ do
		forward t (s * 200 / 100) >> left t 90
		forward t (s * 100 / 100) >> left t 90
	goto t (x0 - s * 100 / 100) (y0 + s * 145 / 100)
	fill t . replicateM_ 2 $ do
		forward t (s * 50 / 100) >> left t 90
		forward t (s * 80 / 100) >> left t 90
	goto t (x0 + s * 50 / 100) (y0 + s * 145 / 100)
	fill t . replicateM_ 2 $ do
		forward t (s * 50 / 100) >> left t 90
		forward t (s * 80 / 100) >> left t 90
	goto t (x0 - s * 166.96 / 100) (y0 - s * 20 / 100)
	fill t $ do
		forward t (s * 51.96 / 100) >> left t 90
		forward t (s * 30 / 100) >> left t 120
		forward t (s * 60 / 100) >> left t 150
	goto t (x0 + s * 110 / 100) (y0 - s * 35 / 100)
	setheading t 45
	fill t . replicateM_ 2 $ do
		forward t (s * 100 / 100) >> left t 90
		forward t (s * 45 / 100) >> left t 90
	goto t (x0 + s * 155.5 / 100) (y0 - s * 151.2 / 100)
	setheading t (- 45)
	fill t $ do
		forward t (s * 103.92 / 100) >> left t 150
		forward t (s * 120 / 100) >> left t 120
		forward t (s * 60 / 100) >> left t 90
	initialize t p s

fill :: Turtle -> IO () -> IO ()
fill t dr = pendown t >> beginfill t >> dr >> endfill t >> penup t

nakanuki :: Turtle -> Double -> IO () -> IO ()
nakanuki t s dr = do
	pencolor t "black"
	pensize t $ s * 20 / 100
	h0 <- heading t
	(x0, y0) <- position t
	pendown t
	dr
	penup t
	pencolor t "white"
	setheading t h0
	setx t x0
	sety t y0
	beginfill t
	dr
	endfill t
	pensize t $ s * 10 / 100
	pencolor t "black"

arc :: Turtle -> Bool -> Double -> Int -> IO ()
arc t lr r a = do
	let stp = r * 2 * pi * 5 / 360
	forward t (stp / 2) >> rot t 5
	replicateM_ (a `div` 5 - 1) $ forward t stp >> rot t 5
	forward t (stp / 2)
	where rot = bool left right lr

initialize, reference :: Turtle -> (Double, Double) -> Double -> IO ()
initialize t (x0, y0) s = do
	penup t
	pencolor t "black"
	pensize t (s / 10)
	setheading t 0
	goto t x0 y0

reference t p@(x0, y0) s = if not debug then return () else do
	initialize t p s
	pensize t 1
	pencolor t "gray"
	backward t s
	pendown t
	forward t $ 2 * s
	penup t
	setheading t 90
	penup t
	goto t x0 y0
	pencolor t (0xdf, 0xdf, 0xdf)
	backward t $ 2 * s
	pendown t
	forward t $ 4 * s
	penup t
	pencolor t "gray"
	goto t x0 y0
	backward t s
	pendown t
	forward t $ 2 * s
	initialize t p s
