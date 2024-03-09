module Memory where

data Rect
  = Rect { width :: {-# UNPACK #-} !Int
         , height :: {-# UNPACK #-} !Int
         }

double :: Rect -> Rect
double Rect{width=x, height=y} = Rect{width=x*2, height=y*2}

myrect = Rect{width=12, height=24}

area = putStrLn ("Area: " ++ show (width myrect * height myrect))
