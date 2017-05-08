module Sampling where

lineBatches w h = [[(u, v) | u <- [0..w-1]] | v <- [0..h-1]]

squareBatches w h count =
    [ [(u, v) | u <- [x0..x1-1], v <- [y0..y1-1]]
    | i <- [0..count-1], let (x0, y0, x1, y1) = batchWindow w h i count  ]

batchWindow  :: Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
batchWindow w h num count =
  let loop nx ny =
        if nx `mod` 2 == 0 && 2 * w * ny < h * nx
          then loop (nx `div` 2) (ny * 2)
          else (nx, ny)
      (nx, ny) = loop count 1
      (j, i) = num `quotRem` nx
      x0 = floor $ (fromIntegral i) / (fromIntegral nx) * (fromIntegral w)
      y0 = floor $ (fromIntegral j) / (fromIntegral ny) * (fromIntegral h)
      x1 = floor $ ((fromIntegral i) + 1) / (fromIntegral nx) * (fromIntegral w)
      y1 = floor $ ((fromIntegral j) + 1) / (fromIntegral ny) * (fromIntegral h)
  in  (x0, y0, x1, y1)
